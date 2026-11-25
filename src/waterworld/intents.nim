## The intent schema: what a skimmer's operator (LLM or scripted) may say, how a
## reply is parsed TOLERANTLY, and how an illegal field is repaired instead of
## rejected. Both policy kinds emit the SAME object, so the two are strictly
## comparable, one validator covers both, and a baseline is legal by
## construction.
##
## RUNE DISCIPLINE. Every cap here is measured in RUNES (Unicode codepoints) and
## every truncation lands on a rune boundary (`runeLen` / `runeSubStr`). Slicing
## a string by BYTE index anywhere on the path to the replay is forbidden: a
## byte-truncated multi-byte character renders fine in a browser and then fails a
## strict UTF-8 parser, which is exactly the class of bug that makes a replay
## unreadable to everything except the one viewer that happened to be lenient.
## `tests/test_intents.nim` pins it with a 4-byte emoji sitting on the boundary.

import std/[json, strutils, unicode]

import sim_types

type
  Mode* = enum
    ## What a skimmer is being told to go for, for the next turn. A CLOSED enum:
    ## an unrecognised mode keeps last turn's, else `sweep` — never nothing, so
    ## no skimmer is ever left uncommanded.
    mHunt = "hunt"
    mEscort = "escort"
    mSweep = "sweep"
    mHold = "hold"
    mAvoid = "avoid"

  IntentSource* = enum
    isLlm = "llm"
    isScripted = "scripted"
    isFallback = "fallback"

  SkimmerIntent* = object
    note*: string          ## <= MaxNoteRunes
    mode*: Mode
    target*: int32         ## plankton index, or -1 for "none"
    partner*: int32        ## skimmer index, or -1 for "none"
    waypointXUm*: int32    ## clamped into the tank, quantised to µm
    waypointYUm*: int32
    leadTicks*: int32      ## 0..24
    standoffMm*: int32     ## 0..2500, quantised to mm
    throttle255*: int32    ## 0..255
    say*: string           ## <= MaxSayRunes, sanitized
    source*: IntentSource
    latencyMs*: int32

  IntentError* = object of ValueError

const
  WaypointMinUm* = 300_000'i32
  WaypointMaxXUm* = ArenaW - 300_000'i32
  WaypointMaxYUm* = ArenaH - 300_000'i32
  MaxLeadTicks* = 24'i32
  MaxStandoffMm* = 2500'i32
  DefaultLeadTicks* = 6'i32
  DefaultStandoffMm* = 1200'i32
  DefaultThrottle255* = 255'i32

proc truncateRunes*(text: string, limit: int): string =
  ## Cuts `text` to at most `limit` RUNES, on a rune boundary. The single place
  ## any recorded string is shortened.
  if limit <= 0:
    return ""
  if text.runeLen <= limit:
    return text
  text.runeSubStr(0, limit)

proc sanitizeSay*(text: string): string =
  ## A skimmer's spectator-side line: capped at MaxSayRunes on a rune boundary
  ## FIRST, then run through the printable-ASCII shout filter. In that order the
  ## rune cut never leaves half a codepoint for the ASCII filter to smear.
  ##
  ## A LEADING brace is excluded: the replay chat stream carries the control
  ## records as JSON objects and tells them from a skimmer's line by a leading
  ## '{' (`replays.nim:251`, `roster.nim:50`), so a line that could start with
  ## one would make that discrimination ambiguous. Braces anywhere else are
  ## printable ASCII like any other character and are kept.
  result = ""
  for rune in text.truncateRunes(MaxSayRunes).runes:
    let value = int(rune)
    if value >= 32 and value < 127:
      result.add($rune)
  result = result.strip()
  while result.len > 0 and result[0] == '{':
    result = result[1 .. ^1].strip()

proc sanitizeNote*(text: string): string =
  ## The operator's own reasoning line, as it reaches the replay and the match
  ## feed. Newlines collapse to spaces so one record stays one line.
  text.replace("\n", " ").replace("\r", " ").strip().truncateRunes(MaxNoteRunes)

proc defaultIntent*(): SkimmerIntent =
  SkimmerIntent(
    note: "", mode: mSweep, target: -1, partner: -1,
    waypointXUm: ArenaW div 2'i32,
    waypointYUm: (ArenaH div 2'i32) - 1_600_000'i32,
    leadTicks: DefaultLeadTicks,
    standoffMm: DefaultStandoffMm,
    throttle255: DefaultThrottle255,
    say: "", source: isScripted, latencyMs: 0)

proc parseMode*(text: string): Mode =
  ## Strict lookup against the five native modes. Unknown or empty mode raises
  ## `IntentError` rather than silently substituting another action.
  let key = text.strip().toLowerAscii()
  for mode in Mode:
    if $mode == key:
      return mode
  raise newException(IntentError, "unknown mode: " & text.truncateRunes(64))

proc extractJsonObject*(text: string): JsonNode =
  ## Accepts either a single raw JSON object or a single markdown code fence
  ## wrapping one JSON object. Does NOT scrape `{...}` out of surrounding prose.
  let trimmed = text.strip()
  var body = trimmed
  if trimmed.startsWith("```"):
    let firstNl = trimmed.find('\n')
    if firstNl < 0:
      raise newException(IntentError, "unterminated code fence in reply")
    let header = trimmed[3 ..< firstNl].strip().toLowerAscii()
    if header.len > 0 and header != "json":
      raise newException(IntentError, "unsupported code fence language: " & header)
    let rest = trimmed[firstNl + 1 .. ^1]
    let closeIdx = rest.find("```")
    if closeIdx < 0:
      raise newException(IntentError, "missing closing code fence in reply")
    let afterClose = rest[closeIdx + 3 .. ^1].strip()
    if afterClose.len > 0:
      raise newException(IntentError, "unexpected text after closing code fence")
    body = rest[0 ..< closeIdx].strip()
  if body.len < 2 or body[0] != '{' or body[^1] != '}':
    var head = trimmed
    if head.runeLen > 160:
      head = head.truncateRunes(160) & "..."
    raise newException(IntentError,
      "reply must be a single JSON object (optionally in one code fence): " &
        head.replace("\n", " "))
  try:
    let node = parseJson(body)
    if node.kind != JObject:
      raise newException(IntentError, "reply JSON must be an object")
    return node
  except IntentError:
    raise
  except CatchableError as error:
    raise newException(IntentError, "invalid JSON object: " & error.msg)

proc readStrictNumber(node: JsonNode, field: string): float =
  ## One numeric field: a finite JSON int or float. No string/unit coercion.
  if node.isNil:
    raise newException(IntentError, field & " must be a number")
  case node.kind
  of JInt:
    float(node.getBiggestInt())
  of JFloat:
    let f = node.getFloat()
    if f != f or f > 1.0e12 or f < -1.0e12:
      raise newException(IntentError, field & " must be a finite number")
    f
  else:
    raise newException(IntentError, field & " must be a JSON number")

proc parseTargetId*(text: string): int32 =
  ## Parses `"F1".."F5"` or `"none"` (case-insensitive). Unknown target raises
  ## `IntentError`.
  let raw = text.strip().toLowerAscii()
  if raw.len == 0 or raw == "none":
    return -1
  if raw.len >= 2 and raw[0] == 'f':
    try:
      let value = parseInt(raw[1 .. ^1])
      if value >= 1 and value <= FoodCount:
        return int32(value - 1)
    except ValueError:
      discard
  raise newException(IntentError, "invalid target: " & text.truncateRunes(64))

proc parsePartnerAlias*(text: string, ownSkimmer: int): int32 =
  ## Parses `"SKIM-1".."SKIM-4"` or `"none"` (case-insensitive). Naming your own
  ## alias is `"none"` per the game rules; an unknown alias raises `IntentError`.
  let raw = text.strip().toLowerAscii()
  if raw.len == 0 or raw == "none":
    return -1
  if raw.startsWith("skim-"):
    try:
      let value = parseInt(raw[5 .. ^1])
      if value >= 1 and value <= SkimmerCount:
        if value - 1 == ownSkimmer:
          return -1
        return int32(value - 1)
    except ValueError:
      discard
  raise newException(IntentError, "invalid partner: " & text.truncateRunes(64))

proc parseIntent*(
  payload: JsonNode,
  previous: SkimmerIntent,
  havePrevious: bool,
  ownSkimmer: int
): SkimmerIntent =
  ## Turns one parsed reply into a legal intent. Requires a valid `"mode"`; all
  ## other fields are optional and default cleanly when omitted or null. Invalid
  ## types or values raise `IntentError` rather than silently substituting.
  if payload.isNil or payload.kind != JObject:
    raise newException(IntentError, "reply is not a JSON object")
  if not payload.hasKey("mode") or payload["mode"].kind != JString:
    raise newException(IntentError, "reply must include string field \"mode\"")

  result = defaultIntent()
  if havePrevious:
    result.waypointXUm = previous.waypointXUm
    result.waypointYUm = previous.waypointYUm
  result.source = isLlm
  result.latencyMs = 0
  result.mode = parseMode(payload["mode"].getStr())

  if payload.hasKey("note") and payload["note"].kind != JNull:
    if payload["note"].kind != JString:
      raise newException(IntentError, "note must be a string")
    result.note = sanitizeNote(payload["note"].getStr())

  if payload.hasKey("say") and payload["say"].kind != JNull:
    if payload["say"].kind != JString:
      raise newException(IntentError, "say must be a string")
    result.say = sanitizeSay(payload["say"].getStr())

  if payload.hasKey("target") and payload["target"].kind != JNull:
    if payload["target"].kind != JString:
      raise newException(IntentError, "target must be a string")
    result.target = parseTargetId(payload["target"].getStr())

  if payload.hasKey("partner") and payload["partner"].kind != JNull:
    if payload["partner"].kind != JString:
      raise newException(IntentError, "partner must be a string")
    result.partner = parsePartnerAlias(payload["partner"].getStr(), ownSkimmer)

  if payload.hasKey("waypoint") and payload["waypoint"].kind != JNull:
    let node = payload["waypoint"]
    var wx, wy: float
    if node.kind == JArray and node.len == 2:
      wx = readStrictNumber(node[0], "waypoint[0]")
      wy = readStrictNumber(node[1], "waypoint[1]")
    elif node.kind == JObject and node.hasKey("x") and node.hasKey("y"):
      wx = readStrictNumber(node["x"], "waypoint.x")
      wy = readStrictNumber(node["y"], "waypoint.y")
    else:
      raise newException(IntentError, "waypoint must be [x, y]")
    # View metres (origin bottom-left, y up) -> sim µm (origin top-left, y down).
    result.waypointXUm = int32(clamp(wx * 1_000_000.0,
      float(WaypointMinUm), float(WaypointMaxXUm)))
    result.waypointYUm = int32(clamp(float(ArenaH) - wy * 1_000_000.0,
      float(WaypointMinUm), float(WaypointMaxYUm)))

  if payload.hasKey("lead_ticks") and payload["lead_ticks"].kind != JNull:
    let val = readStrictNumber(payload["lead_ticks"], "lead_ticks")
    result.leadTicks = int32(clamp(val + 0.5, 0.0, float(MaxLeadTicks)))

  if payload.hasKey("standoff_m") and payload["standoff_m"].kind != JNull:
    let metres = readStrictNumber(payload["standoff_m"], "standoff_m")
    result.standoffMm = int32(clamp(metres * 1000.0, 0.0, float(MaxStandoffMm)))

  if payload.hasKey("throttle") and payload["throttle"].kind != JNull:
    let fraction = readStrictNumber(payload["throttle"], "throttle")
    result.throttle255 = int32(clamp(fraction * 255.0, 0.0, 255.0))

proc throttleFraction*(intent: SkimmerIntent): float {.inline.} =
  float(intent.throttle255) / 255.0

proc standoffMetres*(intent: SkimmerIntent): float {.inline.} =
  float(intent.standoffMm) / 1000.0

proc intentRecord*(
  intent: SkimmerIntent, turn, seat, skimmer: int
): JsonNode =
  ## The replay chat record for one turn's intent. Re-applied at playback into
  ## NON-HASHED sim fields only (the feed, the bubbles, the seat counters), so
  ## it can never affect the simulation.
  %*{
    "k": "intent",
    "turn": turn,
    "seat": seat,
    "alias": skimmerAlias(skimmer),
    "skimmer": skimmer,
    "source": $intent.source,
    "latency_ms": int(intent.latencyMs),
    "note": intent.note,
    "mode": $intent.mode,
    "target": (if intent.target >= 0: foodId(int(intent.target)) else: "none"),
    "partner": (if intent.partner >= 0: skimmerAlias(int(intent.partner))
                else: "none"),
    "waypoint": [int(intent.waypointXUm), int(intent.waypointYUm)],
    "lead_ticks": int(intent.leadTicks),
    "standoff_m": int(intent.standoffMm),
    "throttle": int(intent.throttle255),
    "say": intent.say
  }

proc intentAction*(intent: SkimmerIntent): JsonNode =
  ## A language action that round-trips the installed fixed-point intent.
  %*{"mode": $intent.mode,
    "target": (if intent.target >= 0: foodId(int(intent.target)) else: "none"),
    "partner": (if intent.partner >= 0: skimmerAlias(int(intent.partner)) else: "none"),
    "waypoint": [
      (float(intent.waypointXUm) + 0.5) / 1_000_000.0,
      (float(ArenaH - intent.waypointYUm) - 0.5) / 1_000_000.0],
    "lead_ticks": int(intent.leadTicks),
    "standoff_m": float(intent.standoffMm) / 1000.0 + 1e-9,
    "throttle": min(1.0, float(intent.throttle255) / 255.0 + 1e-9),
    "say": intent.say, "note": intent.note}

proc boundedIntentRecord*(
  intent: SkimmerIntent, turn, seat, skimmer: int
): string =
  ## The serialized intent record, guaranteed <= MaxIntentRecordRunes. The note
  ## is the only unbounded-in-practice field, so it is the one that shrinks; the
  ## cut still lands on a rune boundary. NEVER cut the serialized string — that
  ## would emit broken JSON, the exact failure the rune rule exists to prevent.
  var trimmed = intent
  result = $trimmed.intentRecord(turn, seat, skimmer)
  var guard = 0
  while result.runeLen > MaxIntentRecordRunes and guard < 12:
    inc guard
    trimmed.note = trimmed.note.truncateRunes(
      max(0, trimmed.note.runeLen - max(8, trimmed.note.runeLen div 2)))
    trimmed.say = trimmed.say.truncateRunes(
      max(0, trimmed.say.runeLen - max(4, trimmed.say.runeLen div 2)))
    result = $trimmed.intentRecord(turn, seat, skimmer)

proc registerRecord*(
  seat, skimmer: int, policy, kind, baseline: string
): string =
  ## The REDACTED registration record. The seat's PROMPT is never written:
  ## only the policy label, the kind, and which baseline a scripted seat picked.
  $(%*{
    "k": "register",
    "seat": seat,
    "alias": skimmerAlias(skimmer),
    "skimmer": skimmer,
    "policy": policy.truncateRunes(MaxPolicyLabelRunes),
    "kind": kind,
    "baseline": baseline
  })

proc fallbackRecord*(
  turn, seat, attempt: int, cause, detail: string
): string =
  $(%*{
    "k": "fallback",
    "turn": turn,
    "seat": seat,
    "attempt": attempt,
    "cause": cause,
    "detail": detail.truncateRunes(MaxFallbackDetailRunes)
  })

proc budgetGuardRecord*(turn, remainingSeconds: int): string =
  $(%*{"k": "budget_guard", "turn": turn, "remaining_s": remainingSeconds})
