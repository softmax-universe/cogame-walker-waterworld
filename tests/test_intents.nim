## Strict native action parsing (raw JSON or single code fence, no brace
## scraping, no silent repair of unknown/invalid actions) and RUNE-boundary
## discipline that keeps a replay readable by a strict UTF-8 parser.

import std/[json, strutils, unicode]

import helpers
import waterworld/[sim, intents]

var failures = 0
proc check(name: string, ok: bool, detail = "") =
  if not ok:
    inc failures
    echo "FAIL ", name, (if detail.len > 0: ": " & detail else: "")

proc parse(text: string, previous = defaultIntent(), have = false,
           own = 0): SkimmerIntent =
  parseIntent(extractJsonObject(text), previous, have, own)

proc rejects(text: string, own = 0): bool =
  try:
    discard parse(text, own = own)
    false
  except IntentError:
    true

block nativeJsonAndMinimalKeys:
  let minimal = parse("""{"mode":"hold"}""")
  check("minimal mode-only JSON parses without exhaustive keys", minimal.mode == mHold)
  check("omitted note and say default empty", minimal.note == "" and minimal.say == "")
  let hunt = parse("""{"mode":"hunt","target":"F2","throttle":0.8}""")
  check("hunt with target and throttle parses",
    hunt.mode == mHunt and hunt.target == 1 and hunt.throttle255 >= 200)

block singleFenceAccepted:
  let intent = parse("```json\n{\"mode\":\"escort\",\"partner\":\"SKIM-3\"}\n```")
  check("single json code fence parses", intent.mode == mEscort)
  check("the partner alias resolves", intent.partner == 2, $intent.partner)
  let bareFence = parse("```\n{\"mode\":\"avoid\"}\n```")
  check("single bare code fence parses", bareFence.mode == mAvoid)

block noBraceScrapingOrProse:
  check("prose-prefixed JSON is rejected",
    rejects("""Sure! Here is my order:
{"mode":"hunt","target":"F2","throttle":1.0}"""))
  check("trailing prose after JSON is rejected",
    rejects("""{"mode":"hunt","target":"F3"} trailing text"""))
  check("trailing junk object is rejected",
    rejects("""{"mode":"hold"} {"mode":"avoid"}"""))
  check("prose around code fence is rejected",
    rejects("Here you go:\n```json\n{\"mode\":\"hold\"}\n```"))
  check("text after code fence is rejected",
    rejects("```json\n{\"mode\":\"hold\"}\n```\nDone."))
  check("multiple code fences are rejected",
    rejects("```json\n{\"mode\":\"hold\"}\n```\n```json\n{\"mode\":\"avoid\"}\n```"))

block strictFieldValidation:
  check("unknown mode is rejected instead of repaired",
    rejects("""{"mode":"kamikaze"}"""))
  check("missing mode (note-only) is rejected",
    rejects("""{"note":"just thinking"}"""))
  check("non-string mode is rejected",
    rejects("""{"mode":1}"""))
  check("my own alias as partner is none per game rules",
    parse("""{"mode":"escort","partner":"SKIM-2"}""", own = 1).partner == -1)
  check("unknown partner alias is rejected",
    rejects("""{"mode":"escort","partner":"SKIM-9"}""", own = 0))
  check("another skimmer's alias resolves",
    parse("""{"mode":"escort","partner":"SKIM-2"}""", own = 0).partner == 1)
  check("out-of-set target is rejected",
    rejects("""{"mode":"hunt","target":"F9"}"""))
  check("none target is -1",
    parse("""{"mode":"hunt","target":"none"}""").target == -1)
  check("string throttle shim is rejected",
    rejects("""{"mode":"sweep","throttle":"0.5"}"""))
  check("unparseable throttle is rejected",
    rejects("""{"mode":"sweep","throttle":"abc"}"""))
  check("non-finite lead_ticks string is rejected",
    rejects("""{"mode":"hunt","lead_ticks":"NaN"}"""))
  check("malformed waypoint is rejected",
    rejects("""{"mode":"sweep","waypoint":[null,null]}"""))

block waypointAndClamping:
  let wpObj = parse("""{"mode":"sweep","waypoint":{"x":9.0,"y":6.0}}""")
  check("an {x,y} waypoint parses",
    abs(wpObj.waypointXUm - 9_000_000) < 20_000, $wpObj.waypointXUm)
  check("view y is flipped into sim y",
    abs(wpObj.waypointYUm - 2_000_000) < 20_000, $wpObj.waypointYUm)
  let clamped = parse("""{"mode":"sweep","lead_ticks":9000,"standoff_m":5.0,"throttle":2.0,
    "waypoint":[-40.0,900.0]}""")
  check("lead_ticks clamps to 24", clamped.leadTicks == 24, $clamped.leadTicks)
  check("standoff clamps to 2.5 m", clamped.standoffMm == MaxStandoffMm,
    $clamped.standoffMm)
  check("throttle clamps to full", clamped.throttle255 == 255, $clamped.throttle255)
  check("the waypoint clamps into the tank",
    clamped.waypointXUm >= WaypointMinUm and clamped.waypointXUm <= WaypointMaxXUm and
      clamped.waypointYUm >= WaypointMinUm and clamped.waypointYUm <= WaypointMaxYUm,
    $clamped.waypointXUm & "," & $clamped.waypointYUm)

block undetectedTarget:
  var sim = seatedSim()
  for f in 0 ..< FoodCount:
    sim.food[f].state = psRespawning
  let frame = sim.frameFor(0)
  check("an undetected plankton is not in the sensor frame",
    frame.foodDetection(1) < 0)

block noteCap:
  var long = ""
  for _ in 0 ..< 300:
    long.add("x")
  let intent = parse("""{"mode":"hold","note":"""" & long & """"}""")
  check("a 300-character note is cut to 160 runes",
    intent.note.runeLen == MaxNoteRunes, $intent.note.runeLen)

block runeBoundaryEmoji:
  var text = ""
  for _ in 0 ..< 47:
    text.add("a")
  text.add("\u{1F41F}")
  text.add("\u{1F41F}")
  check("the input really is 49 runes", text.runeLen == 49, $text.runeLen)
  let truncated = text.truncateRunes(MaxSayRunes)
  check("truncation lands on 48 runes", truncated.runeLen == 48,
    $truncated.runeLen)
  check("truncation lands on a rune boundary (validates as UTF-8)",
    truncated.validateUtf8() == -1, $truncated.validateUtf8())
  let record = $(%*{"say": truncated})
  check("the truncated string round-trips through parseJson",
    parseJson(record){"say"}.getStr() == truncated)
  check("the serialized record is valid UTF-8", record.validateUtf8() == -1)
  let sanitized = sanitizeSay(text)
  check("sanitizeSay never emits a partial codepoint",
    sanitized.validateUtf8() == -1)
  check("sanitizeSay strips a leading brace",
    not sanitizeSay("{oops").startsWith("{"))
  check("and only a LEADING one: an interior brace is printable ASCII",
    sanitizeSay("a {tight} squeeze") == "a {tight} squeeze",
    sanitizeSay("a {tight} squeeze"))
  check("a line of nothing but braces sanitizes to nothing",
    sanitizeSay("{{{") == "", sanitizeSay("{{{"))
  let mixed = sanitizeSay("hold \u{1F41F} it")
  check("sanitizeSay drops non-ASCII whole, never half a codepoint",
    mixed.validateUtf8() == -1 and "\u{1F41F}" notin mixed, mixed)

block noUsableField:
  check("a reply with no mode raises", rejects("""{"unrelated":1}"""))
  check("a reply with no JSON object at all raises", rejects("I would rather not."))

block recordCaps:
  var intent = defaultIntent()
  var long = ""
  for _ in 0 ..< 400:
    long.add("n")
  intent.note = long
  intent.say = long
  let record = intent.boundedIntentRecord(9, 2, 3)
  check("the serialized intent record is <= 600 runes",
    record.runeLen <= MaxIntentRecordRunes, $record.runeLen)
  check("the bounded record is still valid JSON",
    parseJson(record){"k"}.getStr() == "intent")
  let registered = registerRecord(0, 2, long, "llm", "shoal")
  check("register.policy is capped at 48 runes",
    parseJson(registered){"policy"}.getStr().runeLen == MaxPolicyLabelRunes)
  let fallback = fallbackRecord(3, 1, 2, "timeout", long)
  check("fallback.detail is capped at 200 runes",
    parseJson(fallback){"detail"}.getStr().runeLen == MaxFallbackDetailRunes)

if failures > 0:
  quit("test_intents: " & $failures & " failure(s)", 1)
echo "test_intents: ok"
