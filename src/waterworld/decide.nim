## The decision layer: the per-turn loop that asks every seat what its skimmer
## goes for next, and ALWAYS has an answer.
##
## Cadence: one turn every `turnTicks` (72 ticks = 3.0 s of sim time), 24 turns
## per episode. At each turn the server builds ALL FOUR seats' request bodies and
## issues them as ONE parallel batch — waterworld is a simultaneous-decision
## game, so querying seats one after another would quadruple the wall clock for
## no gain. Four calls per turn x 24 turns = 96 calls per episode, at most 4 in
## flight.
##
## The binding constraint is not latency, it is the Bedrock sidecar's cap of 30
## requests per minute PER EPISODE: four requests per batch means a batch may
## start at most every 8 s, so `turnSpacingMs` is 12 000 (4 requests / 12 s =
## 20 rpm). That, not the model, is why there are 24 turns and why a turn is
## 3.0 s of sim time.
##
## DEGRADE, NEVER HANG. Every wait here is bounded: attempt 1 gets `attempt1Ms`,
## the single retry gets `retryMs`, the inter-batch floor is a bounded sleep, and
## the whole turn is wrapped in a monotonic `turnBudgetMs` deadline. A provider
## throttle with no other candidate model skips the retry outright (it cannot
## land). On a second failure the seat plays the `shoal` intent for that turn and
## a `fallback` record names the cause. No failure mode leaves a skimmer
## uncommanded: the controller always has an intent — this turn's, else last
## turn's, else `shoal`'s.

import std/[json, math, monotimes, os, strutils, times]

import curly

import sim_types, sim, roster, intents, baselines, control, llm, prompts, capture

type
  SeatPolicy* = object
    ## What one seat registered as. A seat that registers with neither field —
    ## or never registers at all — is `shoal`.
    isLlm*: bool
    prompt*: string
    model*: string
    baseline*: Baseline
    label*: string
    registered*: bool

  TurnFailure* = object
    ## Typed terminal failure for an LLM seat when inference or action parsing
    ## fails. Never substituted with a fallback action or default score.
    failed*: bool
    seat*: int
    kind*: string          ## "player_error" or "episode_inconclusive"
    cause*: string
    message*: string
    rawOutput*: string

  DecisionEngine* = object
    trajectory*: DecisionTrajectory
    selection*: PromptSelection
    client*: LlmClient
    ctl*: ControlState
    seats*: seq[SeatPolicy]
    intents*: seq[SkimmerIntent]
    haveIntent*: seq[bool]
    lastBatchStart*: MonoTime
    batchStarted*: bool
    llmOff*: bool            ## the budget guard fired
    lastFailure*: TurnFailure
    records*: seq[string]    ## chat records queued for the replay writer

proc initDecisionEngine*(sim: SimServer, selection: PromptSelection): DecisionEngine =
  result.trajectory = beginCapture("walker-waterworld", "walker-waterworld-" & $sim.config.seed)
  result.selection = selection
  result.client = newLlmClient(sim.config)
  result.ctl = initControlState()
  result.seats = newSeq[SeatPolicy](SkimmerCount)
  result.intents = newSeq[SkimmerIntent](SkimmerCount)
  result.haveIntent = newSeq[bool](SkimmerCount)
  for i in 0 ..< result.seats.len:
    result.seats[i].baseline = blShoal
    result.seats[i].label = "shoal"
    result.intents[i] = defaultIntent()

proc policyKind*(engine: DecisionEngine, seat: int): string =
  if seat >= 0 and seat < engine.seats.len and engine.seats[seat].isLlm:
    "llm"
  else:
    "scripted"

# ---------------------------------------------------------------------------
#  The per-seat view
# ---------------------------------------------------------------------------
# VIEW COORDINATES are the only coordinates a policy ever sees: metres with the
# origin at the tank's BOTTOM-LEFT corner, x right, y UP. Bearings are degrees
# counter-clockwise from east (0 = right, 90 = up). Every number is rounded to
# 2 decimals. Floats are legal here: this is the presentation of the sensor
# frame, not the frame itself.

proc viewX(um: int32): float = round(float(um) / 1_000_000.0, 2)
proc viewY(um: int32): float = round(float(int64(ArenaH) - int64(um)) / 1_000_000.0, 2)
proc metresPerSec(umPerTick: int32): float =
  round(float(umPerTick) * float(TargetFps) / 1_000_000.0, 2)
proc metres2(um: int32): float = round(float(um) / 1_000_000.0, 2)

proc viewBearing(dx, dy: int32): float =
  ## The view bearing of a SIM-space offset: y is negated because the sim runs
  ## y-down and a policy is told y-up.
  if dx == 0 and dy == 0:
    return 0.0
  var deg = arctan2(-float(dy), float(dx)) * 180.0 / PI
  if deg < 0.0:
    deg += 360.0
  round(deg, 1)

proc viewPoint(x, y: int32): JsonNode = %[viewX(x), viewY(y)]
proc viewVel(vx, vy: int32): JsonNode =
  %[metresPerSec(vx), metresPerSec(-vy)]

proc seatViewJson*(
  engine: DecisionEngine, sim: SimServer, seat, turnIndex: int,
  frame: SensorFrame
): string =
  ## Everything this seat may legitimately know, and nothing else. Built from
  ## the seat's OWN sensor frame: a plankton or poison particle appears only
  ## while its centre is inside 2.40 m of THIS skimmer, and `food_detected`
  ## never carries a total. No other seat's intent, note, say, prompt, latency,
  ## policy label or fallback state is here; neither is `perm`, the seed, the
  ## RNG state, the initial particle table or the variant name; and no real
  ## policy name ever is — skimmers are SKIM-1..SKIM-4 and nothing else.
  let
    skimmer = sim.skimmerForSeat(seat)
    me = sim.skimmers[skimmer]
    turns = sim.turnsPerEpisode()
    tick = sim.gameTicksElapsed()
    speed = int32(isqrt(int64(me.vx) * int64(me.vx) + int64(me.vy) * int64(me.vy)))

  var rays = newJArray()
  for n in 0 ..< SensorCount:
    let ray = frame.rays[n]
    var item = %*{
      "n": n,
      "deg": round(22.5 * float(n), 1),
      "k": $ray.kind
    }
    if ray.kind == rkClear:
      item["d"] = newJNull()
      item["closing"] = newJNull()
    else:
      item["d"] = %metres2(ray.distUm)
      item["closing"] = %metresPerSec(ray.closingUm)
    rays.add(item)

  var food = newJArray()
  for det in frame.food:
    food.add(%*{
      "id": foodId(int(det.index)),
      "deg": viewBearing(det.dx, det.dy),
      "d": metres2(det.distUm),
      "pos": viewPoint(det.x, det.y),
      "vel": viewVel(det.vx, det.vy),
      "closing": metresPerSec(det.closingUm)
    })
  var poison = newJArray()
  for det in frame.poison:
    poison.add(%*{
      "id": poisonId(int(det.index)),
      "deg": viewBearing(det.dx, det.dy),
      "d": metres2(det.distUm),
      "pos": viewPoint(det.x, det.y),
      "vel": viewVel(det.vx, det.vy),
      "closing": metresPerSec(det.closingUm)
    })
  var partners = newJArray()
  for det in frame.partners:
    partners.add(%*{
      "alias": skimmerAlias(int(det.index)),
      "deg": viewBearing(det.dx, det.dy),
      "d": metres2(det.distUm),
      "pos": viewPoint(det.x, det.y),
      "vel": viewVel(det.vx, det.vy),
      "stun_ticks": int(det.stun),
      "in_sensors": det.inSensors
    })

  var node = %*{
    "turn": turnIndex,
    "of": turns,
    "clock": {
      "tick": tick,
      "of": sim.config.maxTicks,
      "left_s": round(float(max(0, sim.config.maxTicks - tick)) /
        float(TargetFps), 1)
    },
    "you": {
      "alias": skimmerAlias(skimmer),
      "skimmer": skimmer,
      "pos": viewPoint(me.x, me.y),
      "vel": viewVel(me.vx, me.vy),
      "speed_m_s": metresPerSec(speed),
      "stun_ticks": int(me.stun),
      "max_speed_m_s": metresPerSec(MaxSkimmerSpeed),
      "radius_m": metres2(SkimmerRadius),
      "sensor_range_m": metres2(int32(sim.config.sensorRangeUm))
    },
    "tank": {
      "w": metres2(ArenaW),
      "h": metres2(ArenaH),
      "rock": {"c": viewPoint(RockCentreX, RockCentreY), "r": metres2(RockRadius)}
    },
    "sensors": rays,
    "food_detected": food,
    "poison_detected": poison,
    "partners": partners,
    "pod": {
      "score": sim.scoreDouble(),
      "captures": int(sim.captures),
      "target": sim.config.captureTarget,
      "nibbles": int(sim.nibbles),
      "poison_hits": int(sim.poisonHits),
      "thrust_cost": sim.thrustCostDouble()
    },
    "rules": {
      "coop_needed": sim.config.coopNeeded,
      "capture_points": 10.0,
      "nibble_points": 0.05,
      "poison_points": -2.0,
      "note": "two skimmers on one plankton AT THE SAME TICK is the only way " &
        "to catch it"
    }
  }
  if seat < engine.haveIntent.len and engine.haveIntent[seat]:
    let last = engine.intents[seat]
    node["your_last_intent"] = %*{
      "mode": $last.mode,
      "partner": (if last.partner >= 0: skimmerAlias(int(last.partner))
                  else: "none"),
      "target": (if last.target >= 0: foodId(int(last.target)) else: "none"),
      "waypoint": viewPoint(last.waypointXUm, last.waypointYUm),
      "lead_ticks": int(last.leadTicks),
      "standoff_m": round(float(last.standoffMm) / 1000.0, 2),
      "throttle": round(last.throttleFraction(), 2)
    }
  else:
    node["your_last_intent"] = newJNull()
  $node

# ---------------------------------------------------------------------------
#  The turn
# ---------------------------------------------------------------------------

proc resultRecord*(sim: SimServer): string =
  ## The `result` control record — the episode's whole results document,
  ## written once into the replay chat stream at episode end. It is what makes
  ## the replay SELF-SUFFICIENT: without it the outcome exists only at
  ## COGAME_RESULTS_URI, which a spectator holding the bytes cannot read. The
  ## document is already valid JSON, so it is embedded verbatim rather than
  ## re-parsed: nothing on the path to the artifact writes may raise.
  "{\"k\":\"result\",\"results\":" & sim.playerResultsJson() & "}"

proc scriptedFor*(
  engine: DecisionEngine, sim: SimServer, seat: int, kind: Baseline,
  frame: SensorFrame, turnIndex: int
): SkimmerIntent =
  scriptedIntent(sim, kind, sim.skimmerForSeat(seat), frame, turnIndex)

proc shoalFor*(
  engine: DecisionEngine, sim: SimServer, seat: int, frame: SensorFrame,
  turnIndex: int
): SkimmerIntent =
  ## The published `shoal` intent for explicit scripted seats.
  var intent = shoalIntent(sim, sim.skimmerForSeat(seat), frame, turnIndex)
  intent.source = isScripted
  intent

proc openSeats*(engine: DecisionEngine, sim: SimServer): seq[int] =
  ## The seats that need an LLM call this turn. Every one of them goes into ONE
  ## batch: `tests/test_engine.nim` asserts the length, which is the structural
  ## form of "seats are never queried sequentially".
  for seat in 0 ..< sim.seatCount():
    if engine.seats[seat].isLlm and not engine.llmOff and
        not engine.client.disabled:
      result.add(seat)

proc batchBodies*(
  engine: DecisionEngine, sim: SimServer, frames: openArray[SensorFrame],
  turnIndex, attempt: int
): seq[string] =
  ## The user message for every open seat, in seat order — one entry per
  ## request in the single parallel batch `turn` issues.
  for seat in engine.openSeats(sim):
    var user = engine.seatViewJson(sim, seat, turnIndex, frames[seat])
    if attempt > 0:
      user.add("\n\nYour previous reply was not usable. Reply with ONLY " &
        "the JSON object described above, starting with '{'.")
    result.add(userMessage(engine.seats[seat].prompt, user))

const FloorSliceMs = 100
  ## The inter-batch floor sleeps in slices this long so the engine's
  ## wall-clock stop is never more than one slice late (see `turn`).

proc waitOutInterBatchFloor*(
  engine: var DecisionEngine, sim: SimServer, turnIndex, elapsedSeconds: int
): int =
  ## Holds the START of consecutive batches `turnSpacingMs` apart, which pins
  ## the episode at <= 20 req/min against the sidecar's 30. Returns the
  ## milliseconds actually slept; the cert fixture sets the spacing to 0, so
  ## offline runs pay nothing.
  ##
  ## STOP-INTERRUPTIBLE, which is what the design note asks for: the floor is up
  ## to 12 s and the engine's wall-clock stop is only evaluated between ticks,
  ## so one `os.sleep` for the whole floor would carry the episode up to a floor
  ## past its own deadline. It sleeps in FloorSliceMs slices instead and cuts
  ## the floor short the moment the wall clock has run out -- the LLM cannot
  ## help an episode that is already over.
  if not engine.batchStarted or sim.config.turnSpacingMs <= 0:
    return 0
  let since = (getMonoTime() - engine.lastBatchStart).inMilliseconds.int
  let floorMs = min(sim.config.turnSpacingMs, sim.config.turnSpacingMs - since)
  if floorMs <= 0:
    return 0
  let floorStart = getMonoTime()
  while result < floorMs:
    sleep(min(FloorSliceMs, floorMs - result))
    result = (getMonoTime() - floorStart).inMilliseconds.int
    if elapsedSeconds + (result + 999) div 1000 >=
        sim.config.wallClockBudgetSeconds:
      echo "waterworld: wall-clock stop reached inside the inter-batch floor ",
        "on turn ", turnIndex, "; cutting the floor short after ", result, " ms"
      break

proc turn*(
  engine: var DecisionEngine,
  sim: SimServer,
  frames: openArray[SensorFrame],
  turnIndex: int,
  elapsedSeconds: int
): seq[string] =
  ## Runs ONE decision turn and installs each seat's intent. Returns the replay
  ## chat records this turn produced. On any LLM backend, invalid-model, or
  ## action-parsing failure, records the failure in `engine.lastFailure` WITHOUT
  ## substituting a fallback action or continuing with default gameplay scores.
  engine.lastFailure = TurnFailure()
  var observations: seq[JsonNode]
  for seat in 0 ..< sim.seatCount():
    observations.add(parseJson(engine.seatViewJson(sim, seat, turnIndex, frames[seat])))
  engine.client.throttled = false

  # --- budget guard: stop cleanly as inconclusive rather than substituting ---
  if not engine.llmOff:
    let turnSeconds =
      (sim.config.turnBudgetMs + sim.config.turnSpacingMs + 999) div 1000
    if elapsedSeconds + 2 * turnSeconds > sim.config.wallClockBudgetSeconds:
      engine.llmOff = true
      result.add(budgetGuardRecord(
        turnIndex, max(0, sim.config.wallClockBudgetSeconds - elapsedSeconds)))
      echo "waterworld: budget guard fired at turn ", turnIndex

  # --- which seats need a call? -------------------------------------------
  var open: seq[int]
  for seat in 0 ..< sim.seatCount():
    if engine.seats[seat].isLlm and not engine.llmOff and
        not engine.client.disabled:
      open.add(seat)
    elif engine.seats[seat].isLlm:
      let cause = if engine.llmOff: "budget_guard" else: "no_credentials"
      let msg =
        if engine.llmOff:
          "seat " & $seat & " turn " & $turnIndex &
            ": wall-clock budget guard fired before LLM turn"
        else:
          "seat " & $seat & " turn " & $turnIndex &
            ": LLM provider is unavailable (no endpoint or credentials)"
      result.add(fallbackRecord(turnIndex, seat, 1, cause, msg))
      echo "waterworld llm: ", msg
      if not engine.lastFailure.failed:
        engine.lastFailure = TurnFailure(
          failed: true,
          seat: seat,
          kind: "episode_inconclusive",
          cause: cause,
          message: msg
        )
    else:
      var intent = engine.scriptedFor(
        sim, seat, engine.seats[seat].baseline, frames[seat], turnIndex)
      intent.source = isScripted
      engine.intents[seat] = intent
      engine.haveIntent[seat] = true

  if engine.lastFailure.failed:
    if not engine.trajectory.isNil:
      for seat in 0 ..< sim.seatCount():
        if engine.seats[seat].isLlm:
          engine.trajectory.recordDecision($turnIndex & "-" & $seat, $seat,
            observations[seat], @[], none(string), newJNull(), asFallback,
            fallbackOrigin = none(string))
    return result

  # --- the rate floor -----------------------------------------------------
  if open.len > 0:
    discard engine.waitOutInterBatchFloor(sim, turnIndex, elapsedSeconds)
  if open.len > 0:
    engine.lastBatchStart = getMonoTime()
    engine.batchStarted = true

  var attempts = newSeq[seq[DecisionAttempt]](SkimmerCount)
  # --- ONE parallel batch per turn (no retries) ---------------------------
  if open.len > 0:
    let deadlineMs = sim.config.attempt1Ms
    var batch: RequestBatch
    var evidence: seq[DecisionAttempt]
    for position, seat in open:
      let user = engine.seatViewJson(sim, seat, turnIndex, frames[seat])
      let sys = systemPrompt(engine.selection, sim.config.captureTarget)
      let fullUser = userMessage(engine.seats[seat].prompt, user)
      let request = engine.client.requestFor(
        sys, fullUser, seat, engine.seats[seat].model)
      evidence.add(requestEvidence($turnIndex & "-" & $seat & "-0",
        engine.seats[seat].label, sys, fullUser, request.body))
      batch.post(request.url, request.headers, request.body, $seat)
    let started = getMonoTime()
    let responses = engine.client.curl.makeRequests(
      batch, max(1, deadlineMs div 1000))
    let latency = (getMonoTime() - started).inMilliseconds.int
    for position, seat in open:
      var receipt = evidence[position]
      receipt.latencyMs = some(float(latency))
      receipt.receivedEvidence(responses[position].response)
      var rawText = ""
      try:
        rawText = engine.client.textOf(
          responses[position].response, responses[position].error,
          batch[position].url)
        receipt.response = %rawText
        var intent = parseIntent(
          extractJsonObject(rawText),
          engine.intents[seat],
          engine.haveIntent[seat],
          sim.skimmerForSeat(seat))
        intent.source = isLlm
        intent.latencyMs = int32(latency)
        engine.intents[seat] = intent
        engine.haveIntent[seat] = true
        receipt.accepted = true
      except LlmError as error:
        receipt.rejectionReason = some(error.msg)
        let kind = if error.kind.len > 0: error.kind else: "episode_inconclusive"
        let cause = if error.cause.len > 0: error.cause else: "transport_error"
        let msg = ("seat " & $seat & " turn " & $turnIndex & " (" & cause &
          "): " & error.msg).truncateRunes(1800)
        result.add(fallbackRecord(turnIndex, seat, 1, cause, error.msg))
        echo "waterworld llm: ", msg
        if not engine.lastFailure.failed:
          engine.lastFailure = TurnFailure(
            failed: true,
            seat: seat,
            kind: kind,
            cause: cause,
            message: msg,
            rawOutput: rawText
          )
      except CatchableError as error:
        receipt.rejectionReason = some(error.msg)
        let rawPreview = rawText.replace("\r", " ").replace("\n", " ").truncateRunes(1000)
        let msg = ("seat " & $seat & " turn " & $turnIndex & " (parse_error): " &
          error.msg & (if rawPreview.len > 0: " | raw: " & rawPreview else: "")
          ).truncateRunes(1800)
        result.add(fallbackRecord(turnIndex, seat, 1, "parse_error",
          error.msg & (if rawPreview.len > 0: " | raw: " & rawPreview else: "")))
        echo "waterworld llm: ", msg
        if not engine.lastFailure.failed:
          engine.lastFailure = TurnFailure(
            failed: true,
            seat: seat,
            kind: "player_error",
            cause: "parse_error",
            message: msg,
            rawOutput: rawText
          )
      attempts[seat].add(receipt)

  if not engine.trajectory.isNil:
    for seat in 0 ..< sim.seatCount():
      let failedSeat = engine.lastFailure.failed and
        (not engine.haveIntent[seat] or
         (attempts[seat].len > 0 and not attempts[seat][^1].accepted))
      if failedSeat:
        engine.trajectory.recordDecision($turnIndex & "-" & $seat, $seat,
          observations[seat], attempts[seat], none(string), newJNull(),
          asFallback, fallbackOrigin = none(string))
        continue
      let intent = engine.intents[seat]
      let action = intent.intentRecord(turnIndex, seat, sim.skimmerForSeat(seat))
      var selected = none(string)
      var status = asFallback
      if intent.source == isLlm:
        status = asAccepted
        for receipt in attempts[seat].mitems:
          if receipt.accepted:
            receipt.parsedAction = action
            selected = some(receipt.attemptId)
      elif intent.source == isScripted:
        var receipt = newDecisionAttempt($turnIndex & "-" & $seat & "-scripted",
          engine.seats[seat].label, aoUnknown)
        receipt.accepted = true
        receipt.parsedAction = action
        selected = some(receipt.attemptId)
        attempts[seat].add(receipt)
        status = asAccepted
      engine.trajectory.recordDecision($turnIndex & "-" & $seat, $seat,
        observations[seat],
        attempts[seat], selected, action, status,
        fallbackOrigin = none(string))

proc intentFor*(
  engine: var DecisionEngine, sim: SimServer, seat: int,
  frame: SensorFrame, turnIndex: int
): SkimmerIntent =
  ## The intent the controller compiles this tick.
  if seat >= 0 and seat < engine.haveIntent.len and engine.haveIntent[seat]:
    return engine.intents[seat]
  engine.shoalFor(sim, max(0, seat), frame, turnIndex)
