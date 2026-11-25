## Hosted calls must reach the native sidecar without provider credentials,
## and registry models must be selectable via config or per-seat override.
include "../src/waterworld/llm"

block:
  putEnv("COWORLD_LLM_ENDPOINT", "http://127.0.0.1:9100/")
  putEnv("COWORLD_LLM_MODEL", "anthropic/claude-sonnet-4.6")
  putEnv("AWS_ENDPOINT_URL_BEDROCK_RUNTIME", "http://retired.invalid")
  putEnv("ANTHROPIC_API_KEY", "local-key-must-not-be-used")
  let client = newLlmClient(GameConfig())
  for slot in 0 .. 1:
    let request = client.requestFor("rules", "private view", slot)
    doAssert request.url == "http://127.0.0.1:9100/v1/messages"
    doAssert request.headers["X-Coworld-Player-Slot"] == $slot
    let body = parseJson(request.body)
    doAssert body["model"].getStr() == "anthropic/claude-sonnet-4.6"
    doAssert not body.hasKey("anthropic_version")
    doAssert not body.hasKey("output_config")

  let perSeat = client.requestFor("rules", "private view", 0, "openai/gpt-4.1-mini")
  doAssert parseJson(perSeat.body)["model"].getStr() == "openai/gpt-4.1-mini"

  var cfg = GameConfig()
  cfg.model = "google/gemini-2.5-flash"
  let cfgClient = newLlmClient(cfg)
  let cfgReq = cfgClient.requestFor("rules", "private view", 0)
  doAssert parseJson(cfgReq.body)["model"].getStr() == "google/gemini-2.5-flash"
  let seatWins = cfgClient.requestFor("rules", "private view", 0, "anthropic/claude-haiku-4.5")
  doAssert parseJson(seatWins.body)["model"].getStr() == "anthropic/claude-haiku-4.5"
  echo "hosted sidecar routing, seat attribution, and model selection passed"
