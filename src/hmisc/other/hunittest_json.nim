import ./hunittest
import ../hasts/[json_ast, json_serde, json_serde_extra]

type
  JsonContext* = ref object of TestContext
    inSkipTest: bool

method report*(ctx: JsonContext, report: TestReport) =
  echo toJson(report)
  ctx.updateState(report)

proc newJsonContext*(): JsonContext =
  JsonContext()
