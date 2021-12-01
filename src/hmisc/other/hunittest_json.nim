import ./hunittest
import ../hasts/[json_ast, json_serde, json_serde_extra]
import ./jsony_converters

import
  std/algorithm

type
  JsonContext* = ref object of TestContext

method report*(ctx: JsonContext, report: TestReport) =
  echo toJson(report)

proc newJsonContext*(): JsonContext =
  JsonContext()
