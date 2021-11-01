import ./hunittest, jsony
import ./jsony_converters

import
  std/algorithm

type
  JsonContext* = ref object of TestContext

method report*(ctx: JsonContext, report: TestReport) =
  echo toJson(report)

proc newJsonContext*(): JsonContext =
  JsonContext()
