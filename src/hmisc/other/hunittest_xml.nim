import
  ./hunittest,
  ../hasts/xml_ast

import
  std/algorithm

type
  XUnitContext* = ref object of TestContext
    w: XmlWriter

method report*(ctx: XUNitContext, report: TestReport) =
  case report.kind:
    of trkSuiteStart:
      ctx.w.xmlStart("assembly", {
        "name": report.name
      })
      ctx.w.indent()

    of trkSuiteEnd:
      ctx.w.dedent()
      ctx.w.xmlEnd("assembly")

    of trkTestEnd:
      ctx.w.dedent()
      ctx.w.xmlEnd("collection")

    of trkTestFail:
      case report.failKind:
        of tfkException:
          let e = report.exception
          ctx.w.ewrapl("failure", {"exception-type": $e.name}):
            ctx.w.writeIndent()
            ctx.w.ewrap("message"):
              ctx.w.writeRaw(e.msg)

            ctx.w.line()

            ctx.w.ewrapl("stacktrace"):
              for entry in e.getStackTraceEntries().reversed():
                ctx.w.xmlSingle("trace", {
                  "line": $entry.line,
                  "proc": $entry.procname,
                  "filename": $entry.filename
                })



        else:
          ctx.w.xmlRawSingle($report.failKind)

      ctx.w.dedent()
      ctx.w.xmlEnd("collection")

    of trkTestStart:
      ctx.w.xmlStart("collection", {
        "name": report.name
      })

      ctx.w.indent()


    of trkCheckOk:
      ctx.w.xmlSingle("test", {
        "result": "Pass",
        "name": report.expr & " at " & report.name
      })

    of trkCheckFail:
      ctx.w.ewrapl("test", {
        "result": "Fail",
        "name": report.expr & " at " & report.name
      }):
        ctx.w.ewrapl("failure", []):
          ctx.w.xmlSingle($report.failKind, [])

    of trkFileEnded:
      ctx.w.close()

    else:
      ctx.w.xmlRawSingle($report.kind)

proc newXUnitContext*(): XUnitContext =
  XUnitContext(w: newXmlWriter(stdout))
