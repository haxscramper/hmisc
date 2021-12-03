import hmisc/preludes/unittest
import hmisc/scripts/nim_test
import hmisc/algo/[hlex_base]

suite "Parser":
  test "FIle":
    var str = initPosStr(
      "/mnt/hmisc/other/oswrap.nim(518, 10)").
      asVar().
      skipFileLineCol()

    show str

  test "Declared in":
    let decl = initPosStr(
      "[template declared in /mnt/hmisc/other/oswrap.nim(518, 10)]").
      asVar().
      skipKindDeclaredIn()

    show decl

  test "call":
    let rep = parseAmbiguousCall(
      "ambiguous call; both oswrap.currentSourceDir() [template declared in /src/hmisc/other/oswrap.nim(518, 10)] and gold.currentSourceDir() [template declared in /hmisc/core/gold.nim(152, 10)] match for: ()")

    check:
      rep.kind == nrError
      rep.error == neAmbiguousCall

  test "Unparseable report":
    let rep = parseNimReport("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ???")

    check rep.kind == nrUnparsed


  test "wrong bin/ld":
    let report = parseNimReport("""
[]
[]
[]
[]
[]
/usr/bin/ld: cannot find -lsomeRandomLibThatIsCertainlyDoesNotExist
collect2: error: ld returned 1 exit status
Error: execution of an external program failed: 'gcc   -o -ldl'
""")


suite "Compiled":
  let dir = getTestTempDir()
  mkDir dir
  let l = getTestLogger()

  let dump = currentSourcePath().getCwdNimDump()
  proc reports(text: string): seq[NimReport] =
    let file = dir.getTempFile("???????.nim")
    file.writeFile(text)
    var conf = dump.initNimRunConf(dir)
    conf.excl nrfHints

    getCompileReportsFor(file, conf)[0].reports.skipKinds()


  test "Correct code reports":
    let rep = reports("echo 12")
    check rep.len == 0

  test "Failed C codegen":
    let rep = reports("{.emit: \"???\".}")[0]
    let diag = rep.gccReport.diags[0][0]

    check:
      diag.kind == "error"
      diag.message == "expected identifier or ‘(’ before ‘?’ token"
      rep.error == neGccFail

  test "Invalid syntax":
    let rep = reports("proc test(): int\n  echo 12")[0]
    check:
      rep.error == neInvalidIndentation


  test "Bad overload":
    let rep = reports("""
proc impl(a: int) = discard
proc impl(a: string) = discard

impl():
  block:
    let t1 = (12, 2)
    let t2 = (t1, t1)
    (t2, t2)

""")

    check rep[0].error == neOverloadFail

suite "Runner":
  let
    dir = getTestTempDir()
    l = getTestLogger()
    dump = currentSourcePath().getCwdNimDump()

  mkDir dir

  proc hook(event: NimRunnerEvent) =
    show event.formatEvent()

  proc runReports(text: string): NimRunResult =
    var conf = dump.initNimRunConf(dir)
    conf.reportEvent = hook
    conf.randomPattern = "test"
    conf.excl nrfHints
    conf.parseCompilation = true
    conf.parseExecution = true

    let file = conf.getRandomFile(dir, "file", ".nim")

    file.writeFile(
      "import hmisc/preludes/unittest\n" & text)


    return getRunReportsFor(file, conf)[0]

  test "Test run reports":
    let report = runReports():
      """
suite "Test suite":
  test "First test":
    check:
      12 == 22
      12 == 12
"""

    show formatRun(@[report], dump)

  test "Failed test compilation":
    let report = runReports():
      """
;asdflkkffjasdf
"""

    check report.kind == nrrkFailedCompilation
