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
    show parseAmbiguousCall(
      "ambiguous call; both oswrap.currentSourceDir() [template declared in /src/hmisc/other/oswrap.nim(518, 10)] and gold.currentSourceDir() [template declared in /hmisc/core/gold.nim(152, 10)] match for: ()")

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

  let dump = getCwdNimDump()
  proc reports(text: string): seq[NimReport] =
    let file = dir.getTempFile("???????.nim")
    file.writeFile(text)
    var conf = dump.initNimRunConf()
    conf.excl nrfHints

    getCompileReportsFor(file, conf)[0].reports.skipKinds()


  test "Correct code reports":
    let rep = reports("echo 12")

    pprint rep

    check rep.len == 0

  test "Failed linker":
    discard
    # let rep = reports("{.passl: \"-lsomeRandomLibThatIsCertainlyDoesNotExist\".}")[0]

    # check:
    #   rep.kind == nrError
    #   rep.error == neLdFail
    #   "-lsomeRandomLibThatIsCertainlyDoesNotExist" in rep.ldReport.message

    # l.reportError(rep, dump)

  test "Failed C codegen":
    let rep = reports("{.emit: \"???\".}")[0]
    let diag = rep.gccReport.diags[0][0]

    check:
      diag.kind == "error"
      diag.message == "expected identifier or ‘(’ before ‘?’ token"

    l.reportError(rep, dump)

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

    l.reportError(rep[0], dump)

suite "Runner":
  let
    dir = getTestTempDir()
    l = getTestLogger()
    dump = getCwdNimDump()

  mkDir dir

  proc runReports(text: string): NimRunResult =
    var conf = dump.initNimRunConf()
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

    show report.formatRun(dump)
