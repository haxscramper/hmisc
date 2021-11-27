import hmisc/preludes/unittest
import hmisc/scripts/nim_test
import hmisc/algo/[hlex_base]

suite "Parser":
  test "FIle":
    var str = initPosStr(
      "/mnt/hmisc/other/oswrap.nim(518, 10)").
      asVar().
      skipFileLineCol()

    echo str

  test "Declared in":
    let decl = initPosStr(
      "[template declared in /mnt/hmisc/other/oswrap.nim(518, 10)]").
      asVar().
      skipKindDeclaredIn()

    echo decl

  test "call":
    echo parseAmbiguousCall(
      "ambiguous call; both oswrap.currentSourceDir() [template declared in /src/hmisc/other/oswrap.nim(518, 10)] and gold.currentSourceDir() [template declared in /hmisc/core/gold.nim(152, 10)] match for: ()")

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


suite "Runner":
  let dir = getTestTempDir()
  mkDir dir
  let l = getTestLogger()

  let dump = getCwdNimDump()
  proc reports(text: string): seq[NimReport] =
    let file = dir.getTempFile("???????.nim")
    file.writeFile(text)
    getCompileReportFor(file, dump, hints = off).skipKinds()

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
