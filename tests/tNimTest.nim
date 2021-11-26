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



suite "Runner":
  let dir = getTestTempDir()
  mkDir dir

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
    let rep = reports("{.passl: \"-lsomeRandomLibThatIsCertainlyDoesNotExist\".}")[0]

    check:
      rep.kind == nrError
      rep.error == neLdFail
      "-lsomeRandomLibThatIsCertainlyDoesNotExist" in rep.ldReport.message

  test "Failed C codegen":
    let rep = reports("{.emit: \"???\".}")[0]
    let diag = rep.gccReport.diags[0][0]

    check:
      diag.kind == "error"
      diag.message == "expected identifier or ‘(’ before ‘?’ token"

  test "Invalid syntax":
    let rep = reports("proc test(): int\n  echo 12")[0]
    check:
      rep.error == neInvalidIndentation


  test "Bad overload":
    let rep = reports("""
proc impl(a: int) = discard
proc impl(a: string) = discard
proc impl(z: float) = discard
proc impl(a, b, c: string) = discard
proc impl(zz: var int) = discard

impl():
  block:
    let t1 = (12, 2, 2)
    let t2 = (t1, t1, t1)
    (t2, t2)

""")

    # echo rep
    pprint rep
