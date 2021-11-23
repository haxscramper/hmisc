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
