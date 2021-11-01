import hmisc/preludes/unittest

testFileStarted()


import
  hmisc/scripts/[nim_gcov, nim_gprof],
  hmisc/other/[hargparse, oswrap, hshell]

startHax()

suite "nim_gprof":
  test "Help string":
    echo nim_gprof.newApp().helpStr(cliNoDefaultOpts)

  test "Run profiler":
    let dir = getAppTempDir() / "nim_gprof"

    mkWithDirStructure dir:
      file "main.nim":
        lit3"""
import hmisc/other/[hargparse, hpprint, blockfmt]

var app = newCliApp(
  "nim_gprof", (0, 1, 0), "haxscramper",
  "pretty-print gprof data for nim program",
  noDefault = @["help", "log-output", "loglevel", "version", "color",
                "force", "dry-run", "quiet"])

let (tree, errors) = app.acceptParams(@[])

let bl = ppblock(tree)
# "/tmp/out".writeFile(bl.codegenRepr())

proc countBlock(bl: LytBlock): int =
  result = bl.len()
  case bl.kind:
    of bkStack, bkLine, bkChoice:
      for sub in bl.elements:
        result += countBlock(sub)

    else:
      inc result

echo countBlock(bl)

pprint tree, 100

pprint @[
 @[1,233,3,4,543,5,6,7,7,8],
 @[1,233,33,4,543,5,6,337,7,8],
 @[1,233,33,4,543,5,6,33337,7,8],
 @[1,233,33,4,543,5,6,33337,7,8],
 @[1,2,3,43,54,5,6,37,337,8],
]

"""
    if hasCmd(shellCmd(gprof)):
      nim_gprof.main(@[$(dir /. "main.nim")])


testFileEnded()
