import hmisc/preludes/unittest

testFileStarted()


import
  hmisc/scripts/[hmisc_putils, nim_gcov, nim_gprof],
  hmisc/other/[hargparse, oswrap, hshell]

startHax()

suite "hmisc_putils":
  test "Help string":
    echo hmisc_putils.newApp().helpStr(cliNoDefaultOpts)

  test "Generate documentation":
    let dir = getAppTempDir() / "hmisc_putils_docs"
    mkWithDirStructure dir:
      file "test1.nim":
        "proc test1proc*() = ##[ documentation ]## discard"

      file "test2.nim": "proc test2*() = discard"
      file "test3.nim": "proc test3*() = discard"
      file "test4.nim": "proc test4*() = discard"
      file "test5.nim": "proc test5*() = discard"
      file "test6.nim": "proc test5*() = discard"
      file "test7.nim": "proc test5*() = discard"

      file "hangbomg.nim":
        lit3"""
# static:
#   for i in 0 .. 20000:
#     discard gorge("sleep 90000")
"""

      file "package.nimble":
        lit3"""
author = "haxscramper"
version = "0.1.2"
description = "test nimble package for documentation generation"
license = "Apache-2.0"
"""

    withDir dir:
      hmisc_putils.main(@["docgen", "--ignore=**/test2.nim"])

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