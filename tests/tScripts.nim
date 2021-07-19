import
  hmisc/scripts/[hmisc_putils],
  hmisc/other/[hargparse, oswrap]

import
  std/unittest

suite "hmisc_putils":
  test "Help string":
    echo hmisc_putils.newApp().helpStr(cliNoDefaultOpts)

  test "Generate documentation":
    let dir = getAppTempDir()
    mkWithDirStructure dir:
      file "test1.nim":
        "proc test1proc*() = ##[ documentation ]## discard"

    withDir dir:
      hmisc_putils.main(@["docgen"])
