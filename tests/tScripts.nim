import
  hmisc/scripts/[hmisc_putils],
  hmisc/other/[hargparse, oswrap],
  hmisc/hdebug_misc

import
  std/unittest

startHax()

suite "hmisc_putils":
  test "Help string":
    echo hmisc_putils.newApp().helpStr(cliNoDefaultOpts)

  test "Generate documentation":
    let dir = getAppTempDir()
    mkWithDirStructure dir:
      file "test1.nim":
        "proc test1proc*() = ##[ documentation ]## discard"

      file "package.nimble":
        """
author = "haxscramper"
version = "0.1.2"
description = "test nimble package for documentation generation"
license = "Apache-2.0"
"""

    withDir dir:
      hmisc_putils.main(@["docgen"])
