# Package

version       = "0.8.4"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "hmisc"
bin           = @["hmisc/scripts/hmisc_putils"]
installExt    = @["nim"]
binDir        = "bin"
namedBin      = {
  "hmisc/scripts/hmisc_putils" : "hmisc-putils"
}.toTable()


requires "nim >= 1.4.0", "sorta", "cligen"


from os import `/`
import std/[strutils]

when fileExists(thisDir() / "src/hmisc/other/nimbleutils.nim"):
  import src/hmisc/other/nimbleutils
else:
  import hmisc/other/nimbleutils

task docgen, "Generate documentation":
  if not fileExists("bin/hmisc-putils"):
    execShell(ShellExpr "nimble build")

  execShell(ShellExpr "bin/hmisc-putils")
