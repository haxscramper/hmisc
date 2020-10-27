# Package

version       = "0.8.4"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "hmisc"
bin           = @["hmisc/scripts/hmisc_docgen"]
installExt    = @["nim"]
binDir        = "bin"
namedBin      = {
  "hmisc/scripts/hmisc_docgen" : "hmisc-docgen"
}.toTable()


requires "nim >= 1.4.0", "sorta", "cligen"

from os import `/`
import strutils

when fileExists(thisDir() / "src/hmisc/other/nimbleutils.nim"):
  import src/hmisc/other/nimbleutils
else:
  import hmisc/other/nimbleutils

task dockertest, "Run test suite in new docker container":
  runDockerTest(AbsDir thisDir(),
                AbsDir "/tmp/docker-hmisc",
                ShellExpr "nimble test")

task installtest, "Test installation from cloned repo":
  runDockerTest(
    AbsDir thisDir(),
    AbsDir "/tmp/docker-hmisc",
    ShellExpr "nimble install -y" &&
      "PATH=$PATH:$HOME/.nimble/bin" &&
      "hmisc-docgen --help")

task docgen, "Generate documentation":
  runDockerTest(
    AbsDir thisDir(),
    AbsDir "/tmp/docker-hmisc",
    ShellExpr "nimble install -y" &&
      "PATH=$PATH:$HOME/.nimble/bin" &&
      "hmisc-docgen")

task testRun, "test things":
  echo commandLineParams
