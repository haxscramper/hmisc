# Package

version       = "0.9.16"
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


requires "nim >= 1.4.0", "cligen"

task docgen, "Generate documentation":
  if not fileExists("bin/hmisc-putils"):
    exec("nimble build")

  exec("hmisc-putils docgen")

task dockertest, "Run tests in docker container":
  exec("hmisc-putils dockertest --projectDir:" & thisDir())

after test:
  exec("nim c --hints:off --verbosity:0 src/hmisc/scripts/hmisc_putils.nim")
