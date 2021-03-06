version       = "0.11.16"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "hmisc"
bin           = @["hmisc/scripts/hmisc_putils"]
installExt    = @["nim"]
binDir        = "bin"

when (1, 2, 2) < (NimMajor, NimMinor, NimPatch):
  namedBin      = {
    "hmisc/scripts/hmisc_putils" : "hmisc-putils"
  }.toTable()

requires "nim >= 1.4.0", "cligen"
requires "fusion"
requires "benchy"
# requires "https://github.com/haxscramper/fusion.git#matching-fixup"

task docgen, "Generate documentation":
  if not fileExists("bin/hmisc-putils"):
    exec("nimble build")

  exec("hmisc-putils docgen")

task dockertest, "Run tests in docker container":
  exec("hmisc-putils dockertest --projectDir:" & thisDir())

after test:
  exec("nim c --hints:off --verbosity:0 src/hmisc/scripts/hmisc_putils.nim")
