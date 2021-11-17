version     = "0.14.0"
author      = "haxscramper"
description = "Collection of helper utilities"
license     = "Apache-2.0"
srcDir      = "src"
packageName = "hmisc"
installExt  = @["nim", "rst"]
binDir      = "bin"

requires "nim >= 1.4.8"
requires "benchy >= 0.0.1"
requires "jsony >= 1.0.4"
requires "unicodedb >= 0.9.0"

task test, "Run tests":
  exec "nim r tests/runall.nim " & currentSourcePath() & " test -- "

task docgen, "Generate documentation":
  exec "nim c -r tests/runall.nim " & currentSourcePath() & " doc -- "

task push, "Execute checks and push ":
  exec "nim r tests/runall.nim " & currentSourcePath() & " push -- "

task newversion, "Tag new version and push it to git":
  exec "nim r tests/runall.nim " & currentSourcePath() & " newversion -- "
