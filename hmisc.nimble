# Package

version       = "0.4.0"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"



# Dependencies

requires "nim >= 1.2.4", "with", "shell"
requires "macroutils"

import strformat

proc runDockerTest(
  projDir, tmpDir, cmd: string, cleanup: bool = true): void =
  if tmpDir.dirExists:
    rmDir tmpDir

  cpDir projDir, tmpDir
  let cmd =
      &"docker run -it --rm -v={tmpDir}:/hmisc nim-base sh -c '" &
      &"cd /hmisc && {cmd}" &
      "'"


  echo(cmd)
  exec(cmd)
  if cleanup:
    rmDir tmpDir


task dockertest, "Run test suite in new docker container":
  runDockerTest(thisDir(), "/tmp/docker-hmisc", "nimble test")

task installtest, "Test installation from cloned repo":
  runDockerTest(thisDir(), "/tmp/docker-hmisc", "nimble install")
