# Package

version       = "0.4.2"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"



# Dependencies

requires "nim >= 1.2.4", "with", "shell"
requires "macroutils"

from os import `/`

when fileExists(thisDir() / "src/hmisc/other/nimbleutils.nim"):
  import src/hmisc/other/nimbleutils
else:
  import hmisc/other/nimbleutils

task dockertest, "Run test suite in new docker container":
  runDockerTest(thisDir(), "/tmp/docker-hmisc", "nimble test")

task installtest, "Test installation from cloned repo":
  runDockerTest(thisDir(), "/tmp/docker-hmisc", "nimble install")

task testall, "Run full test suite in all variations":
  runDockerTest(thisDir(), "/tmp/docker-hmisc/", "nimble testallTask")

task testallTask, "~~~ testall implementation ~~~":
  try:
    exec("choosenim stable")
    exec("nimble test")
    info "Stable test passed"
  except:
    err "Stable test failed"

  try:
    exec("choosenim devel")
    exec("nimble test")
    info "Devel test passed"
  except:
    exec("choosenim devel")
    err "Devel test failed"

  try:
    exec("nimble install")
    info "Installation OK"
  except:
    err "Installation failed"
