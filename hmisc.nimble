# Package

version       = "0.7.0"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "hmisc"


# Dependencies

requires "nim >= 1.4.0", "sorta"

from os import `/`
import strutils

when fileExists(thisDir() / "src/hmisc/other/nimbleutils.nim"):
  import src/hmisc/other/nimbleutils
else:
  import hmisc/other/nimbleutils

task dockertest, "Run test suite in new docker container":
  runDockerTest(AbsDir thisDir(),
                AbsDir "/tmp/docker-hmisc", "nimble test")

task installtest, "Test installation from cloned repo":
  runDockerTest(thisAbsDir(),
                AbsDir "/tmp/docker-hmisc", "nimble install")

task testall, "Run full test suite in all variations":
  runDockerTest(AbsDir thisDir(),
                AbsDir "/tmp/docker-hmisc/",
                "nimble testallTask && nimble docgen")

task testallTask, "~~~ testall implementation ~~~":
  testAllImpl()

task docgen, "Generate documentation":
  var conf = initBuildConf()
  conf.testRun = false
  conf.configureCI()
  runDocgen(conf)

task testRun, "test things":
  echo commandLineParams
