# Package

version       = "0.7.0"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "hmisc"


# Dependencies

requires "nim >= 1.2.4", "with"
requires "macroutils"

from os import `/`
import strutils

when fileExists(thisDir() / "src/hmisc/other/nimbleutils.nim"):
  import src/hmisc/other/nimbleutils
else:
  import hmisc/other/nimbleutils

task dockertest, "Run test suite in new docker container":
  runDockerTest(thisAbsDir(),
                AbsDir "/tmp/docker-hmisc", "nimble test")

task installtest, "Test installation from cloned repo":
  runDockerTest(thisAbsDir(),
                AbsDir "/tmp/docker-hmisc", "nimble install")

task testall, "Run full test suite in all variations":
  runDockerTest(thisAbsDir(),
                AbsDir "/tmp/docker-hmisc/", "nimble testallTask")

task testallTask, "~~~ testall implementation ~~~":
  testAllImpl()

task docgen, "Generate documentation":
  var conf = initBuildConf()
  conf.testRun = false

  if ShellVar("CI").exists():
    conf.envOrParam(
      "git.url",
      ShellExpr "https://github.com/${GITHUB_REPOSITORY}")

    conf.envOrParam("git.commit", ShellExpr "$GITHUB_SHA")
    conf.switch("git.devel", ShellVar("GITHUB_REF").get().split("/")[^1])

  conf.outdir = AbsDir(thisDir() & "/docs")
  runDocgen(conf)

task testRun, "test things":
  echo commandLineParams
