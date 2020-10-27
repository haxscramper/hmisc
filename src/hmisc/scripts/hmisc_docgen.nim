#!/usr/bin/env nim r
import cligen
import ../other/[nimbleutils, oswrap]

proc argHelp*(dfl: AbsDir, a: var ArgcvtParams): seq[string] =
  @["hello"]

proc argParse*(
  dst: var AbsDir, dfl: AbsDir, a: var ArgcvtParams): bool =
  dst = toAbsDir(a.val)

proc tempDirPath(proj: AbsDir): AbsDir =
  AbsDir("/tmp") / proj.splitDir().tail

proc dockertest*(projectDir: AbsDir) =
  ## Run unti tests in new docker container
  runDockerTest(
    projectDir,
    tempDirPath(projectDir),
    ShellExpr "nimble test"
  )

proc installtest*(projectDir: AbsDir) =
  ## Test installation in docker container
  runDockerTest(
    projectDir,
    tempDirPath(projectDir),
    ShellExpr "nimble install"
  )

proc docgen*() =
  ## Generate documentation for current project
  var conf = initBuildConf()
  conf.testRun = false
  conf.configureCI()
  runDocgen(conf)

proc dockerDocGen*(projectDir: AbsDir) =
  runDockerTest(
    projectDir,
    tempDirPath(projectDir),
    ShellExpr(
      "nimble install -y" &&
      "PATH=$PATH:$HOME/.nimble/bin" &&
      "hmisc-docgen docgen"
  ))


when isMainModule:
  # cd "../../.."
  dispatchMulti([dockertest], [installtest], [docgen])
