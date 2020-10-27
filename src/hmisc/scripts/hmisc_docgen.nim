#!/usr/bin/env -S nim r
import cligen
import ../other/[nimbleutils, oswrap]

startColorLogger()

proc argHelp*(dfl: AbsDir, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "AbsDir", ""]

proc argParse*(
  dst: var AbsDir, dfl: AbsDir, a: var ArgcvtParams): bool =
  dst = toAbsDir(a.val)
  result = true

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
    ShellExpr("nimble install -y") &&
      "PATH=$PATH:$HOME/.nimble/bin"
  )

proc docgen*() =
  ## Generate documentation for current project
  var conf = initBuildConf()
  conf.testRun = false
  conf.configureCI()
  runDocgen(conf)

proc dockerDocGen*(projectDir: AbsDir) =
  ## Test documentation generation in new docker container
  info "Creating new docker container"
  debug "Input directory is", projectDir
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
  dispatchMulti(
    [dockertest],
    [installtest],
    [docgen],
    [dockerDocGen]
  )
