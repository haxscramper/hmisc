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

proc installtest*(
  projectDir: AbsDir, localDeps: seq[string] = @[]) =
  ## Test installation in docker container
  let tmpd = tempDirPath(projectDir)
  let cmd = makeLocalDevel(tmpd, localDeps) &&
    "nimble install -y" &&
    "PATH=$PATH:$HOME/.nimble/bin"

  runDockerTest(projectDir, tmpd, cmd)

proc docgen*() =
  ## Generate documentation for current project
  var conf = initBuildConf()
  conf.testRun = false
  conf.configureCI()
  runDocgen(conf)

proc dockerDocGen*(projectDir: AbsDir, localDeps: seq[string] = @[]) =
  ## Test documentation generation in new docker container
  info "Creating new docker container"
  debug "Input directory is", projectDir
  let tmpd = tempDirPath(projectDir)
  let cmd = makeLocalDevel(tmpd, localDeps) &&
    "nimble install -y" &&
    "PATH=$PATH:$HOME/.nimble/bin" &&
    "hmisc-putils docgen"

  runDockerTest(projectDir, tmpd, cmd)


when isMainModule:
  dispatchMulti(
    [dockertest],
    [installtest],
    [docgen],
    [dockerDocGen]
  )
