#!/usr/bin/env -S nim r
import ../other/[nimbleutils, oswrap, hcligen, hshell]
import std/strformat

startColorLogger()


proc tempDirPath(proj: AbsDir): AbsDir =
  AbsDir("/tmp") / proj.splitDir().tail

proc dockertest*(projectDir: AbsDir = cwd(),
                 localDeps: seq[string] = @[],
                 preTestCmds: seq[ShellExpr] = @[]
                ) =
  ## Run unti tests in new docker container
  let tmpd = tempDirPath(projectDir)
  runDockerTest(projectDir, tmpd,
                makeLocalDevel(tmpd, localDeps) &&
                  cdMainProject && &&preTestCmds &&
                  shCmd("nimble", "test")
  )

proc installtest*(
  projectDir: AbsDir, localDeps: seq[string] = @[]) =
  ## Test installation in docker container
  let tmpd = tempDirPath(projectDir)
  let cmd = shAnd:
    makeLocalDevel(tmpd, localDeps)
    cdMainProject
    shCmd("nimble", install, -y)
    shAsgn($$PATH, "$PATH:$HOME/.nimble/bin")

  runDockerTest(projectDir, tmpd, cmd)

proc docgen*() =
  ## Generate documentation for current project
  var conf = initBuildConf()
  conf.testRun = false
  conf.configureCI()
  runDocgen(conf)

proc dockerDocGen*(
  projectDir: AbsDir = cwd(),
  localDeps: seq[string] = @[],
  simulateCI: bool = true) =
  ## Test documentation generation in new docker container
  info "Creating new docker container"
  debug "Input directory is", projectDir
  let tmpd = tempDirPath(projectDir)
  let cmd = shAnd:
    makeLocalDevel(tmpd, localDeps)
    cdMainProject
    shCmd(nimble, install, -y)
    shAsgn($$PATH, "$PATH:$HOME/.nimble/bin")
    shCmd(nimble, docgen)

  var conf: TaskRunConfig
  withDir projectDir:
    conf = initBuildConf()


  runDockerTest(projectDir, tmpd, cmd, envpass = {
    ShellVar("CI") : "true",
    ShellVar("GITHUB_REPOSITORY") : &"{conf.author}/{conf.packageName}",
    ShellVar("GITHUB_SHA") : ShellExpr(
      "git rev-parse HEAD").eval(),
    ShellVar("GITHUB_REF") : ShellExpr(
      "git rev-parse --abbrev-ref HEAD").eval()
  })


import ../hexceptions

when isMainModule:
  try:
    dispatchMulti(
      [dockertest],
      [installtest],
      [docgen],
      [dockerDocGen]
    )
  except:
    pprintErr()
    quit 1
