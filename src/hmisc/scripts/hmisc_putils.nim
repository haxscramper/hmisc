#!/usr/bin/env -S nim r
import ../other/[nimbleutils, oswrap, hcligen, hshell, hlogger]
import ../other/hargparse
import ../algo/hseq_distance
import std/strformat


proc tempDirPath(proj: AbsDir): AbsDir =
  AbsDir("/tmp") / proj.splitDir().tail

proc dockertest*(
    projectDir: AbsDir = cwd(),
    localDeps: seq[string] = @[],
    preTestCmds: seq[ShellExpr] = @[],
    logger: HLogger = newTermLogger()
  ) =
  ## Run unti tests in new docker container
  let tmpd = tempDirPath(projectDir)
  runDockerTest(
    projectDir,
    tmpd,
    makeLocalDevel(tmpd, localDeps, logger) &&
      cdMainProject &&
      &&preTestCmds &&
      cdMainProject &&
      shellCmd("nimble", "test"),
    logger
  )

proc installtest*(
    projectDir: AbsDir,
    localDeps: seq[string] = @[],
    logger: HLogger = newTermLogger()
  ) =
  ## Test installation in docker container
  let tmpd = tempDirPath(projectDir)
  let cmd = shAnd:
    makeLocalDevel(tmpd, localDeps, logger)
    cdMainProject
    shellCmd("nimble", install, -y)
    shAsgn($$PATH, "$PATH:$HOME/.nimble/bin")

  runDockerTest(projectDir, tmpd, cmd, logger)

proc docgen*(
    ignore: seq[GitGlob] = @[], logger: HLogger = newTermLogger()) =
  ## Generate documentation for current project
  var conf = initBuildConf()
  conf.testRun = false
  conf.configureCI()
  runDocgen(conf, ignore)

proc dockerDocGen*(
    projectDir: AbsDir = cwd(),
    localDeps: seq[string] = @[],
    simulateCI: bool = true,
    logger: HLogger = newTermLogger()
  ) =
  ## Test documentation generation in new docker container
  logger.info "Creating new docker container"
  logger.debug "Input directory is", projectDir
  let tmpd = tempDirPath(projectDir)
  let cmd = shAnd:
    makeLocalDevel(tmpd, localDeps, logger)
    cdMainProject
    shellCmd(nimble, install, -y)
    shAsgn($$PATH, "$PATH:$HOME/.nimble/bin")
    shellCmd(nimble, docgen)

  var conf: TaskRunConfig
  withDir projectDir:
    conf = initBuildConf()

  conf.logger = logger

  runDockerTest(projectDir, tmpd, cmd, logger, envpass = {
    ShellVar("CI") : "true",
    ShellVar("GITHUB_REPOSITORY") : &"{conf.author}/{conf.packageName}",
    ShellVar("GITHUB_SHA") : ShellExpr(
      "git rev-parse HEAD").eval(),
    ShellVar("GITHUB_REF") : ShellExpr(
      "git rev-parse --abbrev-ref HEAD").eval()
  })


import ../hexceptions

proc newApp*(): CliApp =
  var app = newCliApp(
    "hmisc_putils", (0, 1, 1), "haxscramper",
    "nim CI helper",
    noDefault = cliNoLoggerConfig & "force"
  )

  # TODO for tomorrow - make sure this thing compiles and runs. I wanted to
  # use `cmdProc` to semcheck things, but in the end it looks like I'm
  # going to need convoluted multistage with `static` instead
  app.add cmd(
    cmdProc(dockertest, "logger")
    "Run tests in docker container",
    argpass(alt = @["dockerTest"])
  )

  app.add cmd(
    cmdProc(installtest, "logger"),
    "Run installation in docker container",
    argpass(alt = @["installTest"])
  )

  app.add cmd(
    cmdProc(docgen, "logger"),
    "Generate documentation for a whole project"
  )

  app.add cmd(
    cmdProc(dockerDocGen, "logger"),
    "Generate documentation in a docker container"
  )

  return app

proc main*(args: seq[string], logger: HLogger = newTermLogger()) =
  var app = newApp()
  app.acceptArgsAndRunBody(logger, args):
    app.runDispatched(
      [dockertest, installtest, docgen, dockerDocGen],
      logger, doQuit = true)


when isMainModule:
  main(paramStrs())

# else:
#   echo newApp().helpStr()
