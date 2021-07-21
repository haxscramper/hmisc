#!/usr/bin/env -S nim r
import ../other/[nimbleutils, oswrap, hshell, hlogger]
import ../other/hargparse
import ../algo/[hseq_distance, halgorithm]
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
  conf.logger = logger
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

func toCliValue*(
    glob: GitGlob, doc: string = "", desc: CliDesc = nil): CliValue =
  specialStringCliValue($glob, doc, desc)

func fromCliValue*(value: CliValue, target: var GitGlob) =
  assertKind(value, {cvkString, cvkSpecialString})
  target = toGitGlob(value.strVal)

func cliCheckFor*(glob: typedesc[GitGlob]): CliCheck =
  checkAcceptAll()

const
  common = @{
      "projectDir": "Override project directory",
      "localDeps":
        """
List of nimble packages that would be linked in the container
instead of begin freshly installed.

- NOTE :: `nimble develop` is used to install packages, so their
  order is important (right now `nimble develop` installs dependencies
  as well)
"""}

  commonArguments = toArrayKeys(common)
  confDockertest = procConf(
    alt = @["dockerTest"],
    ignore = @["logger"],
    commonArguments = commonArguments,
    help = common & @{
      "preTestCmds": "Sequence of shell commands to execute before main one"
  })

  confInstalltest = procConf(
    alt = @["installTest"],
    ignore = @["logger"],
    commonArguments = commonArguments,
    help = common & @{
      "localDeps": ".",
  })

  confDocgen = procConf(
    ignore = @["logger"],
    commonArguments = commonArguments,
    help = common & @{
      "ignore": "List of git globs - " &
        "files to ignore for documentation generation.",
  })

  confDockerDocGen = procConf(
    ignore = @["logger"],
    commonArguments = commonArguments,
    help = common & @{
      "simulateCI": "Declare environment variables present in github CI"
  })

proc newApp*(): CliApp =
  var app = newCliApp(
    "hmisc_putils", (0, 1, 1), "haxscramper",
    "nim CI helper",
    noDefault = cliNoLoggerConfig & "force"
  )

  app.add cmd(dockertest, conf = confDockertest)
  app.add cmd(installtest, conf = confInstalltest)
  app.add cmd(docgen, conf = confDocgen)
  app.add cmd(dockerDocGen, conf = confDockerDocGen)

  return app

proc main*(args: seq[string], logger: HLogger = newTermLogger()) =
  var app = newApp()
  app.acceptArgsAndRunBody(logger, args):
    app.runDispatched(
      [
        (dockertest,   confDockertest,     argpass(logger = logger)),
        (installtest,  confInstalltest,      argpass(logger = logger)),
        (docgen,       confDocgen, argpass(logger = logger)),
        (dockerDocGen, confDockerDocGen,       argpass(logger = logger))
      ],
      logger, doQuit = true)


when isMainModule:
  main(paramStrs())

# else:
#   echo newApp().helpStr()
