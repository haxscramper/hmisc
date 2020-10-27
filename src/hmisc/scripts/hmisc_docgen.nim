import cligen
import ../other/[nimbleutils, oswrap]

proc argHelp*(dfl: AbsDir, a: var ArgcvtParams): seq[string] =
  @["hello"]

proc argParse*(
  dst: var AbsDir, dfl: AbsDir, a: var ArgcvtParams): bool =
  dst = toAbsDir(a.val)

proc dockertest*(projectDir: AbsDir = cwd()) =
  ## Run unti tests in new docker container
  let tempDir: AbsDir = AbsDir("/tmp") / projectDir.splitDir().tail
  runDockerTest(
    projectDir,
    tempDir,
    ShellExpr "nimble test"
  )

proc installtest*(projectDir: AbsDir = cwd()) =
  ## Test installation in docker container
  let tempDir: AbsDir = AbsDir("/tmp") / projectDir.splitDir().tail
  runDockerTest(
    projectDir,
    tempDir,
    ShellExpr "nimble install"
  )

proc docgen*() =
  ## Generate documentation
  var conf = initBuildConf()
  conf.testRun = false
  conf.configureCI()
  runDocgen(conf)

when isMainModule:
  cd "../../.."
  dispatchMulti([dockertest], [installtest])
