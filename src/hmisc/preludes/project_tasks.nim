import
  hmisc/core/all,
  hmisc/scripts/nim_test,
  hmisc/other/[oswrap, hshell, hargparse, hlogger],
  hmisc/algo/[hseq_distance],
  hmisc/types/[colorstring],
  hmisc/hasts/[json_serde, json_serde_extra]


import std/[os, strutils, strformat, sequtils, exitprocs]

type
  NimbleVer* = object
    kind*: string
    ver*: string

  NimbleRequires* = object
    name*: string
    str*: string
    ver*: NimbleVer

  NimbleManifest* = object
    name*: string
    version*: string
    author*: string
    desc*: string
    license*: string
    skipDirs*: seq[string]
    skipFiles*: seq[string]
    skipExt*: seq[string]
    installDirs*: seq[string]
    installFiles*: seq[string]
    installExt*: seq[string]
    requires*: seq[NimbleRequires]
    bin*: seq[string]
    binDir*: string
    srcDir*: string
    backend*: string

startHax()

jsonSerdeFor(ShellVar, loadJsonDistinct, writeJsonDistinct)
jsonSerdeFor(ShellExpr, loadJsonDistinct, writeJsonDistinct)
jsonSerdeFor(ShellGlob, loadJsonDistinct, writeJsonDistinct)

var app = newCliApp(
  "project_tasks",
  (0, 1, 0),
  "haxscramper",
  "CLI helper for nim project CI")

let rootArg = arg("root", "Root project file")

app.add cmd(
  "test",
  "Execute full tests in the directory", @[
    rootArg,
    opt("parse-errors",
         "Parse compilation errors",
         default = cliDefault(toCliValue(true), "true"),
         check = cliCheckFor(bool)),
    opt("ignore",
        "Only run tests from files with matching pattern",
        default = cliDefault(toCliValue(newSeq[string]()), ""),
        check = cliCheckFor(seq[string]))
])

block:
  app.add cmd("doc", "Generate project-wide documentation", @[
    rootArg,
    opt(
      "ignore",
      "file patterns to ignore",
      default = cliDefault(toCliValue(newSeq[string]()), ""),
      check = cliCheckFor(seq[string]),
    )
  ])

app.add cmd("newversion", "Tag and release new version", @[rootArg])
app.add cmd("push", "Run tests locally and push new commit", @[rootArg])

if not app.acceptArgs():
  app.showErrors(newTermLogger())
  quit(1)


let project = AbsFile(app.getCmd().getArg("root") as string)
let root = project.dir()

proc getManifest(): NimbleManifest =
  withDir root:
    let j = shellCmd(nimble, dump, --json).evalShellStdout()
    result = j.fromJson(NimbleManifest)

let manifest = getManifest()

proc sh(str: varargs[string]) =
  execShell shellCmdRaw(@str)

proc check() =
  sh ["nimble", "test"]
  sh ["nimble", "docgen"]


proc hook(event: NimRunnerEvent) =
  echo formatEvent(event)

case app.getCmdName():
  of "test":
    let
      dir = root / "tests"
      cmd = app.getCmd()
      parseRun = cmd.getOpt("parse-errors") as bool

    let tmp = getAppTempDir()
    var conf = getCwdNimDump().initNimRunConf(tmp)
    conf.reportEvent = hook
    conf.megatest = false

    for ignore in cmd.getOpt("ignore") as seq[string]:
      conf.fileGlobs.add toGitGlob(ignore)

    const cache = off

    mkDir tmp
    let cacheFile = tmp.getTempFile("cache.json")

    let runs =
      if cache:
        if exists(cacheFile):
          fromJson(cacheFile, seq[NimRunResult])

        else:
          runTestDir(dir, conf)

      else:
        runTestDir(dir, conf)

    toJson(runs, cacheFile)

    echo formatRun(runs, conf.dump)

    if hasErrors(runs):
      setProgramResult(1)

  of "doc":
    var files: seq[(string, string)]
    var globs: seq[GitGlob]
    let cmd = app.getcmd()
    for ignore in cmd.getOpt("ignore") as seq[string]:
      globs.add toGitGlob(ignore)

    let
      res = &"{root}/docs"
      maxFiles = 12_000

    var cdd: RelDir
    if exists(RelDir("src")):
      cdd = cdd / "src"

    cdd = cdd / manifest.name

    cd cdd

    var cnt = 0
    var resDirs: seq[string]
    var hmiscText: string


    for path in walkDirRec(".", relative = true):
      if globs.accept(path):
        let (dir, name, ext) = path.splitFile()
        if name != manifest.name and ext == ".nim":
          if dir.len > 0 and cnt < maxFiles:
            inc cnt
            hmiscText.add &"import ./{manifest.name}/{dir}/{name}\n"

      else:
        echov "ignoring", path

    let doc = &"tmp_docgen_target_{manifest.name}"
    cd "../.."
    writeFile(&"src/{doc}.nim", hmiscText)
    var args = @[
      "nim",
      "doc2",
      "--project",
      "--index:on",
      "--warnings:off",
      "--errormax:1",
      "--outdir:htmldocs"
    ]

    let ghUrl = getEnv("GITHUB_REPOSITORY", &"file://{res}")
    if ghUrl.len > 0: args.add &"--git.url:\"{ghUrl}\""

    args.add &"src/{doc}.nim"

    # echov args

    sh args

    rmFile RelFile(&"src/{doc}.nim")

  of "push":
    check()
    sh ["git", "push", "origin", "master"]

  of "newversion":
    try:
      let ver = manifest.version.split(".").mapIt(it.parseInt())
      var (major, minor, patch) = (ver[0], ver[1], ver[2])
      inc patch

      let commits = gorgeEx(&"git log --oneline v{manifest.version}..HEAD ").
        output.split("\n").filterIt(it.len > 0).mapIt(&"- {it}").join("\n")

      let msg = &"""
[REPO] Version update {manifest.version} -> {major}.{minor}.{patch}

{commits}
"""

      # debugecho "\e[31m!!\e[39m project_tasks.nim, Line 169 "
      sh ["git", "diff-index", "--quiet", "HEAD", "--"]
      # debugecho "\e[31m!!\e[39m project_tasks.nim, Line 171 "
      check()

      # debugecho "\e[31m!!\e[39m project_tasks.nim, Line 174 "
      let
        text = project.readFile()
        pos = text.find("version")
        endPos = text.find("\n", pos)
        newText = text[0 ..< pos] &
          &"version = \"{major}.{minor}.{patch}\"" & text[endPos .. ^1]

      # debugecho "\e[31m!!\e[39m project_tasks.nim, Line 182 "

      project.writeFile(newText)
      sh ["git", "add", "hmisc.nimble"]

      let
        commitCmd = ["git", "commit", "-m", os.quoteShell(msg)]
        tag = &"v{major}.{minor}.{patch}"
        tagCmd = ["git", "tag", tag]

      sh commitCmd
      sh tagCmd

      sh ["git", "push", "origin", "master"]
      sh ["git", "push", "origin", tag]


    except OsError:
      echo "Have uncomitted changes, commit first before pushing"
      sh ["git", "--no-pager", "diff"]
