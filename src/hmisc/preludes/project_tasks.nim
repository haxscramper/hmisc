import
  ../scripts/nim_test,
  ../other/[oswrap, hshell]

import std/[os, strutils, strformat, sequtils]

startHax()

let args = paramStrs()
let project = AbsFile(args[0])
let root = project.dir()

proc sh(str: varargs[string]) =
  execShell shellCmdRaw(@str)

proc check() =
  sh ["nimble", "test"]
  sh ["nimble", "docgen"]

case args[1]:
  of "test":
    let dir = root / "tests"
    runTestDir(dir, getCwdNimDump(), 1)

  of "doc":
    var files: seq[(string, string)]

    let
      res = &"{root}/docs"
      maxFiles = 12_000

    var cdd: RelDir
    if exists(RelDir("src")):
      cdd = cdd / "src"

    for dir in walkDir(root / cdd, RelDir):
      echo dir
      cdd = dir
      break

    cd cdd

    var cnt = 0
    var resDirs: seq[string]
    var hmiscText: string

    for path in walkDirRec(".", relative = true):
      let (dir, name, ext) = path.splitFile()
      if name != "hmisc" and ext == ".nim":
        if dir.len > 0 and cnt < maxFiles:
          inc cnt
          hmiscText.add &"import ./hmisc/{dir}/{name}\n"

    cd "../.."
    writeFile("src/hmisc.nim", hmiscText)
    var args = @[
      "nim",
      "doc2",
      "--project",
      "--warnings:off",
      "--errormax:1",
      "--outdir:docs"
    ]

    let ghUrl = getEnv("GITHUB_REPOSITORY", &"file://{res}")
    if ghUrl.len > 0: args.add &"--git.url:\"{ghUrl}\""

    args.add "src/hmisc.nim"

    sh args

  of "push":
    check()
    sh ["git", "push", "origin", "master"]

  of "newversion":
    try:
      let text = project.readFile()
      var start = text.find("version") + len("version")

      while text[start] in {' ', '=', '"'}:
        inc start

      let final = text.find("\"", start)
      let version = text[start .. final]
      echo version

      let ver = version.split(".").mapIt(it.parseInt())
      var (major, minor, patch) = (ver[0], ver[1], ver[2])
      inc patch

      let commits = gorgeEx(&"git log --oneline v{version}..HEAD ").
        output.split("\n").filterIt(it.len > 0).mapIt(&"- {it}").join("\n")

      let msg = &"""
[REPO] Version update {version} -> {major}.{minor}.{patch}

{commits}
"""

      sh ["git", "diff-index", "--quiet", "HEAD", "--"]
      check()

      let
        pos = text.find("version")
        endPos = text.find("\n", pos)
        newText = text[0 ..< pos] &
          &"version = \"{major}.{minor}.{patch}\"" & text[endPos .. ^1]

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
