version     = "0.14.0"
author      = "haxscramper"
description = "Collection of helper utilities"
license     = "Apache-2.0"
srcDir      = "src"
packageName = "hmisc"
installExt  = @["nim", "rst"]
binDir      = "bin"

requires "nim >= 1.4.8"
requires "benchy >= 0.0.1"
requires "jsony >= 1.0.4"
requires "unicodedb >= 0.9.0"

import std/[os, strutils, strformat, sequtils]

task test, "Run tests":
  exec "nim r tests/runall.nim"

task docgen, "Generate documentation":
  var files: seq[(string, string)]

  let
    cwd = getCurrentDir()
    res = &"{cwd}/docs"
    cdd = "src/hmisc"
    maxFiles = 12_000

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

  let cmd = join(args, " ")
  echo cmd
  exec cmd

proc sh(str: varargs[string]) =
  let str = str.join(" ")
  exec str

# task testall, "Merge all tests and run them":
#   let outPath = "/tmp/tmp_tests_all.nim"
#   var res: string
#   res.add """
# {.define: hunittestMerge.}
# import hmisc/other/hunittest
# {.push warning[UnusedImport]:off.}
# """
#   let cwd = getCurrentDir()
#   var nojoin: seq[string]
#   var cnt = 0
#   for (kind, path) in walkDir("tests", relative = false):
#     if 10_000 < cnt:
#       break
#     inc cnt

#     if kind == pcFile and path.splitFile().ext == ".nim":
#       let path = cwd / path
#       let content = path.readFile()
#       if content.find("joinable: false") != -1:
#         nojoin.add path

#       else:
#         res.add "import \""
#         res.add path
#         res.add "\""
#         res.add "\n"

#   res.add "\n\nmergedFileEnded()\n"
#   outPath.writeFile(res)

#   let withCov = false

#   if withCov:
#     let
#       dir = "/tmp/hmisc-test"
#       name = "test"

#     mkDir dir

#     let start = getCurrentDir()


#     cd dir
#     sh [
#       "nim",
#       "c",
#       "--nimcache:.",
#       "--debugger:native",
#       "--passC:--coverage",
#       "--passL:--coverage",
#       &"-o:{name}",
#       outPath
#     ]

#     sh &"lcov --base-directory . --directory . --zerocounters -q"
#     sh &"./{name}"
#     sh &"lcov --base-directory . --directory . -c -o {name}.info"

#     sh [
#       "lcov",
#       "--remove", &"{name}.info", "\"lib/*\"",
#       "--remove", &"{name}.info", "\"*generated_not_to_break*\"",
#       "--remove", &"{name}.info", &"\"{dir}/*\"",
#       "-o",
#       &"{name}.info"
#     ]

#     sh &"genhtml -o html {name}.info"
#     cd cwd

#   else:
#     sh "nim r", outPath
#     for file in nojoin:
#       sh "nim r \"" & file & "\""

proc check() =
  sh ["nimble", "testall"]
  sh ["nimble", "docgen"]


task push, "Execute checks and push ":
  check()
  sh ["git", "push", "origin", "master"]


task newversion, "Tag new version and push it to git":
  try:


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
      file = currentSourcePath()
      text = file.readFile()
      pos = text.find("version")
      endPos = text.find("\n", pos)
      newText = text[0 ..< pos] & &"version = \"{major}.{minor}.{patch}\"" & text[endPos .. ^1]

    file.writeFile(newText)
    sh ["git", "add", "hmisc.nimble"]

    let
      commitCmd = [
        "git", "commit", "-m", msg.quoteShell()]

      tag = &"v{major}.{minor}.{patch}"
      tagCmd = ["git", "tag", tag]

    sh commitCmd
    sh tagCmd

    sh ["git", "push", "origin", "master"]
    sh ["git", "push", "origin", tag]


  except OsError:
    echo "Have uncomitted changes, commit first before pushing"
    sh ["git", "--no-pager", "diff"]
