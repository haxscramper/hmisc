version       = "0.11.19"
author        = "haxscramper"
description   = "Collection of helper utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "hmisc"
bin           = @["hmisc/scripts/hmisc_putils"]
installExt    = @["nim"]
binDir        = "bin"

when (1, 2, 2) < (NimMajor, NimMinor, NimPatch):
  namedBin      = {
    "hmisc/scripts/hmisc_putils" : "hmisc-putils"
  }.toTable()

requires "nim >= 1.4.0"
requires "fusion"
requires "benchy >= 0.0.1"
requires "jsony >= 1.0.4"
requires "unicodedb >= 0.9.0"
# requires "https://github.com/haxscramper/fusion.git#matching-fixup"

import std/[os, strutils, strformat]


task docgen, "Generate documentation":
  var files: seq[(string, string)]

  let
    cwd = getCurrentDir()
    res = "/tmp" # &"{cwd}/docs"
    cdd = "src/hmisc"

  cd cdd

  var cnt = 0
  var resDirs: seq[string]
  var hmiscText: string

  for path in walkDirRec(".", relative = true):
    let (dir, name, ext) = path.splitFile()
    if name != "hmisc" and ext == ".nim":
      if dir.len > 0 and cnt < 10:
        inc cnt
        hmiscText.add &"import ./hmisc/{dir}/{name}\n"

  cd ".."
  writeFile("hmisc.nim", hmiscText)
  var args = @[
    "nim",
    "doc",
    "--project",
    "--warnings:off"
  ]

  let commit = getEnv("GITHUB_SHA")
  if commit.len > 0: args.add &"--git.commit:{commit}"

  let ghref = getEnv("GITHUB_REF").split("/")[^1]
  if ghref.len > 0: args.add &"--git.devel:{ghref}"

  let ghUrl = getEnv("GITHUB_REPOSITORY", &"file://{res}")
  if ghUrl.len > 0: args.add &"--git.url:\"{ghUrl}\""

  args.add "hmisc.nim"

  let cmd = join(args, " ")
  echo cmd
  exec cmd
    # let outPath = &"{res}/{dir}/{name}.html"
      # var cmd = &"nim doc --warnings:off --git.commit:{commit} --git.devel:{ghref} --git.url:{ghUrl} -o:{outPath}"
      # cmd &= &" {cwd}/{cdd}/{path.quoteShell()}"
      # files.add (&"{ghUrl}/{dir}/{name}.html", name)
      # echo cmd

      # if cnt < 10:
      #   resDirs.add &"{res}/{dir}"
      #   inc cnt
      #   exec cmd

#   var theindex = """
# <html>
# <head>
#   <title>{{title}}</title>
# </head>
# <body>

# <ul>
# """

#   for (file, name) in files:
#     theindex.add &"<li><a href={file}>{name}</a></li>\n"

#   theindex.add """
# </ul>

# </body>
# </html>"""

#   echo theindex

#   for dir in resDirs:
#     echo dir
#     writeFile(&"{dir}/theindex.html", theindex)

#   if not fileExists("bin/hmisc-putils"):
#     exec("nimble build")

#   exec("""
# hmisc-putils docgen \
#   --ignore='**/treediff/*.nim' \
#   --ignore='**/hcligen.nim'
# """)

  # --ignore='**/zs_matcher.nim' \
  # --ignore='**/similarity_metrics.nim' \
  # --ignore='**/treediff_main.nim' \


task testall, "Merge all tests and run them":
  let outPath = "/tmp/tmp_tests_all.nim"
  var res: string
  res.add """
{.define: hunittestMerge.}
import hmisc/other/hunittest
{.push warning[UnusedImport]:off.}
"""
  let cwd = getCurrentDir()
  var nojoin: seq[string]
  for (kind, path) in walkDir("tests", relative = false):
    if kind == pcFile and path.splitFile().ext == ".nim":
      let path = cwd / path
      let content = path.readFile()
      if content.find("joinable: false") != -1:
        nojoin.add path

      else:
        res.add "import \""
        res.add path
        res.add "\""
        res.add "\n"

  res.add "\n\nmergedFileEnded()\n"
  outPath.writeFile(res)
  exec("nim r " & outPath)
  for file in nojoin:
    exec("nim r \"" & file & "\"")

task dockertest, "Run tests in docker container":
  exec("hmisc-putils dockertest --projectDir:" & thisDir())

# after test:
#   exec("nim c --hints:off --verbosity:0 src/hmisc/scripts/hmisc_putils.nim")
