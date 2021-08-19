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

task docgen, "Generate documentation":
  if not fileExists("bin/hmisc-putils"):
    exec("nimble build")

  exec("""
hmisc-putils docgen \
  --ignore='**/treediff/*.nim' \
  --ignore='**/hcligen.nim'
""")

  # --ignore='**/zs_matcher.nim' \
  # --ignore='**/similarity_metrics.nim' \
  # --ignore='**/treediff_main.nim' \

import std/[os, strutils]

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
