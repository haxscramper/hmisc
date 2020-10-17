#!/usr/bin/env nim

import hmisc/other/[hshell, oswrap, nimbleutils]
import json

let parser = parseJson("""{"aas": 12}""")
echo parser

echo pkgVersion("hmisc")

# echo listDirs(AbsDir "/tmp")

# echo buildFsTree(allowExts = @["cfg", "cpp"])
# echo buildFsTree(AbsDir "/tmp", allowExts = @["nim"])

if false:
  discard runShell("nice")

  for line in iterstdout("hello"):
    discard line

withDir(AbsDir "/mnt/workspace/github/hmisc"):
  execShell("nimble docgen")
