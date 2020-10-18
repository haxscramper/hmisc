#!/usr/bin/env nim

import hmisc/other/[hshell, oswrap, nimbleutils]
import json

let parser = parseJson("""{"aas": 12}""")
echo parser

echo pkgVersion("hmisc")
echo getEnv(ShellVar "HOME")

# echo listDirs(AbsDir "/tmp")

# echo buildFsTree(allowExts = @["cfg", "cpp"])
# echo buildFsTree(AbsDir "/tmp", allowExts = @["nim"])

echo interpolateShell("Hello${pwd}", doRaise = true).get()

if false:
  discard runShell(ShellExpr "nice")

  for line in iterstdout(ShellExpr "hello"):
    discard line


# withDir(AbsDir "/mnt/workspace/github/hmisc"):
#   execShell("nimble docgen")
