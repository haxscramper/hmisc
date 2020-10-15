#!/usr/bin/env nim

import hmisc/other/[hshell, oswrap, nimbleutils]

echo pkgVersion("hmisc")

# echo listDirs(AbsDir "/tmp")

echo buildFsTree(allowExts = @["cfg", "cpp"])
echo buildFsTree(AbsDir "/tmp", allowExts = @["nim"])

if false:
  discard runShell("nice")

  for line in iterstdout("hello"):
    discard line
