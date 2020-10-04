#!/usr/bin/env nim

import hmisc/other/nimbleutils
import hmisc/other/hshell

echo pkgVersion("hmisc")

echo listDirs("/tmp")


if false:
  discard runShell("nice")

  for line in iterstdout("hello"):
    discard line
