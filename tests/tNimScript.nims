#!/usr/bin/env nim

import hmisc/other/nimbleutils

echo pkgVersion("hmisc")

echo listDirs("/tmp")

import hmisc/other/hshell

if false:
  discard runShell("nice")

  for line in iterstdout("hello"):
    discard line
