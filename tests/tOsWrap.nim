import sugar, strutils, sequtils, strformat
import ../src/hmisc/other/[hshell, oswrap, pathwrap]

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Pathwrap":
  test "test":
    echo AbsDir("/a/b/c") /../ 2 /../ "hello"

  test "A":
    echo getNewTempDir()

    for path in "/tmp".walkDir():
      case path.kind:
        of pcDir:
          echo path
        else:
          discard

