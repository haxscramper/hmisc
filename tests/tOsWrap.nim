import sugar, strutils, sequtils, strformat
import ../src/hmisc/other/[hshell, oswrap, pathwrap]

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Pathwrap":
  test "test":
    echo AbsDir("/a/b/c") /../ 2 /../ "hello"

