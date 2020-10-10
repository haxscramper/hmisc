import sugar, strutils, sequtils, strformat
import ../src/hmisc/helpers
import ../src/hmisc/other/[hshell, oswrap, pathwrap]

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Pathwrap":
  test "test":
    assertEq AbsDir("/a/b/c") /../ 2 /../ RelDir("hello"), AbsDir("/hello")

  test "A":
    echo getNewTempDir()

    for path in "/tmp".walkDir():
      case path.kind:
        of pcDir:
          echo path
        else:
          discard

  test "dirs":
    for dir in parentDirs(cwd()):
      echo dir

  test "Os errros":
    try:
      discard newPathError(toAbsDir("/tmp"), pekExpectedRel):
        "Expected relative directory"

      let path = "12"
      raise newPathError(AbsFile("12"), pekExpectedAbs): fmtJoin:
        "Input path {path} has type {$typeof(path)}, but contains"
        "invalid string - expected absolute path"
    except:
      discard


suite "Shell":
  test "shell":
    expect ShellError:
      discard runShell("hello")

  test "Options":
    var cmd = makeGnuCmd("cat")
    # cmd["hello"] = "world"
    # cmd["nice"]
