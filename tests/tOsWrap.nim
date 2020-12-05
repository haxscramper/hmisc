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

    for path in toAbsDir("/tmp").walkDir():
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
      discard runShell(ShellExpr "hello")

  test "Options":
    var cmd = makeGnuShellCmd("cat")
    # cmd["hello"] = "world"
    # cmd["nice"]

  test "OS":
    echo getCurrentOs()
    static:
      echo getCurrentOs()

    when not isPackageInstalled("hunspell"):
      echo "Install hunspell using ", getInstallCmd("hunspell")

    static:
      let missing = getMissingDependencies({
        { Distribution.ArchLinux } : @["hunspell-12"]
      })

      for (pkg, cmd) in missing:
        echo "Missing ", pkg, ", install it using ", cmd


suite "User directories":
  test "xdg":
    echo getUserConfigDir()
    echo getAppConfigDir()

    echo getUserCacheDir()
    echo getAppCacheDir()

    echo getUserDataDir()
    echo getAppDataDir()

    echo getUserRuntimeDir()
    echo getAppRuntimeDir()

