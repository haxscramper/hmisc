import std/[strutils, sequtils, strformat,
            deques, options, json, parseutils]

import hmisc/helpers

import unittest
import hmisc/other/[hshell, hshell_convert, oswrap]

suite "Hshell":
  test "Echo":
    assertEq runShell(ShellExpr "echo '1'").stdout.strip(), "1"

  test "Test json converter":
    proc outConvert(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
      if que.len > 0:
        return some(%["stdout", "file", que.popFirst])

    proc errConvert(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
      discard

    var cmd = makeGnuShellCmd("ls")

    let iter = makeShellJsonIter(
      cmd,
      outConvert,
      errConvert
    )

    for res in iter:
      if res[0].getStr() == "stdout":
        assert res[1].getStr() == "file"

  test "Nim compiler output":
    withTempDir(true):
      let file = "test.nim"
      file.writeFile("""
echo 1 * '2'
""")



      var cmd = makeNimShellCmd("nim")
      cmd.cmd "c"
      cmd - "r"
      cmd.arg file

      # execShell(cmd, doRaise = false)
      let iter = makeShellJsonIter(
        cmd,
        nimCmdOutConverter,
        nimCmdErrConverter,
        doRaise = false
      )

      for msg in iter:
        echo msg.pretty
