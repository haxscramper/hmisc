import std/[strutils, sequtils, strformat, deques, options, json]

import hmisc/helpers

import unittest
import hmisc/other/hshell

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
