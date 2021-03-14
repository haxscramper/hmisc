import std/[strutils, sequtils, strformat, segfaults,
            deques, options, parseutils, enumerate, unittest]

import hmisc/helpers
import hmisc/other/[hshell, hshell_convert, oswrap, hjson, nimbleutils]

import hmisc/scripts/hmisc_putils

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

    let iter = makeShellRecordIter(
      cmd,
      outConvert,
      errConvert
    )

    for res in iter:
      if res[0].getStr() == "stdout":
        assert res[1].getStr() == "file"

  test "json parser":
    var cmd = makeGnuShellCmd("ls")
    cmd - "l"
    cmd.arg "/tmp"

    let iter = makeShellRecordIter(
      cmd,
      lslOutConverter,
      lslErrConverter,
      doRaise = false
    )

    for idx, msg in enumerate(iter):
      if idx > 10:
        break
      else:
        discard msg["permissions"]

  test "Strace parser":
    var cmd = makeGnuShellCmd("strace")
    cmd.arg "ls"
    let iter = makeShellRecordIter(
      cmd,
      straceOutConverter,
      straceErrConverter,
      doRaise = false
    )

    for entry in iter:
      discard
      # echo entry


  test "Shell ast generation":
    assertEq shAnd(shCmd ls, shCmd ls).toStr(), "ls && ls"
    assertEq shOr(shCmd ls, shCmd ls, shAnd(shCmd ls, shCmd ls)).toStr(),
      "ls || ls || (ls && ls)"

    assertEq shCmd(echo, -n, "hello").toStr(), "echo -n hello"
    assertEq shCmd(echo, -n, "\"he llo\"").toStr(), "echo -n '\"he llo\"'"
    assertEq shCmd(`dashed-name`).toStr(), "dashed-name"
    assertEq shCmd("string-name").toStr(), "string-name"
    assertEq toStr($$hello < 12), "[ $hello -lt 12 ]"

  test "Shell ast execution":
    var cmd: ShellAst
    cmd &&= ShellExpr("echo '12'")

    assertEq evalShellStdout(cmd), "12"

    execShell shAnd(shCmd "true", shCmd "true")
    execShell shOr(shCmd "false", shCmd "true")

    expect ShellError:
      execShell shAnd(shCmd "false")

    assertEq evalShellStdout(shCmd(echo, -n, "hello")), "hello"

    let cmd1: ShellCmd = shCmd("sh").withIt do:
      it - "c"
      it.expr shCmd("sh").withIt do:
        it - "c"
        it.expr shAnd(shCmd(echo, 2), shCmd(echo, 1))

    assertEq cmd1.evalShellStdout(), "2\n1"
    execShell(cmd1)

  test "Operators":
    var cmd = &&[
      shCmd(echo, -n, 0)
    ] && shCmd(echo, -n, 2) && &&[
      shCmd(echo, -n, 3),
      shCmd(echo, -n, 4)
    ]

    var more: seq[ShellExpr]

    cmd &&= &&more

    assertEq evalShellStdout(cmd), "0234"

    more &= ShellExpr("echo -n 5")
    cmd &&= &&more

    assertEq evalShellStdout(cmd), "02345"

  test "Shell code execution":
    for oneline in [true, false]:
      let cmd = shStmtList(
        shAsgn($$i, "0"),
        shWhile(
          ($$i < 4),
          shCmd(echo, "[hello]"),
          shAsgn($$i, $$i + 1)
        )
      )

      let expr = cmd.toStr(oneline = oneline)

      assertEq evalShellStdout(ShellExpr(expr)),
        "[hello]\n[hello]\n[hello]\n[hello]"

  test "Shell ast & makeShellCmd":
    block:
      let cmd = shCmd("nimble", "install")
      # Nice side effect - you can now comment on different flags and use
      # checks/loops without worrying about correct
      # spacing/concatnation/prefixes etc.
      let doCleanup = true
      let dockerCmd = shCmd("docker").withIt do:
        it.cmd "run" # Add subcommand
        it - "i"
        it - "t"
        if doCleanup:
          it - "rm" # Remove container after test execution
        it - ("v", "/tmp/tmp-mount:/project") # Key-value pair
        it.arg "nim-base"
        it.arg "sh"
        it - "c"
        it.expr:
          shAnd:
            shCmd(cd, "/project/main")
            cmd # Can easily build complicated commands from variables


      if false:
        execShell(dockerCmd)

    block:
      let doCleanup = true
      let tmpDir = "/tmp/docker"
      let cmd = makeGnuShellCmd("docker").withIt do:
        it.cmd "run" # Add subcommand
        it - "i"
        it - "i"
        if doCleanup:
          it - "rm" # Remove container after test execution
        it - ("v", $tmpDir & ":/project") ## Key-value pair
        it.arg "nim-base"
        it.arg "sh"
        it - "c"
        it.expr:
          shAnd:
            shCmd cd, "/project"
            shCmd cd, "/project"
            shOr:
              shCmd ls, a
              shCmd ls, a
              shCmd ls, a
