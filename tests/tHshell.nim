import std/[
  strutils, sequtils, strformat, segfaults,
  deques, options, parseutils, enumerate
]

import
  hmisc/core/all,
  hmisc/algo/[hlex_base, hparse_base],
  hmisc/scripts/hmisc_putils,
  hmisc/preludes/unittest,
  hmisc/other/[
    hshell, hshell_convert, oswrap, hjson, nimbleutils
  ]

import
  hmisc/scripts/hmisc_putils

if not toBool($$CI):
  startHax()



suite "Shell output parser":
  test "Strace":
    var cmd = makeGnuShellCmd("strace echo '\"\"'")
    cmd.arg "ls"
    let (_, errIter) = makeShellRecordIter(
      cmd,
      straceOutConverter,
      straceErrConverter,
      doRaise = false
    )

    for entry in errIter:
      discard
      # echo entry

  test "Strace parser with string view":
    let cmd = shellCmd(strace, echo, "-")
    var text: string
    try:
      text = cmd.evalShell().stderr
    except ShellError:
      skip()

    var str = initPosStr(text)
    var strView = initPosStrView(str)

    var state: Option[HsLexer[StrTok]]
    while not strView.finished():
      discard straceErrConverter(strView, cmd, state)

    # echo "Err converter finished"

    state = none(HSLexer[StrTok])
    while not str.finished():
      discard straceErrConverter(str, cmd, state)


  test "ls json parser":
    var cmd = makeGnuShellCmd("ls")
    cmd - "l"
    cmd.arg "/tmp"

    let (outIter, _) = makeShellRecordIter(
      cmd,
      lslOutConverter,
      lslErrConverter,
      doRaise = false
    )

    for idx, msg in enumerate(outIter):
      if idx > 10:
        break
      else:
        discard msg["permissions"]

suite "Hshell":
  test "Echo":
    check runShell(ShellExpr "echo '1'").stdout.strip() == "1"

  test "Parallel process launch":
    var cmds = repeat((shellCmd(echo, "test"), false), 20)
    for res in runShellResult(cmds):
      discard

  # test "Process timeout":
  #   doAssert shelLResult(
  #     shellCmd(sleep, 1_000_000), execTimeoutMs = 1000).wasTerminated

  #   let res = shellResult(
  #     toShellCmd(shellCmd(echo, "1") && shellCmd(sleep, 1_000_000)),
  #     execTimeoutMs = 1000)

  #   check res.execResult.stdout == "1\n"
  #   doAssert res.wasTerminated


  # test "Test json converter":
  #   proc outConvert(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  #     if que.len > 0:
  #       return some(%["stdout", "file", que.popFirst])

  #   proc errConvert(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  #     discard

  #   var cmd = makeGnuShellCmd("ls")

  #   let iter = makeShellRecordIter(
  #     cmd,
  #     outConvert,
  #     errConvert
  #   )

  #   for res in iter:
  #     if res[0].getStr() == "stdout":
  #       assert res[1].getStr() == "file"


  test "Shell ast generation":
    check shAnd(
      shellCmd ls,
      shellCmd ls).toStr() == "ls && ls"

    check shOr(
      shellCmd ls,
      shellCmd ls, shAnd(shellCmd ls, shellCmd ls)).toStr() ==
      "ls || ls || (ls && ls)"

    check shellCmd(echo, -n, "hello").toStr() == "echo -n hello"
    check shellCmd(echo, -n, "\"he llo\"").toStr() == "echo -n '\"he llo\"'"
    check shellCmd(`dashed-name`).toStr() == "dashed-name"
    check shellCmd("string-name").toStr() == "string-name"
    check toStr($$hello < 12) == "[ $hello -lt 12 ]"

  test "Shell ast execution":
    var cmd: ShellAst
    cmd &&= ShellExpr("echo '12'")

    check evalShellStdout(cmd) == "12"

    execShell shAnd(shellCmd(true), shellCmd(true))
    execShell shOr(shellCmd "false", shellCmd(true))

    expect ShellError:
      execShell shAnd(shellCmd "false")

    check evalShellStdout(shellCmd(echo, -n, "hello")) == "hello"

    let cmd1: ShellCmd = shellCmd("sh").withIt do:
      it - "c"
      it.expr shellCmd("sh").withIt do:
        it - "c"
        it.expr shAnd(shellCmd(echo, 2), shellCmd(echo, 1))

    check cmd1.evalShellStdout() == "2\n1"
    execShell(cmd1)

  test "Operators":
    var cmd = &&[
      shellCmd(echo, -n, 0)
    ] && shellCmd(echo, -n, 2) && &&[
      shellCmd(echo, -n, 3),
      shellCmd(echo, -n, 4)
    ]

    var more: seq[ShellExpr]

    cmd &&= &&more

    check evalShellStdout(cmd) == "0234"

    more &= ShellExpr("echo -n 5")
    cmd &&= &&more

    check evalShellStdout(cmd) == "02345"

  test "Shell code execution":
    for oneline in [true, false]:
      let cmd = shStmtList(
        shAsgn($$i, "0"),
        shWhile(
          ($$i < 4),
          shellCmd(echo, "[hello]"),
          shAsgn($$i, $$i + 1)
        )
      )

      let expr = cmd.toStr(oneline = oneline)

      check evalShellStdout(ShellExpr(expr)) == "[hello]\n[hello]\n[hello]\n[hello]"

  test "Shell ast & makeShellCmd":
    block:
      let cmd = shellCmd("nimble", "install")
      # Nice side effect - you can now comment on different flags and use
      # checks/loops without worrying about correct
      # spacing/concatnation/prefixes etc.
      let doCleanup = true
      let dockerCmd = shellCmd("docker").withIt do:
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
            shellCmd(cd, "/project/main")
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
            shellCmd cd, "/project"
            shellCmd cd, "/project"
            shOr:
              shellCmd ls, a
              shellCmd ls, a
              shellCmd ls, a
