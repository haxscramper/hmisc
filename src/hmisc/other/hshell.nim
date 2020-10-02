when not defined(NimScript):
  import osproc, streams, os

import strutils, strformat, strtabs, sequtils
# import ../algo/halgorithm

# TODO better way of building command for execution.
# TODO overload for `runShell` that accepts callbacks failed execution.
# TODO generate log calls?
# TODO easy way to pipe things to stdout
# TODO pretty-print long shell commands on failure - can split on `&&`
#      and left-align commands. Highlight `--flags`, `commands` and
#      arguments.
# TODO option to force colored output when shell runner
# TODO implement functions for callsite checks in program execution
#      Determine if all file parameters are present (create separate
#      `fileArg` procedure), if binary itself is available and so on.

type
  ShellError* = ref object of OSError
    cmd*: string ## Command that returned non-zero exit code
    cwd*: string ## Absolute path of initial command execution directory
    retcode*: int ## Exit code
    errstr*: string ## Stderr for command
    outstr*: string ## Stdout for command

  CmdConf* = enum
    ccNimFlags
    ccRegularFlags
    ccOneDashFlags

  Cmd* = object
    bin*: string
    opts: seq[string]
    conf: CmdConf
    envVals: seq[tuple[key, val: string]]

func toBegin(cmd: Cmd, fl: string): string =
  if cmd.conf == ccOneDashFlags or fl.len == 1:
    &"-{fl}"
  else:
    &"--{fl}"


func flag*(cmd: var Cmd, fl: string) =
  cmd.opts.add(cmd.toBegin(fl))

func opt*(cmd: var Cmd, inKey, val: string) =
  let key = cmd.toBegin(inKey)
  cmd.opts.add case cmd.conf:
    of ccNimFlags: &"{key}:{val}"
    else:
      if inKey.len == 1:
        &"{key}{val}"
      else:
        &"{key}={val}"

func env*(cmd: var Cmd, key, val: string): void =
  cmd.envVals.add (key, val)

func opt*(cmd: var Cmd, opts: openarray[tuple[key, val: string]]) =
  for (key, val) in opts:
    cmd.opt(key, val)

func cmd*(cmd: var Cmd, sub: string) =
   cmd.opts.add sub

func raw*(cmd: var Cmd, str: string) =
  cmd.opts.add str

func strArg*(cmd: var Cmd, sub: string) =
  cmd.opts.add:
    if not ((sub[0] == '"') and (sub[^1] == '"')):
      &"\"{sub}\""
    else:
      sub

func arg*(cmd: var Cmd, arg: string) =
  cmd.opts.add arg

func makeNimCmd*(bin: string): Cmd =
  result.conf = ccNimFlags
  result.bin = bin

func makeX11Cmd*(bin: string): Cmd =
  ## Create command for `X11` cli tools (single dash)
  result.conf = ccOneDashFlags
  result.bin = bin

func makeGnuCmd*(bin: string): Cmd =
  ## Create command for CLI applications that conform to GNU standard
  ## for command line interface `link
  ## <https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html>`_
  result.conf = ccRegularFlags
  result.bin = bin

func makeFileCmd*(file: string, conf: CmdConf = ccRegularFlags): Cmd =
  result.conf = conf
  if file.startsWith("/"):
    result.bin = file
  else:
    result.bin = "./" & file

func toStr*(cmd: Cmd): string =
  (@[ cmd.bin ] & cmd.opts).join(" ")

when not defined(NimScript):
  proc printShellError*() =
    when defined(NimScript):
      echo getCurrentExceptionMsg()
    else:
      let err = ShellError(getCurrentException())
      echo err.errstr

      echo err.outstr

iterator iterstdout*(command: string): string =
  # TODO raise exception on failed command
  # REVIEW how cleanup is performed when iterator finishes main loop?
  when defined(NimScript):
    let (res, code) = gorgeEx(command, "", "")
    for line in res.split("\n"):
      yield line
  else:
    let pid = startProcess(command, options = {poEvalCommand})

    let outStream = pid.outputStream
    var line = ""

    while pid.running:
      try:
        let streamRes = outStream.readLine(line)
        if streamRes:
          yield line
      except IOError, OSError:
        assert outStream.isNil

    let rem = outStream.readAll().split("\n")
    for line in (if rem.len > 0: rem[0..^2] else: rem):
      yield line

proc startShell*(
  cmd: Cmd, options: set[ProcessOption] = {
    poEvalCommand, poParentStreams}): Process =

  result = startProcess(
    cmd.toStr(),
    options = options,
    env = if cmd.envVals.len > 0: newStringTable(cmd.envVals) else: nil
  )

  if result.isNil:
    raise ShellError(
      msg: "Command '" & cmd.toStr() & "' failed to start",
      cwd: getCurrentDir(),
      cmd: cmd.toStr()
    )

proc runShell*(
  command: Cmd,
  doRaise: bool = true,
  stdin: string = "",
  # env: seq[tuple[key, val: string]] = @[],
  options: set[ProcessOption] = {poEvalCommand},
  maxErrorLines: int = 12,
  discardOut: bool = false
             ): tuple[
  stdout, stderr: string, code: int] =
  ## Execute shell command and return it's output. `stdin` - optional
  ## parameter, will be piped into process. `doRaise` - raise
  ## exception (default) if command finished with non-zero code.
  ## `command` - text of the command.
  ## ## Arguments
  ## :maxErrorLines: max number of stderr lines that would be appended to
  ##   exception. Any stderr beyond this range will be truncated

  let
    env = command.envVals
    command = command.toStr()

  if not discardOut and (poParentStreams in options):
    raiseAssert(
      "Stream access not allowed when you use poParentStreams. " &
        "Either set `discardOut` to true or remove `poParentStream` from options"
    )

  when not defined(NimScript):
    let pid = startProcess(
      command,
      options = options,
      env = if env.len > 0: newStringTable(env) else: nil
    )

    if not discardOut:
      let ins = pid.inputStream()
      ins.write(stdin)
      # ins.flush()
      ins.close()

      let outStream = pid.outputStream
      var line = ""

      while pid.running:
        try:
          let streamRes = outStream.readLine(line)
          if streamRes:
            result.stdout &= line & "\n" # WARNING remove trailing newline
                                         # on the stdout
        except IOError, OSError:
          assert outStream.isNil
          echo "process died" # NOTE possible place to raise exception

      result.stdout &= outStream.readAll()
      result.code = pid.peekExitCode()
      result.stderr = pid.errorStream.readAll()
    else:
      while pid.running():
        discard

    close(pid)

  else:
    let nscmd = &"cd {getCurrentDir()} && " & command
    if not discardOut:
      let (res, code) = gorgeEx(nscmd, "", "")
      result.stdout = res
      result.code = code


  if doRaise and result.code != 0:
    let envAdd =
      if env.len > 0:
        "With env variables " &
          env.mapIt(&"{it.key}={it.val}").join(" ") & "\n"
      else:
        ""

    var msg = &"Command '{command}'\nExecuted in directory " &
      getCurrentDir() & &"\n{envAdd}Exited with non-zero code:\n"

    let split = result.stderr.split("\n")
    msg.add split[0 ..< min(split.len(), maxErrorLines)].join("\n")

    raise ShellError(
      msg: msg,
      retcode: result.code,
      errorCode: int32(result.code),
      errstr: result.stderr,
      outstr: result.stdout,
      cwd: getCurrentDir(),
      cmd: command
    )

# pr# oc runShell*(
  # cmd: Cmd,
  # doRaise: bool = true,
  # stdin: string = "",
  # env: seq[tuple[key, val: string]] = @[],
  # options: set[ProcessOption] = {poEvalCommand},
  # maxErrorLines: int = 12
  # discardOut: bool = false
  #            ): tuple[
  # stdout, stderr: string, code: int] =
  # cmd.toStr().runShell(
  #   env = cmd.envVals,
  #   options = options,
  #   maxErrorLines = maxErrorLines,
  #   discardOut = discardOut
  # )

# when not defined(nimscript):
#   template withDir*(dir: string; body: untyped): untyped =
#     var curDir = getCurrentDir()
#     try:
#       setCurrentDir(dir)
#       body
#     finally:
#       setCurrentDir(curDir)
