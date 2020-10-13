when not defined(NimScript):
  import osproc, streams
else:
  type
    ProcessOption* = enum
      poEchoCmd
      poUsePath
      poEvalCommand
      poStdErrToStdOut
      poParentStreams
      poInteractive
      poDaemon

import oswrap
import strutils, strformat, sequtils

const hasStrtabs = cbackend or (NimMajor, NimMinor, NimPatch) > (1, 2, 6)

when hasStrtabs: # https://github.com/nim-lang/Nim/pull/15172
  import strtabs

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
# TODO Support command chaining using `&&`, `||` (`and`, `or`) and pipes
#      `|` for redirecting output streams.

type
  ShellExecEffect = object of IOEffect
  ShellError* = ref object of OSError
    cmd*: string ## Command that returned non-zero exit code
    cwd*: AbsDir ## Absolute path of initial command execution directory
    retcode*: int ## Exit code
    # TODO REFACTOR rename to `stdout` and `stderr`
    errstr*: string ## Stderr for command
    outstr*: string ## Stdout for command

  CmdFlagConf* = enum
    ccRegularFlags ## `-f` or `--flag`
    ccOneDashFlags ## `-f` or `-flag`

  CmdConf = object
    flagConf*: CmdFlagConf
    kvSep*: string

  CmdPartKind* = enum
    cpkSubCommand
    cpkArgument
    cpkOption
    cpkFlag
    cpkRaw

  CmdPart* = object
    case kind*: CmdPartKind
      of cpkSubCommand:
        subcommand*: string
      of cpkArgument:
        argument*: string
      of cpkFlag:
        flag*: string
      of cpkOption:
        key*: string
        value*: string

        case overrideKv*: bool ## Override key-value separator for
          ## configuration. Used in cases like `-I` flag in C
          ## compilers that othewise handle `--key=value` pairs.
          of true:
            kvSep*: string
          of false:
            discard

      of cpkRaw:
        rawstring*: string

  Cmd* = object
    bin: string
    opts: seq[CmdPart]
    conf: CmdConf
    envVals: seq[tuple[key, val: string]]

const
  GnuCmdConf* = CmdConf(
    flagConf: ccRegularFlags,
    kvSep: "="
  )

  NimCmdConf* = CmdConf(
    flagConf: ccRegularFlags,
    kvSep: ":"
  )

  X11CmdConf* = CmdConf(
    flagConf: ccOneDashFlags,
    kvSep: " "
  )

converter toCmd*(a: string): Cmd =
  ## Implicit conversion of string to command
  ##
  ## WARNING: `cmd` will be treated as `bin` and if `poEvalCommand` is
  ## used, execution of command will most likely fail at runtime.
  ##
  ## NOTE: `GnuCmdConf` is used
  result.conf = GnuCmdConf
  result.bin = a

func flag*(cmd: var Cmd, fl: string) =
  ## Add flag for command
  cmd.opts.add CmdPart(kind: cpkFlag, flag: fl)

func opt*(cmd: var Cmd, inKey, val: string) =
  ## Add option (key-value pairs) for command
  cmd.opts.add CmdPart(kind: cpkOption, key: inKey, value: val)

func env*(cmd: var Cmd, key, val: string): void =
  ## Add environment variable configuration for command
  cmd.envVals.add (key, val)

func opt*(cmd: var Cmd, opts: openarray[tuple[key, val: string]]) =
  ## Add sequence of key-value pairs
  for (key, val) in opts:
    cmd.opt(key, val)

func subCmd*(cmd: var Cmd, sub: string) =
  ## Add subcommand
  cmd.opts.add CmdPart(kind: cpkSubCommand, subcommand: sub)

func cmd*(cmd: var Cmd, sub: string) =
  ## Add subcommand
  cmd.opts.add CmdPart(kind: cpkSubCommand, subcommand: sub)

func raw*(cmd: var Cmd, str: string) =
  ## Add raw string for command (for things like `+2` that are not
  ## covered by default options)
  cmd.opts.add Cmdpart(kind: cpkRaw, rawstring: str)

func arg*(cmd: var Cmd, arg: string) =
  ## Add argument for command
  cmd.opts.add CmdPart(kind: cpkArgument, argument: arg)

func `-`*(cmd: var Cmd, fl: string) =
  ## Add flag for command
  cmd.flag fl

func `-`*(cmd: var Cmd, kv: (string, string)) =
  ## Add key-value pair for command
  cmd.opt(kv[0], kv[1])

func `-`*(cmd: var Cmd, kv: tuple[key, sep, val: string]) =
  cmd.opts.add CmdPart(
    kind: cpkOption, key: kv.key, value: kv.val, overrideKv: true,
    kvSep: kv.sep)

func makeNimCmd*(bin: string): Cmd =
  result.conf = NimCmdConf
  result.bin = bin

func makeX11Cmd*(bin: string): Cmd =
  ## Create command for `X11` cli tools (single dash)
  result.conf = X11CmdConf
  result.bin = bin

func makeGnuCmd*(bin: string): Cmd =
  ## Create command for CLI applications that conform to GNU standard
  ## for command line interface `link
  ## <https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html>`_
  result.conf = GnuCmdConf
  result.bin = bin

func makeFileCmd*(file: string, conf: CmdConf = GnuCmdConf): Cmd =
  result.conf = conf
  if file.startsWith("/"):
    result.bin = file
  else:
    result.bin = "./" & file

# func quoteShell*(str: string): string = str

func toStr*(part: CmdPart, conf: CmdConf): string =
  let longPrefix =
    case conf.flagConf:
      of ccRegularFlags: "--"
      of ccOneDashFlags: "-"

  case part.kind:
    of cpkRaw:
      return part.rawstring
    of cpkSubCommand:
      return part.subcommand
    of cpkFlag:
      if part.flag.len > 1:
        return longPrefix & part.flag
      else:
        return "-" & part.flag
    of cpkOption:
      let kv = if part.overrideKv: part.kvSep else: conf.kvSep
      if part.key.len > 1:
        return longPrefix & part.key & kv & part.value.quoteShell()
      else:
        return "-" & part.key & kv & part.value.quoteShell()
    of cpkArgument:
      return part.argument.quoteShell()


func toStr*(cmd: Cmd): string =
  (@[ cmd.bin ] & cmd.opts.mapIt(it.toStr(cmd.conf))).join(" ")

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

when cbackend:
  proc startShell*(
    cmd: Cmd, options: set[ProcessOption] = {
      poEvalCommand, poParentStreams}): Process =

    when hasStrtabs:
      result = startProcess(
        cmd.toStr(),
        options = options,
        env = if cmd.envVals.len > 0: newStringTable(cmd.envVals) else: nil
      )
    else:
      if cmd.envVals.len > 0:
        raiseAssert(
          "Env variable passing is not supported for nimscript <= 1.2.6")
      else:
        result = startProcess(cmd.toStr(), options = options)

    if result.isNil:
      raise ShellError(
        msg: "Command '" & cmd.toStr() & "' failed to start",
        cwd: getCurrentDir(),
        cmd: cmd.toStr()
      )

proc runShell*(
  cmd: Cmd,
  doRaise: bool = true,
  stdin: string = "",
  options: set[ProcessOption] = {poEvalCommand},
  maxErrorLines: int = 12,
  discardOut: bool = false
             ): tuple[
  stdout, stderr: string, code: int] {.tags: [
    ShellExecEffect,
    ExecIOEffect,
    ReadEnvEffect,
    RootEffect
    ].} =
  ## Execute shell command and return it's output. `stdin` - optional
  ## parameter, will be piped into process. `doRaise` - raise
  ## exception (default) if command finished with non-zero code.
  ## `command` - text of the command.
  ## ## Arguments
  ## :maxErrorLines: max number of stderr lines that would be appended to
  ##   exception. Any stderr beyond this range will be truncated
  let
    env = cmd.envVals
    command = cmd.toStr()

  if not discardOut and (poParentStreams in options):
    # TODO add support for showing output *and* piping results. This
    # will certainly involve some fuckery with filtering out ansi
    # codes (because colored output is basically /the/ reason to have
    # piping to parent shell).
    raiseAssert(
      "Stream access not allowed when you use poParentStreams. " &
        "Either set `discardOut` to true or remove `poParentStream` from options"
    )

  when not defined(NimScript):
    let pid =
      if poEvalCommand in options:
        startProcess(
          command,
          options = options,
          env = if env.len > 0: newStringTable(env) else: nil
        )
      else:
        startProcess(
          cmd.bin,
          options = options,
          args = cmd.opts.mapIt(it.toStr(cmd.conf)),
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
      result.stderr = pid.errorStream.readAll()
    else:
      while pid.running():
        discard

    result.code = pid.peekExitCode()
    close(pid)

  else:
    let nscmd = &"cd {cwd()} && " & command
    if poParentStreams in options:
      exec(nscmd)
    else:
      let (res, code) = gorgeEx(nscmd, "", "")

      if not discardOut:
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
      $cwd() & &"\n{envAdd}Exited with non-zero code:\n"

    let split =
      when cbackend:
        result.stderr.split("\n")
      else:
        result.stdout.split("\n")

    msg.add split[0 ..< min(split.len(), maxErrorLines)].join("\n")

    raise ShellError(
      msg: msg,
      retcode: result.code,
      errorCode: int32(result.code),
      errstr: result.stderr,
      outstr: result.stdout,
      cwd: cwd(),
      cmd: command
    )


proc execShell*(cmd: string): void =
  ## `shExec` overload for regular string.
  ##
  ## WARNING see implicit `toCmd` documentation for potential
  ## pitfalls. It is recommended to use `shExec(cmd: Cmd)` overload -
  ## this version exists only for quick prototyping.
  discard runShell(cmd, discardOut = true, options = {
    poEvalCommand, poParentStreams, poUsePath})


proc evalShell*(cmd: string): auto =
  var opts = {poEvalCommand, poUsePath}
  runShell(cmd, options = opts)

proc evalShellStdout*(cmd: string): string =
  let res = runShell(cmd, options = {poEvalCommand, poUsePath})
  return res.stdout


proc execShell*(cmd: Cmd): void =
  ## Execute shell command with stdout/stderr redirection into parent
  ## streams. To capture output use `runShell`
  discard runShell(cmd, doRaise = true, discardOut = true, options = {
    poParentStreams, poUsePath})
