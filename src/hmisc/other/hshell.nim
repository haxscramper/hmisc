import parseutils

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
import std/[strutils, strformat, sequtils, options, deques, json]

const hasStrtabs = cbackend or (NimMajor, NimMinor, NimPatch) > (1, 2, 6)

when hasStrtabs: # https://github.com/nim-lang/Nim/pull/15172
  import std/strtabs

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
# TODO move command-line flags collections into separate type to use for
#      working with external libraries that accept list of command-line
#      flags (like libclang for example)
# TODO Add 'subshell' command type - for passing strings that are shell
#      expression themselves. Correct quoting etc.

export ShellVar, ShellExpr

type
  ShellExecEffect = object of IOEffect
  ShellError* = ref object of OSError
    cmd*: string ## Command that returned non-zero exit code
    cwd*: AbsDir ## Absolute path of initial command execution directory
    retcode*: int ## Exit code
    # TODO REFACTOR rename to `stdout` and `stderr`
    errstr*: string ## Stderr for command
    outstr*: string ## Stdout for command

  ShellExecResult* = tuple[stdout, stderr: string, code: int] ## Shell command
  ## execution result - standard error, output and return code.
  ShellResult* = object
    execResult*: ShellExecResult ## Result of command execution
    hasBeenSet*: bool ## Whether or not result been set
    case resultOk*: bool ## No failures (return code == 0)
      of true:
        nil
      of false:
        exception*: ShellError ## Addiional information in case of failure. Can
                               ## be raised or immediately inspected on return.

  ShellCmdFlagConf* = enum
    ## Shell command flags syntax
    ccRegularFlags ## `-f` or `--flag`
    ccOneDashFlags ## `-f` or `-flag`

  ShellCmdConf = object
    flagConf*: ShellCmdFlagConf
    kvSep*: string

  ShellCmdPartKind* = enum
    cpkSubCommand ## Subcommand string
    cpkArgument ## String argument to command
    cpkOption ## Key-value pair
    cpkFlag ## Boolean flag (on/off)
    cpkRaw ## Raw parameter
    cpkSubExpr ## Another shell subexpression,
    ## for things like `sh -c "some code"`

  ShellCmdPart* = object
    case kind*: ShellCmdPartKind
      of cpkSubCommand:
        subcommand*: string
      of cpkArgument:
        argument*: string
      of cpkFlag:
        flag*: string
      of cpkOption:
        key*: string
        val*: string

        case overrideKv*: bool ## Override key-value separator for
          ## configuration. Used in cases like `-I` flag in C
          ## compilers that othewise handle `--key=value` pairs.
          of true:
            kvSep*: string
          of false:
            discard

      of cpkRaw:
        rawstring*: string
      of cpkSubExpr:
        expr*: ShellExpr

  ShellCmd* = object
    bin: string
    opts: seq[ShellCmdPart]
    conf: ShellCmdConf
    envVals: seq[tuple[key, val: string]]

const
  GnuShellCmdConf* = ShellCmdConf(
    flagConf: ccRegularFlags,
    kvSep: "="
  )

  NimShellCmdConf* = ShellCmdConf(
    flagConf: ccRegularFlags,
    kvSep: ":"
  )

  X11ShellCmdConf* = ShellCmdConf(
    flagConf: ccOneDashFlags,
    kvSep: " "
  )

converter toShellCmd*(a: ShellExpr): ShellCmd =
  ## Implicit conversion of string to command
  ##
  ## WARNING: `cmd` will be treated as `bin` and if `poEvalCommand` is
  ## used, execution of command will most likely fail at runtime.
  ##
  ## NOTE: `GnuShellCmdConf` is used
  result.conf = GnuShellCmdConf
  result.bin = a.string

func initCmdOption*(key, val: string): ShellCmdPart =
  ## Create shell command option
  ShellCmdPart(kind: cpkOption, key: key, val: val)

func initCmdFlag*(fl: string): ShellCmdPart =
  ## Create shell command flag
  ShellCmdPart(kind: cpkFlag, flag: fl)

proc initCmdEnvOrOption*(
  env: ShellVar,
  key, val: string, allowEmpty: bool = false): ShellCmdPart =
  ## Init shell command value for key `key` either from environment variable
  ## `env` or using provided fallback value `val`. `allowEmpty` - whether or not
  ## to use value if environment variable is empty (but exists).

  result = ShellCmdPart(kind: cpkOption, key: key)
  if existsEnv(env) and (getEnv(env).len > 0 or allowEmpty):
    result.val = getEnv(env)
  else:
    result.val = val


func isEmpty*(cmd: ShellCmd): bool =
  (cmd.bin.len == 0) and (cmd.opts.len == 0)

func flag*(cmd: var ShellCmd, fl: string) =
  ## Add flag for command
  cmd.opts.add ShellCmdPart(kind: cpkFlag, flag: fl)

func opt*(cmd: var ShellCmd, inKey, val: string) =
  ## Add option (key-value pairs) for command
  cmd.opts.add ShellCmdPart(kind: cpkOption, key: inKey, val: val)

func env*(cmd: var ShellCmd, key, val: string): void =
  ## Add environment variable configuration for command
  cmd.envVals.add (key, val)

func opt*(cmd: var ShellCmd, opts: openarray[tuple[key, val: string]]) =
  ## Add sequence of key-value pairs
  for (key, val) in opts:
    cmd.opt(key, val)

func cmd*(cmd: var ShellCmd, sub: string) =
  ## Add subcommand
  cmd.opts.add ShellCmdPart(kind: cpkSubCommand, subcommand: sub)

func raw*(cmd: var ShellCmd, str: string) =
  ## Add raw string for command (for things like `+2` that are not
  ## covered by default options)
  cmd.opts.add ShellCmdpart(kind: cpkRaw, rawstring: str)

func expr*(cmd: var ShellCmd, subexpr: ShellExpr) =
  cmd.opts.add ShellCmdPart(kind: cpkSubExpr, expr: subexpr)

func arg*(cmd: var ShellCmd, arg: string | AnyPath) =
  ## Add argument for command
  cmd.opts.add ShellCmdPart(
    kind: cpkArgument, argument: arg.getStr())

func `-`*(cmd: var ShellCmd, fl: string) =
  ## Add flag for command
  cmd.flag fl

func `-`*(cmd: var ShellCmd, path: AnyPath) =
  ## Overload to add filesystem entry as command argument
  cmd - path.getStr()

func `-`*[Path: AnyPath](cmd: var ShellCmd, kv: (string, Path)) =
  cmd.opt(kv[0], kv[1].getStr())

func `-`*(cmd: var ShellCmd, kv: (string, string)) =
  ## Add key-value pair for command
  cmd.opt(kv[0], kv[1])

func `-`*(cmd: var ShellCmd, kv: tuple[key, sep, val: string]) =
  cmd.opts.add ShellCmdPart(
    kind: cpkOption, key: kv.key, val: kv.val, overrideKv: true,
    kvSep: kv.sep)

func makeNimShellCmd*(bin: string): ShellCmd =
  ## Create command for nim core tooling (":" for key-value separator)
  result.conf = NimShellCmdConf
  result.bin = bin

func makeX11ShellCmd*(bin: string): ShellCmd =
  ## Create command for `X11` cli tools (single dash)
  result.conf = X11ShellCmdConf
  result.bin = bin

func makeGnuShellCmd*(bin: string): ShellCmd =
  ## Create command for CLI applications that conform to GNU standard
  ## for command line interface `link
  ## <https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html>`_
  result.conf = GnuShellCmdConf
  result.bin = bin

func makeFileShellCmd*(
  file: string, conf: ShellCmdConf = GnuShellCmdConf): ShellCmd =
  result.conf = conf
  if file.startsWith("/"):
    result.bin = file
  else:
    result.bin = "./" & file

func makeFileShellCmd*(
  file: AnyFile, conf: ShellCmdConf = GnuShellCmdConf): ShellCmd =
  makeFileShellCmd(file.getStr(), conf)

# func quoteShell*(str: string): string = str

func toStr*(part: ShellCmdPart, conf: ShellCmdConf): string =
  ## Convret shell command part to string representation
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
        return longPrefix & part.key & kv & part.val.quoteShell()
      else:
        return "-" & part.key & kv & part.val.quoteShell()
    of cpkArgument:
      return part.argument.quoteShell()
    of cpkSubExpr:
      return part.expr.string


func toStrSeq*(cmd: ShellCmd): seq[string] =
  result = @[ cmd.bin ]
  for op in cmd.opts:
    if op.kind == cpkSubExpr:
      # WARNING escaping of the subshell commands needs to be
      # configured separately.
      result &= "'" & op.toStr(cmd.conf) & "'"
    else:
      result &= op.toStr(cmd.conf)
  # @[ cmd.bin ] & cmd.opts.mapIt(it.toStr(cmd.conf))

func toStr*(cmd: ShellCmd): string = cmd.toStrSeq().join(" ")

func toLogStr*(cmd: ShellCmd): string =
  ## Convert shell command to pretty-printed shell representation
  # TODO add newline escapes `\` at the end of the string
  for str in cmd.toStrSeq():
    if result.len + str.len + 1 > 80:
      result &= "\n"
    elif result.len > 0:
      result &= " "

    result &= str

func splitShell*(str: string): seq[string] =
  # IMPLEMENT
  str.split(" ")

func wrapShell*(str: string, maxw: int = 80): string =
  var buf: seq[string] = @[""]
  for str in str.splitShell():
    if buf[^1].len + str.len + 1 > maxw - 2:
      buf[^1] = buf[^1].alignLeft(maxw - 2) & " \\\n"
      buf.add ""
    elif buf[^1].len > 0:
      buf[^1] &= " "

    buf[^1] &= str

  return buf.join("")



when not defined(NimScript):
  proc printShellError*() =
    when defined(NimScript):
      echo getCurrentExceptionMsg()
    else:
      let err = ShellError(getCurrentException())
      echo err.errstr

      echo err.outstr

iterator iterstdout*(command: ShellExpr): string =
  ## Iterate every line of shell expression output
  # TODO raise exception on failed command
  # REVIEW how cleanup is performed when iterator finishes main loop?
  when defined(NimScript):
    let (res, code) = gorgeEx(command.string, "", "")
    for line in res.split("\n"):
      yield line
  else:
    let pid = startProcess(command.string, options = {poEvalCommand})

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
    cmd: ShellCmd,
    options: set[ProcessOption] = {poUsePath}): Process =
    ## Launch process using shell command

    let
      env = cmd.envVals
      command = cmd.toStr()

    result =
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

    if result.isNil:
      raise ShellError(
        msg: "Command '" & cmd.toStr() & "' failed to start",
        cwd: getCurrentDir(),
        cmd: cmd.toStr()
      )

proc updateException(
  res: var ShellResult, cmd: ShellCmd, maxErrorLines: int) =
  let
    env = cmd.envVals
    command = cmd.toStr()

  if res.execResult.code != 0:
    # echo res
    let envAdd =
      if env.len > 0:
        "With env variables " &
          env.mapIt(&"{it.key}={it.val}").join(" ") & "\n"
      else:
        ""

    var msg = &"Command '{command}'\nExecuted in directory " &
      $cwd() & &"\n{envAdd}Exited with non-zero code:\n"

    let split = res.execResult.stderr.split("\n") &
      res.execResult.stdout.split("\n")

    msg.add split[0 ..< min(split.len(), maxErrorLines)].join("\n")

    res.exception = ShellError(
      msg: msg,
      retcode: res.execResult.code,
      errorCode: int32(res.execResult.code),
      errstr: res.execResult.stderr,
      outstr: res.execResult.stdout,
      cwd: cwd(),
      cmd: command
    )
    # echo "Exception"
  else:
    # echo "ALl ok"
    res = ShellResult(
      resultOk: true,
      execResult: res.execResult
    )

type
  StrQue* = Deque[string]
  StreamConverter* = proc(
    que: var StrQue, cmd: ShellCmd): Option[JsonNode] {.noSideEffect.}

when cbackend:
  proc makeShellJsonIter*(
    cmd: ShellCmd,
    outConvert: StreamConverter,
    errConvert: StreamConverter,
    options: set[ProcessOption] = {poEvalCommand, poUsePath},
    maxErrorLines: int = 12,
    doRaise: bool = true
       ): iterator(): JsonNode =
    ## Iterate over output of the shell command converted to json
    return iterator(): JsonNode =
      let
        process = startShell(cmd, options)
        stdoutStream = process.outputStream()
        stderrStream = process.errorStream()

      var
        stdoutQue: StrQue
        stderrQue: StrQue

      template yieldStream(strm: untyped) =
        `strm Que`.addLast line
        let `strm Res` = outConvert(`strm Que`, cmd)
        if `strm Res`.isSome():
          yield `strm Res`.get()

      block:
        var line = ""
        while process.running:
          discard stdoutStream.readLine(line)
          yieldStream(stdout)

          discard stderrStream.readLine(line)
          yieldStream(stderr)

      block:
        for line in stdoutStream.readAll().split("\n"):
          yieldStream(stdout)

        for line in stderrStream.readAll().split("\n"):
          yieldStream(stderr)

      var res = ShellResult()
      res.execResult.code = process.peekExitCode()
      updateException(res, cmd, maxErrorLines)

      if not res.resultOk and doRaise:
        raise res.exception



proc shellResult*(
  cmd: ShellCmd,
  stdin: string = "",
  options: set[ProcessOption] = {poEvalCommand},
  maxErrorLines: int = 12,
  discardOut: bool = false): ShellResult {.tags: [
    ShellExecEffect,
    ExecIOEffect,
    ReadEnvEffect,
    RootEffect
  ].} =

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
    let pid = startShell(cmd, options)
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
            result.execResult.stdout &= line & "\n"
            # WARNING remove trailing newline on the stdout
        except IOError, OSError:
          assert outStream.isNil
          echo "process died" # NOTE possible place to raise exception

      result.execResult.stdout &= outStream.readAll()
      result.execResult.stderr = pid.errorStream.readAll()
    else:
      while pid.running():
        discard

    result.execResult.code = pid.peekExitCode()
    close(pid)

  else:
    let nscmd = &"cd {cwd()} && " & cmd.toStr()
    if poParentStreams in options:
      exec(nscmd)
    else:
      let (res, code) = gorgeEx(nscmd, "", "")

      if not discardOut:
        result.execResult.stdout = res
        result.execResult.code = code


  updateException(result, cmd, maxErrorLines)



proc runShell*(
  cmd: ShellCmd,
  doRaise: bool = true,
  stdin: string = "",
  options: set[ProcessOption] = {poEvalCommand},
  maxErrorLines: int = 12,
  discardOut: bool = false): tuple[stdout, stderr: string, code: int] =
  ## Execute shell command and return it's output. `stdin` - optional
  ## parameter, will be piped into process. `doRaise` - raise
  ## exception (default) if command finished with non-zero code.
  ## `command` - text of the command.
  ## ## Arguments
  ## :maxErrorLines: max number of stderr lines that would be appended to
  ##   exception. Any stderr beyond this range will be truncated
  let output = shellResult(
    cmd, stdin, options, maxErrorLines, discardOut)
  result = output.execResult

  if (not output.resultOk) and doRaise:
    raise output.exception


proc execShell*(cmd: ShellExpr): void =
  ## `shExec` overload for regular string.
  ##
  ## WARNING see implicit `toShellCmd` documentation for potential
  ## pitfalls. It is recommended to use `shExec(cmd: ShellCmd)` overload -
  ## this version exists only for quick prototyping.
  discard runShell(cmd, discardOut = true, options = {
    poEvalCommand, poParentStreams, poUsePath})


proc evalShell*(cmd: ShellExpr): auto =
  var opts = {poEvalCommand, poUsePath}
  runShell(cmd, options = opts)

proc evalShellStdout*(cmd: ShellExpr): string =
  let res = runShell(cmd, options = {poEvalCommand, poUsePath})
  return res.stdout


proc evalShellStdout*(cmd: ShellCmd): string =
  return runShell(cmd, options = {poEvalCommand, poUsePath}).stdout

proc execShell*(cmd: ShellCmd, doRaise: bool = true): void =
  ## Execute shell command with stdout/stderr redirection into parent
  ## streams. To capture output use `runShell`
  discard runShell(cmd, doRaise = doRaise, discardOut = true, options = {
    poParentStreams, poUsePath})

proc eval*(expr: ShellExpr): string =
  shellResult(expr).execResult.stdout.strip()

export get, isSome, isNone

proc interpolateShell*(
  expr: ShellExpr,
  allowEmpty: bool = false,
  doRaise: bool = false): Option[string] =
  var buf: string
  for (kind, val) in interpolatedFragments(expr.string):
    case kind:
      of ikStr: buf &= val
      of ikDollar: buf &= "$"
      of ikVar:
        if not (existsEnv(ShellVar val) or allowEmpty):
          return none(string)
        else:
          let envVal = getEnv(ShellVar val)
          if envVal.len == 0 and not allowEmpty:
            return none(string)
          else:
            buf &= envVal
      of ikExpr:
        let res = shellResult(ShellExpr val)
        if not res.resultOk and doRaise:
          raise res.exception
        elif res.execResult.stdout.len == 0 and not allowEmpty:
          return none(string)
        else:
          buf &= res.execResult.stdout


  return some(buf)


proc initCmdInterpOrOption*(
  interpol: ShellExpr,
  key, val: string, allowEmpty: bool = false): ShellCmdPart =
  result = ShellCmdPart(kind: cpkOption, key: key)

  let res = interpolateShell(interpol, allowEmpty = allowEmpty)

  if res.isSome():
    result.val = res.get()
  else:
    result.val = val
