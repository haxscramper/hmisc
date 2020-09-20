when not defined(NimScript):
  import osproc, streams, os

import strutils, strformat
import ../algo/halgorithm

# TODO better way of building command for execution.
# TODO overload for `runShell` that accepts callbacks
#      failed execution. Make `runShell` raise
#      exception if command exited with non-zero code
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
    opts*: seq[string]
    conf: CmdConf

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

func opt*(cmd: var Cmd, opts: openarray[tuple[key, val: string]]) =
  for (key, val) in opts:
    cmd.opt(key, val)

func cmd*(cmd: var Cmd, sub: string) =
   cmd.opts.add sub

func strArg*(cmd: var Cmd, sub: string) =
  cmd.opts.add:
    if not sub.enclosedIn("\""):
      &"\"{sub}\""
    else:
      sub

func makeNimCmd*(bin: string): Cmd =
  result.conf = ccNimFlags
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

proc runShell*(
  command: string, doRaise: bool = true, stdin: string = ""): tuple[
  stdout, stderr: string, code: int] =

  when not defined(NimScript):
    let pid = startProcess(command, options = {poEvalCommand})

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

    close(pid)

  else:
    let nscmd = &"cd {getCurrentDir()} && " & command
    let (res, code) = gorgeEx(nscmd, "", "")
    result.stdout = res
    result.code = code


  if doRaise and result.code != 0:
    raise ShellError(
      msg: &"Command '{command}' executed in directory " &
        getCurrentDir() & " exited with non-zero code",
      retcode: result.code,
      errorCode: int32(result.code),
      errstr: result.stderr,
      outstr: result.stdout,
      cwd: getCurrentDir(),
      cmd: command
    )

proc runShell*(cmd: Cmd, doRaise: bool = true, stdin: string = ""): tuple[
  stdout, stderr: string, code: int] =
  cmd.toStr().runShell()

# when not defined(nimscript):
#   template withDir*(dir: string; body: untyped): untyped =
#     var curDir = getCurrentDir()
#     try:
#       setCurrentDir(dir)
#       body
#     finally:
#       setCurrentDir(curDir)
