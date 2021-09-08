import
  ./wrap_cmd_exec

import
  ../core/all

type
  PsCommandFlag* = enum
    psf_AllProcesses ## Select all processes.  Identical to -e.
    psf_AllButLeaderProcesses ## Select all processes except both session
    ## leaders (see getsid(2)) and processes not associated with a
    ## terminal.

    psf_AllowNoTTY ## Lift the BSD-style "must have a tty" restriction,
    ## which is imposed upon the set of all processes when some BSD-style
    ## (without "-") options are used or when the ps personality setting is
    ## BSD-like. The set of processes selected in this manner is in
    ## addition to the set of processes selected by other means. An
    ## alternate description is that this option causes ps to list all
    ## processes owned by you (same EUID as ps), or to list all processes
    ## when used together with the a option.

const psfCommandStrings* = toMapArray {
  psf_AllButLeaderProcesses: "a",
  psf_AllowNoTTY: "x"
}

type
  PsRecord* = object
    # user*: string
    pid*: Pid
    # percCpu*: float
    # percMem*: float
    # vsz*: int
    # rss*: int
    tty*: string
    stat*: Option[string]
    # start*: string
    time*: string
    command*: seq[string]

  PsResult* = object of CmdExecResult
    records*: seq[PsRecord]
    cmd*: PsCommand

  PsCommand* = object
    flags*: set[PsCommandFlag]

proc parseResult(res: var PsResult) =
  let so = res.execResult.getStdout()
  # echo so
  for idx, line in enumerate(so.splitRecords()):
    if idx > 0:
      var rec: PsRecord
      var idx = 0
      rec.pid = line[postInc idx].parseInt().Pid()
      rec.tty = line[postInc idx]

      if psf_AllowNoTTY in res.cmd.flags:
        rec.stat = some line[postInc idx]

      rec.time = line[postInc idx]
      rec.command = line[postInc(idx) .. ^1]

      res.records.add rec




proc toShellCmd*(cmd: PsCommand): ShellCmd =
  result = makeGnuShellCmd("ps").withIt do:
    for flag in cmd.flags:
      it.flag psfCommandStrings[flag]

proc exec*(cmd: PsCommand, stdin: string = ""): PsResult =
  result.cmd = cmd
  result.execResult = shellResult(
    cmd.toShellCmd(), stdin = stdin, options = cmdExecFlags)

  parseResult(result)

when isMainModule:
  import ../other/hpprint

  let res = exec PsCommand(flags: {
    psf_AllButLeaderProcesses,
    psf_AllowNoTTY
  })

  for rec in res.records:
    if "emacs" in rec.command:
      pprint rec
