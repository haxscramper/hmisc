import ./wrap_cmd_exec
import hmisc/core/all
import hmisc/other/oswrap
import std/osproc

type
  XephyrCommandFlag* = enum
    xephyrf_hostCursor ## set 'cursor acceleration': The host's cursor is
    ## reused. This is only really there to aid debugging by avoiding
    ## server paints for the cursor. Performance improvement is negligible.

    xephyrf_Resizeable ## Allow the Xephyr window to be resized, even if
    ## not embedded into a parent window. By default,
    ## the Xephyr window has a fixed size.

    xephurf_noHostGrab ## Disable grabbing the keyboard and mouse.

    xephyrf_Retro ## start with classic stipple and cursor

    xephyrf_SwCursor ## Render cursors in software in Xephyr

  XephyrCommand = object
    flags*: set[XephyrCommandFlag]
    displayIdx {.requiresinit.}: int

const
  xephyrCommandStrings* = toMapArray {
    xephyrf_hostCursor: "host-cursor",
    xephyrf_Resizeable: "resizeable",
    xephurf_noHostGrab: "no-host-grap",
    xephyrf_Retro: "retro",
    xephyrf_SwCursor: "sw-cursor"
  }

proc toShellCmd*(cmd: XephyrCommand): ShellCmd =
  result = makeX11ShellCmd("Xephyr").withIt do:
    it.addFlags(cmd.flags, xephyrCommandStrings)

    it.arg &":{cmd.displayIdx}"

proc start*(cmd: XephyrCommand): Process =
  startShell(cmd.toShellCmd(), cmdExecFlags + { poDaemon })

import ./ps

proc findXephyr*(): Option[(Pid, seq[string])] =
  let res = exec PsCommand(flags: {
    psf_AllButLeaderProcesses,
    psf_AllowNoTTY
  })

  for rec in res.records:
    if "Xephyr" in rec.command:
      echo rec.command
      return some (rec.pid, rec.command)

template withXephyr*(body: untyped): untyped =
  bind withEnv
  let xe = findXephyr()
  if xe.isSome():
    let display: string = xe.get()[1][^1]
    withEnv({ $$DISPLAY: display }):
      body

  else:
    raise newEnvironmentAssertionError(
      "`withXephyr` requires at least one instance of `Xephyr` ",
      "running.")



# when isMainModule:
#   echo xe

# when isMainModule:
#   let xe = start XephyrCommand(
#     displayIdx: 1,
#     flags: {
#       xephyrf_Retro,
#       xephyrf_SwCursor,
#       xephyrf_Resizeable})

