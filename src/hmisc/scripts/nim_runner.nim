import hmisc/other/[hshell, oswrap]
import hmisc/helpers

import macros, strutils


dumpTree:
  Hee

let instr = paramStr(1)

let file = instr & ".bin"

var cmd = makeNimShellCmd("nim").withIt do:
  it.cmd "c"
  it.opt "hints", "off"
  it.opt "verbosity", "0"
  it.opt "warnings", "off"
  it.opt "o", file
  it.arg instr

var fileCmd = makeFileShellCmd(file)

let (compArgs, fileArgs) = paramStrs().splitOnIt(it == "--")

compArgs.eachIt(cmd.raw it)
fileArgs.eachIt(fileCmd.raw it)

var buf: string

withTempDir true:
  let res = runShell(cmd, false)
  buf.add res.stdout.strip() & "\n" & res.stderr.strip()
  if res.code == 0:
    let res = runShell(fileCmd)
    buf.add "\n" & res.stdout.strip() & "\n" & res.stderr.strip()
