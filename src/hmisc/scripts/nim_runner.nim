import hmisc/other/[hshell, oswrap]
import hmisc/helpers

import macros, strutils


dumpTree:
  Hee

let instr = paramStr(1)

let file = instr & ".bin"

var cmd = makeNimCmd("nim").withIt do:
  it.cmd "c"
  it.opt "hints", "off"
  it.opt "verbosity", "0"
  it.opt "warnings", "off"
  it.opt "o", file
  it.strArg instr

var buf: string

withinTempDir true:
  let res = runShell(cmd, false)
  buf.add res.stdout.strip() & "\n" & res.stderr.strip()
  if res.code == 0:
    let res = runShell(makeFileCmd(file))
    buf.add "\n" & res.stdout.strip() & "\n" & res.stderr.strip()

echo buf.strip()
