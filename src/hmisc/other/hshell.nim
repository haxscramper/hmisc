import streams, strutils, osproc

iterator iterstdout*(command: string): string =
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

proc runShell*(command: string): tuple[
  outstr, outerr: string,
  outcode: int]
  =
  let pid = startProcess(command, options = {poEvalCommand})

  let outStream = pid.outputStream
  var line = ""

  while pid.running:
    try:
      let streamRes = outStream.readLine(line)
      if streamRes:
        result.outstr &= line & "\n"
    except IOError, OSError:
      assert outStream.isNil
      echo "process died"

  result.outstr &= outStream.readAll()
  result.outcode = pid.peekExitCode()
  if result.outcode != 0:
    result.outerr = pid.errorStream.readAll()
