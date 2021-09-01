import
  ../core/all,
  ../other/oswrap


import
  std/streams

type
  LatexWriter* = ref object
    stream: Stream
    indentBuf: string

using
  writer: var LatexWriter

template add(text: string): untyped = writer.stream.write(text)

proc newLatexWriter*(stream: Stream): LatexWriter =
  LatexWriter(stream: stream)

proc newLatexWriter*(file: AbsFile): LatexWriter =
  newLatexWriter(newFileStream(file, fmWrite))

proc newLatexWriter*(file: File): LatexWriter =
  newLatexWriter(newFileStream(file))

proc space*(writer) = writer.stream.write(" ")
proc line*(writer) = writer.stream.write("\n")

proc write*(writer; text: varargs[string]) = writer.stream.write(text)
proc raw*(writer; text: varargs[string]) = writer.stream.write(text)
proc close*(writer) = writer.stream.close()
proc comment*(writer; text: string) =
  add "% "
  add text
  writer.line()

proc `%`*(writer; text: string) = writer.comment(text)

proc indent*(writer) = writer.indentBuf.add "  "
proc dedent*(writer) =
  writer.indentBuf.setLen(max(writer.indentBuf.len - 2, 0))

proc writeInd*(writer) = writer.stream.write(writer.indentBuf)
proc indRaw*(writer; text: string) = writer.writeInd(); writer.raw(text)


proc cmd*(
    writer;
    cmdName: string,
    cmdParams: openarray[string],
    cmdArg: string,
    args: openarray[string]
  ) =

  add r"\"
  add cmdName
  for param in cmdParams:
    add "["
    add param
    add "]"

  add "{"
  add cmdArg
  add "}"

  for arg in args:
    add "{"
    add arg
    add "}"

  add "\n"

proc cmd*(writer; name, arg: string, args: openarray[string] = []) =
  writer.cmd(name, [], arg, args)

proc cmd*(writer; name: string, params: openarray[string], arg: string) =
  writer.cmd(name, params, arg, [])

template env*(
    writer;
    envName: string,
    args: openarray[string] = [],
    body: untyped
  ): untyped =

  writer.cmd "begin", envName, args
  writer.indent()

  body

  writer.dedent()
  writer.cmd "end", envName


template flatEnv*(
    writer;
    envName: string,
    args: openarray[string] = [],
    body: untyped
  ): untyped =

  writer.cmd "begin", envName, args
  body
  writer.cmd "end", envName

proc use*(writer; params: openarray[string], name: string) =
  writer.cmd("usepackage", params, name, [])
