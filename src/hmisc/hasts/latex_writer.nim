import
  ../core/all,
  ../other/oswrap,
  ./base_writer


import
  std/streams

type
  LatexWriter* = ref object
    base: BaseWriter
    indentBuf: string

genBaseWriterProcs(LatexWriter)

using
  writer: var LatexWriter

proc writeIndRaw*(writer; text: varargs[string, `$`]) =
  writer.writeIndent()
  writer.writeRaw(text)

proc comment*(writer; text: string) =
  writer.writeIndent()
  writer.writeRaw "% "
  writer.writeRaw(text)
  writer.line()

proc `%`*(writer; text: string) =
  writer.comment(text)

proc cmd*(
    writer;
    cmdName: string,
    cmdParams: openarray[string],
    args: openarray[string],
    post: string = "",
    inline: bool = false
  ) =

  if not inline: writer.writeIndent()
  writer.writeRaw r"\", cmdName
  for param in cmdParams:
    writer.writeRaw "[", param, "]"

  for arg in args:
    writer.writeRaw "{", arg, "}"

  writer.writeRaw post

  if not inline: writer.line()

proc cmd*(
    writer; name: string,
    args: openarray[string] = [],
    post: string = "",
    inline: bool = false
  ) =

  writer.cmd(name, [], @args, post = post, inline = inline)

template env*(
    writer;
    envName: string,
    args: openarray[string] = [],
    body: untyped
  ): untyped =

  writer.cmd("begin", envName & @args)
  writer.indent()

  body

  writer.dedent()
  writer.cmd("end", [envName])


template flatEnv*(
    writer;
    envName: string,
    args: openarray[string] = [],
    body: untyped
  ): untyped =

  writer.cmd("begin", envName & @args)
  body
  writer.cmd("end", [envName])

