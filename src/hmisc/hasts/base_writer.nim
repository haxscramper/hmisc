import std/[streams, macros]
import ../other/oswrap

type
  BaseWriter* = object
    stream: Stream
    indentBuf: string
    ignoreIndent: int

using
  writer: var BaseWriter

proc newBaseWriter*(): BaseWriter =
  BaseWriter(stream: newStringStream())

proc newBaseWriter*(stream: Stream): BaseWriter =
  BaseWriter(stream: stream)

proc newBaseWriter*(file: File): BaseWriter =
  newBaseWriter(newFileStream(file))

proc newBaseWriter*(file: AbsFile): BaseWriter =
  assertExists(file.dir())
  newBaseWriter(newFileStream(file, fmWrite))

proc close*(writer) = writer.stream.close()
proc space*(writer) = writer.stream.write(" ")
proc line*(writer) = writer.stream.write("\n")
proc indent*(writer) = writer.indentBuf.add "  "
proc dedent*(writer) =
  writer.indentBuf.setLen(max(writer.indentBuf.len - 2, 0))

proc writeInd*(writer) =
  if writer.ignoreIndent > 0:
    dec writer.ignoreIndent

  else:
    writer.stream.write(writer.indentBuf)

proc readAll*(writer): string =
  writer.stream.setPosition(0)
  writer.stream.readAll()

proc writeRaw*(writer; text: varargs[string, `$`]) =
  for t in text:
    writer.stream.write(t)

proc ignoreNextIndent*(writer) = inc writer.ignoreIndent

macro genBaseWriterProcs*(wtype: untyped): untyped =
  let dol = nnkAccQuoted.newTree(ident"$")
  let newn = ident("new" & wtype.strVal)
  quote do:
    proc writeInd*(writer: var `wtype`) = writer.base.writeInd()
    proc space*(writer:    var `wtype`) = writer.base.space()
    proc line*(writer:     var `wtype`) = writer.base.line()
    proc indent*(writer:   var `wtype`) = writer.base.indent()
    proc dedent*(writer:   var `wtype`) = writer.base.dedent()
    proc close*(writer:    var `wtype`) = writer.base.close()
    proc readAll*(writer:  var `wtype`): string = writer.base.readAll()
    proc writeRaw*(writer: var `wtype`, text: varargs[string, `dol`]) =
      writer.base.writeRaw(text)

    proc `newn`*(stream: Stream | File | AbsFile): `wtype` =
      `wtype`(base: newBaseWriter(stream))

    proc `newn`*(): `wtype` = `wtype`(base: newBaseWriter())
