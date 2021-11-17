import
  ./xml_ast,
  ../core/all,
  ./base_writer

export xml_ast

import
  std/[
    tables,
    options,
    xmltree,
    with,
    macros,
    strutils,
    typetraits
  ]

type
  XmlState = object
    ptrs: Table[int, pointer] ## Table ID to implementations
    refs: Table[int, int] ## Refs to IDs


  XmlSerializer* = object of XmlWriter
    state: XmlState

  XmlDeserializer* = object of HXmlParser
    state: XmlState

using
  writer: var XmlSerializer
  reader: var XmlDeserializer

genBaseWriterProcs(XmlSerializer)

proc `$`*(p: XmlDeserializer): string = p.displayAt()

proc newXmlDeserializer*(
    arg: string, traceNext: bool = false): XmlDeserializer =
  let r = newHXmlParser(arg, traceNext)

  return XmlDeserializer(
    base: r.base,
    traceNext: r.traceNext,
    file: r.file
  )

template loadPrimitive*(
    stream: var HXmlParser, target: typed, tag: string,
    loadAttr: untyped, loadField: untyped
  ): untyped =

  if stream.isAttribute():
    loadAttr
    stream.next()

  else:
    stream.skipElementStart(tag)
    loadField
    stream.next()
    stream.skipElementEnd(tag)

proc loadEnumWithPrefix*[E](
    r: var HXmlParser; kind: var E, tag, prefix: string) =
  loadPrimitive(
    r, kind, tag,
    (kind = parseEnum[E](prefix & r.strVal())),
    (kind = parseEnum[E](prefix & r.strVal()))
  )

#=============================  attributes  ==============================#

proc toXmlString*[T](item: Option[T]): string =
  mixin toXmlString
  if item.isSome():
    return toXmlString(item.get())

proc toXmlString*(item: string): string = item
proc toXmlString*(item: enum | bool | float): string = $item
proc toXmlString*(item: SomeInteger): string = $item


proc xmlAttribute*[T](writer; key: string, value: Option[T]) =
  if value.isSome():
    xmlAttribute(writer, key, value.get())

proc xmlAttribute*(
    writer; key: string, value: SomeInteger | bool | float | enum) =
  writer.xmlAttribute(key, toXmlString(value))

proc xmlAttribute*[A, B](writer; key: string, value: HSlice[A, B]) =
  xmlAttribute(writer, key, toXmlString(value.a) & ":" & toXmlString(value.b))

#=========================  Basic datatypes IO  ==========================#

proc writeXml*(
  writer; value: string | SomeInteger | bool | SomeFloat | enum, tag: string) =
  writer.writeIndent()
  writer.xmlStart(tag, false)
  writer.writeEscaped($value)
  writer.xmlEnd(tag, false)
  writer.line()

proc loadXml*(reader; target: var bool, tag: string) =
  loadPrimitive(
    reader, target, tag,
    loadAttr = (target = reader.strVal().parseBool()),
    loadField = (target = reader.strVal().parseBool()),
  )

proc loadXml*[I: SomeUnsignedInt](reader; target: var I, tag: string) =
  loadPrimitive(
    reader, target, tag,
    loadAttr = (target = I(reader.strVal().parseBiggestUInt())),
    loadField = (target = I(reader.strVal().parseBiggestUInt())),
  )

proc loadXml*[I: SomeSignedInt](reader; target: var I, tag: string) =
  loadPrimitive(
    reader, target, tag,
    loadAttr = (target = I(reader.strVal().parseBiggestInt())),
    loadField = (target = I(reader.strVal().parseBiggestInt())),
  )

proc loadXml*(reader; target: var char, tag: string) =
  loadPrimitive(
    reader, target, tag,
    loadAttr = (target = reader.strVal()[0]),
    loadField = (target = reader.strVal()[0]),
  )

proc loadXml*(reader; target: var SomeFloat, tag: string) =
  loadPrimitive(
    reader, target, tag,
    loadAttr = (target = reader.strVal().parseFloat()),
    loadField = (target = reader.strVal().parseFloat()),
  )


#==========================  Built-in generics  ==========================#

proc writeXml*[T](writer; values: seq[T], tag: string) =
  mixin writeXml
  for it in values:
    writer.writeXml(it, tag)

proc loadXml*[T](reader; target: var seq[T], tag: string) =
  mixin loadXml
  while reader.kind in {xmlElementOpen, xmlElementStart} and
        reader.elementName() == tag:
    var tmp: T
    loadXml(reader, tmp, tag)
    target.add tmp

proc readXmlKeyValues*[K, V, Res](reader; target: var Res, tag: string) =
  mixin loadXml
  while reader.elementName() == tag:
    var key: K = default(K)
    var val: V = default(V)
    reader.skipElementStart(tag)
    loadXml(reader, key, "key")
    loadXml(reader, val, "value")
    reader.skipElementEnd(tag)
    target[key] = val

proc writeXmlKeyValues*[K, V, Res](writer; target: Res, tag: string) =
  mixin writeXml
  for key, value in pairs(target):
    writer.xmlStart(tag)
    writer.indent()
    writer.writeXml(key, "key")
    writer.writeXml(value, "value")
    writer.dedent()
    writer.xmlEnd(tag)


proc writeXml*[K, V](writer; table: OrderedTable[K, V], tag: string) =
  writeXmlKeyValues[K, V, OrderedTable[K, V]](writer, table, tag)

proc loadXml*[A, B](reader; target: var OrderedTable[A, B], tag: string) =
  readXmlKeyValues[A, B, OrderedTable[A, B]](reader, target, tag)

proc writeXml*[K, V](writer; table: Table[K, V], tag: string) =
  writeXmlKeyValues[K, V, Table[K, V]](writer, table, tag)

proc loadXml*[A, B](reader; target: var Table[A, B], tag: string) =
  readXmlKeyValues[A, B, Table[A, B]](reader, target, tag)

proc writeXml*[R, V](writer; table: array[R, V], tag: string) =
  writeXmlKeyValues[R, V, array[R, V]](writer, table, tag)

proc loadXml*[R, V](reader; target: var array[R, V], tag: string) =
  readXmlKeyValues[R, V, array[R, V]](reader, target, tag)


proc writeXml*[T](writer; opt: Option[T], tag: string) =
  if opt.isSome():
    writeXml(writer, opt.get(), tag)

proc loadXml*[T](reader; target: var Option[T], tag: string) =
  mixin loadXml
  if (reader.isAttribute() and reader.attrKey() == tag) or
     (reader.elementName() == tag):
    var tmp: T
    loadXml(reader, tmp, tag)
    target = some(tmp)

proc writeXml*(writer; value: Slice[int], tag: string) =
  with writer:
    xmlOpen(tag)
    xmlAttribute("a", $value.a)
    xmlAttribute("b", $value.b)
    xmlCloseEnd()

proc loadXml*(reader; target: var Slice[int], tag: string) =
  if reader.atAttr:
    let val = reader.strVal().split(":")
    target.a = parseInt(val[0])
    target.b = parseInt(val[1])
    reader.next()


  else:
    with reader:
      skipOpen(tag)
      loadXml(target.a, "a")
      loadXml(target.b, "b")
      skipClose()

proc writeXml*[E: enum](writer; values: set[E], tag: string) =
  writer.xmlStart(tag)
  writer.indent()
  if 0 < len(values):
    writer.writeIndent()

  for item in values:
    writer.xmlOpen("item")
    writer.xmlAttribute("val", $item)
    writer.xmlCloseEnd()


  writer.dedent()
  writer.xmlEnd(tag)

proc loadXml*[E: enum](reader; target: var E, tag: string) =
  if reader.atAttr():
    target = parseEnum[E](reader.attrValue())
    reader.next()

  else:
    reader.skipStart(tag)
    target = parseEnum[E](reader.strVal())
    reader.next()
    reader.skipEnd(tag)


proc loadXml*[E](reader; target: var set[E], tag: string) =
  reader.skipElementStart(tag)
  while reader.elementName() == "item":
    reader.next()
    assert reader.attrKey() == "val"
    target.incl parseEnum[E](reader.attrValue())
    reader.next()

  reader.skipElementEnd(tag)



template loadXml*[T](
    reader;
    target: var seq[T],
    tag: string, mixedStr: T, fieldAsgn: untyped
  ) =
  mixin loadXml
  while reader.kind in {
    xmlElementOpen, xmlElementStart, xmlCharData, xmlWhitespace
  }:
    case reader.kind:
      of xmlElementOpen, xmlElementStart:
        if reader.elementName() == tag:
          var tmp: T
          loadXml(reader, tmp, tag)
          target.add tmp

        else:
          break

      of xmlElementCharData, xmlWhitespace:
        var next {.inject.} = mixedStr
        var str: string
        loadXml(reader, str, "")
        fieldAsgn = str
        target.add next

      else:
        break


proc loadXml*(reader; target: var string, tag: string) =
  if reader.isAttribute():
    target = reader.strVal()
    reader.next()

  else:
    reader.skipElementStart(tag)
    while reader.kind() in {xmlWhitespace, xmlCharData, XmlEventKind.xmlCData}:
      target &= reader.strVal()
      next(reader)

    reader.skipElementEnd(tag)

proc loadXml*(reader; target: var AbsFile, tag: string) =
  var tmp: string
  loadXml(reader, tmp, tag)
  target = AbsFile(tmp)

proc loadXml*(reader; target: var XmlNode, tag: string) =
  parseXsdAnyType(target, reader, tag)




macro isDiscriminantField*(obj: typed, name: static[string]): untyped =
  proc auxImpl(f: NimNode): NimNode =
    case f.kind:
      of nnkSym: f.getTypeImpl()
      of nnkTupleConstr, nnkObjectTy, nnkTupleTy: f
      else: raise newUnexpectedKindError(f)

  proc fieldList(impl: NimNode): seq[string] =
    case impl.kind:
      of nnkBracketExpr: result = impl[1].auxImpl().fieldList()
      of nnkTupleConstr, nnkIdentDefs, nnkTupleTy: discard
      of nnkObjectTy: result = fieldList(impl[2])
      of nnkOfBranch: result = fieldList(impl[^1])
      of nnkElse: result = fieldList(impl[0])
      of nnkRecList:
        for node in items(impl):
          result.add fieldList(node)

      of nnkRecCase:
        result.add impl[0][0].strVal()
        for sub in items(impl[1..^1]):
          result.add fieldList(sub)

      else: raise newUnexpectedKindError(impl, impl.treeRepr())

  return newLit(name in obj.auxImpl().fieldList())


proc knownRef[T](state: var XmlState, target: ref T): bool =
  cast[int](target) in state.refs

proc getRefId[T](state: var XmlState, target: ref T): int =
  let mem = cast[int](target)
  if mem in state.refs:
    return state.refs[mem]

  else:
    result = len(state.refs)
    state.refs[mem] = result

const xmlRefId = "id"

proc readRefId(reader): int =
  var id: int
  loadXml(reader, id, xmlRefId)
  return id

proc knownId(state: var XmlState, id: int): bool =
  id in state.ptrs

proc storeFields[T](writer; target: T, tag: string) =
  for name, field in fieldPairs(target):
    when isDiscriminantField(T, name):
      xmlAttribute(writer, name, field)

  for name, field in fieldPairs(target):
    when hasCustomPragma(field, Attr) and not isDiscriminantField(T, name):
      xmlAttribute(writer, name, field)

  xmlClose(writer)
  line(writer)
  indent(writer)
  {.warning: "[TODO] if there are no regular fields make a single-line object".}

  for name, field in fieldPairs(target):
    when not hasCustomPragma(field, Attr) and not isDiscriminantField(T, name):
      writeXml(writer, field, name)

  dedent(writer)


proc storeObject[T](writer; target: T, tag: string) =
  when target is ref:
    if isNil(target):
      xmlOpen(writer, tag)
      xmlCloseEnd(writer)

    elif knownRef(writer.state, target):
      xmlOpen(writer, tag)
      xmlAttribute(writer, xmlRefId, $writer.state.getRefId(target))

      xmlCloseEnd(writer)

    else:
      xmlOpen(writer, tag)
      xmlAttribute(writer, xmlRefId, $writer.state.getRefId(target))
      storeFields(writer, target[], tag)
      xmlEnd(writer, tag)

  else:
    xmlOpen(writer, tag)
    storeFields(writer, target, tag)
    xmlEnd(writer, tag)

proc loadFields[T](reader; target: var T, tag: string) =
  {.cast(uncheckedAssign).}:
    assert reader.kind notin {xmlElementEnd}

    for name, field in fieldPairs(target):
      when isDiscriminantField(T, name):
        loadXml(reader, field, name)

    for name, field in fieldPairs(target):
      when hasCustomPragma(field, Attr) and not isDiscriminantField(T, name):
        loadXml(reader, field, name)

    if reader.kind == xmlElementClose:
      skipClose(reader)

    for name, field in fieldPairs(target):
      when not hasCustomPragma(field, Attr) and not isDiscriminantField(T, name):
        loadXml(reader, field, name)


proc loadObject[T](reader; target: var T, tag: string) =
  when target is ref:
    if reader.kind == xmlElementStart:
      target = nil
      reader.skipElementStart(tag)
      reader.skipElementEnd(tag)

    else:
      skipOpen(reader, tag)
      let id = reader.readRefId()
      if reader.state.knownId(id):
        target = cast[T](reader.state.ptrs[id])
        skipClose(reader)
        skipElementEnd(reader, tag)

      else:
        if reader.kind == xmlElementClose: reader.next()
        new(target)
        reader.state.ptrs[id] = cast[pointer](target)
        loadFields(reader, target[], tag)
        skipElementEnd(reader, tag)

  else:
    let op = reader.kind == xmlElementOpen
    if op:
      skipOpen(reader, tag)
    else:
      skipElementStart(reader, tag)

    loadFields(reader, target, tag)
    skipEnd(reader, tag)



proc writeXml*[T: object](writer; target: T, tag: string) =
  storeObject(writer, target, tag)

proc writeXml*[T: tuple](writer; target: T, tag: string) =
  storeObject(writer, target, tag)

proc loadXml*[T: object](reader; target: var T, tag: string) =
  loadObject(reader, target, tag)

proc loadXml*[T: tuple](reader; target: var T, tag: string) =
  loadObject(reader, target, tag)

proc writeXml*[T: ref object](writer; target: T, tag: string) =
  storeObject(writer, target, tag)

proc writeXml*[T: ref tuple](writer; target: T, tag: string) =
  storeObject(writer, target, tag)

proc loadXml*[T: ref object](reader; target: var T, tag: string) =
  loadObject(reader, target, tag)

proc loadXml*[T: ref tuple](reader; target: var T, tag: string) =
  loadObject(reader, target, tag)


proc loadXmlDistinct*[T: distinct](reader; target: var T, tag: string) =
  type Base = distinctBase(T)
  var base: Base = default(Base)
  loadXml(reader, base, tag)
  target = T(base)

proc writeXmlDistinct*[T: distinct](writer; target: T, tag: string) =
  type Base = distinctBase(T)
  writeXml(writer, Base(target), tag)

template xmlSerdeFor*(TypeName, readerCall, writerCall: untyped): untyped =
  proc loadXml*(
    reader: var XmlDeserializer, target: var TypeName, tag: string) =
    readerCall(reader, target, tag)

  proc writeXml*(
    writer: var XmlSerializer, target: TypeName, tag: string) =
    writerCall(writer, target, tag)


proc fromXml*[T](text: string, target: typedesc[T], tag: string): T =
  var reader = newXmlDeserializer(text)
  loadXml(reader, result, tag)

proc toXml*[T](obj: T, tag: string): string =
  var writer = newXmlSerializer()
  writeXml(writer, obj, tag)
  return writer.readAll()
