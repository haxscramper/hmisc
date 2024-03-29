import
  ./xml_ast,
  ../core/all,
  ./base_writer,
  ./serde_macros

export xml_ast

import
  std/[
    tables,
    options,
    xmltree,
    with,
    macros,
    strutils,
    typetraits,
    streams
  ]

type
  XmlSerializer* = object of XmlWriter
    state: SerdeState

  XmlDeserializer* = object of HXmlParser
    state: SerdeState

using
  writer: var XmlSerializer
  reader: var XmlDeserializer

genBaseWriterProcs(XmlSerializer)

proc `$`*(p: XmlDeserializer): string = p.displayAt()

proc newXmlDeserializer*(
    arg: string, traceNext: bool = false): XmlDeserializer =
  var tmp = newHXmlParser(arg, traceNext)
  result = XmlDeserializer(
    base: tmp.base,
    traceNext: tmp.traceNext,
    file: tmp.file
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

proc writeXmlItems*[T](
    writer;
    values: T,
    listTag: string,
    itemTag: string = "item"
  ) =
  mixin writeXml
  writer.xmlStart(listTag)
  writer.indent()
  for it in items(values):
    writer.writeXml(it, itemTag)

  writer.dedent()
  writer.xmlEnd(listTag)

template loadXmlItems*[T](
    reader: var XmlDeserializer,
    tmp: var T,
    listTag: string,
    itemTag: string = "item",
    insertCall: untyped
  ) =

  mixin loadXml
  reader.skipStart(listTag)
  while reader.kind in {xmlElementOpen, xmlElementStart} and
        reader.elementName() == itemTag:
    loadXml(reader, tmp, itemTag)
    insertCall

  reader.skipEnd(listTag)

proc writeXml*[T](writer; values: seq[T], tag: string) =
  writeXmlItems(writer, values, tag, "item")

proc loadXml*[T](reader; target: var seq[T], tag: string) =
  var tmp: T
  loadXmlItems[T](reader, tmp, tag, "item"):
    add(target, tmp)

proc loadXmlPairs*[K, V, Res](
    reader;
    target: var Res,
    list: string,
    itemTag: string = "item",
    keyTag: string = "key",
    valueTag: string = "value"
  ) =

  mixin loadXml
  reader.skipStart(list)
  while reader.kind in {xmlElementOpen, xmlElementStart} and
        reader.elementName() == itemTag:

    reader.skipElementStart(itemTag)

    var key: K = default(K)
    loadXml(reader, key, keyTag)

    var val: V = default(V)
    loadXml(reader, val, valueTag)

    reader.skipElementEnd(itemTag)
    target[key] = val

  reader.skipEnd(list)

proc writeXmlPairs*[K, V, Res](
    writer;
    target: Res,
    tag: string,
    itemTag: string = "item",
    keyTag: string = "key",
    valueTag: string = "value"
  ) =
  mixin writeXml
  writer.xmlStart(tag)
  writer.indent()
  for key, value in pairs(target):
    writer.xmlStart(itemTag)
    writer.indent()
    writer.writeXml(key, keyTag)
    writer.writeXml(value, valueTag)
    writer.dedent()
    writer.xmlEnd(itemTag)

  writer.dedent()
  writer.xmlEnd(tag)



const xmlRefId = "id"

proc readRefId(reader): int =
  var id: int
  loadXml(reader, id, xmlRefId)
  return id


template wrapRefWrite*[T](
    writer; target: T, tag: string, body: untyped): untyped =

  bind getRefId
  if isNil(target):
    xmlOpen(writer, tag)
    xmlCloseEnd(writer)

  elif knownRef(writer.state, target):
    xmlOpen(writer, tag)
    xmlAttribute(writer, xmlRefId, $getRefId(writer.state, target))

    xmlCloseEnd(writer)

  else:
    xmlOpen(writer, tag)
    xmlAttribute(writer, xmlRefId, $getRefId(writer.state, target))
    body
    xmlEnd(writer, tag)

template wrapRefLoad*[T](
    writer; target: T, tag: string, body: untyped): untyped =
  ## Wrap implementation of the ref type serialization.
  bind readRefId, knownId
  if reader.kind == xmlElementStart:
    target = nil
    reader.skipElementStart(tag)
    reader.skipElementEnd(tag)

  else:
    skipOpen(reader, tag)
    let id = readRefId(reader)
    if knownId(reader.state, id):
      target = getRef[T](reader.state, id)
      skipClose(reader)
      skipElementEnd(reader, tag)

    else:
      if reader.kind == xmlElementClose: reader.next()
      new(target)
      setRef(reader.state, target, id)
      body
      skipElementEnd(reader, tag)


# ~~~~ Ordered Table ~~~~ #

proc writeXml*[K, V](writer; table: OrderedTable[K, V], tag: string) =
  writeXmlPairs[K, V, OrderedTable[K, V]](writer, table, tag)

proc loadXml*[A, B](reader; target: var OrderedTable[A, B], tag: string) =
  loadXmlPairs[A, B, OrderedTable[A, B]](reader, target, tag)

# ~~~~ Table ~~~~ #

proc writeXml*[K, V](writer; table: Table[K, V], tag: string) =
  writeXmlPairs[K, V, Table[K, V]](writer, table, tag)

proc loadXml*[A, B](reader; target: var Table[A, B], tag: string) =
  loadXmlPairs[A, B, Table[A, B]](reader, target, tag)

# ~~~~ Table ref ~~~~ #

proc writeXml*[K, V](writer; table: TableRef[K, V], tag: string) =
  wrapRefWrite(writer, table, tag):
    writeXmlPairs[K, V, TableRef[K, V]](writer, table, tag)

proc loadXml*[A, B](reader; target: var TableRef[A, B], tag: string) =
  wrapRefLoad(reader, target, tag):
    loadXmlPairs[A, B, TableRef[A, B]](reader, target, tag)

# ~~~~ Oredered table ref ~~~~ #

proc writeXml*[K, V](writer; table: OrderedTableRef[K, V], tag: string) =
  wrapRefWrite(writer, table, tag):
    writeXmlPairs[K, V, OrderedTableRef[K, V]](writer, table, tag)

proc loadXml*[A, B](reader; target: var OrderedTableRef[A, B], tag: string) =
  wrapRefLoad(reader, target, tag):
    loadXmlPairs[A, B, OrderedTableRef[A, B]](reader, target, tag)

# ~~~~ Array ~~~~ #

proc writeXml*[R, V](writer; table: array[R, V], tag: string) =
  writeXmlItems[array[R, V]](writer, table, tag, "item")

proc loadXml*[R, V](reader; target: var array[R, V], tag: string) =
  var idx = low(R)
  var tmp = default(V)
  var isLast = false
  loadXmlItems[V](reader, tmp, tag, "item"):
    assert not isLast
    target[idx] = tmp
    if idx != high(R):
      idx = succ(idx)
    else:
      isLast = true

# ~~~~ Option ~~~~ #

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

proc writeXml*[E](writer; values: set[E], tag: string) =
  writeXmlItems(writer, values, tag, "item")

proc loadXml*[E](reader; target: var set[E], tag: string) =
  var tmp: E = default(E)
  loadXmlItems(reader, tmp, tag, "item"):
    target.incl tmp


proc loadXml*[E: enum](reader; target: var E, tag: string) =
  if reader.atAttr():
    target = parseEnum[E](reader.attrValue())
    reader.next()

  else:
    reader.skipStart(tag)
    target = parseEnum[E](reader.strVal())
    reader.next()
    reader.skipEnd(tag)





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

proc loadXml*(reader; target: var cstring, tag: string) =
  var tmp: string
  loadXml(reader, tmp, tag)
  target = allocCstringArray([tmp])[0]

proc writeXml*(writer; value: cstring, tag: string) =
  writeXml(writer, $value, tag)

proc loadXml*(reader; target: var AbsFile, tag: string) =
  var tmp: string
  loadXml(reader, tmp, tag)
  target = AbsFile(tmp)

proc loadXml*(reader; target: var XmlNode, tag: string) =
  parseXsdAnyType(target, reader, tag)




proc storeFields[T](writer; target: T, tag: string) =
  for name, field in fieldPairs(target):
    when isDiscriminantField(T, name):
      xmlAttribute(writer, name, field)

  for name, field in fieldPairs(target):
    when hasCustomPragma(field, Attr) and not isDiscriminantField(T, name):
      xmlAttribute(writer, name, field)

  var first = true
  for name, field in fieldPairs(target):
    if first:
      xmlClose(writer)
      line(writer)
      indent(writer)
      first = false


    when not hasCustomPragma(field, Attr) and not isDiscriminantField(T, name):
      writeXml(writer, field, name)

  if first:
    xmlClose(writer)

  else:
    dedent(writer)


proc storeXmlObject[T](writer; target: T, tag: string) =
  when target is ref:
    wrapRefWrite(writer, target, tag):
      storeFields(writer, target[], tag)

  else:
    xmlOpen(writer, tag)
    storeFields(writer, target, tag)
    xmlEnd(writer, tag)

proc loadXmlFields[T](reader; target: var T, tag: string) =
  {.cast(uncheckedAssign).}:
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


proc loadXmlObject[T](reader; target: var T, tag: string) =
  assert reader.kind in {xmlElementOpen, xmlElementStart}, $reader
  when target is ref:
    wrapRefLoad(reader, target, tag):
      loadXmlFields(reader, target[], tag)

  else:
    if reader.kind == xmlElementOpen:
      skipOpen(reader, tag)

    else:
      skipElementStart(reader, tag)

    loadXmlFields(reader, target, tag)
    skipEnd(reader, tag)



proc writeXml*[T: object](writer; target: T, tag: string) =
  storeXmlObject(writer, target, tag)

proc writeXml*[T: tuple](writer; target: T, tag: string) =
  storeXmlObject(writer, target, tag)

proc loadXml*[T: object](reader; target: var T, tag: string) =
  loadXmlObject(reader, target, tag)

proc loadXml*[T: tuple](reader; target: var T, tag: string) =
  loadXmlObject(reader, target, tag)

proc writeXml*[T: ref object](writer; target: T, tag: string) =
  storeXmlObject(writer, target, tag)

proc writeXml*[T: ref tuple](writer; target: T, tag: string) =
  storeXmlObject(writer, target, tag)

proc loadXml*[T: ref object](reader; target: var T, tag: string) =
  loadXmlObject(reader, target, tag)

proc loadXml*[T: ref tuple](reader; target: var T, tag: string) =
  loadXmlObject(reader, target, tag)


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


proc fromXml*[T](
    text: string,
    target: typedesc[T],
    tag: string,
    traceNext: bool = false
  ): T =

  var reader = newXmlDeserializer(text, traceNext)
  loadXml(reader, result, tag)

proc toXml*[T](obj: T, tag: string): string =
  var writer = newXmlSerializer()
  writeXml(writer, obj, tag)
  return writer.readAll()
