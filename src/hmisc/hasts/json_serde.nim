import ./json_ast, ./serde_macros, ./base_writer
import hmisc/core/all
import std/[
  parsejson, typetraits, macros,
  tables, with, strutils
]



type
  JsonSerdeError* = object of ParseError

  JsonSerializer* = object of JsonWriter
    state: SerdeState

  JsonDeserializer* = object of JsonParser
    sstate: SerdeState
    traceNext: bool

using
  reader: var JsonDeserializer
  writer: var JsonSerializer

genBaseWriterProcs(JsonSerializer)

proc `$`*(p: JsonDeserializer): string = p.displayAt()

proc next*(reader) =
  parsejson.next(reader)
  if reader.traceNext:
    echo reader.displayAt()

  if reader.kind == jsonError:
    raiseParseErr(reader, "err")

proc asgnAux*[V](target: var V, source: V) =
  target = source

proc newJsonDeserializer*(
    arg: string, traceNext: bool = false): JsonDeserializer =
  asgnAux[JsonParser](result, newJsonParser(arg))
  result.traceNext = traceNext

template expectAt*(
    reader; at: set[JsonEventKind], procname: string
  ): untyped {.dirty.} =
  bind kind, getLine, getColumn, displayAt
  {.line: instantiationInfo(fullPaths = true).}:
    if kind(reader) notin at:
      when not defined(nimdoc):
        raise (ref JsonSerdeError)(
          msg:
            "Invalid curent parser event kind for '" &
              procname & "'. Expected any of " &
              $at & ", but found " &
              $reader.kind() &
              " at " & displayAt(reader),
          line: parsejson.getLine(reader),
          column: parsejson.getColumn(reader)
        )

proc skip(reader; at: set[JsonEventKind], procname: string) =
  expectAt(reader, at, procname)
  reader.next()

#==========================  Basic datatype IO  ==========================#

proc loadJson*(reader; target: var bool) =
  expectAt(reader, {jsonTrue, jsonFalse}, "loadJson(bool)")
  target = reader.kind == jsonTrue
  reader.next()

proc writeJson*(writer; target: bool) =
  if target:
    writer.write jsonTrue

  else:
    writer.write jsonFalse

proc loadJson*[I: SomeUnsignedInt](reader; target: var I) =
  target = I(reader.str().parseBiggestUInt())
  reader.skip({jsonInt}, "loadJson(SomeUnsignedInt)")

proc loadJson*[I: SomeSignedInt](reader; target: var I) =
  target = I(reader.str().parseBiggestInt())
  reader.skip({jsonInt}, "loadJson(SomeSignedInt)")

proc writeJson*(writer; target: SomeInteger) =
  writer.writeRaw($target)

proc loadJson*[T: SomeFloat](reader; target: var T) =
  expectAt(reader, {jsonFloat}, "loadJson(float)")
  target = T(getFloat(reader))
  reader.next()

proc writeJson*(writer; target: SomeFloat) =
  writer.writeRaw($target)

proc loadJson*(reader; target: var string) =
  expectAt(reader, {jsonString}, "loadJson(string)")
  target = reader.str()
  reader.next()

proc writeJson*(writer; target: string) =
  writer.writeStr(target)

proc loadJson*(reader; target: var char) =
  expectAt(reader, {jsonString}, "loadJson(char)")
  target = reader.str()[0]
  reader.next()

proc writeJson*(writer; target: char) =
  writer.writeStr($target)


proc readRefId(reader): int =
  var id: int
  loadJson(reader, id)
  return id

template wrapRefWrite*[T](
    writer; target: T, body: untyped): untyped =

  bind getRefId, writeField, comma
  if isNil(target):
    writer.write jsonNull

  elif knownRef(writer.state, target):
    writer.write jsonObjectStart
    writeField(writer, "refid")
    writeJson(writer, getRefId(writer.state, target))
    writer.write jsonObjectEnd

  else:
    writer.write jsonObjectStart
    writeField(writer, "refid")
    writeJson(writer, getRefId(writer.state, target))
    comma(writer)
    writeField(writer, "value")
    body
    writer.write jsonObjectEnd

template wrapRefLoad*[T](
    writer; target: T, body: untyped): untyped =
  ## Wrap implementation of the ref type serialization.
  bind readRefId, knownId, getRef, setRef
  const name = "wrapRefLoad(" & $typeof(T) & ")"
  if reader.kind == jsonNull:
    target = nil
    skip(reader, {jsonNull}, name)

  else:
    skip(reader, {jsonObjectStart}, name)
    skip(reader, {jsonString}, name)
    let id = readRefId(reader)

    if knownId(reader.sstate, id):
      target = getRef[T](reader.sstate, id)

    else:
      skip(reader, {jsonString}, name)
      new(target)
      setRef(reader.sstate, target, id)
      body

    skip(reader, {jsonObjectEnd}, name)

#==========================  Built-in generics  ==========================#

proc readJsonKeyValues*[K, V, Res](reader; target: var Res) =
  mixin loadJson
  reader.skip({jsonArraystart}, "readJsonKeyValues()")
  while kind(reader) in {jsonArrayStart} or
        reader.tok in {tkComma}:
    reader.skip({jsonArrayStart}, "readJsonKeyValues()")
    var key: K = default(K)
    loadJson(reader, key)

    var val: V = default(V)
    loadJson(reader, val)

    reader.skip({jsonArrayEnd}, "readJsonKeyValues()")
    target[key] = val

  reader.skip({jsonArrayEnd}, "readJsonKeyValues()")

proc writeJsonKeyValues*[K, V, Res](writer; target: Res) =
  mixin writeJson
  writer.write jsonArrayStart
  var first = true
  for key, value in pairs(target):
    if not first:
      writer.comma()

    first = false

    writer.write jsonArrayStart
    writer.writeJson(key)
    writer.comma()
    writer.writeJson(value)
    writer.write jsonArrayEnd

  writer.write jsonArrayEnd

proc writeJsonItems*[T](writer; values: T) =
  mixin writeJson
  var first = true
  writer.write jsonArrayStart
  for it in items(values):
    if not first:
      writer.comma()

    first = false
    writer.writeJson(it)

  writer.write jsonArrayEnd

template loadJsonItems*(
    reader: var JsonDeserializer,
    tmp, insertCall: untyped
  ) =

  mixin loadJson
  skip(reader, {jsonArrayStart}, "loadJsonItems()")
  while kind(reader) != jsonArrayEnd:
    loadJson(reader, tmp)
    insertCall

  skip(reader, {jsonArrayEnd}, "loadJsonItems()")

proc writeJson*[T](writer; values: seq[T]) =
  writeJsonItems(writer, values)

proc loadJson*[T](reader; target: var seq[T]) =
  var tmp: T = default(T)
  loadJsonItems(reader, tmp):
    add(target, tmp)


# ~~~~ Ordered Table ~~~~ #

proc writeJson*[K, V](writer; table: OrderedTable[K, V]) =
  writeJsonKeyValues[K, V, OrderedTable[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var OrderedTable[A, B]) =
  readJsonKeyValues[A, B, OrderedTable[A, B]](reader, target)

# ~~~~ Table ~~~~ #

proc writeJson*[K, V](writer; table: Table[K, V]) =
  writeJsonKeyValues[K, V, Table[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var Table[A, B]) =
  readJsonKeyValues[A, B, Table[A, B]](reader, target)

# ~~~~ Table ref ~~~~ #

proc writeJson*[K, V](writer; table: TableRef[K, V]) =
  wrapRefWrite(writer, table):
    writeJsonKeyValues[K, V, TableRef[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var TableRef[A, B]) =
  wrapRefLoad(reader, target):
    readJsonKeyValues[A, B, TableRef[A, B]](reader, target)

# ~~~~ Oredered table ref ~~~~ #

proc writeJson*[K, V](writer; table: OrderedTableRef[K, V]) =
  wrapRefWrite(writer, table):
    writeJsonKeyValues[K, V, OrderedTableRef[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var OrderedTableRef[A, B]) =
  wrapRefLoad(reader, target):
    readJsonKeyValues[A, B, OrderedTableRef[A, B]](reader, target)

# ~~~~ Array ~~~~ #

proc writeJson*[R, V](writer; table: array[R, V]) =
  writeJsonKeyValues[R, V, array[R, V]](writer, table)

proc loadJson*[R, V](reader; target: var array[R, V]) =
  readJsonKeyValues[R, V, array[R, V]](reader, target)

# ~~~~ Option ~~~~ #

proc writeJson*[T](writer; opt: Option[T]) =
  if opt.isSome():
    writeJson(writer, opt.get())

  else:
    writer.write jsonNull

proc loadJson*[T](reader; target: var Option[T]) =
  mixin loadJson
  if reader.kind == jsonNull:
    reader.skip({jsonNull}, "loadJson(Option[T])")

  else:
    var tmp: T = default(T)
    loadJson(reader, tmp)
    target = some(tmp)

proc writeJson*[E](writer; values: set[E]) =
  writeJsonItems(writer, values)

proc loadJson*[E](reader; target: var set[E]) =
  var tmp: E
  loadJsonItems(reader, tmp):
    target.incl tmp

proc loadJson*[E: enum](reader; target: var E) =
  target = parseEnum[E](reader.str())
  reader.next()

proc writeJson*[E: enum](writer; target: E) =
  writeJson(writer, $target)


#===============================  Objects  ===============================#

proc storeFields[T](writer; target: T, asArray: bool = false) =
  var first = true
  for name, field in fieldPairs(target):
    when isDiscriminantField(T, name):
      if not first: writer.comma()
      first = false
      if not asArray: writeField(writer, name)

      writeJson(writer, field)

  for name, field in fieldPairs(target):
    when not isDiscriminantField(T, name):
      if not first: writer.comma()
      first = false
      if not asArray: writeField(writer, name)
      writeJson(writer, field)

proc storeObject[T](writer; target: T, asArray: bool = false) =
  when target is ref:
    wrapRefWrite(writer, target):
      storeObject(writer, target[])

  else:
    writer.write tern(asArray, jsonArrayStart, jsonObjectStart)
    storeFields(writer, target)
    writer.write tern(asArray, jsonArrayEnd, jsonObjectEnd)

proc loadFields[T](reader; target: var T, asArray: bool = false) =
  {.cast(uncheckedAssign).}:
    var first = true
    var tmpField: string
    for name, field in fieldPairs(target):
      when isDiscriminantField(T, name):
        if not asArray:
          loadJson(reader, tmpField)

        loadJson(reader, field)
        first = false

    for name, field in fieldPairs(target):
      when not isDiscriminantField(T, name):
        if not asArray:
          loadJson(reader, tmpField)

        loadJson(reader, field)
        first = false


proc loadObject[T](
    reader; target: var T, asArray: bool = false) =
  const name = "loadObject(" & $typeof(T) & ")"
  when target is ref:
    expectAt(reader, {jsonObjectStart, jsonNull, jsonArrayStart}, name)
    wrapRefLoad(reader, target):
      loadObject(reader, target[])

  else:
    skip(reader, tern(asArray, {jsonArrayStart}, {jsonObjectStart}), name)
    loadFields(reader, target)
    skip(reader, tern(asArray, {jsonArrayEnd}, {jsonObjectEnd}), name)



proc writeJson*[T: object](writer; target: T) =
  storeObject(writer, target)

proc loadJson*[T: object](reader; target: var T) =
  loadObject(reader, target)

proc writeJson*[T: tuple](writer; target: T) =
  storeObject(writer, target)

proc loadJson*[T: tuple](reader; target: var T) =
  loadObject(reader, target)

proc writeJson*[T: ref object](writer; target: T) =
  storeObject(writer, target)

proc writeJson*[T: ref tuple](writer; target: T) =
  storeObject(writer, target)

proc loadJson*[T: ref object](reader; target: var T) =
  loadObject(reader, target)

proc loadJson*[T: ref tuple](reader; target: var T) =
  loadObject(reader, target)

#==============================  Distinct  ===============================#

proc loadJsonDistinct*[T: distinct](reader; target: var T) =
  type Base = distinctBase(T)
  var base: Base = default(Base)
  loadJson(reader, base)
  target = T(base)

proc writeJsonDistinct*[T: distinct](writer; target: T) =
  type Base = distinctBase(T)
  writeJson(writer, Base(target))

#===========================  High-level API  ============================#

template jsonSerdeFor*(TypeName, readerCall, writerCall: untyped): untyped =
  proc loadJson*(
    reader: var JsonDeserializer, target: var TypeName) =
    readerCall(reader, target)

  proc writeJson*(
    writer: var JsonSerializer, target: TypeName) =
    writerCall(writer, target)

proc fromJson*[T](
    text: string,
    target: typedesc[T],
    traceNext: bool = false
  ): T =
  var reader = newJsonDeserializer(text, traceNext = traceNext)
  loadJson(reader, result)

proc toJson*[T](obj: T): string =
  var writer = newJsonSerializer()
  writeJson(writer, obj)
  return writer.readAll()
