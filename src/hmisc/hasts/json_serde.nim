import ./json_ast, ./serde_macros, ./base_writer
import hmisc/core/all
import std/[
  parsejson, typetraits, macros,
  tables, with, strutils
]

export json_ast, JsonEventKind

type
  SerdeFlags* = enum
    SerAttr ## Field is an attribute (currently triggered only in XML
            ## serde)
    SerSkip ## Skip field serialization

template Serde*(args: varargs[SerdeFlags]) {.pragma.}

## Serialization and deserialization for nim types. Similar to the
## https://github.com/treeform/jsony in terms of the API, but mainly
## focused on supporting more complicated serialization cases, such as
## objects without default values (jsony currently cannot handle
## `{.requiresinit.}` data), cyclic data, object variants with more than
## one discriminant etc.
##
## Implementation is based on the `std/parsejson` and certainly not as
## performant as `jsony` which can operates directly on string buffer.
## Current serialization perfrmance is comparable to the `std/json`. Use of
## event-based lexer from the stdlib allows to provide higher-level API for
## user-defined parsers - you don't need to manually skip quotes, spaces,
## colon tokens and so on, and can instead use high-level operations such
## as `kind()`, `str()`, `getInt()`. Same applies to the writer -
## `writeField()`, `writeIndent()` (possible to implement JSON writer that
## creates a pretty-printed output).
##
##
## Deserialization has several configuration options that allow to either
## ignore or raise exception when unexpected field is encountered, provide
## different ways of serializing key-value pairs (either using json arrays
## or via objects).

type
  JsonSerdeError* = object of ParseError
    ## Base type for all json serde errors (general json parsing errors are
    ## reported using types defined in `std/parsejson` and/or `json_ast`
    ## modules)
    whenUsing*: string ## Name of the type for which serialization or
                       ## deserialization operation was performed.

  JsonSerdeUnknownFieldError* = object of JsonSerdeError
    ## Json deserialization error raised when input data contains unknown
    ## field and json deserializer options does not contain
    ## `jsonIgnoreUnknownFields`
    field*: string ## Unexpected field name found

  JsonSerdeUnexpectedTypeError* = object of JsonSerdeError
    ## Json deserialization error raise when parser contains unexpected
    ## type at point.
    foundKind*: JsonEventKind

  JsonSerializer* = object of JsonWriter
    ## Json serializer object
    state: SerdeState

  JsonDeserializerOptions* = enum
    ## Runtime configuration for the json deserializer
    jsoIgnoreUnknownFields ## Ignore all unknown fields found

  JsonDeserializer* = object of JsonParser
    ## Json deserializer object
    sstate: SerdeState
    traceNext: bool
    options: set[JsonDeserializerOptions]

using
  reader: var JsonDeserializer
  writer: var JsonSerializer

genBaseWriterProcs(JsonSerializer)

const
  defaultJsonDeserializerOptions* = {jsoIgnoreUnknownFields}

proc `$`*(p: JsonDeserializer): string = p.displayAt()

proc next*(reader) =
  ## Get new parser event. If error is found exception is raised
  parsejson.next(reader)
  if reader.traceNext:
    echo reader.displayAt()

  if reader.kind == jsonError:
    raiseParseErr(reader, "err")


proc newJsonDeserializer*(
    arg: string,
    traceNext: bool = false,
    options: set[JsonDeserializerOptions] = defaultJsonDeserializerOptions
  ): JsonDeserializer =

  asgnAux[JsonParser](result, newJsonParser(arg))
  result.traceNext = traceNext
  result.options = options

# template expectAt*(
#     reader; at: set[JsonEventKind], procname: string
#   ): untyped {.dirty.} =
#   bind kind, getLine, getColumn, displayAt
#   {.line: instantiationInfo(fullPaths = true).}:
#     if kind(reader) notin at:
#       when not defined(nimdoc):
#         raise (ref JsonSerdeError)(
#           msg:
#             "Invalid curent parser event kind for '" &
#               procname & "'. Expected any of " &
#               $at & ", but found " &
#               $reader.kind() &
#               " at " & displayAt(reader),
#           line: parsejson.getLine(reader),
#           column: parsejson.getColumn(reader)
#         )

proc expectAt*[Parsing](
    reader;
    kinds: set[JsonEventKind],
    parsing: typedesc[Parsing]
  ) =

  if kind(reader) notin kinds:
    raise (ref JsonSerdeUnexpectedTypeError)(
      msg: mjoin(
        "Unexpected json event type when parsing ",
        mq1($Parsing),
        ". Found ", kind(reader), ", but expected ",
        $kinds
      ),
      line: getLine(reader),
      column: getColumn(reader),
      whenUsing: $Parsing,
      foundKind: kind(reader)
    )


proc skip*[Parsing](
    reader;
    at: set[JsonEventKind],
    parsing: typedesc[Parsing],
  ) =

  expectAt(reader, at, parsing)
  reader.next()

#==========================  Basic datatype IO  ==========================#

proc loadJson*(reader; target: var bool) =
  expectAt(reader, {jsonTrue, jsonFalse}, bool)
  target = reader.kind == jsonTrue
  reader.next()

proc writeJson*(writer; target: bool) =
  if target:
    writer.write jsonTrue

  else:
    writer.write jsonFalse

proc loadJson*[I: SomeUnsignedInt](reader; target: var I) =
  target = I(reader.str().parseBiggestUInt())
  reader.skip({jsonInt}, I)

proc loadJson*[I: SomeSignedInt](reader; target: var I) =
  target = I(reader.str().parseBiggestInt())
  reader.skip({jsonInt}, I)

proc writeJson*(writer; target: SomeInteger) =
  writer.writeRaw($target)

proc loadJson*[T: SomeFloat](reader; target: var T) =
  expectAt(reader, {jsonFloat}, T)
  target = T(getFloat(reader))
  reader.next()

proc writeJson*(writer; target: SomeFloat) =
  writer.writeRaw($target)

proc loadJson*(reader; target: var string) =
  expectAt(reader, {jsonString}, string)
  target = reader.str()
  reader.next()

proc writeJson*(writer; target: string) =
  writer.writeStr(target)

proc loadJson*(reader; target: var cstring) =
  var tmp: string
  loadJson(reader, tmp)
  target = allocCstringArray([tmp])[0]

proc writeJson*(writer; value: cstring) =
  writer.writeStr($value)

proc loadJson*(reader; target: var char) =
  expectAt(reader, {jsonString}, char)
  target = reader.str()[0]
  reader.next()

proc writeJson*(writer; target: char) =
  writer.writeStr($target)


proc readRefId(reader): int =
  var id: int
  loadJson(reader, id)
  return id

const jsonRefidField = "__refid"

template wrapRefWrite*[T](
    writer; target: T,
    isAcyclic: bool = false,
    body: untyped
  ): untyped =

  bind getRefId, writeField, comma
  if isNil(target):
    writer.write jsonNull

  elif isAcyclic:
    body

  elif knownRef(writer.state, target):
    writer.write jsonObjectStart
    writeField(writer, jsonRefidField)
    writeJson(writer, getRefId(writer.state, target))
    writer.write jsonObjectEnd

  else:
    writer.write jsonObjectStart
    writeField(writer, jsonRefidField)
    writeJson(writer, getRefId(writer.state, target))
    comma(writer)
    writeField(writer, "value")
    body
    writer.write jsonObjectEnd

template wrapRefLoad*[T](
    writer;
    target: T,
    isAcyclic: bool = false,
    body: untyped
  ): untyped =
  ## Wrap implementation of the ref type serialization.
  bind readRefId, knownId, getRef, setRef
  if reader.kind == jsonNull:
    target = nil
    skip(reader, {jsonNull}, typeof(T))

  elif isAcyclic:
    new(target)
    body

  else:
    skip(reader, {jsonObjectStart}, typeof(T))
    skip(reader, {jsonString}, typeof(T))
    let id = readRefId(reader)

    if knownId(reader.sstate, id):
      target = getRef[T](reader.sstate, id)

    else:
      skip(reader, {jsonString}, typeof(T))
      new(target)
      setRef(reader.sstate, target, id)
      body

    skip(reader, {jsonObjectEnd}, typeof(T))

func toJsonField*(t: SomeInteger | string | SomeFloat): string =
  ## Convert value to a json field
  $t

# func fromJsonField*(field: string, target: var SomeUnsignedInteger) =
#   tar

proc jsonRenameField*[T](target: var T, field: var string) =
  ## Default implementation of the json field rename hook. Implement this
  ## procedure for your type in order to perform field name conversion.
  ## Default implementation does not modify passed string
  discard

proc jsonPostLoad*[T](target: var T) =
  ## Default implementation of the post load hook. Implement this procedure
  ## for your type in order to execute trigger after each new object is
  ## loaded. Default implementation does nothing.
  discard

proc jsonDefaultValue*[T](t: typedesc[T]): T =
  ## Create a default value for a passed type. Default implementation calls
  ## `default()` and most likely wont have to be overriden direcly, unless
  ## there are some specific setup needed for reading data with json.
  mixin default
  default(T)

#==========================  Built-in generics  ==========================#

proc loadJsonPairs*[K, V, Res](
    reader;
    target: var Res,
  ) =

  mixin loadJson
  reader.skip({jsonArraystart}, Res)
  while kind(reader) in {jsonArrayStart} or
        reader.tok in {tkComma}:
    reader.skip({jsonArrayStart}, Res)
    var key: K = jsonDefaultValue(K)
    loadJson(reader, key)

    var val: V = jsonDefaultValue(V)
    loadJson(reader, val)

    reader.skip({jsonArrayEnd}, Res)
    target[key] = val

  reader.skip({jsonArrayEnd}, Res)

template writeJsonTwoElementIterator*[T](
    writer;
    target: iterable[T],
    multiline: bool = false,
    arrayPairs: static[bool] = true
  ): untyped =

  bind sepComma
  mixin writeJson
  var first = true
  wrap(
    writer,
    tern(arrayPairs, jsonArrayStart, jsonObjectStart),
    multiline
  ):
    for key, value in target:
      sepComma(writer, first, multiline)

      first = false

      when arrayPairs:
        writer.write jsonArrayStart
        writer.writeJson(key)
        writer.comma()
      else:
        writeField(writer, toJsonField(key))

      writer.writeJson(value)

      when arrayPairs:
        writer.write jsonArrayEnd

proc writeJsonPairs*[Res](
    writer;
    target: Res,
    multiline: bool = false,
    arrayPairs: static[bool] = true
  ) =
  ## - @arg{arrayPairs} :: Map sequence of pairs to array of pairs.
  ##  `[[key1, val1], [key2, val2]]`. If set to false type must
  ##  implement `toJsonField`
  writeJsonTwoElementIterator(writer, pairs(target), multiline, arrayPairs)


proc writeJsonItems*[T](writer; values: T, multiline: bool = false) =
  mixin writeJson
  var first = true

  wrap(writer, jsonArrayStart, multiline):
    for it in items(values):
      writer.sepComma(first, multiline)

      writer.writeJson(it)
      first = false

template loadJsonItems*[T](
    reader: var JsonDeserializer,
    tmp, insertCall: untyped
  ) =

  mixin loadJson
  skip(reader, {jsonArrayStart}, typedesc(T))
  while kind(reader) != jsonArrayEnd:
    loadJson(reader, tmp)
    insertCall

  skip(reader, {jsonArrayEnd}, typedesc(T))

proc writeJson*[T](writer; values: seq[T]) =
  writeJsonItems(writer, values)

proc loadJson*[T](reader; target: var seq[T]) =
  var tmp: T = jsonDefaultValue(T)
  loadJsonItems[seq[T]](reader, tmp):
    add(target, tmp)


# ~~~~ Ordered Table ~~~~ #

proc writeJson*[K, V](writer; table: OrderedTable[K, V]) =
  writeJsonPairs[OrderedTable[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var OrderedTable[A, B]) =
  loadJsonPairs[A, B, OrderedTable[A, B]](reader, target)

# ~~~~ Table ~~~~ #

proc writeJson*[K, V](writer; table: Table[K, V]) =
  writeJsonPairs[Table[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var Table[A, B]) =
  loadJsonPairs[A, B, Table[A, B]](reader, target)

# ~~~~ Table ref ~~~~ #

proc writeJson*[K, V](writer; table: TableRef[K, V]) =
  wrapRefWrite(writer, table):
    writeJsonPairs[TableRef[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var TableRef[A, B]) =
  wrapRefLoad(reader, target):
    loadJsonPairs[A, B, TableRef[A, B]](reader, target)

# ~~~~ Oredered table ref ~~~~ #

proc writeJson*[K, V](writer; table: OrderedTableRef[K, V]) =
  wrapRefWrite(writer, table):
    writeJsonPairs[OrderedTableRef[K, V]](writer, table)

proc loadJson*[A, B](reader; target: var OrderedTableRef[A, B]) =
  wrapRefLoad(reader, target):
    loadJsonPairs[A, B, OrderedTableRef[A, B]](reader, target)

# ~~~~ Array ~~~~ #

proc writeJson*[R, V](writer; table: array[R, V]) =
  writeJsonItems[array[R, V]](writer, table)

proc loadJson*[R, V](reader; target: var array[R, V]) =
  var idx = low(R)
  var tmp = jsonDefaultValue(V)
  var isLast = false
  loadJsonItems[array[R, V]](reader, tmp):
    assert not isLast
    target[idx] = tmp
    if idx != high(R):
      idx = succ(idx)
    else:
      isLast = true

# ~~~~ Option ~~~~ #

proc writeJson*[T](writer; opt: Option[T]) =
  if opt.isSome():
    writeJson(writer, opt.get())

  else:
    writer.write jsonNull

proc loadJson*[T](reader; target: var Option[T]) =
  mixin loadJson
  if reader.kind == jsonNull:
    reader.skip({jsonNull}, typeof(Option[T]))

  else:
    var tmp: T = jsonDefaultValue(T)
    loadJson(reader, tmp)
    target = some(tmp)

proc writeJson*[E](writer; values: set[E]) =
  writeJsonItems(writer, values)

proc loadJson*[E](reader; target: var set[E]) =
  var tmp: E
  loadJsonItems[set[E]](reader, tmp):
    target.incl tmp

proc loadJson*[E: enum](reader; target: var E) =
  target = parseEnum[E](reader.str())
  reader.next()

proc writeJson*[E: enum](writer; target: E) =
  writeJson(writer, $target)


#===============================  Objects  ===============================#

proc writeJsonFields[T](
    writer;
    target: T,
    asArray: bool = false,
    multiline: bool = false
  ) =

  var first = true

  for name, field in fieldPairs(target):
    when isDiscriminantField(T, name):
      if SerSkip notin getCustomPragmaValuesSet(
        field, Serde, default(set[SerdeFlags])
      ):

        writer.sepComma(first, multiline)

        first = false
        if not asArray: writeField(writer, name)

        writeJson(writer, field)

  for name, field in fieldPairs(target):
    when not isDiscriminantField(T, name):
      if SerSkip notin getCustomPragmaValuesSet(
        field, Serde, default(set[SerdeFlags])
      ):
        writer.sepComma(first, multiline)

        first = false
        if not asArray: writeField(writer, name)
        writeJson(writer, field)


proc writeJsonObject*[T](
    writer;
    target: T,
    asArray: bool = false,
    multiline: bool = false,
    isAcyclic: bool = false
  ) =

  when target is ref:
    wrapRefWrite(writer, target, isAcyclic):
      writeJsonObject(writer, target[], asArray, multiline)

  else:
    wrap(writer, tern(asArray, jsonArrayStart, jsonObjectStart), multiline):
      writeJsonFields(writer, target, asArray, multiline)

proc loadFields*[T](reader; target: var T, asArray: bool = false) =
  mixin jsonRenameField
  {.cast(uncheckedAssign).}:
    if asArray:
      block fieldRead:
        for name, field in fieldPairs(target):
          loadJson(reader, field)
          if kind(reader) == jsonArrayEnd:
            break fieldRead

    else:
      while kind(reader) != jsonObjectEnd:
        var knownField = false
        var tmpField: string
        loadJson(reader, tmpField)
        jsonRenameField(target, tmpField)

        for name, field in fieldPairs(target):
          if not knownField and
             isDiscriminantField(T, name) and
             tmpField == name:
            loadJson(reader, field)
            knownField = true

        for name, field in fieldPairs(target):
          if not knownField and
             not isDiscriminantField(T, name) and
             tmpField == name:
            loadJson(reader, field)
            knownField = true

        if not knownField:
          if jsoIgnoreUnknownFields in reader.options:
            reader.skipBalanced()

          else:
            raise (ref JsonSerdeUnknownFieldError)(
              msg: "Found unexpected field '" & tmpField &
                "' during parsing of the " & $typeof(T),
              field: tmpField,
              line: getLine(reader),
              column: getColumn(reader)
            )




proc loadJsonObject*[T](
    reader;
    target: var T,
    asArray: bool = false,
    isAcyclic: bool = false
  ) =

  when target is ref:
    expectAt(reader, {jsonObjectStart, jsonNull, jsonArrayStart}, T)
    wrapRefLoad(reader, target, isAcyclic):
      loadJsonObject(reader, target[], asArray = asArray)

  else:
    skip(reader, tern(asArray, {jsonArrayStart}, {jsonObjectStart}), T)
    loadFields(reader, target, asArray = asArray)
    skip(reader, tern(asArray, {jsonArrayEnd}, {jsonObjectEnd}), T)

    jsonPostLoad(target)



proc writeJson*[T: object](writer; target: T) =
  writeJsonObject(writer, target)

proc loadJson*[T: object](reader; target: var T) =
  loadJsonObject(reader, target)

proc writeJson*[T: tuple](writer; target: T) =
  writeJsonObject(writer, target, asArray = not isNamedTuple(T))

proc loadJson*[T: tuple](reader; target: var T) =
  loadJsonObject(reader, target, asArray = not isNamedTuple(T))

proc writeJson*[T: ref object](writer; target: T) =
  writeJsonObject(writer, target)

proc loadJson*[T: ref object](reader; target: var T) =
  loadJsonObject(reader, target)

proc writeJson*[T: ref tuple](writer; target: T) =
  writeJsonObject(writer, target, asArray = not isNamedTuple(T))

proc loadJson*[T: ref tuple](reader; target: var T) =
  loadJsonObject(reader, target, asArray = not isNamedTuple(T))


#==============================  Distinct  ===============================#

proc loadJsonDistinct*[T: distinct](reader; target: var T) =
  type Base = distinctBase(T)
  var base: Base = jsonDefaultValue(Base)
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
    options: set[JsonDeserializerOptions] = defaultJsonDeserializerOptions,
    traceNext: bool = false,
  ): T =
  var reader = newJsonDeserializer(
    text,
    traceNext = traceNext,
    options = options
  )

  loadJson(reader, result)

proc toJson*[T](obj: T): string =
  var writer = newJsonSerializer()
  writeJson(writer, obj)
  return writer.readAll()

template withItWriter*(body: untyped): untyped =
  block:
    var it {.inject.} = newJsonSerializer()
    body
    it.readAll()
