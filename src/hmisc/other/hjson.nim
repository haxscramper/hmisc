import std/[ json, options, tables, strutils, macros,
             sequtils, enumerate, unittest ]

export json except items

import ../macros/[cl_logic, introspection]

##[

This module provides helper functions for working with json that don't
silently ignore errors. If something is missing exception is raised.

- `as*` functions - throw exception if node kind does not match
  (stdlib implementations silently return default value)

]##

runnableExamples:
  let js = parseJson("""{"a" : 12}""")

template jsonConversion(target, conversionProc: untyped): untyped =
  ## Implementation for the convertsion procs. Throw exception if type
  ## does not match.
  let targetKind {.inject.} = target
  if node.kind == targetKind:
    return node.conversionProc()
  else:
    raise newException(
      ValueError,
      "Json node of kind " & $node.kind &
        " cannot be converted to " & $targetKind
    )

func asBool*(node: JsonNode): bool =
  ## Convert node to bool and throw exception if node kind does not match
  jsonConversion(JBool, getBool)

func asStr*(node: JsonNode): string =
  ## Convert node to string and throw exception if node kind does not match
  jsonConversion(JString, getStr)

func asInt*(node: JsonNode): int =
  ## Convert node to int and throw exception if node kind does not match.
  jsonConversion(JInt, getInt)

func asFloat*(node: JsonNode): float =
  ## Convert node to float and throw exception if node kind does not match
  jsonConversion(JFloat, getFloat)

func asSeq*(node: JsonNode): seq[JsonNode] =
  ## Convert node to sequence and throw exception if kind does not match
  jsonConversion(JArray, getElems)

func asTable*(node: JsonNode): OrderedTable[string, JsonNode] =
  ## Convert node to table and throw exception if kind does not match
  jsonConversion(JObject, getFields)

iterator items*(node: JsonNode): JsonNode =
  assert node.kind == JArray
  for node in node.getElems():
    yield node

func joinArr*(node: JsonNode): string =
  ## Convert node to list of strings and join them. Throw exception if
  ## kind does not mach.
  if not (node.kind == JArray and node.getElems().allIt(it.kind == JString)):
    raise newException(
      ValueError,
     "Only array of strings can be joined using `joinArr`"
    )

  node.asSeq().mapIt(it.asStr()).join("")

func asStrSeq*(node: JsonNode): seq[string] =
  ## Convert node to sequence of strings and join them. Throw
  ## exception if kind does not match.
  node.asSeq().map(asStr)

func `%`*(c: char): JsonNode = %($c)

template unref(val: untyped): untyped =
  when val is ref: val[]
  else: val

type
  JsonConvertOpt = enum
    jcoCompactNil
    jcoCompactOptions
    jcoCompactEmptySeqs
    jcoDirectEnumName

  JsonConvertOpts* = set[JsonConvertOpt]

const defaultJsonConvertOptions* = {
  jcoCompactNil,
  jcoCompactOptions,
  jcoCompactEmptySeqs,
  jcoDirectEnumName
}

func toJson*(
    arg: string | SomeInteger | bool | float | char,
    opts: JsonConvertOpts = defaultJsonConvertOptions
  ): JsonNode =

  mixin toJson
  %arg

func toJson*[T](
    arg: seq[T] | openarray[T] | set[T],
    opts: JsonConvertOpts = defaultJsonConvertOptions
  ): JsonNode =

  mixin toJson
  result = newJArray()
  for elem in items(arg):
    result.add toJson(elem)

func toJson*[K, V](table: Table[K, V]): JsonNode =
  result = newJObject()
  mixin toJson
  for key, val in pairs(table):
    result[key] = toJson(val)

func toJson*[T](
    arg: openarray[(string, T)] | seq[(string, T)],
    opts: JsonConvertOpts = defaultJsonConvertOptions
  ): JsonNode =

  mixin toJson
  result = newJObject()
  for (key, val) in items(arg):
    result[key] = toJson(val)

func toJson*(
    arg: openarray[(string, JsonNode)],
    opts: JsonConvertOpts = defaultJsonConvertOptions
  ): JsonNode =

  result = newJObject()
  for (key, val) in items(arg):
    result[key] = val

func toJson*(
    arg: enum,
    opts: JsonConvertOpts = defaultJsonConvertOptions
  ): JsonNode =

  if jcoDirectEnumName in opts:
    newJString(directEnumName(arg))

  else:
    newJString($arg)


func toJson*(
    arg: object | tuple | ref object | ref tuple,
    ignoreFields: static[seq[string]],
    opts: JsonConvertOpts = defaultJsonConvertOptions
  ): JsonNode =

  when arg is ref:
    if isNil(arg):
      return newJNull()

  mixin toJson
  result = newJObject()
  for key, val in fieldPairs(unref arg):
    when key notin ignoreFields:
      when val is Option:
        if jcoCompactOptions in opts:
          if val.isSome():
            result[key] = toJson(val.get())

        else:
          result[key] = toJson(val)

      elif val is seq:
        if (jcoCompactEmptySeqs notin opts) or val.len > 0:
          result[key] = toJson(val)

      else:
        when val is ref:
          if isNil(val):
            if jcoCompactNil notin opts:
              result[key] = newJNull()

          else:
            result[key] = toJson(val)

        else:
          result[key] = toJson(val)

func toJson*(
    arg: object | tuple | ref object | ref tuple,
    opts: JsonConvertOpts = defaultJsonConvertOptions
  ): JsonNode =

  const emptySeq = newSeq[string]()
  toJson(arg, emptySeq, opts)


proc toPretty*(j: JsonNode, maxWidth: int = 80): string =
  var memo: Table[(JsonNode, int), tuple[ok: bool, w: int]]
  proc fits(j: JsonNode, width: int): tuple[ok: bool, w: int] =
    if (j, width) in memo:
      result = memo[(j, width)]

    else:
      case j.kind:
        of JInt, JNull, JFloat, JBool:
          result[1] = len($j)
          result[0] = width > result[1]

        of JString:
          result[1] = j.str.len + 2
          result[0] = width > result[1]

        of JArray:
          let width = width - 2
          result[0] = true
          for elem in items(j):
            let (ok, w) = fits(elem, width - result[1] - 2)
            result[1] += w + 2
            if not ok:
              result[0] = false
              break

        of JObject:
          result[0] = true
          for key, value in pairs(j):
            result[1] += key.len + 4
            let (ok, w) = fits(value, width - result[1])
            result[1] += w

            if not ok:
              result[0] = false
              break

          # echo "result for ", j
          # echo width
          # echo result


      memo[(j, width)] = result

  proc aux(j: JsonNode, buf: var string, indent: int) =
    let pref = " ".repeat(indent)
    case j.kind:
      of JInt, JString, JFloat, JBool, JNull:
        buf.add $j

      of JArray:
        let fitIdx = mapIt(j, fits(it, maxWidth - indent - 2))
        # echo fitIdx
        # echo maxWidth - indent - 2
        let compact = fits(j, maxWidth - indent - 2)
        let allObject = not compact.ok and allIt(j, it.kind == JObject)

        buf.add "["

        for idx, entry in enumerate(items(j)):
          if idx != 0:
            buf.add (if compact.ok: ", " else: ",")

          if allObject:
            if fitIdx[idx].ok:
              buf.add "\n"
              buf.add pref

          elif not compact.ok:
            buf.add "\n"
            buf.add pref

          aux(entry, buf, cond2(
            (compact.ok, 0),
            (allObject and fitIdx[idx].ok, indent),
            (indent + 2)
          ))


        if fitIdx[^1].ok and not compact.ok:
          buf.add "\n"
          if indent >= 2:
            buf.add " ".repeat(indent - 2)

        buf.add "]"

      of JObject:
        let compact = j.fits(maxWidth - indent)
        buf.add "{"
        for idx, (field, value) in enumerate(pairs(j)):
          if idx != 0:
            buf.add (if compact.ok: ", " else: ",")

          if not compact.ok:
            buf.add "\n"
            buf.add pref

          buf.add "\"" & field & "\": "
          aux(value, buf, if compact.ok: 0 else: indent + 2)

        if not compact.ok:
          buf.add "\n"
          if indent >= 2:
            buf.add " ".repeat(indent - 2)

        buf.add "}"


  aux(j, result, 0)
