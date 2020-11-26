import json
import tables
import sequtils
import strutils
export json except items

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
      "Json node of kind" & $node.kind &
        "cannot be converted to" & $targetKind
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
