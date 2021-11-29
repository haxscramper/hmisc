import std/[streams, parsejson, json]
import hmisc/core/all

import ./base_writer

type
  JsonWriter* = object of RootObj
    base*: BaseWriter

genBaseWriterProcs(JsonWriter)

using
  writer: var JsonWriter

proc write*(writer; event: JsonEventKind) =
  writer.writeRaw():
    case event:
      of jsonTrue: "true"
      of jsonFalse: "false"
      of jsonNull: "null"
      of jsonObjectStart: "{"
      of jsonObjectEnd: "}"
      of jsonArrayStart: "["
      of jsonArrayEnd: "]"
      else: raise newUnexpectedKindError(event)

proc write*(writer; tk: TokKind) =
  writer.writeRaw():
    case tk:
      of tkTrue: "true"
      of tkFalse: "false"
      of tkNull: "null"
      of tkCurlyLe: "{"
      of tkCurlyRi: "}"
      of tkBracketLe: "["
      of tkBracketRi: "]"
      of tkColon: ":"
      of tkComma: ","
      else: raise newUnexpectedKindError(tk)

proc comma*(writer) = writer.writeRaw(", ")
proc colon*(writer) = writer.writeRaw(": ")

proc writeField*(writer; name: string) =
  writer.writeRaw(escapeJson(name))
  writer.writeRaw(": ")

proc writeStr*(writer; text: string) =
  writer.writeRaw(escapeJson(text))

proc newJsonParser*(
    text: string,
    filename: string = "<text>"
  ): JsonParser =

  open(result, newStringStream(text), filename)
  next(result)

proc getStr*(parser: JsonParser): string = parser.str()

proc currentEventToStr*(parser: JsonParser): string =
  result.add $parser.kind
  result.add " "
  result.add $parser.tok
  result.add " "
  result.add():
    case parser.kind:
      of jsonError:       errorMsg(parser)
      of jsonEof:         "[EOF]"
      of jsonString:      parser.getStr()
      of jsonInt:         parser.str()
      of jsonFloat:       parser.str()
      of jsonTrue:        "true"
      of jsonFalse:       "false"
      of jsonNull:        "null"
      of jsonObjectStart: "{"
      of jsonObjectEnd:   "}"
      of jsonArrayStart:  "["
      of jsonArrayEnd:    "]"

proc displayAt*(parser: JsonParser): string =
  result = $parser.getFilename() & "(" & $parser.getLine &
    ":" & $parser.getColumn & ") "
  result.add currentEventToStr(parser)

var p = newJsonParser("[1,2,3]")

iterator eventsUntil*(
    p: var JsonParser,
    endTok: set[TokKind]): JsonEventKind =

  while p.tok notin endTok:
    yield p.kind()
    p.next()

iterator eventsUntil*(
    p: var JsonParser,
    endTok: set[JsonEventKind]): JsonEventKind =

  while p.kind notin endTok:
    yield p.kind()
    p.next()
