import
  std/[enumerate, strutils, tables, with, strtabs, options, streams]

import
  ../core/all,
  ../types/colorstring,
  ../other/[oswrap, rx],
  ../algo/[hlex_base, hparse_base]

export hparse_base


type
  JsonTokenKind* = enum
    jtkLCurly
    jtkRCurly
    jtkString
    jtkNumber
    jtkColon
    jtkComma
    jtkKey
    jtkLBrace
    jtkRBrace
    jtkFloat
    jtkEof

  JsonTok = HsTok[JsonTokenKind]

  JsonReader* = object
    str: PosStr
    tok: JsonTok

  JsonWriter* = object
    stream: Stream
    indentBuf: string

using
  w: var JsonWriter
  r: var JsonReader

proc close*(w: var JsonWriter) = w.stream.close()

proc `=destroy`*(w: var JsonWriter) = discard

proc newJsonWriter*(stream: Stream): JsonWriter =
  JsonWriter(stream: stream)

proc newJsonWriter*(file: AbsFile): JsonWriter =
  newJsonWriter(newFileStream(file, fmWrite))

proc newJsonWriter*(file: File): JsonWriter =
  newJsonWriter(newFileStream(file))

proc write*(w; str: string) = w.stream.write(str)
proc space*(w) = w.write(" ")
proc line*(w) = w.write("\n")
proc indent*(w) = w.indentBuf.add "  "
proc dedent*(w) =
  w.indentBuf.setLen(max(w.indentBuf.len - 2, 0))

proc writeInd*(w) = w.write(w.indentBuf)

proc kind*(r: JsonReader): JsonTokenKind = r.tok.kind
proc finished*(r): bool = r.str.finished()

proc next*(r) =
  if r.str.finished():
    r.tok = initEOF(r.str, jtkEOF)

  else:
    case r.str[0]:
      of IntegerStartChars:
        r.tok = initTok(r.str, r.str.popDigit(), jtkNumber)

      of '\"':
        r.tok = initTok(r.str, r.str.popStringLit(), jtkString)
        if r.str[':']:
          r.tok.kind = jtkKey

      of '{', '}', '[', ']', ':', ',':
        r.tok = initCharTok(r.str, {
          '}': jtkRCurly,
          '{': jtkLCurly,
          ']': jtkRBrace,
          '[': jtkLBrace,
          ',': jtkComma,
          ':': jtkColon
        })

      of Whitespace:
        r.str.skipWhile(Whitespace)
        r.next()

      else:
        raiseUnexpectedChar(r.str)

proc newJsonReader*(s: Stream): JsonReader =
  result = JsonReader(str: initPosStr(s))
  result.next()

proc newJsonReader*(s: string): JsonReader =
  newJsonReader(newStringStream(s))



proc jsonTokens*(str: string): seq[JsonTok] =
  var r = newJsonReader(str)
  while not r.finished():
    r.next()
    result.add r.tok


proc writeJson*(w; val: SomeInteger | string | SomeFloat | enum | bool) =
  w.write($val)

proc loadJson*(r; val: var SomeInteger) =
  r.assertKind({jtkNumber})
  val = parseInt(r.tok.str)
  r.next()

proc loadJson*(r; val: var string) =
  r.assertKind({jtkString, jtkKey})
  val = r.tok.str
  r.next()

proc loadJson*[E: enum](r; val: var E) =
  r.assertKind({jtkString, jtkKey})
  val = parseEnum[E](r.tok.str)
  r.next()
