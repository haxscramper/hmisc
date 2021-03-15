## Collection of helper procs to convert output of various command
## programs to json

import ./hshell, ./hjson
import ../algo/[tscanf, hstring_algo, halgorithm, hlex_base, hparse_base]
import ../base_errors, ../hdebug_misc, ../helpers
import std/[
  # strscans,
  strutils, sequtils, deques, macros, streams,
  options, segfaults, strformat, parseutils
]

proc readLine*(stream: Stream): string =
  discard stream.readLine(result)

func withIdent(strs: seq[string], ident: int): seq[string] =
  for str in strs:
    if str.startsWith(" ".repeat(ident)):
      result.add str
    else:
      break

proc lslOutConverter*(stream: var PosStr, cmd: ShellCmd): Option[JsonNode] =
  let line = stream.readLine().split(" ").filterIt(it.len != 0)
  if line.len == 0 or line[0] == "total":
    return

  else:
    return some(
      %*{
        "entry-type" : line[0][0],
        "permissions" : {
          "root" : line[0][1..3],
          "group" : line[0][4..6],
          "user" : line[0][7..9],
         },
        "owner" : line[2],
        "group" : line[3],
        "size" : line[4],
        "created" : line[5..7].join(" "),
        "filename" : line[8..^1].join(" ")
      }
    )

proc lslErrConverter*(stream: var PosStr, cmd: ShellCmd): Option[JsonNode] =
  discard





type
  StrTokKind = enum
    stkNum
    stkLPar
    stkRPar
    stkStrLit
    stkEllipsis
    stkComma
    stkLCurly
    stkRCurly
    stkLBrace
    stkRBrace
    stkComment
    stkEq
    stkIdent
    stkPipe
    stkQuestion
    stkPlus

  StrTok = HsTok[StrTokKind]

  StraceRecordKind = enum
    srkCall

  StraceRecord = object
    case kind*: StraceRecordKind
      of srkCall:
        callName*: string
        callArgs*: seq[string]
        callResult*: int

proc initTok(str: var PosStr, kind: StrTokKind): StrTok =
  StrTok(str: str.popRange(), kind: kind)

proc initTok(str: char | string, kind: StrTokKind): StrTok =
  StrTok(str: $str, kind: kind)

proc lexCall*(str: var PosStr): Option[StrTok] =
  case str[]:
    of IdentStartChars:
      result = some initTok(str.popIdent(), stkIdent)

    of '(', ')', '[', ']', '{', '}', ',', '=', '|', '?', '+':
      let ch = str.popChar()
      result = some initTok(ch, mapChar(ch, {
        '(': stkLPar,
        ')': stkRPar,
        '[': stkLBrace,
        ']': stkRBrace,
        '{': stkLCurly,
        '}': stkRCurly,
        ',': stkComma,
        '=': stkEq,
        '|': stkPipe,
        '?': stkQuestion,
        '+': stkPlus
      }))

    of '"':
      result = some initTok(str.popStringLit(), stkStrLit)

    of ' ':
      str.advance()

    of '.':
      result = some initTok(str.popWhile({'.'}), stkEllipsis)

    of Digits, '-':
      result = some initTok(str.popDigit(), stkNum)

    of '/':
      if str[+1, '*']:
        str.advance(2)
        str.pushRange()

        while not str["*/"]:
          str.advance()

        result = some initTok(str, stkComment)
        str.advance(2)

    of '\n':
      str.advance(1)

    else:
      raiseImplementError(&"[{str[]} (str[].int)] {$str}")


proc parseCall*(str: var PosStr): StraceRecord =
  var lex = initLexer(str, lexCall)
  let hasKind = hsTokHasKind[StrTok, StrTokKind]

  let head = lex.hsParseIdent("ident")
  let args = lex.hsInsideBalanced(
    hasKind(stkLPar),
    hasKind(stkRPar)
  ).hsSplitSep(hasKind(stkComma))
  lex.skip(stkEq)
  echov args



proc straceOutConverter*(str: var PosStr, cmd: ShellCmd):
  Option[StraceRecord] =

  discard

proc straceErrConverter*(str: var PosStr, cmd: ShellCmd):
  Option[StraceRecord] =

  if str.startsWith(IdentChars, "("):
    return some parseCall(str)

  else:
    str.skipLine()
