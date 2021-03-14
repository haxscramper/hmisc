## Collection of helper procs to convert output of various command
## programs to json

import ./hshell, ./hjson
import ../algo/[tscanf, hstring_algo, halgorithm]
import ../base_errors
import std/[
  # strscans,
  strutils, sequtils, deques, macros,
  options, segfaults, strformat, parseutils
]

func withIdent(strs: seq[string], ident: int): seq[string] =
  for str in strs:
    if str.startsWith(" ".repeat(ident)):
      result.add str
    else:
      break

func join*(que: StrQue, sep: string = " "): string =
  for idx, val in que:
    if idx > 0:
      result &= sep

    result &= val

func parseNimTypeError*(errl: seq[string]): JsonNode =
  var posLines = newJArray()

  var idx = 0
  while tscanf(errl[idx], "${until({'('})}($i, $i)$[anything]"):
    posLines.add %{
      "file" : %ml[0],
      "line" : %ml[1],
      "column" : %ml[2]
    }

    inc idx

  debugecho errl[idx]
  assert errl[idx]["but expected one of:"]
  inc idx

  while errl[idx][["proc", "template", "macro"]]:
    let forProc = errl[idx] & errl[idx + 1 ..^ 1].withIdent(2)
    idx += forProc.len
    debugecho forProc.join("\n")

  result = %{
    "position-trace" : posLines
  }


func popFirstToSeq*[T](que: var Deque[T], cnt: int): seq[T] =
  for _ in 0 .. cnt:
    result.add que.popFirst()

func nimCmdOutConverter*(
  que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  if "Error: type mismatch" in que[0]:
    var idx = 0
    var foundEnd = false
    while idx < que.len:
      if que[idx].startswith("expression:"):
        foundEnd = true
        break

      inc idx

    if foundEnd:
      return some(parseNimTypeError(que.popFirstToSeq(idx)))


func nimCmdErrConverter*(
  que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  discard

func lslOutConverter*(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  let line = que.popFirst().split(" ").filterIt(it.len != 0)
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

func lslErrConverter*(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  discard



#====================  Base string parser primitives  ====================#
type
  PosStr = object
    ## Helper type for scanning complicated strings
    str*: string
    pos*: int
    ranges*: seq[int]

const
  LowerAsciiLetters = {'a' .. 'b'}
  HighAsciiLetters = {'A' .. 'Z'}
  HexDigitsLow = {'a', 'b', 'c', 'd', 'e', 'f'} + Digits
  HexDigitsHigh = {'A', 'B', 'C', 'D', 'E', 'F'} + Digits
  HexDigits = HexDigitsLow + HexDigitsHigh

template atom*(input: PosStr; idx: int; c: char): bool =
  input.str[input.pos + idx] == c

template atom*(input: PosStr; idx: int; s: set[char]): bool =
  input.str[input.pos + idx] in s

template hasNxt*(input: PosStr; idx: int): bool =
  input.pos + idx < input.str.len

proc finished*(str: PosStr): bool = not str.hasNxt(0)


template nxt*(input: var PosStr; idx, step: int = 1) =
  inc(idx, step)
  inc(input.pos, step)


macro scanpTemp*(str: typed, pattern: varargs[untyped]): untyped =
  result = nnkStmtList.newTree()
  let tmp = genSym(nskVar, "tmp")
  result.add newVarStmt(tmp, newLit(0))
  result.add newCall("scanp", str, tmp)
  for patt in pattern:
    result[^1].add patt

func `[]`*(str: PosStr, idx: int = 0): char {.inline.} = 
  str.str[str.pos + idx]

func `[]`*(str: PosStr, offset: int, patt: char | set[char] | string): 
  bool {.inline.} =

  when patt is char:
    result = (str.pos + offset < str.str.len) and str[offset] == patt

  elif patt is set[char]:
    result = (str.pos + offset < str.str.len) and str[offset] in patt

  else:
    result = true
    for idx, ch in patt:
      if not str[offset + idx, ch]:
        return false

func `[]`*(str: PosStr, patt: char|set[char]|string): bool {.inline.} =
  return str[0, patt]



func `@`*(str: PosStr): seq[char] =
  for ch in str.str[str.pos .. ^1]:
    result.add ch

func `$`*(str: PosStr): string {.inline.} =
  &"[{str.pos}: {str.str[str.pos .. ^1]}]"

func pushRange(str: var PosStr) {.inline.} =
  str.ranges.add str.pos

func popRange(str: var PosStr, leftShift: int = 0, rightShift: int = 0): 
  string {.inline.} =

  let start = str.ranges.pop
  return str.str[(start + leftShift) ..<
                 min(str.pos + rightShift, str.str.len)]

func advance*(str: var PosStr, step: int = 1) {.inline.} =
  inc(str.pos, step)

func skipWhile*(str: var PosStr, chars: set[char]) {.inline.} =
  if str[] in chars:
    while str[chars]:
      str.advance()

func popWhile*(str: var PosStr, chars: set[char]): string {.inline.} =
  str.pushRange()
  str.skipWhile(chars)
  return str.popRange()

func popChar*(str: var PosStr): char {.inline.} =
  result = str.str[str.pos]
  str.advance()

func popStringLit*(str: var PosStr): string {.inline.} =
  str.pushRange()

  str.advance()
  while not str['"']:
    str.advance()

  str.advance()

  return str.popRange()

func popDigit*(str: var PosStr): string {.inline.} =
  str.pushRange()
  if str["0x"]:
    str.advance(2)
    str.skipWhile(HexDigits + {'-'})

  else:
    str.skipWhile(Digits + {'-'})

  return str.popRange()

func popIdent*(str: var PosStr, chars: set[char] = IdentChars):
  string {.inline.} = str.popWhile(chars)



func toMapArray*[K, V](map: openarray[(K, V)]): array[K, V] =
  for (k, v) in map:
    result[k] = v

func toKeySet*[K, V](map: openarray[(K, V)]): set[K] =
  for (k, v) in map:
    result.incl k

func toValSet*[K, V](map: openarray[(K, V)]): set[V] =
  for (k, v) in map:
    result.incl v

func mapChar*[Cat: enum](
  ch: char, map: static[openarray[tuple[key: char, val: Cat]]]): Cat =

  const
    chars = toKeySet(map)
    map = toMapArray(map)

  if ch notin chars:
    raiseArgumentError(
      &"Unexpected input char: got '{ch}', but expected {chars}")

  return map[ch]


type
  HsTokTree[Rule, Tok] = object
    case isToken*: bool
      of true:
        token*: Tok

      of false:
        rule*: Rule
        subnodes*: seq[HsTokTree[Rule, Tok]]

  HsTok[K] = object
    kind*: K
    str*: string

  HsLexer[Tok] = object
    tokens*: seq[Tok]
    pos*: int

  HsTokPredicate[T] = proc(tok: T): bool

func `$`*[K](tok: HsTok[K]): string =
  var kindStr = $tok.kind
  kindStr = kindStr[kindStr.skipWhile(LowerAsciiLetters) .. ^1]

  var str = tok.str
  if not(str['"'] and str[^'"']):
    str = str.wrap('"', '"')

  return &"({kindStr} {str})"

func `@`*[T](lex: HSLexer[T]): seq[T] = lex.tokens[lex.pos .. ^1]
func `$`*[T](lex: HSLexer[T]): string =
  result = &"[{lex.pos}: "
  for idx, token in pairs(lex.tokens[lex.pos .. ^1]):
    if idx > 0:
      result &= ", "

    result &= $token

  result &= "]"

func `[]`*[T](lex: var HSlexer[T], offset: int = 0): T =
  lex.tokens[lex.pos + offset]

func advance*[T](lex: var HSlexer[T], step: int = 1) =
  inc(lex.pos, step)

func pop*[T](lex: var HsLexer[T]): T =
  result = lex.tokens[lex.pos]
  lex.advance()

func initLexer*[T](tokens: seq[T]): HsLexer[T] =
  HsLexer[T](tokens: tokens)

func skip*[T, En](lexer: var HsLexer[T], kind: En) =
  assertKind(lexer[], kind)
  lexer.advance()

func hsTokHasKind*[T, En](kind: En): HsTokPredicate[T] =
  proc cb(tok: T): bool =
    tok.kind == kind

  return cb

func hsParseIdent*[R, T](lex: var HsLexer[T], rule: R):
  HsTokTree[R, T] = HsTokTree[R, T](isToken: true, token: lex.pop())

func hsInsideBalanced*[T](
    lex: var HsLexer[T], isOpen, isClose: HsTokPredicate[T],
    withWrap: bool = false
  ): seq[T] =

  var cnt: int

  if isOpen(lex[]):
    inc cnt
    if withWrap:
      result.add lex.pop()

    else:
      lex.advance()

    while cnt > 0:
      if isOpen(lex[]):
        inc cnt

      elif isClose(lex[]):
        dec cnt

      if cnt > 0 or withWrap:
        result.add lex.pop()

      else:
        lex.advance()

func hsSplitSep*[T](tokens: seq[T], isSep: HsTokPredicate[T]): seq[seq[T]] =
  result.add @[]
  for tok in tokens:
    if isSep(tok):
      result.add @[]

    else:
      result[^1].add tok

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

proc parseCall*(str: string): StraceRecord =
  var str = PosStr(str: str)
  var name: string
  var idx: int

  var toks: seq[StrTok]
  while not finished(str):
    case str[]:
      of IdentStartChars:
        toks.add initTok(str.popIdent(), stkIdent)

      of '(', ')', '[', ']', '{', '}', ',', '=', '|', '?':
        let ch = str.popChar()
        toks.add initTok(ch, mapChar(ch, {
          '(': stkLPar,
          ')': stkRPar,
          '[': stkLBrace,
          ']': stkRBrace,
          '{': stkLCurly,
          '}': stkRCurly,
          ',': stkComma,
          '=': stkEq,
          '|': stkPipe,
          '?': stkQuestion
        }))

      of '"':
        toks.add initTok(str.popStringLit(), stkStrLit)

      of ' ':
        str.advance()

      of '.':
        toks.add initTok(str.popWhile({'.'}), stkEllipsis)

      of Digits, '-':
        toks.add initTok(str.popDigit(), stkNum)

      of '/':
        if str[+1, '*']:
          str.advance(2)
          str.pushRange()

          while not str["*/"]:
            str.advance()

          toks.add initTok(str, stkComment)
          str.advance(2)

      else:
        raiseImplementError(&"[{str[]}] {str}")

  var lex = initLexer(toks)

  let hasKind = hsTokHasKind[StrTok, StrTokKind]

  let head = lex.hsParseIdent("ident")
  let args = lex.hsInsideBalanced(
    hasKind(stkLPar),
    hasKind(stkRPar)
  ).hsSplitSep(hasKind(stkComma))
  lex.skip(stkEq)
  echo $lex



proc straceOutConverter*(que: var StrQue, cmd: ShellCmd):
  Option[StraceRecord] =

  if que[^1].startsWith(IdentChars, "("):
    echo que[^1]
    return some parseCall(que.popFirst())

  else:
    echo &"que: {que[^1]}"

  discard

func straceErrConverter*(que: var StrQue, cmd: ShellCmd):
  Option[StraceRecord] =

  discard



