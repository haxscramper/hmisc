import std/[strutils, strformat, parseutils, options]
import ../base_errors, ../hdebug_misc
import ./hlex_base, ./hstring_algo

type
  HsTokTree*[Rule, Tok] = object
    case isToken*: bool
      of true:
        token*: Tok

      of false:
        rule*: Rule
        subnodes*: seq[HsTokTree[Rule, Tok]]

  HsTok*[K: enum] = object
    kind*: K
    str*: string

  HsLexer*[Tok] = object
    tokens*: seq[Tok]
    cb*: HsLexCallback[Tok]
    pos*: int
    str*: ptr PosStr

  HsLexCallback*[T] = proc(str: var PosStr): Option[T]
  HsTokPredicate*[T] = proc(tok: T): bool

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

func resetBuffer*[T](lex: var HSLexer[T]) =
  lex.pos = 0
  lex.tokens.setLen(0)

func nextToken*[T](lex: var HSLexer[T]): T =
  var tok: Option[T]
  while tok.isNone():
    tok = lex.cb(lex.str[])

  return tok.get()

func fillNext*[T](lex: var HSLexer[T], chars: int) =
  let needed = chars - (lex.tokens.len - lex.pos - 1)
  if needed > 0:
    for _ in 0 ..< needed:
      lex.tokens.add nextToken(lex)

func haxNxt*[T](lex: HsLexer[T], offset: int): bool =
  lex.pos + offset < lex.tokens.len

func `[]`*[T](lex: var HSlexer[T], offset: int = 0): T =
  if not haxNxt(lex, offset):
    fillNext(lex, offset)

  lex.tokens[lex.pos + offset]

func `[]`*[T](lex: var HsLexer[T], slice: Slice[int]): seq[T] =
  for i in slice:
    result.add lex[i]


func `[]`*[K](tokens: seq[HsTok[K]], offset: int, kind: set[K]|K): bool =
  when kind is set:
    tokens[offset].kind in kind

  else:
    tokens[offset].kind == kind

func `[]`*[K](tokens: seq[HsTok[K]], kind: set[K]|K): bool =
  tokens[0, kind]

func advance*[T](lex: var HSlexer[T], step: int = 1) =
  inc(lex.pos, step)

func pop*[T](lex: var HsLexer[T]): T =
  result = lex[]
  lex.advance()

func initLexer*[T](str: var PosStr, lexCb: HsLexCallback[T]): HsLexer[T] =
  HsLexer[T](str: addr str, cb: lexCb)

func skip*[T, En](lexer: var HsLexer[T], kind: En) =
  assertKind(lexer[], kind)
  lexer.advance()

proc skipTo*[T](lex: var HsLexer[T], chars: set[char]) =
  lex.str[].skipWhile(AllChars - chars)

func hsParseIdent*[R, T](lex: var HsLexer[T], rule: R):
  HsTokTree[R, T] = HsTokTree[R, T](isToken: true, token: lex.pop())

func hsInsideBalanced*[T, K](
    lex: var HsLexer[T], openKinds, closeKinds: set[K],
    withWrap: bool = false
  ): seq[T] =

  var cnt: int

  if lex[].kind in openKinds:
    inc cnt
    if withWrap:
      result.add lex.pop()

    else:
      lex.advance()

    while cnt > 0:
      if lex[].kind in openKinds:
        inc cnt

      elif lex[].kind in closeKinds:
        dec cnt

      if cnt > 0 or withWrap:
        result.add lex.pop()

      else:
        lex.advance()

func hsSplitSep*[T, K](tokens: seq[T], sep: set[K]): seq[seq[T]] =
  result.add @[]
  for tok in tokens:
    if tok.kind in sep:
      result.add @[]

    else:
      result[^1].add tok

func hsSplitKeyValue*[T, K](tokens: seq[T], kvDelimiter: set[K], itemSep: set[K]):
  seq[tuple[key, value: seq[T]]] =

  var init = (newSeq[T](), newSeq[T]())
  result.add init

  var inKey: bool = true
  for tok in tokens:
    if tok.kind in itemSep:
      inKey = true
      result.add init

    if tok.kind in kvDelimiter:
      inKey = false

    else:
      if inKey:
        result[^1].key.add tok

      else:
        result[^1].value.add tok

func getInsideBalanced*[T, K](
  tokens: seq[T], openKinds, closeKinds: set[K], withWrap: bool = false):
  seq[T] =

  var pos = 0
  var cnt: int
  if tokens[pos].kind in openKinds:
    inc cnt
    if withWrap:
      result.add tokens[pos]
      inc pos

    else:
      inc pos

    while cnt > 0 and pos < tokens.len:
      if tokens[pos].kind in openKinds:
        inc cnt

      elif tokens[pos].kind in closeKinds:
        dec cnt

      if cnt > 0 or withWrap:
        result.add tokens[pos]
        inc pos

      else:
        inc pos

func expectKind*[T, K](lex: var HsLexer[T], kind: set[K]) =
  lex[].assertKind(kind)

func pushRange*[T](lex: var HsLexer[T]) =
  lex.str[].pushRange()

func popRange*[T](lex: var HsLexer[T]): string =
  lex.str[].popRange()
