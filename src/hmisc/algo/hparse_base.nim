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

  HsTok*[K] = object
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

func `[]`*[T](lex: var HSlexer[T], offset: int = 0): T =
  fillNext(lex, offset)
  lex.tokens[lex.pos + offset]

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
