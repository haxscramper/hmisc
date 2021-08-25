import std/[
  strutils, strformat, parseutils,
  options, segfaults, parseutils,
  sequtils
]

## - TODO :: Require lexer callback to return at least one token, always

import
  ../core/all,
  ../types/colorstring,
  ../algo/clformat,
  ./hlex_base,
  ./hstring_algo,
  ./hseq_mapping,
  ./halgorithm

type
  HsTokTree*[Rule, Tok] = object
    kind*: Rule
    case isToken*: bool
      of true:
        token*: Tok
        str: string

      of false:
        subnodes*: seq[HsTokTree[Rule, Tok]]


  HsTok*[K: enum | SomeInteger | char] = object
    # uint*-based positional information allows to save up to 50% of token
    # size, but I decided it is not really necessary as tokens themselves
    # are not really inteded to be stored in large numbers anywhere.
    # Although if necessary it might also be possible to implement SSO
    # string and make token a fully stack-allocated object with size of 16
    # bytes

    # line*: uint32
    # column*: uint16

    line*: int
    column*: int
    offset*: int
    kind*: K
    case isSlice*: bool
      of true:
        baseStr*: ref string
        finish*: int
        extra*: seq[PosStrSlice]

      of false:
        str*: string

  HsLexer*[Tok] = object
    explicitEof*: bool ## Lexer callback as separate token to denote end of
    ## file that is returned when end is reached. - `finished()` will
    ## always return false.
    ##
    ## - WARNING :: If set to `true` user MUST handle string end.
    tokens*: seq[Tok]
    cb*: HsLexCallback[Tok]
    pos*: int
    str*: PosStr

  LexerIndentKind* = enum
    likIncIndent ## Indentation increased on current position
    likDecIndent ## Decreased on current position
    likSameIndent ## No indentation change
    likNoIndent ## Not at position where indentation can be determine (e.g.
                ## is inside of a identifier or at the start of the line)

  HsLexerState*[Flag:
    enum | char | uint8 | uint16 | int8 | int16 | bool] = ref object

    flagStack: seq[Flag]
    flagSet: array[Flag, int]
    indent: int
    indentLevels: int

  HsLexCallback*[T] = proc(str: var PosStr): seq[T]
  # HsMultiLexCallback*[T] = proc(str: var PosStr): seq[T]

  HsTokPredicate*[T] = proc(tok: T): bool

  ParseError* = ref object of CatchableError
    # REFACTOR merge HsLexerError and parse error into single entry. Move
    # declaration into core exceptions handling. Ideally it should also be
    # used as a base implementation for `code_errors.nim`, intead of
    # current legacy implementation that was hacked over multiple
    # iterations.
    line*: int
    column*: int
    baseOffset*: int
    annotation*: string

  UnexpectedTokenError* = ref object of ParseError
    gotToken*: string
    expected*: seq[string]
    linePos*: int
    lineText*: string

  ParseFail = object of CatchableError

  ParseResult[Val] = object
    case ok*: bool
      of true:
        value*: Val

      of false:
        fail*: ParseFail

  HsParseCallback*[T, Val] = proc(toks: var HsLexer[T]): ParseResult[Val]


proc lift*[F](state: var HsLexerState[F], flag: F) =
  state.flagStack.add flag
  inc state.flagSet[flag]

proc drop*[F](state: var HsLexerState[F], flag: F) =
  assert flag == state.flagStack.pop()
  dec state.flagSet[flag]

proc toFlag*[F](state: var HsLexerState[F], newFlag: F) =
  let flag = state.flagStack.pop()
  dec state.flagSet[flag]
  inc state.flagSet[newFlag]
  state.flagStack.add newFlag

proc topFlag*[F](state: var HsLexerState[F]): F =
  state.flagStack[^1]

proc hasFlag*[F](state: HsLexerState[F], flag: F): bool =
  state.flagSet[flag] > 0

proc newLexerState*[F](startFlag: F): HsLexerState[F] =
  result = HsLexerState[F](flagStack: @[startFlag])
  inc result.flagSet[startFlag]

proc newUnexpectedCharError*[F](
    str: var PosStr, state: var HsLexerState[F], expected: string = ""): ref UnexpectedCharError =

  result = newUnexpectedCharError(str, expected)
  result.msg.add " in state "
  result.msg.add $state.topFlag()

proc newUnexpectedTokenError*[K](
    lexer: var HsLexer[HsTok[K]],
    expected: set[K] = {},
    expectedMsg: string = ""
  ): UnexpectedTokenError =
  ## - TODO :: Support optional coloring for both displayed tokens
  ##   (via color map and line ranges) and displayed annotations.
  ## - TODO :: Support visualization mapping for tokens using
  ##   `array[K, string]` to display more human-friendly display.

  let (line, linePos) = lexer.str[].lineAround(lexer[].offset)
  result = UnexpectedTokenError(
    line: lexer[].line,
    column: lexer[].column,
    lineText: line,
    linePos: linePos,
    gotToken: $lexer[],
    expected: mapIt(expected, $it)
  )

  var buf = initRuneGrid()

  when not defined(nimdoc):
    var pos = &"{result.line}:{result.column}"

    buf[0, 0] = &"Unexpected token: expected {expected}, but got"
    buf[1, 0] = pos
    buf[1, pos.len + 1] = result.lineText
    let arrow = pos.len + 1 + linePos
    buf[2, arrow] = "^"
    buf[3, arrow] = "|"

    if expectedMsg.len > 0 and
       not(lexer[] of expected):
      buf[4, arrow] = result.gotToken
      buf[5, arrow] = expectedMsg

    elif expectedMsg.len > 0:
      buf[4, arrow] = expectedMsg

    else:
      buf[4, arrow] = result.gotToken


    result.msg = $buf


proc expectKind*[K](lex: var HsLexer[HsTok[K]], kind: set[K] | K) =
  if not(lex[] of kind):
    raise newUnexpectedTokenError(lex, asSet(kind))

func getIndent*[F](state: HsLexerState[F]): int = state.indent
func setIndent*[F](state: var HsLexerState[F], ind: int) =
  state.indent = ind

func getIndentLevels*[F](state: HsLexerState[F]): int = state.indentLevels

func clear*[F](state: var HsLexerState[F]) =
  state.flagStack = @[]
  state.indent = 0
  state.indentLevels = 0
  for val in mitems(state.flagSet):
    val = 0

proc skipIndent*[F](
    state: var HsLexerState[F], str: var PosStr): LexerIndentKind =
  if str[Newline]:
    str.next()

  if not str[{' '}]:
    if state.indent == 0:
      result = likSameIndent

    else:
      result = likDecIndent
      state.indentLevels = 0
      state.indent = 0

  else:
    str.skipWhile({' '})
    if state.indent > str.column:
      dec state.indentLevels
      result = likDecIndent

    elif state.indent < str.column:
      inc state.indentLevels
      result = likIncIndent

    else:
      result = likSameIndent

    state.indent = str.column



proc lineCol*[K](tok: HsTok[K]): LineCol {.inline.} =
  (line: tok.line, column: tok.column)

proc `==`*[K](tok: HsTok[K], other: tuple[kind: K, value: string]): bool =
  tok.kind == other.kind and tok.str == other.value

proc `==`*[K](
    tok: openarray[HsTok[K]], other: openarray[tuple[kind: K, value: string]]
  ): bool =

  result = len(tok) == len(other)
  if result:
    for idx, _ in pairs(tok):
      if tok[idx] != other[idx]:
        return false

proc add*[R, T](tree: var HsTokTree[R, T], sub: HsTokTree[R, T]) =
  tree.subnodes.add sub

proc newHTree*[R, T](
    kind: R, subnodes: seq[HsTokTree[R, T]]): HsTokTree[R, T] =
  HsTokTree[R, T](isToken: false, kind: kind, subnodes: subnodes)

proc newHTree*[R, T](kind: R, token: T): HsTokTree[R, T] =
  HsTokTree[R, T](isToken: true, token: token, kind: kind)

proc newHTree*[R, T](lexer: HsLexer[T], kind: R): HsTokTree[R, T] =
  HsTokTree[R, T](isToken: false, kind: kind)

proc newHTree*[R, T](
    lexer: HsLexer[HsTok[T]], kind: R, tok: HsTok[T]): HsTokTree[R, HsTok[T]] =
  HsTokTree[R, HsTok[T]](isToken: true, token: tok)

proc newHTree*[R, T](
    lexer: HsLexer[T], kind: R, str: string): HsTokTree[R, T] =
  HsTokTree[R, T](isToken: true, str: str)

proc wrap*[R, T](tree: sink HsTokTree[R, T], wrap: R): HsTokTree[R, T] =
  HsTokTree[R, T](isToken: false, kind: wrap, subnodes: @[tree])

proc initEof*[K](str: var PosStr, kind: K): HsTok[K] =
  HsTok[K](kind: kind, line: str.line, column: str.column, isSlice: false)

proc initTok*[K](
    str: var PosStr, kind: K, tryPop: bool = true): HsTok[K] =
  if str.ranges.len == 0:
    result = HsTok[K](
      kind: kind, isSlice: false,
      line: str.line, column: str.column)

  if str.ranges.len > 0 and tryPop:
    result.str = str.popRange()

proc initTok*[K](kind: K): HsTok[K] = HsTok[K](kind: kind)

proc initTok*[K](str: char | string, kind: K): HsTok[K] =
  HsTok[K](str: $str, kind: kind, isSlice: false)

template initTok*[K](
    posStr: PosStr, inStr: char | string, inKind: K): HsTok[K] =
  ## Create token using positional information from `posStr`.
  ##
  ## - WHY :: Template was used to avoid eager argument evaluation that
  ##   prevented `str.initTok(str.popIdent(), 'i')` use pattern -
  ##   identifier pop was evaluated first, causing `str` to change
  ##   positions
  HsTok[K](
    line: posStr.line,
    column: posStr.column,
    kind: inKind,
    offset: posStr.pos,
    str: $inStr,
    isSlice: false)

proc initTok*[K](str: PosStr, inKind: K): HsTok[K] =
  result = HsTok[K](
    kind: inKind,
    isSlice: true,
    line: str.line,
    column: str.column,
    offset: str.pos
  )

  if str.isSlice:
    result.baseStr = str.baseStr
    if str.slices.len == 1:
      result.finish = str.slices.first().finish

    else:
      raise newArgumentError(
        "Cannot create slice token from fragmented positional string -",
        "expected exactly one slice, but found ",
        str.slices.len)

template initTok*[K](
    posStr: PosStr, inStr: PosStr, inKind: K): HsTok[K] =
  var res = HsTok[K](
    kind: inKind,
    isSlice: true,
    line: posStr.line,
    column: posStr.column,
    offset: posStr.pos
  )

  let str = inStr
  if str.isSlice:
    res.baseStr = str.baseStr
    if str.slices.len == 1:
      let first = str.slices[0]
      res.line = first.line
      res.column = first.column
      res.offset = first.start
      res.finish = first.finish

    else:
      raise newArgumentError(
        "Cannot create slice token from fragmented positional string -",
        "expected exactly one slice, but found ",
        str.slices.len)

  res

func initSliceTok*[K](str: var PosStr, inKind: K): HsTok[K] =
  initTok(str, str.popSlice(), inKind)

proc initAdvanceTok*[K](
    str: var PosStr, advance: int, inKind: K,
    expected: set[char] = AllChars
  ): HsTok[K] =
  initTok(str, str.popPointSlice(
    advance = advance, expected = expected), inKind)


func strVal*[K](tok: HsTok[K]): string =
  if tok.isSlice:
    assertRef tok.baseStr
    tok.baseStr[][tok.offset .. tok.finish]

  else:
    tok.str

proc initCharTok*[Cat: enum](
    str: var PosStr,
    map: static[openarray[tuple[key: char, val: Cat]]]
  ): HsTok[Cat] =
  let ch = str.popChar()
  initTok(ch, mapChar(ch, map))

proc initPosStr*[K](tok: HsTok[K]): PosStr =
  if tok.isSlice:
    result = PosStr(
      isSlice: true,
      baseStr: tok.baseStr,
      pos: tok.offset,
      line: tok.line,
      column: tok.column,
      slices: @[PosStrSlice(
        line: tok.line,
        column: tok.column,
        start: tok.offset,
        finish: tok.finish)])

    result.slices.add tok.extra



  else:
    raise newImplementError()

func initPosStr*[K](tokens: seq[HsTok[K]]): PosStr =
  var strs: seq[PosStr]
  for tok in tokens:
    strs.add initPosStr(tok)

  return concat(strs)

func isFail*[T](parse: ParseResult[T]): bool = not parse.ok
func isOk*[T](parse: ParseResult[T]): bool = parse.ok
func initParseResult*[T](value: T): ParseResult[T] =
  ParseResult[T](ok: true, value: value)

func initParseResult*[T](value: seq[T]): ParseResult[seq[T]] =
  ParseResult[T](ok: true, value: value)


func initParseResult*[Tfrom, Tto](res: ParseResult[Tfrom]): ParseResult[Tto] =
  if res.isFail():
    ParseResult[Tto](ok: false, fail: res.fail)

  else:
    initParseResult[Tto](res.value)

func initParseResult*[T](fail: ParseFail): ParseResult[T] =
  ParseResult[T](ok: false, fail: fail)

func toHsParse*[K, Val](
    arg: proc(lexer: var HsLexer[HsTok[K]]): Val, first: set[K]
  ): HsParseCallback[HsTok[K], Val] =

  return proc(lexer: var HsLexer[HsTok[K]]): ParseResult[Val] =
    if lexer[].kind in first:
      result = initParseResult(arg(lexer))

    else:
      result = initParseResult[Val](ParseFail())




func add*[T](res: var ParseResult[seq[T]], value: seq[T] | T) =
  res.value.add value

func add*[T](res: var ParseResult[seq[T]], value: ParseResult[T]) =
  res.value.add value.value

func len*[R, T](t: HsTokTree[R, T]): int =
  if not t.isToken: result = t.subnodes.len()

iterator items*[R, T](t: HsTokTree[R, T]): HsTokTree[R, T] =
  if not t.isToken:
    for sub in items(t.subnodes):
      yield sub

iterator pairs*[R, T](t: HsTokTree[R, T]): (int, HsTokTree[R, T]) =
  if not t.isToken:
    for idx, sub in pairs(t.subnodes):
      yield (idx, sub)

func `[]`*[R, T](t: HsTokTree[R, T], idx: int): HsTokTree[R, T] =
  t.subnodes[idx]

proc get*[V](parseResult: ParseResult[V]): V =
  return parseResult.value

func `$`*[K](tok: HsTok[K]): string =
  var kindStr = $tok.kind
  kindStr = kindStr[kindStr.skipWhile(LowerAsciiLetters) .. ^1]

  var str = tok.strVal()
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

func hasNxt*[T](lex: HsLexer[T], offset: int): bool =
  lex.pos + offset < lex.tokens.len

proc finished*[T](lex: HsLexer[T]): bool =
  not hasNxt(lex, 0) and lex.str.finished()

proc `?`*[T](lex: HsLexer[T]): bool =
  lex.explicitEof or not lex.finished()


proc nextToken*[T](lex: var HSLexer[T]): bool =
  var tok: seq[T]
  while ?lex and tok.empty():
    tok = lex.cb(lex.str)

  if tok.empty():
    return false

  else:
    lex.tokens.add tok
    return true

proc fillNext*[T](lex: var HSLexer[T], chars: int) =
  while chars - (lex.tokens.len - lex.pos - 1) > 0:
    if not nextToken(lex):
      if chars - (lex.tokens.len - lex.pos - 1) > 0:
        raise newGetterError("No more tokens")


proc `[]`*[T](lex: var HSlexer[T], offset: int = 0): T =
  if not hasNxt(lex, offset):
    fillNext(lex, offset)

  result = lex.tokens[lex.pos + offset]


proc `[]`*[T](lex: var HsLexer[T], slice: Slice[int]): seq[T] =
  for i in slice:
    result.add lex[i]

proc `[]`*[K](tokens: seq[HsTok[K]], offset: int, kind: set[K]|K): bool =
  when kind is set:
    tokens[offset].kind in kind

  else:
    tokens[offset].kind == kind

proc `[]`*[T, K](lex: var HsLexer[T], kind: set[K] | K): bool =
  when kind is set: return lex[].kind in kind
  else:             return lex[].kind == kind


proc `[]`*[T, K](lex: var HsLexer[T], offset: int, kind: set[K] | K): bool =
  when kind is set: return lex[offset].kind in kind
  else:             return lex[offset].kind == kind


proc `[]`*[K](lex: var HsLexer[HsTok[K]], kind: set[K]|K): bool =
  lex[0, kind]

proc `[]`*[K](lex: var HsLexer[HsTok[K]], kind1, kind2: set[K]|K): bool =
  lex[0, kind1] and lex[1, kind2]

proc `[]`*[K](lex: var HsLexer[HsTok[K]], kind1, kind2, kind3: set[K]|K): bool =
  lex[0, kind1] and lex[1, kind2] and lex[2, kind3]

func `[]`*[K](tokens: seq[HsTok[K]], kind: set[K]|K): bool =
  tokens[0, kind]

func `[]`*[K](tokens: seq[HsTok[K]], kind1, kind2: set[K]|K): bool =
  tokens[0, kind1] and tokens[1, kind2]

func `[]`*[K](tokens: seq[HsTok[K]], kind1, kind2, kind3: set[K]|K): bool =
  tokens[0, kind1] and tokens[1, kind2] and tokens[2, kind3]

func returnTo*[T](lex: var HsLexer[T], position: int) =
  lex.pos = position

func getPosition*[T](lex: var HsLexer[T]): int = lex.pos

func next*[T](lex: var HSlexer[T], step: int = 1) =
  inc(lex.pos, step)

func advance*[T](lex: var HSlexer[T], step: int = 1)
  {.deprecated: "Alias for `next`"} =
  next(lex, step)

proc pop*[T](lex: var HsLexer[T]): T =
  result = lex[]
  lex.next()

proc pop*[K: enum](lex: var HsLexer[HsTok[K]], kind: set[K] | K): HsTok[K] =
  expectKind(lex, kind)
  result = lex[]
  lex.next()

proc popAsStr*[K](lex: var HsLexer[HsTok[K]]): PosStr =
  result = initPosStr(lex[])
  lex.next()

proc popAsStr*[K](lex: var HsLexer[HsTok[K]], kind: set[K] | K): PosStr =
  expectKind(lex, kind)
  result = initPosStr(lex[])
  lex.next()

proc initLexer*[T](
    str: PosStr, lexCb: HsLexCallback[T],
    explicitEof: bool = false): HsLexer[T] =
  HsLexer[T](str: str, cb: lexCb, explicitEof: explicitEof)

proc initLexer*[T](tokens: seq[T]): HsLexer[T] =
  HsLexer[T](tokens: tokens)

proc initLexer*[T](
    lexCb: HsLexCallback[T], explicitEof: bool = false): HsLexer[T] =
  HsLexer[T](cb: lexCb, explicitEof: explicitEof)

proc setStr*[T](lexer: var HsLexer[T], str: PosStr) =
  lexer.str = str

proc skip*[T, En](lexer: var HsLexer[T], kind: set[En] | En) =
  when not defined(nimdoc):
    if not(lexer[] of kind):
      raise newUnexpectedTokenError(lexer, asSet(kind))

    lexer.next()

proc skip*[T, En](
    lexer: var HsLexer[T], kind: set[En] | En, expected: string) =
  if not(lexer[] of kind) or lexer[].strVal() != expected:
    raise newUnexpectedTokenError(
      lexer,
      asSet(kind),
      &"Token value mismatch - wanted '{expected}', but got {lexer[].strVal()}"
    )

  lexer.next()

proc trySkip*[T, En](lexer: var HsLexer[T], kind: set[En]): bool =
  if lexer[] of kind:
    result = true
    lexer.next()


proc skip*[T, En](lexer: var HsLexer[T], kind1, kind2: set[En]) =
  lexer.skip(kind1)
  lexer.skip(kind2)

proc skip*[T, En](lexer: var HsLexer[T], kind1, kind2, kind3: set[En]) =
  lexer.skip(kind1)
  lexer.skip(kind2)
  lexer.skip(kind3)

proc skipTo*[T](lex: var HsLexer[T], chars: set[char]) =
  lex.str[].skipWhile(AllChars - chars)

proc parseIdent*[R, T](lex: var HsLexer[T], kind: R):
  HsTokTree[R, T] = HsTokTree[R, T](isToken: true, token: lex.pop())


func pushRange*[T](lex: var HsLexer[T]) =
  lex.str[].pushRange()

func popRange*[T](lex: var HsLexer[T]): string =
  lex.str[].popRange()

proc getAll*[T](lex: var HsLexer[T]): seq[T] =
  while not lex.finished():
    result.add lex.pop()

proc lexAll*[T](str: var PosStr, impl: HsLexCallback[T]): seq[T] =
  var lexer = initLexer(str, impl)
  return lexer.getAll()

proc initLexer*[T](strs: seq[PosStr], impl: HsLexCallback[T]): HsLexer[T] =
  result = HsLexer[T](cb: impl)
  var buf: seq[T]
  for str in strs:
    var str = str
    result.str = addr str
    while not result.finished():
      buf.add result.pop()

  result.tokens = buf
  result.pos = 0

proc insideBalanced*[T, K](
    lex: var HsLexer[T], openKinds, closeKinds: set[K],
    withWrap: bool = false
  ): seq[T] =

  if lex.finished():
    return

  var cnt: int

  if lex[openKinds]:
    inc cnt
    if withWrap:
      result.add lex.pop()

    else:
      lex.next()

    while cnt > 0:
      if lex[openKinds]:
        inc cnt

      elif lex[closeKinds]:
        dec cnt

      if cnt > 0 or withWrap:
        result.add lex.pop()

      else:
        lex.next()

func splitSep*[T, K](tokens: seq[T], sep: set[K]): seq[seq[T]] =
  result.add @[]
  for tok in tokens:
    if tok.kind in sep:
      result.add @[]

    else:
      result[^1].add tok

func splitKeyValue*[T, K](
    tokens: seq[T], kvDelimiter: set[K], itemSep: set[K]):
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

func foldNested*[T, K](
    tokens: seq[T],
    openKinds, closeKinds, delimiterKinds: set[K]
  ): seq[HsTokTree[T, T]] =

  proc aux(idx: var int): HsTokTree[T, T] =
    if (idx + 1 < tokens.len) and
       (tokens[idx + 1].kind in openKinds):

      result = HsTokTree[T, T](isToken: false, kind: tokens[idx])

    else:
      result = HsTokTree[T, T](isToken: true, token: tokens[idx])

    inc idx
    if tokens[idx].kind in openKinds:
      inc idx
      while idx < tokens.len:
        if tokens[idx].kind in openKinds:
          result.subnodes.add aux(idx)

        elif tokens[idx].kind in delimiterKinds:
          inc idx

        elif tokens[idx].kind in closeKinds:
          inc idx
          break

        else:
          result.subnodes.add aux(idx)

  var idx: int
  while idx < tokens.len:
    result.add aux(idx)
    if idx < tokens.len and
       tokens[idx].kind in delimiterKinds:
      inc idx

func parsePlus*[T, V](parser: HsParseCallback[T, V]):
  HsParseCallback[T, seq[V]] =

  return proc(toks: var HsLexer[T]): ParseResult[seq[V]] =
    var item = parser(toks)
    if item.isFail():
      return initParseResult[seq[T]](item.fail)

    else:
      result = initParseResult(@[item])
      var position = toks.getPosition()
      while item.isOk():
        item = parser(toks)
        if not item.isOk():
          toks.returnTo(position)

        else:
          result.value.add item


func parseDelimitedStar*[T, V, Skip](
    itemParse: HsParseCallback[T, V],
    delimiterParse: HsParseCallback[T, Skip]
  ): HsParseCallback[T, seq[V]] =

  return proc(toks: var HsLexer[T]): ParseResult[seq[V]] =
    var position = toks.getPosition()

    var item: ParseResult[V] = itemParse(toks)
    if item.isFail():
      return initParseResult[seq[V]](item.fail)

    else:
      result = initParseResult[seq[V]](@[item.get()])
      var position = toks.getPosition()
      # var delimiter = delimiterParse(toks)
      if delimiterParse(toks).isFail():
        toks.returnTo(position)

      else:
        while item.isOk():
          item = itemParse(toks)
          if not item.isOk():
            toks.returnTo(position)

          else:
            result.add item

            position = toks.getPosition()
            if delimiterParse(toks).isFail():
              toks.returnTo(position)
              break

func `^*`*[T, V](itemParse, delimiterParse: HsParseCallback[T, V]):
  HsParseCallback[T, seq[V]] =

  ## Shortcut version for [[code:parseDelimitedStar()]]

  parseDelimitedStar(itemParse, delimiterParse)

func parseToken*[T](token: T): HsParseCallback[T, T] =
  return proc(toks: var HsLexer[T]): ParseResult[T] =
    if toks[] == token:
      toks.next()
      initParseResult(toks[])

    else:
      initParseResult[T](ParseFail())


func parseTokenKind*[K](kind: K): HsParseCallback[HsTok[K], HsTok[K]] =
  return proc(toks: var HsLexer[HsTok[K]]): ParseResult[HsTok[K]] =
    if toks[].kind == kind:
      result = initParseResult(toks[])
      toks.next()

    else:
      result = initParseResult[HsTok[K]](ParseFail())


func parseLongestMatch*[T, V](parsers: seq[HsParseCallback[T, V]]):
  HsParseCallback[T, V] =

  return proc(toks: var HsLexer[T]): ParseResult[T] =
    var position = toks.getPosition()
    var matches: seq[tuple[match: ParseResult[T], endPosition: int]]
    for parser in parsers:
      toks.returnTo(position)
      let match = parser(toks)
      matches.add (match, toks.getPosition())


    sortIt(matches, it.endPosition)
    for (match, position) in matches:
      if match.ok:
        toks.returnTo(position)
        return match

    # IMPLEMENT better error reporting? This is a main source of unreadable
    # error messages from parser combinators (choice operator)
    return matches[0].match

func parseFirstMatch*[T, V](parsers: seq[HsParseCallback[T, V]]):
  HsParseCallback[T, V] =

  return proc(toks: var HsLexer[T]): ParseResult[T] =
    var position = toks.getPosition()
    var lastFail: ParseResult[T]
    for parser in parsers:
      toks.returnTo(position)
      let match = parser(toks)
      if match.ok:
        return match

      else:
        lastFail = match

    return lastFail

func strVal*[R, T](tree: HsTokTree[R, T]): string = tree.token.str


func treeRepr*[R, T](
    tree: HsTokTree[R, T],
    colored: bool = true,
    pathIndexed: bool = false,
    positionIndexed: bool = true,
    maxdepth: int = 120,
    baseStr: PosStr = PosStr(line: 0, column: 0)
  ): ColoredText =

  coloredResult()

  proc aux(n: HsTokTree[R, T], level: int, idx: seq[int]) =
    if pathIndexed:
      add idx.join("", ("[", "]")) & "    "

    elif positionIndexed:
      if level > 0:
        addIndent(level - 1)
        add to8Bit("#" & $idx[^1], 10)
        add to8Bit("/" & strutils.alignLeft($level, 2), 5)
        add " "

      else:
        add "    "

    else:
      add "  ".repeat(level)

    if level > maxdepth:
      add " ..."
      return

    let kind = $n.kind
    add toCyan(kind[kind.skipWhile({'a' .. 'z'}) .. ^1], colored)

    if n.isToken:
      let tok = $n.token.kind
      add " " & toYellow(
        tok[tok.skipWhile({'a' .. 'z'}) .. ^1], colored)

      if '\n' in n.token.str:
        var tmp = toColoredText(n.token.str)

        if tmp.len == 0:
          tmp = toColoredText("∅")

        else:
          let nlCount = replaceTailNewlines(tmp)
          # var nlCount = 0
          # while nlCount < tmp.high and
          #       tmp[tmp.high - nlCount] in {'\n'}:
          #   inc nlCount

          # tmp.setLen(tmp.len - nlCount)
          # var suffix = ""
          # if nlCount == 0 and n.token.str[^1] in {'\n'}:
          #   inc nlCount
          # for nl in 0 ..< nlCount:
          #   suffix.add toRed("⮒", colored)

          if n.token.str.count('\n') == 0:
            add " \"" & tmp.toGreen(colored) & "\""
            add to8Bit(&" ({n.token.line}:{n.token.column})", 2, 3, 3)

          else:
            add to8Bit(&" ({n.token.line}:{n.token.column})", 2, 3, 3)
            add "\n"
            add tmp.indent(idx.len * 2 + 6).toGreen(colored)



      else:
        # if n.token.str[0] in {' '} or n.token.str[^1] in {' '}:
        add " \"" & toGreen(n.token.str, colored) & "\""

        # else:
        #   add " " & toGreen(n.token.str, colored)

        add to8Bit(&" ({n.token.line}:{n.token.column})", 2, 3, 3)


    else:
      if n.len > 0:
        add "\n"

      for newIdx, subn in n:
        aux(subn, level + 1, idx & newIdx)
        if level + 1 > maxDepth:
          break

        if newIdx < n.len - 1:
          add "\n"

  aux(tree, 0, @[])

proc toColored*[K](
    toks: seq[HsTok[K]],
    colorMap: array[K, PrintStyling] = default(array[K, PrintStyling]),
    wrapLine: int = 80
  ): ColoredRuneGrid =

  # const colorMap = toMapArray(colorMap)

  var
    line = toks[0].line
    row = 0
    column = 0

  for tok in toks:
    if tok.line > line:
      inc row
      line = tok.line
      column = 0

    let
      str = tok.str
      kind = dropLowerPrefix($tok.kind)

    if colorMap[tok.kind].isValid():
      result[row * 2, column] = toColored(str, colorMap[tok.kind])

    else:
      result[row * 2, column] = toColored(str, initStyle(fgRed))


    result[row * 2 + 1, column] = kind
    column += max(len(str), len(kind)) + 1
    if column > wrapLine:
      inc row
      column = 4
      result[row * 2, 0] = "..."
