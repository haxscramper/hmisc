import
  ./hparse_base,
  ../helpers, ../base_errors, ../hdebug_misc

type
  ExprTokKind* = enum
    etkLBrack
    etkRBrack
    etkLPar
    etkRPar
    etkIdentName
    etkIdentOp
    etkDot
    etkComma
    etkSemicolon
    etkFloat
    etkIntLit
    etkStrLit
    etkFloatLit
    etkBoolLit
    etkCharLit

    etkExprClose

  ExprNodeKind* = enum
    enkCall
    enkBracket
    enkBracketExpr
    enkDotExpr
    enkInfix
    enkPrefix
    enkTuple
    enkIdent

    enkIntLit
    enkStrLit
    enkFloatLit
    enkCharLit
    enkBoolLit

  CommonParseContext*[N, T] = object
    tokenMap: array[T, ExprTokKind]
    hasTokenMapping: set[T]

    revTokenMap: array[ExprTokKind, T]

    treeMap: array[ExprNodeKind, N]
    hasTreeMapping: set[ExprNodeKind]

    precLevel: proc(tok: HsTokTree[N, HsTok[T]]): int


func commonPrecLevel*[N, T](tok: HsTokTree[N, HsTok[T]]): int =
  discard

func initCommonExprContext*[N, T](
    tokenMap: openarray[(T, ExprTokKind)],
    treeMap: openarray[(ExprNodeKind, N)],
    precLevel: proc(tok: HsTokTree[N, HsTok[T]]): int
  ): CommonParseContext[N, T] =


  CommonParseContext[N, T](
    tokenMap: toMapArray(tokenMap),
    hasTokenMapping: toKeySet(tokenMap),
    revTokenMap: toRevMapArray(tokenMap),
    treeMap: toMapArray(treeMap),
    hasTreeMapping: toKeySet(treeMap),
    precLevel: precLevel
  )


template `[]`[N, T](ctx: CommonParseContext[N, T], tok: ExprTokKind): T =
  ctx.revTokenMap[tok]

template `[]`[N, T](ctx: CommonParseContext[N, T], tok: T): ExprTokKind =
  assert tok in ctx.hasTokenMapping, $tok & " is not mapped to any token"
  ctx.tokenMap[tok]

template `[]`[N, T](ctx: CommonParseContext[N, T], tree: ExprNodeKind): N =
  assert tree in ctx.hasTreeMapping, $tree & " is not mapped to any tree"
  ctx.treeMap[tree]

proc map[N, T](ctx: CommonParseContext[N, T], toks: set[ExprTokKind]): set[T] =
  for tok in toks:
    result.incl ctx[tok]


proc parseCommonCall[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]
  ): HsTokTree[N, HsTok[T]]

proc parseCommonBrack[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]
  ): HsTokTree[N, HsTok[T]]

proc parseCommonExpr*[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]
  ): HsTokTree[N, HsTok[T]]

proc getCommonInfix[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]
  ): seq[tuple[tree: HsTokTree[N, HsTok[T]], isOp: bool]] =

  let parTokens = ctx.map({etkLPar, etkLBrack})
  let inPar =  lex[].kind in parTokens
  var cnt = 0

  if inPar:
    inc cnt
    lex.skip(parTokens)

  while (
    if inPar: cnt > 0
    else: not (ctx[lex[].kind] in {etkSemicolon, etkComma, etkRPar})
  ):
    case ctx[lex[].kind]:
      of etkIntLit:
        # FIXME any expression can be used as head for parser call? At
        # least strings.
        result.add((newHTree(ctx[enkIntLit], lex.pop()), false))

      of etkIdentName:
        case ctx[lex[+1].kind]:
          of etkLPar:
            result.add((parseCommonCall(lex, ctx), false))

          of etkLBrack:
            result.add((parseCommonBrack(lex, ctx), false))

          else:
            result.add((newHTree(ctx[enkIdent], lex.pop()), false))


      of etkIdentOp:
        result.add((newHTree(ctx[enkIdent], lex.pop()), true))

      of etkRPar, etkRBrack:
        if cnt == 0:
          break

        else:
          dec cnt
          lex.advance()

      of etkComma, etkExprClose:
        break

      of etkLPar:
        if lex[+1].kind == ctx[etkRPar]:
          lex.advance(2)

        else:
          result.add((parseCommonExpr(lex, ctx), false))

      of etkLBrack:
        result.add((parseCommonBrack(lex, ctx), false))

      of etkDot:
        let head = result.pop()[0]
        lex.advance()
        let body = parseCommonExpr(lex, ctx)
        result.add((newHTree(ctx[enkDotExpr], @[head, body]), false))

      else:
        raise newImplementKindError(ctx[lex[].kind])

proc foldCommonInfix[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]): HsTokTree[N, HsTok[T]] =

  proc foldExprAux(
      tokens: seq[(HsTokTree[N, HsTok[T]], bool)],
      pos: var int, prec: int = 0
    ): HsTokTree[N, HsTok[T]] =

    result = tokens[pos][0]
    inc(pos)
    while pos < tokens.len and prec < ctx.precLevel(tokens[pos][0]):
      if pos >= tokens.len:
        break

      let token = tokens[pos][0]
      inc(pos)
      if tokens[pos][1]:
        result = newHTree(
          ctx.treeMap[enkInfix], @[
            result, token,
            foldExprAux(tokens, pos, ctx.precLevel(token))
        ])


  var pos: int = 0
  return getCommonInfix(lex, ctx).foldExprAux(pos)

proc parseCommonCall[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]): HsTokTree[N, HsTok[T]] =
  result = newHTree(ctx[enkCall], @[newHTree(ctx[enkIdent], lex.pop())])
  lex.skip(ctx[etkLPar])
  while not lex[ctx[etkRPar]]:
    result.add foldCommonInfix(lex, ctx)
    if lex[ctx[etkComma]]:
      lex.advance()

  lex.skip(ctx[etkRPar])


proc parseCommonBrack[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]): HsTokTree[N, HsTok[T]] =

  if ctx[lex[].kind] == etkLBrack:
    result = lex.newHTree(ctx[enkBracket])

  else:
    result = lex.newHTree(ctx[enkBracketExpr])
    result.add lex.newHTree(ctx[enkIdent], "[]")
    result.add lex.newHTree(ctx[enkIdent], lex.pop(ctx[etkIdentName]))

  lex.skip(ctx[etkLBrack])
  while not lex[ctx[etkRBrack]]:
    result.add parseCommonExpr(lex, ctx)
    if ctx[lex[].kind] == etkComma:
      lex.advance()

  lex.skip(ctx[etkRBrack])


proc parseCommonExpr*[N, T](
    lex: var HsLexer[HsTok[T]], ctx: CommonParseContext[N, T]): HsTokTree[N, HsTok[T]] =

  case ctx[lex[].kind]:
    of etkLBrack:
      result = parseCommonBrack(lex, ctx)

    of etkLPar, etkIntLit, etkIdentName, etkStrLit:
      result = foldCommonInfix(lex, ctx)

    else:
      raise newImplementKindError(lex[].kind)
