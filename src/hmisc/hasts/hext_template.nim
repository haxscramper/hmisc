## Extendable templating framework

#[

- Pass information about syntactical context in to writer. I want to at
  least access indentation information, but it might be possible to expand
  this into things like 'surrounding blocks' or 'file names' later on.

]#

import
  std/[tables, options, macros, streams, strformat]

import
  ../core/all,
  ../types/colorstring,
  ../macros/matching,
  ../algo/[hlex_base, hparse_base, hparse_common, halgorithm]

export hparse_base, colorstring

type
  HxTokenKind* = enum
    htoContent
    htoStmtOpen, htoStmtOpenStrip
    htoStmtClose, htoStmtCloseStrip
    htoExprOpen
    htoExprClose
    htoCommentOpen
    htoCommentClose

    htoForKwd, htoEndForKwd
    htoWhileKwd, htoEndWhileKwd
    htoIfKwd, htoElseKwd, htoElifKwd, htoEndIfKwd
    htoCaseKwd, htoOfKwd, htoEndCaseKwd,
    htoRawKwd, htoEndRawKwd
    htoMacroKwd, htoEndMacroKwd,

    htoExtendsKwd,
    htoEndKwd,
    htoInKwd,

    htoBlockKwd, htoEndBlockKwd

    htoIdent

    htoIntLit
    htoStrLit
    htoFloatLit
    htoBoolLit

    htoDot
    htoLPar
    htoRPar
    htoLCurly
    htoRCurly
    htoLBrace
    htoRBrace
    htoComma
    htoColon
    htoSemicolon

    htoMacro
    htoEndMacro
    htoSetKwd, htoEndSetKwd,
    htoEof

const
  htoEndKinds* = {
    htoEndForKwd,
    htoEndKwd,
    htoEndCaseKwd,
    htoEndIfKwd,
    htoEndWhileKwd,
    htoEndMacroKwd,
    htoEndBlockKwd
  }

  htoAllKinds* = { low(HxTokenKind) .. high(HxTokenKInd) }
  htoKwdKinds* = { htoForKwd .. htoEndBlockKwd }
  htoDelimKinds* = { htoStmtOpen .. htoCommentClose }
  htoColorMap* = toMapArray({
    htoAllKinds: initStyleBg(5, 0, 0),
    htoKwdKinds: initStyle(fgGreen),
    htoDelimKinds: initStyle(fgCyan),
    { htoIdent }: initStyleFg(3, 3, 3),
    { htoContent }: initStyle(fgYellow)
  })

type
  HxNodeKind* = enum
    hnoContent
    hnoStmtList
    hnoExpr
    hnoDotExpr
    hnoStmt
    hnoFor
    hnoWhile
    hnoIf, hnoElifBranch
    hnoCase, hnoOfBranch
    hnoMacro
    hnoSet, hnoSetBlock
    hnoIdent
    hnoSym

    hnoCall

  HxTok* = HsTok[HxTokenKind]
  HxLexer* = HsLexer[HxTok]
  HxTree* = HsTokTree[HxNodeKind, HxTok]

  HxLexerState = enum
    elsContent ## Default parsing state
    elsPossibleRaw ## Potential start of `raw` statement
    elsRawText ## Inside of the raw block
    elsPossibleEndraw ## Potential start of `endraw` statement
    elsExpr ## Expression content

  HxNode* = ref HxNodeObj
  HxNodeObj* = object
    case kind*: HxNodeKind
      else:
        discard


  HxTypeKind = enum
    htyString
    htyInt
    htyFloat
    htyBool
    htyRecord

  NEType = object
    case kind*: HxTypeKind
      of htyRecord:
        recFields: Table[string, NEType]

      else:
        discard


proc newHextLexer*(): HxLexer =
  var state = newLexerState(elsContent)
  proc heLex(str: var PosStr): seq[HxTok] =
    if str.finished():
      result.add initEof(str, htoEof)

    else:
      case state.topFlag()
        of elsExpr, elsPossibleRaw, elsPossibleEndRaw:
          case str[]:
            of '-':
              result.add (
                if str[+1, "%}"]:
                  state.toFlag(elsContent)
                  initTok(str, str.popNext(3), htoStmtCloseStrip)
                else: initTok(str, str.popNext(1), htoIdent)
              )

            of '}':
              result.add (
                if str[+1, '}']:
                  state.toFlag(elsContent)
                  initTok(str, str.popNext(2), htoExprClose)
                else: initTok(str, str.popChar(), htoRCurly)
              )

            of '%':
              result.add (
                if str[+1, '}']:
                  state.toFlag(elsContent)
                  initTok(str, str.popNext(2), htoStmtClose)
                else: initTok(str, str.popChar(), htoIdent)
              )


            # of '-', '}', '%', '#':

            of IdentStartChars:
              result.add initTok(str, str.popIdent(), htoIdent)
              let kwd = case result.first().strVal():
                of "block": htoBlockKwd
                of "endblock": htoEndBlockKwd
                of "raw": htoRawKwd
                of "endraw": htoEndRawKwd
                of "extends": htoExtendsKwd
                of "for": htoForKwd
                of "endfor": htoEndForKwd
                of "if": htoIfKwd
                of "endif": htoEndIfKwd
                of "in": htoInKwd
                of "end": htoEndKwd
                of "else": htoElseKwd
                else: htoIdent

              result.first().kind = kwd

            of HorizontalSpace:
              str.skipWhile(HorizontalSpace)
              return heLex(str)

            of '.':
              result.add initCharTok(str, {
                '.': htoDot
              })

            else:
              raise newUnexpectedCharError(str, state)

        of elsContent, elsRawText:
          case str[]:
            of '{':
              if str[+1, "%-"]:
                result.add initTok(str, str.popNext(3), htoStmtOpenStrip)
                state.toFlag(elsExpr)

              elif str[+1, '%']:
                result.add initTok(str, str.popNext(2), htoStmtOpen)
                state.toFlag(elsExpr)

              elif str[+1, '{']:
                result.add initTok(str, str.popNext(2), htoExprOpen)
                state.toFlag(elsExpr)

              elif str[+1, '#']:
                result.add initTok(str, str.popNext(2), htoCommentOpen)
                state.toFlag(elsExpr)

              else:
                result.add initTok(str, str.popUntil('{'), htoContent)

            else:
              result.add initTok(str, str.popUntil('{'), htoContent)


  initLexer(heLex, some initTok(htoEof))

proc lexHext*(str: string): seq[HxTok] =
  var str = initPosStr(str)
  var lexer = newHextLexer()
  lexer.setStr(str)
  return lexer.getAll()


const
  exprFirst = { htoIdent }
  hxStmtFirst = { htoStmtOpen, htoStmtOpenStrip }
  hxStmtLast = { htoStmtClose, htoStmtCloseStrip }
  hxExprFirst = { htoExprOpen }
  hxExprLast = { htoExprClose }




const
  hextExprContext = initCommonExprContext[HxNodeKind, HxTokenKind]({
    htoDot: etkDot,
    htoIdent: etkIdentName,
    htoLBrace: etkLBRack,
    htoStmtClose, htoExprClose: etkExprClose
  }, {
    enkCall: hnoCall,
    enkIdent: hnoIdent,
    enkDotExpr: hnoDotExpr
  }, commonPrecLevel[HXNodeKind, HxTokenKind])



proc parseExpr(lexer: var HxLexer): HxTree =
  result = parseCommonExpr(lexer, hextExprContext)


proc parseHxStmtList(lexer: var HxLexer): HxTree

proc parseForStmt(lexer: var HxLexer): HxTree =
  lexer.skip(hxStmtFirst)
  result = lexer.newHTree(hnoFor)
  lexer.skip(htoForKwd)

  result.add lexer.parseIdent(hnoIdent)

  lexer.skip(htoInKwd)
  result.add parseExpr(lexer)
  lexer.skip(hxStmtLast)
  result.add parseHxStmtList(lexer)
  if lexer[hxStmtFirst, {htoElseKwd}]:
    lexer.skip(hxStmtFirst)
    lexer.skip({htoElseKwd})
    lexer.skip(hxStmtLast)
    result.add parseHxStmtList(lexer)

  lexer.skip(hxStmtFirst, {htoEndKwd, htoEndForKwd}, hxStmtLast)


proc parseIfStmt(lexer: var HxLexer): HxTree =
  lexer.skip(htoIfKwd)

proc parseCaseStmt(lexer: var HxLexer): HxTree =
  lexer.skip(htoCaseKwd)

proc parseSetStmt(lexer: var HxLexer): HxTree =
  lexer.skip(htoSetKwd)

proc parseHxExpr(lexer: var HxLexer): HxTree =
  lexer.skip(hxExprFirst)
  result = newHTree(hnoExpr, @[parseExpr(lexer)])
  lexer.skip(hxExprLast)

proc parseHxStmtList(lexer: var HxLexer): HxTree =
  result = lexer.newHTree(hnoStmtList)
  while lexer[].kind != htoEof:
    case lexer[].kind:
      of htoStmtOpen, htoStmtOpenStrip:
        case lexer[+1].kind:
          of htoForKwd: result.add parseForStmt(lexer).wrap(hnoStmt)
          of htoIfKwd: result.add parseIfStmt(lexer).wrap(hnoStmt)
          of htoCaseKwd: result.add parseCaseStmt(lexer).wrap(hnoStmt)
          of htoSetKwd: result.add parseSetStmt(lexer).wrap(hnoStmt)
          of htoEndKinds, htoElseKwd: break
          else:
            lexer.next()
            raise newUnexpectedTokenError(lexer)

      of htoContent:
        result.add newHtree(hnoContent, lexer.pop())

      of htoExprOpen:
        result.add parseHxExpr(lexer)

      else:
        raise newImplementKindError(lexer[])


proc parseHext*(lexer: var HxLexer): HxTree =
  parseHxStmtList(lexer)

proc parseHext*(str: string): HxTree =
  var lexer = newHextLexer()
  var str = initPosStr(str)
  lexer.setStr(str)
  return parseHext(lexer)

const hextMinTypeId* = 127

type
  HextValueKind* = enum
    hvkInt
    hvkStr
    hvkFloat
    hvkBool
    hvkRecord
    hvkSeq
    hvkRefBox

  HextOpKind* = enum
    hopForLoop
    hopStmtList
    hopLoadIdentFromCtx


  HextBaseType = int|string|float|bool
  HextTypeIdRange* = range[0 .. high(int)]
  HextUserTypeIdRange* = range[hextMinTypeId .. high(int)]

  RefBox = object
    refv: pointer
    typeid: HextUserTypeIdRange

  HextValue*[T] = object
    case kind*: HextValueKind
      of hvkInt:
        intVal*: int

      of hvkStr:
        strVal*: string

      of hvkFloat:
        floatVal*: float

      of hvkBool:
        boolVal*: bool

      of hvkRefBox:
        refBox*: int

      of hvkRecord:
        recordTypeId: HextUserTypeIdRange
        recordVal*: T

      of hvkSeq:
        seqVal*: seq[HextValue[T]]

  HextProc*[T, UC] = proc(
    args: seq[HextValue[T]], ctx: UC): Option[HextValue[T]]

  HextProcMap*[T, UC] = object
    procs: Table[string, HextProc[T, UC]]

  HextAstCtx*[T, UC] = ref object
    uc: UC
    procs: HextProcMap[T, UC]
    scope: seq[Table[string, HextValue[T]]]


func `[]=`*[T, UC](map: var HextProcMap[T, UC], name: string, val: HextProc[T, UC]) =
    map.procs[name] = val

proc unboxValue*[T](val: HextValue[T], idx: static[int]): auto =
  when idx == hvkInt.int:
    return val.intVal

  elif idx == hvkStr.int:
    return val.strVal

  elif idx == hvkFloat.int:
    return val.floatVal

  elif idx == hvkSeq.int:
    return val.seqVal

  elif hextMinTypeId <= idx:
    return unboxValue(next.recordVal, idx - hexMinTypeId)


proc boxIdOf*[T](target: typedesc[HextValue[T]], valType: typedesc[HextBaseType]): int =
  when valType is int:
    return hvkInt.int

  elif valType is string:
    return hvkStr.int

  elif valType is float:
    return hvkFloat.int

  elif valType is bool:
    return hvkBool.int

proc unboxValue*[T, Out](val: HextValue[T], target: typedesc[Out]): Out =
  const id = boxIdOf(HextValue[T], typeof(target))
  unboxValue(val, id)

func hextVal*[T](arg: int): HextValue[T] =
  HextValue[T](kind: hvkInt, intVal: arg)

func hextVal*[T](arg: string): HextValue[T] =
  HextValue[T](kind: hvkStr, strVal: arg)

func hextVal*[T](arg: bool): HextValue[T] =
  HextValue[T](kind: hvkBool, boolVal: arg)

func hextVal*[T](arg: float): HextValue[T] =
  HextValue[T](kind: hvkFloat, floatVal: arg)

proc boxValue*[T](
    target: typedesc[HextValue[T]], value: HextBaseType): HextValue[T] =

  hextVal[T](value)

proc boxValue*[T, V](
    target: typedesc[HextValue[T]], value: seq[V]): HextValue[T] =
  result = HextValue[T](kind: hvkSeq)
  for item in value:
    result.seqVal.add boxValue(HextValue[T], item)

proc boxValue*[T](
    target: typedesc[HextValue[T]], value: T, boxId: HextTypeIdRange
  ): HextValue[T] =

  result = HextValue[T](
    kind: hvkRecord, recordVal: value,
    recordTypeId: boxId + hextMinTypeId)


proc boxedType*[T, UC](map: HextProcMap[T, UC]): T = discard
proc contextType*[T, UC](map: HextProcMap[T, UC]): UC = discard

iterator items*[T](val: HextValue[T]): HextValue[T] =
  mixin boxedItems
  if val.kind == hvkSeq:
    for item in items(val.seqVal):
      yield item

  elif val.kind == hvkRecord:
    for item in boxedItems(HextValue[T], val.recordVal):
      yield item

proc getStrVal(nn: NimNode): string =
  case nn.kind:
    of nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym:
      result = nn.strVal()

    of nnkAccQuoted:
      for sub in nn:
        result.add sub.strVal()

    else:
      raiseImplementError("")

macro wrap*[T, UC](map: HextProcMap[T, UC], sig, name: untyped): untyped =
  let
    returnType = sig[0][0]
    vmBoxType = genSym(nskType, "BoxType")
    mvCtxType = genSym(nskType, "UCType")
    nameLit = newLit(name.getStrVal())
    hasReturn = returnType.kind != nnkEmpty

  var argsCall = newCall(name)
  var idx = 0
  for arg in sig[0][1..^1]:
    for name in arg[0..^3]:
      argsCall.add newCall(
        "unboxValue",
        nnkBracketExpr.newTree(ident"args", newLit(idx)),
        arg[^2]
      )
      inc idx

  let setResult =
    if hasReturn:
      quote do:
        let res = boxValue(HextValue[`vmBoxType`], `argsCall`)
        return some(res)

    else:
      quote do:
        boxValue(HextValue[`vmBoxType`], `argsCall`)


  result = quote do:
    block:
      type
        `vmBoxType` = typeof boxedType(map)
      map[`nameLit`] = (
        proc(
          args {.inject.}: seq[HextValue[`vmBoxType`]],
          ctx {.inject.}: typeof(contextType(map))
          ): Option[HextValue[`vmBoxType`]] =
            `setResult`
      )

var map: HextProcMap[int, int]
wrap(map, proc(a, b: int): float, `+`)

proc pushScope*[T, UC](ctx: var HextAstCtx[T, UC]) =
  ctx.scope.add initTable[string, HextValue[T]]()

proc popScope*[T, UC](ctx: var HextAstCtx[T, UC]) =
  discard ctx.scope.pop

proc `[]=`*[T, UC](ctx: var HextAstCtx[T, UC], varname: string, value: HextValue[T]) =
  ctx.scope[^1][varname] = value

proc `[]=`*[T, UC, In](ctx: var HextAstCtx[T, UC], varname: string, value: In) =
  ctx.scope[^1][varname] = boxValue(HextValue[T], value)

proc `[]`*[T, UC](ctx: var HextAstCtx[T, UC], varname: string): HextValue[T] =
  for scope in ctx.scope:
    if varname in scope:
      return scope[varname]

  raise newArgumentError(
    &"Cannot get var '{varname}' from context")


proc evalAst*[T, V, UC](
    ast: T, op: HextOpKind, ctx: var HextAstCtx[V, UC]): HextValue[V] =
  mixin evalAst
  case op:
    of hopForLoop:
      let expr = evalAst(ast[1], ctx)
      ctx.pushScope()
      for value in items(expr):
        ctx[ast[0].strVal] = value
        discard evalAst(ast[2], ctx)

      ctx.popScope()

    of hopStmtList:
      for node in items(ast):
        discard evalAst(node, ctx)


    of hopLoadIdentFromCtx:
      result = ctx[ast.strVal]


proc newHextAstCtx*[V, UC](
    user: UC, initValues: openarray[(string, HextValue[V])] = @[]): HextAstCtx[V, UC] =
  result = HextAstCtx[V, UC]()
  result.pushScope()
  for (key, val) in initValues:
    result[key] = val

  result.uc = user

type
  HextWriteCtx = ref object
    stream: Stream


const
  opPairs = {
    hnoStmtList, hnoStmt: hopStmtList,
    hnoIdent: hopLoadIdentFromCtx
  }

  opMap = toMapArray(opPairs)
  mappedOp = toKeySet(opPairs)


proc write*[T](stream: Stream, value: HextValue[T]) =
  case value.kind:
    of hvkStr: stream.write value.strVal
    of hvkInt: stream.write $value.intVal
    of hvkFloat: stream.write $value.floatVal
    of hvkBool: stream.write $value.boolVal
    of hvkRefBox: stream.write "<ref>"
    of hvkSeq:
      for item in value.seqVal:
        stream.write item

    of hvkRecord:
      stream.write value.recordVal


proc evalAst*[V, UC](node: HxTree, ctx: var HextAstCtx[V, UC]): HextValue[V] =
  mixin getField
  case node.kind:
    of hnoContent:
      ctx.uc.write(node.strVal())

    of mappedOp:
      result = evalAst(node, opMap[node.kind], ctx)

    of hnoExpr:
      ctx.uc.write(evalAst(node[0], ctx))

    of hnoFor:
      let expr = evalAst(node[1], ctx)
      ctx.pushScope()
      var cnt = 0
      for value in items(expr):
        ctx[node[0].strVal] = value
        discard evalAst(node[2], ctx)
        inc cnt

      if cnt == 0 and len(node) == 4:
        discard evalAst(node[3], ctx)

      ctx.popScope()

    of hnoDotExpr:
      let head = ctx[node[0].strVal]
      when V is object or V is tuple:
        return getField(HextValue[V], head.recordVal, node[1].strVal)

      else:
        raise newImplementError()

    else:
      raise newImplementKindError(node)

proc evalHext*[T, Uc](
    node: HxTree, stream: Uc,
    initValues: openarray[(string, HextValue[T])] = @[]) =
  var ctx = newHextAstCtx[T, Uc](stream, initValues)
  discard evalAst(node, ctx)


proc evalHext*[T, Uc](
    text: string, stream: Uc,
    initValues: openarray[(string, HextValue[T])] = @[]) =
  let node = parseHext(text)
  var ctx = newHextAstCtx[T, Uc](stream, initValues)
  discard evalAst(node, ctx)
