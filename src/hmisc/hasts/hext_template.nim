## Extendable templating framework

import
  std/[tables, options],
  ../base_errors, ../hdebug_misc,
  ../algo/[hlex_base, hparse_base, halgorithm],
  ../types/colorstring

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

    htoDot,
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
    hnoFor
    hnoWhile
    hnoIf, hnoElifBranch
    hnoCase, hnoOfBranch
    hnoMacro
    hnoSet, hnoSetBlock
    hnoIdent
    hnoSym

  HxTok* = HsTok[HxTokenKind]
  HxLexer* = HsLexer[HxTok]
  HxTree* = HsTokTree[HxNodeKind, HxTok]

  HxLexerState = enum
    elsContent ## Default parsing state
    elsPossibleRaw ## Potential start of `raw` statement
    elsRawText ## Inside of the raw block
    elsPossibleEndraw ## Potential start of `endraw` statement
    elsLogic ## Expression content

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
  proc heLex(str: var PosStr): Option[HxTok] =
    if str.finished():
      result = some initEof(str, htoEof)

    else:
      case state.topFlag()
        of elsLogic, elsPossibleRaw, elsPossibleEndRaw:
          case str[]:
            of '-':
              result = some (
                if str[+1, "%}"]:
                  state.toFlag(elsContent)
                  initTok(str, str.popNext(3), htoStmtCloseStrip)
                else: initTok(str, str.popNext(1), htoIdent)
              )

            of '}':
              result = some (
                if str[+1, '}']:
                  state.toFlag(elsContent)
                  initTok(str, str.popNext(2), htoExprClose)
                else: initTok(str, str.popChar(), htoRCurly)
              )

            of '%':
              result = some (
                if str[+1, '}']:
                  state.toFlag(elsContent)
                  initTok(str, str.popNext(2), htoStmtClose)
                else: initTok(str, str.popChar(), htoIdent)
              )


            # of '-', '}', '%', '#':

            of IdentStartChars:
              result = some initTok(str, str.popIdent(), htoIdent)
              let kwd = case result.get().strVal():
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

              result.get().kind = kwd

            of HorizontalSpace:
              str.skipWhile(HorizontalSpace)
              return heLex(str)

            else:
              str.raiseUnexpectedChar()

        of elsContent, elsRawText:
          case str[]:
            of '{':
              if str[+1, "%-"]:
                result = some initTok(str, str.popNext(3), htoStmtOpenStrip)
                state.toFlag(elsLogic)

              elif str[+1, '%']:
                result = some initTok(str, str.popNext(2), htoStmtOpen)
                state.toFlag(elsLogic)

              elif str[+1, '{']:
                result = some initTok(str, str.popNext(2), htoExprOpen)
                state.toFlag(elsLogic)

              elif str[+1, '#']:
                result = some initTok(str, str.popNext(2), htoCommentOpen)
                state.toFlag(elsLogic)

              else:
                result = some initTok(str, str.popUntil('{'), htoContent)

            else:
              result = some initTok(str, str.popUntil('{'), htoContent)


  initLexer(heLex, true)

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





proc parseExpr(lexer: var HxLexer): HxTree =
  case lexer[].kind:
    of htoIdent:
      result = newHTree(hnoIdent, lexer.pop())

    else:
      raiseImplementKindError(lexer[])


proc parseHxStmtList(lexer: var HxLexer): HxTree

proc parseForStmt(lexer: var HxLexer): HxTree =
  lexer.skip(hxStmtFirst)
  result = lexer.newHTree(hnoFor)
  lexer.skip(htoForKwd)
  let vars = parseDelimitedStar(
    toHsParse[HxTokenKind, HxTree](parseExpr, exprFirst),
    parseTokenKind(htoComma)
  )(lexer)

  result.add vars.value[0]


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
  result = parseExpr(lexer)
  lexer.skip(hxExprLast)

proc parseHxStmtList(lexer: var HxLexer): HxTree =
  result = lexer.newHTree(hnoStmtList)
  while lexer[].kind != htoEof:
    case lexer[].kind:
      of htoStmtOpen, htoStmtOpenStrip:
        case lexer[+1].kind:
          of htoForKwd: result.add parseForStmt(lexer)
          of htoIfKwd: result.add parseIfStmt(lexer)
          of htoCaseKwd: result.add parseCaseStmt(lexer)
          of htoSetKwd: result.add parseSetStmt(lexer)
          of htoEndKinds, htoElseKwd: break
          else:
            lexer.advance()
            raise unexpectedTokenError(lexer)

      of htoContent:
        result.add newHtree(hnoContent, lexer.pop())

      of htoExprOpen:
        result.add parseHxExpr(lexer)

      else:
        raiseImplementKindError(lexer[])


proc parseHext*(lexer: var HxLexer): HxTree =
  parseHxStmtList(lexer)

proc parseHext*(str: string): HxTree =
  var lexer = newHextLexer()
  var str = initPosStr(str)
  lexer.setStr(str)
  return parseHext(lexer)
