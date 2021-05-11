import
  ../base_errors, ../hdebug_misc,
  ../algo/[hlex_base, hparse_base]

import std/[options]


type
  MustacheTokenKind = enum
    mtkCurlyOpen
    mtkCurlyClose
    mtkSectionStart
    mtkSectionEnd
    mtkDot
    mtkPartial
    mtkIdent
    mtkInvert
    mtkChangeWrapper
    mtkContent
    mtkEof

  MustacheAstKind = enum
    makStmtList
    makSection
    makContent
    makPartial
    makWrapSwitch
    makIdent
    makGetExpr

  MTok = HsTok[MustacheTokenKind]
  MLexer = HsLexer[MTok]
  MTree = HsTokTree[MustacheAstKind, MTok]
  MState = HsLexerState[bool]


proc newMustacheLexer*(): MLexer =
  var state = newLexerState(false)
  proc lexerImpl(str: var PosStr): Option[MTok] =
    if str.finished():
      result = some initEof(str, mtkEof)

    elif state.topFlag():
      case str[]:
        of '#', '>', '/', '=', '^', '.':
          result = some initCharTok(str, {
            '#': mtkSectionStart,
            '/': mtkSectionEnd,
            '^': mtkInvert,
            '=': mtkChangeWrapper,
            '>': mtkPartial,
            '.': mtkDot
          })

        of '}':
          str.assertAhead("}}")
          result = some initTok(mtkCurlyClose)
          str.advance(2)
          state.toFlag(false)

        of ' ':
          str.skipWhile({' '})
          result = str.lexerImpl()

        of IdentChars:
          result = some initTok(str, str.popIdent(), mtkIdent)

        else:
          raiseUnexpectedChar(str)

    else:
      case str[]:
        of '{':
          if str[+1, '{']:
            result = some initTok(mtkCurlyOpen)
            str.advance(2)
            state.toFlag(true)

          else:
            result = some initTok(str, str.popUntil('{'), mtkContent)

        else:
          result = some initTok(str, str.popUntil('{'), mtkContent)

  return initLexer(lexerImpl, true)

proc parseGetExpr(lexer: var MLexer): MTree =
  lexer.advance()
  result = lexer.newHTree(makGetExpr)
  if lexer[].kind in {mtkSectionStart, mtkSectionEnd}:
    lexer.advance()

  while lexer[].kind in {mtkIdent, mtkDot}:
    if lexer[].kind == mtkIdent:
      result.add newHTree(makIdent, lexer.pop())

    else:
      lexer.advance()

  lexer.advance()


proc parseStmtList(lexer: var MLexer): MTree =
  result = lexer.newHTree(makStmtList)
  while lexer[].kind != mtkEof:
    case lexer[].kind:
      of mtkContent:
        result.add newHTree(makContent, lexer.pop())

      of mtkCurlyOpen:
        case lexer[+1].kind:
          of mtkSectionStart:
            let
              start = parseGetExpr(lexer)
              section = parseStmtList(lexer)
              finish = parseGetExpr(lexer)

            result.add newHTree(makSection, @[start[0], section])

          of mtkIdent:
            result.add parseGetExpr(lexer)

          of mtkChangeWrapper:
            raiseImplementError("")

          of mtkPartial:
            lexer.advance()
            result.add newHTree(makPartial, lexer.pop())
            lexer.advance()

          of mtkSectionEnd:
            return

          else:
            raiseImplementKindError(lexer[+1])

      else:
        raiseImplementKindError(lexer[])

proc mustacheParse*(str: string): MTree =
  echov "123123"
  var str = initPosStr(str)
  var lexer = newMustacheLexer()
  lexer.setStr(str)
  return parseStmtList(lexer)
  # echov lexer.str[].finished()
  # let tokens = lexer.getAll()
  # echov tokens


when isMainModule:
  startHax()
  let tree = mustacheParse("""
<h2>Names</h2>
{{#names}}
  <strong>{{name}}</strong>
{{/names}}
""")

  echo treeRepr(tree)

  echo "ok"
