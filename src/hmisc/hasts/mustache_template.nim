import
  ../core/all,
  ../algo/[hlex_base, hparse_base]

import std/[options, streams, tables]
export hparse_base

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
  proc lexerImpl(str: var PosStr): seq[MTok] =
    if str.finished():
      result.add initEof(str, mtkEof)

    elif state.topFlag():
      case str[]:
        of '#', '>', '/', '=', '^', '.':
          result.add initCharTok(str, {
            '#': mtkSectionStart,
            '/': mtkSectionEnd,
            '^': mtkInvert,
            '=': mtkChangeWrapper,
            '>': mtkPartial,
            '.': mtkDot
          })

        of '}':
          str.assertAhead("}}")
          result.add initTok(mtkCurlyClose)
          str.next(2)
          state.toFlag(false)
          if str['\n']:
            str.next()

        of ' ':
          str.skipWhile({' '})
          result = str.lexerImpl()

        of IdentChars:
          result.add initTok(str, str.popIdent(), mtkIdent)

        else:
          raise newUnexpectedCharError(str)

    else:
      case str[]:
        of '{':
          if str[+1, '{']:
            result.add initTok(mtkCurlyOpen)
            str.next(2)
            state.toFlag(true)

          else:
            result.add initTok(str, str.popUntil('{'), mtkContent)

        else:
          result.add initTok(str, str.popUntil('{'), mtkContent)

  return initLexer(lexerImpl, some initTok(mtkEof))

proc parseGetExpr(lexer: var MLexer): MTree =
  lexer.next()
  result = lexer.newHTree(makGetExpr)
  if lexer[].kind in {mtkSectionStart, mtkSectionEnd}:
    lexer.next()

  while lexer[].kind in {mtkIdent, mtkDot}:
    if lexer[].kind == mtkIdent:
      result.add newHTree(makIdent, lexer.pop())

    else:
      lexer.next()

  lexer.next()


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

            result.add newHTree(makSection, @[start, section])

          of mtkIdent:
            result.add parseGetExpr(lexer)

          of mtkChangeWrapper:
            raiseImplementError("")

          of mtkPartial:
            lexer.next()
            result.add newHTree(makPartial, lexer.pop())
            lexer.next()

          of mtkSectionEnd:
            return

          else:
            raise newImplementKindError(lexer[+1])

      else:
        raise newImplementKindError(lexer[])

proc mustacheParse*(str: string): MTree =
  var str = initPosStr(str)
  var lexer = newMustacheLexer()
  lexer.setStr(str)
  return parseStmtList(lexer)


