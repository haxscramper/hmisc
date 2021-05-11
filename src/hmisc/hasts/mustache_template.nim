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
    makSection
    makContent
    makWrapSwitch
    makGetData

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


  # var str = initPosStr(str)
  # echov str[]
  # echov str.finished()
  return initLexer(lexerImpl)


proc mustacheParse*(str: string): MTree =
  echov "123123"
  var str = initPosStr(str)
  var lexer = newMustacheLexer()
  lexer.setStr(str)
  echov lexer.str[].finished()
  let tokens = lexer.getAll()
  echov tokens


when isMainModule:
  startHax()
  let tree = mustacheParse("""
<h2>Names</h2>
{{#names}}
  <strong>{{name}}</strong>
{{/names}}
""")
