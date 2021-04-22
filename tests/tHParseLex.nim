import hmisc/algo/[hparse_base, hlex_base]

import std/[unittest, options]

suite "Hlex base":
  test "test":
    var str = initPosStr("""
of true:
  sliceIdx*: int
  baseString*: ptr string
  slices*: seq[PosStrSlice]
""")

    str.skipToEol()
    let indent = str.getIndent()
    echo indent
    while str.hasIndent(indent):
      str.advance(indent)
      str.startSlice()
      str.skipToEol()
      str.finishSlice()

    var subStr = initPosStr(str)

    echo subStr


suite "Positional string low-level parse API":
  test "Pop numbers":
    var str = initPosStr("1234")
    check str.popDigit() == "1234"

  test "Positional information":
    var str = initPosStr("012\n456")
    discard str.popDigit()
    str.skip('\n')
    check str.line == 1
    check str.column == 0

proc simpleLexerImpl(str: var PosStr): Option[HsTok[char]] =
  if not ?str: return

  case str[]:
    of IdentStartChars:
      result = some str.initTok(str.popIdent(), 'i')

    of Digits:
      result = some str.initTok(str.popDigit(), 'd')

    of PunctChars:
      let ch = str[]
      result = some str.initTok(str.popChar(), ch)

    else:
      str.advance()


suite "Lexer":
  test "Edge case checks":
    var str = initPosStr("a b c d e f g ;")
    var lex = initLexer(str, simpleLexerImpl)

    while ?lex and not lex[';']:
      lex.advance()

    check lex[].strVal() == ";"

  test "Token positional information":
    let tokens = lexAll("a\nb", simpleLexerImpl)
    check tokens[0].line == 0
    check tokens[1].line == 1
    check tokens[0].column == 0
    check tokens[1].column == 0

  test "Multistate lexer":
    var state = newLexerState(0u8)

    proc lexerImpl(str: var PosStr): Option[HsTok[char]] =
      case state.topFlag():
        of 0:
          case str[]:
            of IdentStartChars:
              result = some initTok(str.popIdent(), '0')
              state.toFlag(1)

            else:
              str.advance()


        of 1:
          case str[]:
            of IdentStartChars:
              result = some initTok(str.popIdent(), '1')
              state.toFlag(0)

            else:
              str.advance()


        else:
          discard

    let tokens = lexAll("a b c", lexerImpl)
    check tokens[0].kind == '0'
    check tokens[1].kind == '1'
    check tokens[2].kind == '0'

  test "Indentation lexer":
    var state = newLexerState('-')
    proc lexerImpl(str: var PosStr): Option[HsTok[char]] =
      case str[]:
        of '\n', ' ':
          let indent = state.skipIndent(str)
          return case indent:
            of likIncIndent:  some initTok(" ", '>')
            of likDecIndent:  some initTok(" ", '<')
            of likSameIndent: some initTok(" ", '=')
            of likNoIndent:   some initTok(" ", '?')


        of IdentStartChars:
          result = some initTok(str.popIdent(), 'i')

        of PunctChars:
          let ch = str[]
          result = some initTok(str.popChar(), ch)

        else:
          str.advance()

    let toks = lexAll("""
test
  indent
  same
dedent""", lexerImpl)

    doAssert toks == @[
      ('i', "test"),
      ('>', " "), ('i', "indent"),
      ('=', " "), ('i', "same"),
      ('<', " "), ('i', "dedent")
    ]
