import hmisc/algo/[hparse_base, hlex_base]

import std/[unittest, options]

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

suite "Lexer":
  test "Token positional information":
    proc lexerImpl(str: var PosStr): Option[HsTok[char]] =
      case str[]:
        of IdentStartChars: return some str.initTok(str.popIdent(), 'i')
        of Digits:          return some str.initTok(str.popDigit(), 'd')
        of PunctChars:      return some str.initTok(str.popChar(), str[])
        else:               str.advance()

    let tokens = lexAll("a\nb", lexerImpl)
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
