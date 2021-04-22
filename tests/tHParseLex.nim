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
