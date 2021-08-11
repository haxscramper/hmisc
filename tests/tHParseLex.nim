import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/preludes/unittest,
  hmisc/other/hpprint

import std/[options]

configureDefaultTestContext(
  skipAfterException = true
)


template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template varStr(inStr: string, slices: openarray[Slice[int]]): untyped =
  var str = initPosStr(asPtr inStr, slices)
  str

suite "Primitives":
  test "Pop range while":
    var str = initPosStr("---?")
    str.pushRange()
    while ?str and str['-']:
      str.advance()

    check str.popRangeIndices()[0] == 0 .. 2

  test "Regular string advancements":
    block skipWhile:
      block:
        var str = initPosStr("000000000000_")
        check str['0']
        str.skipWhile({'0'})
        check str['_']

      block:
        var str = initPosStr("")
        str.skipWhile({'0'})
        check not ?str

    block skipUntil:
      var str = initPosStr("_________]")
      check str['_']
      str.skipUntil({']'})
      check str[']']
      str.advance()
      check not ?str

    block popEntries:
      check:
        varStr("hello").popIdent() == "hello"
        varStr("").popIdent() == ""
        varStr("01234").popNext(4) == "0123"
        varStr("").popBacktickIdent() == ""
        varStr("test").popBacktickIdent() == "test"
        varStr("`hel lo`").popBacktickIdent() == "hel lo"


    block popDigit:
      var str = initPosStr("-123 123 0xABCDE 0b100")

      check str.popDigit() == "-123"
      str.skipSpace()

      check str.popDigit() == "123"
      str.skipSpace()

      check str.popDigit() == "0xABCDE"
      str.skipSpace()

      check str.popDigit() == "0b100"
      str.skipSpace()

  test "Subslice advancements":
    block advance_over_fragmented_range:
      var str = varStr("0_1_2_3_4_5", [0..0, 2..2, 4..4, 6..6, 8..8])
      check str['0']

      str.pushRange()
      check str.getRangeIndices() == @[0..0]

      str.advance()
      check str.getRangeIndices() == @[0..0, 2..2]

      str.advance()
      check:
        str.getRangeIndices() == @[0..0, 2..2, 4..4]
        str.getRange() == "012"

    block larger_fragmented_ranges:
      var str = varStr("012_3_456_", [0..2, 4..4, 6..8])
      str.pushRange()
      check:
        str['0']
        str.pos == 0
        str.getRangeIndices() == @[0..0]

      str.advance()
      str.advance()
      str.advance()

      check:
        str['3']
        str.pos == 4
        str.getRangeIndices() == @[0..2, 4..4]

      str.advance()

      check:
        str['4']
        str.pos == 6
        str.getRangeIndices() == @[0..2, 4..4, 6..6]

      str.advance(); check str['5']
      str.advance()

      check:
        str['6']
        str.pos == 8
        str.getRangeIndices() == @[0..2, 4..4, 6..7]

      str.advance()

      check:
        not ?str
        str.getRangeIndices() == @[0..2, 4..4, 6..8]



    block partial_range:
      var str = initPosStr(asPtr "01234", [0 .. 1])
      check str.pos == 0
      skip(str, '0')

      check str.pos == 1
      skip(str, '1')

      check:
        str.sliceIdx == 1
        not ?str

    block fragmented_range:
      var str = initPosStr(asPtr "0_1", [0 .. 0, 2 .. 2])
      check:
        str[] == '0'
        str.pos == 0

      str.advance()
      check:
        str.pos == 2
        str[] == '1'

    block skip_fragmented_range:
      var str = initPosStr(asPtr "0_1_2", [0 .. 0, 2 .. 2, 4 .. 4])
      skip(str, '0')
      skip(str, '1')
      check ?str
      skip(str, '2')
      check not ?str

    block pop_while_fragmented_range:
      var str = varStr("0_0_00____", [0..0, 2..2, 4..5])

      # Unrolled `while str['0']`

      check str['0']; str.advance()
      check str['0']; str.advance()
      check str['0']; str.advance()
      check str['0']; str.advance()

      check not ?str

    block skip_while:
      var str = varStr("0_0_", [0..0,2..2])
      str.pushRange()
      str.skipWhile({'0'})
      let zeros = str.popRange()
      check zeros == "00"

    block fragmented_digit:
      var str = varStr("0_1_2", [0..0, 2..2, 4..4])
      let num = popDigit(str)
      check num == "012"

    block if_with_space:
      var str = varStr("if ", [0..12])
      check str['i']
      str.skipWhile({'i', 'f'})
      check str[' ']

    block if_ident_chars:
      var str = varStr("if?", [0..12])
      str.pushRange()
      check str['i']
      str.skipWhile(IdentChars)
      check:
        str['?']
        str.pos == 2
        str.getRangeIndices() == @[0 .. 1]

    block pop_ident:
      var str = varStr("if ", [0..12])
      check str.popIdent() == "if"

    block pop_ident_superfragmented:
      var str = varStr("if ", [0..0, 1..1, 2..2])
      str.pushRange()
      check str['i']
      str.skipWhile(IdentChars)
      check:
        str[' ']
        str.pos == 2
        str.getRangeIndices() == @[0..0, 1..1]
        str.getRange() == "if"




    block popEdges:
      var str = varStr("else_", [0 .. 3])
      str.pushRange()

      if str[IdentChars]: str.advance()
      if str[IdentChars]: str.advance()
      if str[IdentChars]: str.advance()

      check str.getRangeIndices() == @[0..2]

      startHax()
      if str[IdentChars]:
        str.advance()

      stopHax()


      check:
        str.getRangeIndices() == @[0..3]
        not ?str

      if str[IdentChars]: str.advance()





      check:
        str['_']
        str.pos == 3
        str.getRangeIndices() == @[0..3]
        str.getRange() == "else"




suite "Hlex base":
  test "Use example":
    var str = initPosStr("[if (a == b)][{echo 12}][else")
    # var str = initPosStr("[if (a == b)]")
    block extractSlices:
      ## Iterate over string, skipping braces, and extract all characters
      ## in the subslices
      while ?str:
        if str['[']:
          str.advance()
          str.startSlice()
          while ?str and not str[']']:
            str.advance()

          str.finishSlice()
          if ?str: str.advance()

    var chars = initPosStr(str)
    check chars.sliceStrings() == @["if (a == b)", "{echo 12}", "else"]

    show chars
    startHax()

    var str2 = chars
    echov str2.popIdent()


    block parseSlices:
      ## Use subslice string as if it was a regular, concatenated
      ## "aaabbbcccaaa"
      var tokens: seq[string]
      while ?chars:
        echov chars
        case chars[]:
          of IdentChars:
            tokens.add chars.popIdent()

          of ' ':
            chars.advance()

          of PunctChars, MathChars:
            tokens.add $chars.popChar()

          else:
            raise newUnexpectedCharError(chars)

      show tokens

      check tokens == @[
        "if", "(", "a", "=", "=", "b", ")", "{",
          "echo", "12",
        "}", "else"
      ]



  test "Repeatedly nested slices":
    ## Repeatedly extract nested string slices and parse them again.
    ## Real-world use example - `#+table` form haxorg which might be
    ## indented, and in turn also contain another `#+table`.


  test "test":
    var str = initPosStr("""
of true:
  sliceIdx*: int
  baseString*: ptr string
  slices*: seq[PosStrSlice]
""")

    str.skipToEol()
    let indent = str.getIndent()
    show indent
    while str.hasIndent(indent):
      str.advance(indent)
      str.startSlice()
      str.skipToEol()
      str.finishSlice()

    var subStr = initPosStr(str)

    show subStr


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

    check toks == @[
      ('i', "test"),
      ('>', " "), ('i', "indent"),
      ('=', " "), ('i', "same"),
      ('<', " "), ('i', "dedent")
    ]
