import
  hmisc/preludes/unittest

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/other/hpprint

import std/[options, strscans, tables]

configureDefaultTestContext(
  skipAfterException = true,
  skipAfterCheckFail = true
)


template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template varStr(inStr: string, slices: openarray[Slice[int]]): untyped =
  var str = initPosStr(asPtr inStr, slices)
  str

suite "Primitives":
  {.configure.}:
    showCoverage("advance")

  test "Pop range while":
    var str = initPosStr("---?")
    str.pushRange()
    while ?str and str['-']:
      str.advance()

    check str.popRangeIndices()[0] == 0 .. 2

  test "Regular string advancements":
    block skip_while:
      block:
        var str = initPosStr("000000000000_")
        check str['0']
        str.skipWhile({'0'})
        check str['_']

      block:
        var str = initPosStr("")
        str.skipWhile({'0'})
        check not ?str

    block line_information:
      var str = initPosStr("0\n123\n456")
      check:
        str.line == 0
        str.column == 0

      str.advance()
      check:
        str.line == 0
        str.column == 1

      str.advance()
      check:
        str.line == 1
        str.column == 0

    block skip_until:
      var str = initPosStr("_________]")
      check str['_']
      str.skipUntil({']'})
      check str[']']
      str.advance()
      check not ?str

    block pop_entries:
      check:
        varStr("hello").popIdent() == "hello"
        varStr("").popIdent() == ""
        varStr("01234").popNext(4) == "0123"
        varStr("").popBacktickIdent() == ""
        varStr("test").popBacktickIdent() == "test"
        varStr("`hel lo`").popBacktickIdent() == "hel lo"

    block range_paranoid:
      block:
        var str = varStr("0123456789")
        str.pushRange()
        check:
          str.pos == 0
          str.getRangeIndices() == @[0 .. -1]
          str.ranges[0].pos == 0

        str.advance()

        check:
          str.pos == 1
          str.getRangeIndices() == @[0 .. 0]

      block:
        starthax()
        var str = varStr("0123456789", [0..90])
        str.pushRange()
        check:
          str.pos == 0
          str.fragmentedRanges[0] == @[0 .. 0]
          str.getRangeIndices() == @[0 .. -1]

        str.advance()
        check:
          str.pos == 1
          str.fragmentedRanges[0] == @[0 .. 1]
          str.getRangeIndices() == @[0 .. 0]


    block pop_digit:
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

      str.pushRange()
      check:
        str['0']
        str.getRangeIndices() == @[0 .. -1]

      str.advance()
      check:
        str['1']
        str.getRangeIndices() == @[0 .. 0, 2 .. 1]

      str.advance()
      check:
        str['2']
        str.getRangeIndices() == @[0 .. 0, 2 .. 2, 4 .. 3]
        str.getRange() == "01"

    block larger_fragmented_ranges:
      let baseStr = "012_3_456_"
      var str = varStr(baseStr, [0..2, 4..4, 6..8])
      str.pushRange()
      check:
        str['0']
        str.pos == 0
        ## By default right offset `-1` is applied, so without no
        ## `advance()` range indices would be an empty sequence
        str.getRangeIndices() == @[0 .. -1]

        ## String has single active range, and without any `advance()`
        ## calls it is currently empty
        baseStr[str.getRangeIndices()[0]] == ""

        ## This can be changed by specifying @arg{rightShift}
        str.getRangeIndices(rightShift = 0) == @[0 .. 0]

        ## And now currently active range also uses first character in the
        ## base string
        baseStr[str.getRangeIndices(rightShift = 0)[0]] == "0"

      str.advance()
      str.advance()
      str.advance()

      check:
        str['3']
        str.pos == 4
        ## After advancing several times new fragmented range was added -
        ## index `3` is not placed in the allowed elements, so it is
        ## skipped. Right offset of `-1` is applied to the last range
        ## fragment only
        str.getRangeIndices() == @[0 .. 2, 4 .. 3]

        ## Collecting active ranges should give us all the characters up
        ## until `str['3']`
        baseStr[str.getRangeIndices()[0]] == "012"
        baseStr[str.getRangeIndices()[1]] == ""
        baseStr[str.getRangeIndices()[0]] & baseStr[str.getRangeIndices()[1]] == "012"

        ## `getRange()` can be used for the same purpose
        str.getRange() == "012"

      str.advance()

      check:
        str['4']
        str.pos == 6
        ## Advancing over framented range yet again adds new subslice - `4..4`
        str.getRangeIndices() == @[0 .. 2, 4 .. 4, 6 .. 5]

      str.advance(); check str['5']
      str.advance()

      check:
        str['6']
        str.pos == 8
        str.getRangeIndices() == @[0 .. 2, 4 .. 4, 6 .. 7]
        str.getRange() == "012345"
        str.getRange(rightShift = 0) == "0123456"

      str.advance()

      check:
        str.pos == 9
        not ?str
        str.getRangeIndices() == @[0 .. 2, 4 .. 4, 6 .. 8]



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
      check:
        str[] == 'i'

      str.skipWhile(IdentChars)
      check:
        str[] == '?'
        str.pos == 2
        str.getRangeIndices() == @[0 .. 1]
        str.getRange() == "if"

    block pop_ident:
      var str = varStr("if ", [0..12])
      check str.popIdent() == "if"

    block pop_ident_superfragmented:
      var str = varStr("if ", [0..0, 1..1, 2..2])
      str.pushRange()
      check:
        str[] == 'i'

      str.skipWhile(IdentChars)

      check:
        str[] == ' '
        str.pos == 2
        str.getRangeIndices() == @[0 .. 0, 1 .. 1, 2 .. 1]
        str.getRange() == "if"




    block popEdges:
      var str = varStr("else_", [0 .. 3])
      str.pushRange()

      if str[IdentChars]: str.advance()
      if str[IdentChars]: str.advance()

      check:
        str[] == 's'

      if str[IdentChars]: str.advance()

      check:
        str.getRangeIndices() == @[0 .. 2]
        str[] == 'e'

      if str[IdentChars]:
        str.advance()

      check:
        str.getRangeIndices() == @[0 .. 3]
        not ?str

      if str[IdentChars]:
        str.advance()

      check:
        str.pos == 4
        str.getRangeIndices() == @[0 .. 3]
        str.getRange() == "else"


  test "Parse slices":
    let base = "[if (a == b)][{echo 12}][else"
    var s = varStr(base, [1 .. 11, 14 .. 22, 25 .. 29])
    check:
      s['i']
      s.popIdent() == "if"
      s.trySkip(' ')
      s.trySkip('(')
      s.trySkip('a')
      s.trySkip(' ')
      s.trySkip('=')
      s.trySkip('=')
      s.trySkip(' ')
      s[] == 'b'; s.trySkip('b')
      s[] == ')'
      s.sliceIdx == 0
      s[] == base[11]
      base[11] == ')'

      s.trySkip(')')
      s[] == '{'; s.trySkip('{')
      s.popIdent() == "echo"
      s.trySkip(' ')
      s.popDigit() == "12"


suite "Hlex base":
  test "Use example":
    let base = "[if (a == b)][{echo 12}][else"
    var str = initPosStr(base)
    block extractSlices:
      ## Iterate over string, skipping braces, and extract all characters
      ## in the subslices
      while ?str:
        if str['[']:
          str.advance()
          str.startSlice()
          while ?str and not str[']']:
            str.advance()

          if ?str:
            check:
              str[] == ']'

          if ?str:
            str.finishSlice()

          else:
            str.finishSlice(0)


          if ?str: str.advance()

    show str.sliceBuffer

    let top = str.sliceBuffer[0]

    check:
      top[0] == 1 .. 11
      base[top[0]] == "if (a == b)"

      top[1] == 14 .. 22
      base[top[1]] == "{echo 12}"

      top[2] == 25 .. 28
      base[top[2]] == "else"

    var chars = initPosStr(str)
    check chars.sliceStrings() == @["if (a == b)", "{echo 12}", "else"]

    block parseSlices:
      ## Use subslice string as if it was a regular, concatenated
      ## "if (a == b) {echo 12} else"
      var tokens: seq[string]
      while ?chars:
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


suite "C preprocessor reimplementation":
  test "ifdef garbage":
    let code = lit3"""
    #ifdef 1 == 2
      // random text that would not be parsed,
      // might even contain more #ifdef, just
      // don't start lines
    #endif
    """

    var str = varStr(code)
    var slices: seq[string]
    while ?str:
      case str[]:
        of '#':
          str.pushRange()
          str.startSlice()
          str.skipToEOL()
          slices.add str.popRange()
          str.finishSlice()

        else:
          str.pushRange()
          str.startSlice()
          while ?str and not str['#']:
            str.skipToEol()

          slices.add str.popRange()
          str.finishSlice()

    var
      if2 = initPosStr(str, true)
      body = initPosStr(str, true)
      if1 = initPosStr(str, true)

    check:
      strdiff slices[0], if1.getAll()
      strdiff slices[1], body.getAll()
      strdiff slices[2], if2.getAll()

      strdiff slices[0], "#ifdef 1 == 2\n"
      strdiff slices[1], """
  // random text that would not be parsed,
  // might even contain more #ifdef, just
  // don't start lines
"""
      strdiff slices[2], "#endif\n"

  proc lexerCb(str: var PosStr): Option[PosStr] =
    case str[]:
      of '#':
        str.startSlice()
        str.skipToNewline()
        if ?str and str[-1] == '\\':
          while ?str and str[-1] == '\\':
            str.advance()
            str.skipToNewline()

        else:
          str.advance()

        return some str.popSlice()

      else:
        str.startSlice()
        while ?str and not str['#']:
          str.skipToEol()

        return some str.popSlice()

  test "Lexer callbacks":
    startHax()
    block one_define:
      var tokens = lexAll(varStr("#define"), lexerCb)
      check:
        tokens[0].getAll() == "#define"

    block consecutive_defines:
      var tokens = lexAll(varStr("#define\n#define"), lexerCb)
      check:
        tokens[0].getAll() == "#define\n"
        tokens[1].getAll() == "#define"

    block continued_defines:
      var tokens = lexAll(varStr("#def\\\n line2"), lexerCb)
      check:
        strdiff tokens[0].getAll(), "#def\\\n line2"



suite "Nim cfg parser":
  discard

suite "Simple org-mode replacement":
  discard
