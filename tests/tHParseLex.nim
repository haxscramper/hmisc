import hmisc/preludes/unittest

testFileStarted()

import
  hmisc/preludes/unittest

import
  hmisc/algo/[hparse_base, hlex_base],
  hmisc/other/hpprint,
  hmisc/types/colorstring

import std/[options, strscans, sets, sequtils, unicode, algorithm]

configureDefaultTestContext(
  skipAfterException = true,
  skipAfterCheckFail = true
)

# Missing tests for
#
# - `linesAround` and error message generation.


template varStr(inStr: string): untyped =
  var str = initPosStr(inStr)
  str

template varStr(inStr: string, slices: openarray[Slice[int]]): untyped =
  var str = initPosStr(asRef inStr, slices)
  str

suite "Primitives":
  test "Skip until":
    block until_including:
      var str = varStr("___+]")
      str.skipUntil(']', including = true)
      check str[] == ']'

    block until_excluding:
      var str = varStr("___+]")
      str.skipUntil(']', including = false)
      check str[] == '+'

    block to_eol:
      var str = initPosStr("__+\n")
      str.skipToEol()
      check str[] == '\n'

    block until_eol:
      var str = initPosStr("__+\n__&\n")
      str.skipBeforeEol()
      check str[] == '+'

    block past_eol:
      var str = initPosStr("__+\n&")
      str.skipPastEol()
      check str[] == '&'

    block pop_until_slice:
      var s0 = varStr("[123]")
      check s0[] == '['
      s0.skipUntil({']'}, including = true)
      check s0[] == ']'

      var s1 = varStr("[123]")
      check:
        s1[] == '['
        s1.popUntilSlice({']'}, including = false).strVal() == "[123"
        s1[] == '3'

        varStr("[123]").popUntilSlice({']'}, true).strVal() == "[123]"

    block skip_before:
      var s = varStr("_]")
      s.skipBefore(']')
      check s[] == '_'

    block skip_to:
      var s = varStr("_]")
      s.skipTo(']')
      check s[] == ']'

    block skip_past:
      var s = varStr("_]?")
      s.skipPast(']')
      check s[] == '?'

  test "Pop range while":
    var str = initPosStr("---?")
    str.pushRange()
    while ?str and str['-']:
      str.next()

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

      str.next()
      check:
        str.line == 0
        str.column == 1

      str.next()
      check:
        str.line == 1
        str.column == 0

    block skip_until:
      var str = initPosStr("_________]")
      check str['_']
      str.skipUntil({']'})
      check str[']']
      str.next()
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

        str.next()

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

        str.next()
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
    block next_over_fragmented_range:
      var str = varStr("0_1_2_3_4_5", [0..0, 2..2, 4..4, 6..6, 8..8])

      str.pushRange()
      check:
        str['0']
        str.getRangeIndices() == @[0 .. -1]

      str.next()
      check:
        str['1']
        str.getRangeIndices() == @[0 .. 0, 2 .. 1]

      str.next()
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
        ## `next()` range indices would be an empty sequence
        str.getRangeIndices() == @[0 .. -1]

        ## String has single active range, and without any `next()`
        ## calls it is currently empty
        baseStr[str.getRangeIndices()[0]] == ""

        ## This can be changed by specifying @arg{rightShift}
        str.getRangeIndices(rightShift = 0) == @[0 .. 0]

        ## And now currently active range also uses first character in the
        ## base string
        baseStr[str.getRangeIndices(rightShift = 0)[0]] == "0"

      str.next()
      str.next()
      str.next()

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

      str.next()

      check:
        str['4']
        str.pos == 6
        ## Advancing over framented range yet again adds new subslice - `4..4`
        str.getRangeIndices() == @[0 .. 2, 4 .. 4, 6 .. 5]

      str.next(); check str['5']
      str.next()

      check:
        str['6']
        str.pos == 8
        str.getRangeIndices() == @[0 .. 2, 4 .. 4, 6 .. 7]
        str.getRange() == "012345"
        str.getRange(rightShift = 0) == "0123456"

      str.next()

      check:
        str.pos == 9
        not ?str
        str.getRangeIndices() == @[0 .. 2, 4 .. 4, 6 .. 8]



    block partial_range:
      var str = initPosStr(asRef "01234", [0 .. 1])
      check str.pos == 0
      skip(str, '0')

      check str.pos == 1
      skip(str, '1')

      check:
        str.sliceIdx == 1
        not ?str

    block fragmented_range:
      var str = initPosStr(asRef "0_1", [0 .. 0, 2 .. 2])
      check:
        str[] == '0'
        str.pos == 0

      str.next()
      check:
        str.pos == 2
        str[] == '1'

    block skip_fragmented_range:
      var str = initPosStr(asRef "0_1_2", [0 .. 0, 2 .. 2, 4 .. 4])
      skip(str, '0')
      skip(str, '1')
      check ?str
      skip(str, '2')
      check not ?str

    block pop_while_fragmented_range:
      var str = varStr("0_0_00____", [0..0, 2..2, 4..5])

      # Unrolled `while str['0']`

      check str['0']; str.next()
      check str['0']; str.next()
      check str['0']; str.next()
      check str['0']; str.next()

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




    block pop_edges:
      var str = varStr("else_", [0 .. 3])
      str.pushRange()

      if str[IdentChars]: str.next()
      if str[IdentChars]: str.next()

      check:
        str[] == 's'

      if str[IdentChars]: str.next()

      check:
        str.getRangeIndices() == @[0 .. 2]
        str[] == 'e'

      if str[IdentChars]:
        str.next()

      check:
        str.getRangeIndices() == @[0 .. 3]
        not ?str

      if str[IdentChars]:
        str.next()

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

  test "popBalancedSlice":
    block inside_parens:
      var str = varStr("[0123]")
      let slice = str.popBalancedSlice({'['}, {']'})
      check slice.strVal() == "[0123]"

    block unclosed_noraise:
      var str = varStr("[0123")
      let slice = str.popBalancedSlice({'['}, {']'}, doRaise = false)
      check slice.strVal() == "[0123"

    block unclosed_end_noraise:
      var str = varStr("[0123\n")
      let slice = str.popBalancedSlice({'['}, {']'}, doRaise = false)
      check:
        slice.strVal() == "[0123"
        str[] == '\n'

    block raise_missing_end_at_eof:
      var str = varStr("[0123")
      expect UnbalancedWrapError as err:
        let slice = str.popBalancedSlice({'['}, {']'})

      check:
        err.line == 0
        err.column == 5

    block raise_missing_end_at_delimiter:
      var str = varStr("[0123\n]")
      expect UnbalancedWrapError as err:
        let slice = str.popBalancedSlice({'['}, {']'})

      check:
        err.line == 0
        err.column == 5

    block cvs_arguments:
      const
        o = {'('}
        c = {')'}

      var str = varStr("((1), (1(2(3,4))), (3, 4, 5))")

      check:
        str.trySkip('(')
        str.popBalancedSlice(o, c).strVal() == "(1)"
        str.trySkip(", ")
        str.popBalancedSlice(o, c).strVal() == "(1(2(3,4)))"
        str.trySkip(", ")
        str.popBalancedSlice(o, c).strVal() == "(3, 4, 5)"
        str.trySkip(')')

    block cvs_loop:
      for (str, args) in {
        "(1)": @["1"],
        "(1,2)": @["1", "2"],
        "(1,3,4)": @["1", "3", "4"],
        "((1))": @["(1)"],
        "((1), (2))": @["(1)", "(2)"],
        "()": @[]
      }:
        var s = varStr(str)
        s.skip('(')
        var buf: seq[string]
        while ?s and not s[')']:
          if s['(']:
            buf.add s.popBalancedSlice({'('}, {')'}).strVal()

          else:
            buf.add s.popUntil({',', ')'})

          if not s.trySkip(','):
            break

          else:
            s.skipWhile({' '})

        s.skip(')')

        check buf == args


  test "Line and column tracking":
    block regular_string:
      var str = varStr("01\n0")
      check:
        str.line == 0
        str.column == 0
        str.pos == 0
        str[] == '0'

      str.next()
      check:
        str.line == 0
        str.column == 1
        str.pos == 1
        str[] == '1'

      str.next()
      check:
        str.line == 0
        str.column == 2
        str.pos == 2
        str[] == '\n'

      str.next()
      check:
        str.line == 1
        str.column == 0
        str.pos == 3
        str[] == '0'

    block pop_slice_oneline:
      var str = varStr("0123")
      let s1 = str.popPointSlice()
      check:
        s1.line == 0
        s1.column == 0
        s1.pos == 0
        s1.strVal() == "0"

      let s2 = str.popPointSlice(advance = 2)
      check:
        s2.line == 0
        s2.column == 1
        s2.pos == 1
        s2.strVal() == "12"

    block pop_slice_helpers:
      var str = varStr("#+title")
      check:
        str[] == '#'
        str.pos == 0
        str.line == 0
        str.column == 0

      let s1 = str.popPointSlice(advance = 2)

      check:
        str[] == 't'
        str.pos == 2
        str.line == 0
        str.column == 2

        s1.pos == 0
        s1.line == 0
        s1.column == 0
        s1.strVal() == "#+"


      str.startSlice()
      str.skipWhile(IdentChars)
      let s2 = str.popSlice()

      check:
        str[] == '\x00'
        str.pos == 7
        str.line == 0
        str.column == 7

        s2.line == 0
        s2.column == 2
        s2.strVal() == "title"

    block unicode_text:
      var str = varStr("бвг\nбвг")
      check:
        str[] == "б"[0]
        str.pos == 0
        str.line == 0
        str.column == 0
        str.runeAt() == uc"б"

      str.next()
      check:
        str[] == "в"[0]
        str.pos == 2
        str.line == 0
        str.column == 1
        str.runeAt() == uc"в"

    block advance_n_positions:
      var str = varStr("бвг")
      check:
        str.pos == 0
        str.line == 0
        str.column == 0
        str.runeAt() == uc"б"

      str.next(2)
      check:
        str[] == "г"[0]
        str.pos == 4
        str.line == 0
        str.column == 2
        str.runeAt() == uc"г"


  test "Advance backwards":
    block go_to_the_end:
      var str = varStr("0123")
      check:
        str[] == '0'
        str.pos == 0
        str.column == 0

      str.skipToEof()
      check:
        str[] == '3'
        str.pos == 3
        str.column == 3

    block backwards_over_ascii:
      var str = varStr("0123")
      str.gotoEof()
      str.next(-1)
      check:
        str[] == '2'
        str.pos == 2
        str.column == 2
        str.line == 0

    block backwards_over_unicode:
      # TODO test backwards advance over regular characters,
      # TODO test advance over unicode in 'byteAdvance' mode

      var str = varStr("бвг")
      str.gotoEof()
      check:
        ## By default (`byteAdvance: bool = false`) gotoEof moves to the
        ## last unicode rune in available input.
        str[] == "г"[0]
        str.runeAt() == uc"г"
        str.column == 2
        ## Final position is 5, but it contains unicode continuation rune,
        ## so correction was made
        str.pos == 4

      ## Move back one unicode rune
      str.next(-1)

      check:
        str[] == "в"[0]
        str.runeAt() == uc"в"
        str.column == 1
        str.pos == 2



suite "Hlex base":
  test "Use example":
    let base = "[if (a == b)][{echo 12}][else"
    var str = initPosStr(base)
    block extractSlices:
      ## Iterate over string, skipping braces, and extract all characters
      ## in the subslices
      while ?str:
        if str['[']:
          str.next()
          str.startSlice()
          while ?str and not str[']']:
            str.next()

          if ?str:
            check:
              str[] == ']'

          if ?str:
            str.finishSlice()

          else:
            str.finishSlice(0)


          if ?str: str.next()

    let top = str.sliceBuffer

    check:
      top[0][0] == 1 .. 11
      base[top[0][0]] == "if (a == b)"

      top[1][0] == 14 .. 22
      base[top[1][0]] == "{echo 12}"

      top[2][0] == 25 .. 28
      base[top[2][0]] == "else"

    var chars = initPosStr(str, allSlice = true)
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
            chars.next()

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
    var str = varStr("0123")
    str.startSlice()
    str.next()
    str.startSlice()
    str.next()
    str.finishAllSlice(rightShift = 0)

    check:
      str.sliceBuffer[0] == @[0 .. 2]
      str.sliceBuffer[1] == @[1 .. 2]

    let s2 = str.popSlice(rightShift = 0)
    let s1 = str.popSlice(rightShift = 0)

    check:
      s1.strVal() == "012"
      s2.strVal() == "12"


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
      str.next(indent)
      str.startSlice()
      str.skipPastEol()
      str.finishSlice()

    var subStr = initPosStr(str)

    show subStr

proc simpleLexerImpl(str: var PosStr): seq[HsTok[char]] =
  if not ?str: return

  case str[]:
    of IdentStartChars:
      result.add str.initTok(str.popIdent(), 'i')

    of Digits:
      result.add str.initTok(str.popDigit(), 'd')

    of PunctChars:
      let ch = str[]
      result.add str.initTok(str.popChar(), ch)

    else:
      str.next()


suite "Lexer":
  test "Edge case checks":
    var str = initPosStr("a b c d e f g ;")
    var lex = initLexer(str, simpleLexerImpl)

    while ?lex and not lex[';']:
      lex.next()

    check lex[].strVal() == ";"

  test "Token positional information":
    let tokens = lexAll(varStr "a\nb", simpleLexerImpl)
    check tokens[0].line == 0
    check tokens[1].line == 1
    check tokens[0].column == 0
    check tokens[1].column == 0

  test "Multistate lexer":
    var state = newLexerState(0u8)

    proc lexerImpl(str: var PosStr): seq[HsTok[char]] =
      case state.topFlag():
        of 0:
          case str[]:
            of IdentStartChars:
              result.add initTok(str.popIdent(), '0')
              state.toFlag(1)

            else:
              str.next()


        of 1:
          case str[]:
            of IdentStartChars:
              result.add initTok(str.popIdent(), '1')
              state.toFlag(0)

            else:
              str.next()


        else:
          discard

    let tokens = lexAll(varStr "a b c", lexerImpl)
    check tokens[0].kind == '0'
    check tokens[1].kind == '1'
    check tokens[2].kind == '0'

  test "Indentation lexer":
    var state = newLexerState('-')
    proc lexerImpl(str: var PosStr): seq[HsTok[char]] =
      case str[]:
        of '\n', ' ':
          let indent = state.skipIndent(str)
          result.add():
            case indent:
              of likIncIndent:  initTok(" ", '>')
              of likEmptyLine:  initTok(" ", '_')
              of likDecIndent:  initTok(" ", '<')
              of likSameIndent: initTok(" ", '=')
              of likNoIndent:   initTok(" ", '?')


        of IdentStartChars:
          result.add initTok(str.popIdent(), 'i')

        of PunctChars:
          let ch = str[]
          result.add initTok(str.popChar(), ch)

        else:
          str.next()

    let toks = lexAll(varStr lit3"""
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
      // random text that would not be parsed, |
      // might even contain more #ifdef, just  |
      // don't start lines                     |
    #endif
    """

    var str = varStr(code)
    var slices: seq[string]
    while ?str:
      case str[]:
        of '#':
          str.pushRange()
          str.startSlice()
          str.skipPastEOL()
          slices.add str.popRange()
          str.finishSlice()

        else:
          str.pushRange()
          str.startSlice()
          while ?str and not str['#']:
            str.skipPastEol()

          slices.add str.popRange()
          str.finishSlice()

    var
      if2 = initPosStr(str)
      body = initPosStr(str)
      if1 = initPosStr(str)

    let text = lit3(2, """
        // random text that would not be parsed, |
        // might even contain more #ifdef, just  |
        // don't start lines                     |
        """)

    check:
      strdiff slices[0], if1.getAll()
      strdiff slices[1], body.getAll()
      strdiff slices[2], if2.getAll()

      strdiff slices[0], "#ifdef 1 == 2\n"
      strdiff slices[1], text
      strdiff slices[2], "#endif\n"


  proc lex(str: var PosStr, names: var HashSet[string]): seq[PosStr] =
    while ?str:
      case str[]:
        of '#':
          str.startSlice()
          str.next()
          let kind = str.popIdent()
          if kind == "define" and str[' ']:
            str.skip(' ')
            let name = str.popIdent()
            names.incl name


          str.skipToEol()
          if ?str and str[-1] == '\\':
            while ?str and str[-1] == '\\':
              str.next()
              str.skipToEol()

          else:
            str.next()

          let slice = str.popSlice()
          result.add slice

        of IdentChars:
          str.startSlice()
          str.skipWhile(IdentChars)
          let idStr = str.peekSlice().strVal()

          if idStr in names:
            result.add str.popSlice()
            if str['(']:
              result.add str.popPointSlice({'('})
              while ?str and not str[')']:
                if str['(']:
                  result.add str.popBalancedSlice({'('}, {')'})

                else:
                  result.add str.asSlice(str.skipUntil({',', ')'}))

                if str[',']:
                  result.add str.popWhileSlice({',', ' '})

                else:
                  break

              result.add str.popPointSlice({')'})

          else:
            str.skipBalancedSlice({'('}, {')'})
            result.add str.popSlice()


        of ' ':
          result.add str.popWhileSlice({' '})

        else:
          str.startSlice()
          while ?str and not str['#']:
            str.skipToEol()

          result.add str.popSlice()

  test "Lexer callbacks":
    startHax()
    proc lex(str: string): seq[string] =
      var names: HashSet[string]
      for token in varStr(str).lex(names):
        result.add token.strVal()

    block one_define:
      var tokens = lex("#define")
      check tokens == @["#define"]

    block consecutive_defines:
      var tokens = lex("#define\n#define")
      check tokens == @["#define\n", "#define"]

    block continued_defines:
      var tokens = lex("#def\\\n line2")
      check:
        strdiff tokens[0], "#def\\\n line2"

    block define_and_call:
      var tokens = lex("#define test\ntest other")
      check tokens == @["#define test\n", "test", " ", "other"]

    block define_with_args:
      check:
        lex("#define test\ntest(1)") == @["#define test\n", "test", "(", "1", ")"]
        lex("#define random\ntest(1)") == @["#define random\n", "test(1)"]





suite "Nim cfg parser":
  type
    CfgToken = enum
      ctSection
      ctComment
      ctLongDash
      ctShortDash
      ctEq
      ctColon
      ctRStrLit
      ctStrLit
      ct3StrLit
      ctIf
      ctEnd
      ctOpOr
      ctOrAnd
      ctIdent

      ctEof


  startHax()
  proc lex(str: var PosStr): seq[HsTok[CfgToken]] =
    if not ?str:
      result.add initTok(str, ctEof)

    else:
      case str[]:
        of IdentChars:
          if str['r', '"']:
            str.startSlice()
            str.skipStringLit()
            result.add str.initTok(str.popSlice(), ctRStrLit)

          else:
            str.startSlice()
            while str[IdentChars + {'.'}]:
              str.next()

            result.add str.initTok(str.popSlice(), ctIdent)

        of '"':
          str.startSlice()
          str.skipStringLit()
          result.add str.initTok(str.popSlice(), ctStrLit)

        of '[':
          result.add str.initTok(ctSection, str.asSlice(str.skipPast(']')))

        of '=': result.add str.initTok(str.popPointSlice(), ctEq)
        of ':': result.add str.initTok(str.popPointSlice(), ctColon)
        of '-':
          if str['-', '-']:
            result.add str.initTok(
              str.popPointSlice(advance = 2), ctLongDash)

          else:
            result.add str.initTok(str.popPointSlice(), ctShortDash)

        of '@':
          str.startSlice()
          str.next()
          str.skipWhile(IdentChars)
          case str.peekSlice().strVal():
             of "@if": result.add str.initTok(str.popSlice(), ctIf)
             of "@end": result.add str.initTok(str.popSlice(), ctEnd)
             else: raise newMalformedTokenError(str.popSlice(), "@if or @end")


        of ';':
          str.startSlice()
          while ?str and str[';']:
            str.skipPastEol()

          result.add str.initTok(str.popSlice(0), ctComment)

        of ' ', '\n':
          str.skipWhile({' ', '\n'})
          result = lex(str)

        else:
          raise newUnexpectedCharError(str)

  proc lex(str: string): seq[HsTok[CfgToken]] = lexAll(varStr str, lex)
  proc lexStrs(str: string): seq[string] =
    for tok in lex(str):
      if tok.kind != ctEof:
        result.add str[tok.offset .. tok.finish]


  test "Primitive elements":
    block section:
      let t = lex("[Common]")[0]
      check:
        matchdiff(t,
          (line: 0, column: 0, offset: 0, finish: 7, isSlice: true))

    block comments:
      let
        text = "; comment"
        tok = lex(text)[0]

      check:
        text == text[tok.offset .. tok.finish]
        matchdiff(
          tok,
          (line: 0, column: 0, offset: 0, finish: 8, isSlice: true))



    block multiline_comment:
      let
        text = "; comment\n;line 2"
        tok = lex(text)[0]

      check:
        text == text[tok.offset .. tok.finish]
        matchdiff(
          tok,
          (line: 0, column: 0, offset: 0, finish: 16, isSlice: true))

    block switches:
      check:
        lexStrs("cc=gcc") == @["cc", "=", "gcc"]
        lexStrs("--cc=gcc") == @["--", "cc", "=", "gcc"]

    block controls:
      check:
        lexStrs(lit3"""
          @if cudnn:
            define:"cuda"
          @end
        """) == @[
          "@if",
          "cudnn",
          ":",
          "define",
          ":",
          "\"cuda\"",
          "@end"
        ]


type
  OrgTextToken = enum
    otWord
    otBigIdent

    otBoldStart, otBoldEnd, otBoldInline
    otSpace

    otEof

  OrgStructureToken = enum
    osCommandPrefix
    osIdent
    osColon
    osText

    osIndent
    osDedent
    osSameIndent
    osNoIndent

    osListStart
    osSubtreeStars
    osSubtreeTag
    osCompletion

    osEof

proc lexOrgStructure(str: var PosStr): seq[HsTok[OrgStructureToken]] =
  if not ?str:
    result.add str.initTok(osEof)

  else:
    case str[]:
      of '#':
        if str[+1, '+']:
          result.add str.initAdvanceTok(2, osCommandPrefix)

        str.peek(IdentChars)
        # pprint str
        str.startSlice()
        str.skipWhile(IdentChars)
        # pprint str
        let cmd = str.popSlice()
        # pprint cmd

        result.add initTok(cmd, osIdent)
        result.add str.initAdvanceTok(1, osColon, {':'})
        str.skipWhile({' '})

        case cmd.strVal():
          of "title":
            str.startSlice()
            str.skipToEol()
            result.add str.initSliceTok(osText)

          else:
            raise newImplementKindError(str.strVal())


      else:
        raise newUnexpectedCharError(str)

proc lexOrgText(str: var PosStr): seq[HsTok[OrgTextToken]] =
  if not ?str:
    result.add str.initTok(otEof)

  else:
    case str[]:
      of MaybeLetters:
        var allUp = true

        str.startSlice()
        while ?str and str[MaybeLetters + {'-', '_'}]:
          if not str[HighAsciiLetters + {'-', '_'}]:
            allUp = false

          str.next()

        result.add str.initSliceTok(if allUp: otBigIdent else: otWord)

      of ' ':
        result.add str.initTok(str.popWhileSlice({' '}), otSpace)

      of '*':
        if str[+1, '*']:
          result.add str.initTok(
            str.popPointSlice(advance = 2), otBoldInline)

        elif str[-1, ' '] or str.atStart():
          result.add str.initTok(str.popPointSlice(), otBoldStart)

        elif str[+1, ' '] or str.beforeEnd():
          result.add str.initTok(str.popPointSlice(), otBoldEnd)

        else:
          raise newImplementError($str)

      else:
        raise newUnexpectedCharError(str)

suite "Simple org-mode":
  test "Lex text":
    let lex = lexOrgText
    block words:
      check:
        matchdiff lexAll(varStr "test", lex), [
          (kind: otWord, strVal: "test")
        ]

        matchdiff lexAll(varStr "TODO", lex), [
          (kind: otBigIdent, strVal: "TODO")
        ]

    block markup:
      let tokens = lexAll(varStr("*word* *BIG_IDENT* **IN**line bold"), lex)

      check:
        matchdiff tokens, [
          (kind: otBoldStart, offset: 0),
          (kind: otWord, strVal: "word", offset: 1),
          (kind: otBoldEnd, offset: 5),

          (kind: otSpace, strVal: " "),

          (kind: otBoldStart),
          (kind: otBigIdent, strVal: "BIG_IDENT"),
          (kind: otBoldEnd),
          (kind: otSpace, strVal: " "),
          (kind: otBoldInline, strVal: "**"),
          (kind: otBigIdent, strVal: "IN"),
          (kind: otBoldInline, strVal: "**"),
          (kind: otWord, strVal: "line"),
          (kind: otSpace, line: 0, column: 29, offset: 29, finish: 29, strVal: " "),
          (kind: otWord, line: 0, column: 30, offset: 30, finish: 33, strVal: "bold")
        ]

  test "Lex structure":
    let lex = lexOrgStructure
    block title:
      let tokens = lexAll(varStr "#+title: *bold*", lexOrgStructure)
      var textStr = initPosStr(tokens[^1])
      let textTokens = lexAll(textStr, lexOrgText)

      check:
        matchdiff tokens, [
          (kind: osCommandPrefix, strVal: "#+", column: 0),
          (kind: osIdent, strVal: "title", column: 2),
          (kind: osColon, strVal: ":", column: 7),
          (kind: osText, strVal: "*bold*", column: 9)
        ]

        matchdiff textTokens, [
          (kind: otBoldStart, column: 9, line: 0, offset: 9, finish: 9, strVal: "*"),
          (kind: otWord, column: 10, line: 0, offset: 10, finish: 13, strVal: "bold"),
          (kind: otBoldEnd, column: 14, line: 0, offset: 14, finish: 14, strVal: "*")
        ]

  test "Lex indented list":
    var state = newLexerState(0u8)
    proc lexerImpl(str: var PosStr): seq[HsTok[OrgStructureToken]] =
      if not ?str:
        result.add str.initTok(osEof)

      else:
        case str[]:
          of '-':
            let indent = str.column
            result.add str.initTok(
              str.asSlice str.skipWhile({'-', ' '}), osListStart)

            str.startSlice()
            var atEnd = false
            while ?str and not atEnd:
              str.skipPastEol()
              if str.getIndent() < indent:
                atEnd = true

              else:
                var store = str
                store.skipWhile({' '})
                if store["- "]:
                  atEnd = true

            result.add str.initTok(str.popSlice(), osText)

          of '\n':
            for level in 0 ..< state.getIndentLevels():
              result.add str.initTok(osDedent)

            str.next()
            state.setIndent(0)

          of ' ':
            let indent = state.skipIndent(str)
            result.add():
              case indent:
                of likIncIndent:  str.initTok(osIndent)
                of likDecIndent:  str.initTok(osDedent)
                of likEmptyLine:  str.initTok(osSameIndent)
                of likSameIndent: str.initTok(osSameIndent)
                of likNoIndent:   str.initTok(osNoIndent)

          else:
            raise newUnexpectedCharError(str)


    block single_list_item:
      state.clear()
      let tokens = lexAll(varStr("- list"), lexerImpl)
      check:
        matchdiff tokens, [
          (kind: osListStart),
          (kind: osText)
        ]

    block indented_list_item:
      state.clear()
      let str = lit3"""
      - list
        - indented
      """

      let tokens = lexAll(varStr(str), lexerImpl)
      check:
        matchdiff tokens, [
          (kind: osListStart),
          (kind: osText),
          (kind: osIndent),
          (kind: osListStart),
          (kind: osText)
        ]

    block indented_two_items:
      state.clear()
      let str = lit3"""
      - list
        - indented
        - second item
      """

      let tokens = lexAll(varStr(str), lexerImpl)
      check:
        matchdiff tokens, [
          (kind: osListStart),
          (kind: osText),
          (kind: osIndent),
          (kind: osListStart),
          (kind: osText),
          (kind: osSameIndent),
          (kind: osListStart),
          (kind: osText)
        ]

    block indented_two_items:
      state.clear()
      let str = lit3"""
      - list
        - indented
        - second item

      - list2
        - indented2
        - second item2
      """

      let tokens = lexAll(varStr(str), lexerImpl)

      check:
        matchdiff @(kind, strVal), [
          tokens: [
            #[ 00 ]# (osListStart,   "- "),
            #[ 01 ]# (osText,        "list\n"),
            #[ 02 ]# (osIndent),
            #[ 03 ]# (osListStart,   "- "),
            #[ 04 ]# (osText,        "indented\n"),
            #[ 05 ]# (osSameIndent),
            #[ 06 ]# (osListStart,   "- "),
            #[ 07 ]# (osText,        "second item\n"),
            #[ 08 ]# (osDedent),

            #[ 09 ]# (osListStart,   "- "),
            #[ 10 ]# (osText,        "list2\n"),
            #[ 11 ]# (osIndent),
            #[ 12 ]# (osListStart,   "- "),
            #[ 13 ]# (osText,        "indented2\n"),
            #[ 14 ]# (osSameIndent),
            #[ 16 ]# (osListStart,   "- "),
            #[ 17 ]# (osText,        "second item2\n")
          ]
        ]


  test "Lex subtree trailing tags":
    proc lexSubtree(str: var PosStr): seq[HsTok[OrgStructureToken]] =
      if not ?str:
        result.add str.initTok(osEof)

      else:
        case str[]:
          of '*':
            result.add str.initTok(
              str.asSlice str.skipWhile({'*'}), osSubtreeStars)

            str.skip({' '})
            var body = str.asSlice str.skipToEol()

            var headerTokens: seq[HsTok[OrgStructureToken]]

            body.gotoEof()
            if body[':']:
              let finish = body.getPos()
              body.back()

              var tagEnded = false
              while ?body and not tagEnded:
                while ?body and body[IdentChars]:
                  body.back()

                body.skipBack({':'})
                if body[' ']:
                  tagEnded = true

              let start = body.getPos(+1)
              headerTokens.add body.initTok(
                str.sliceBetween(start, finish), osSubtreeTag)

            while body[' ']:
              body.back()

            if body[']']:
              let finish = body.getPos()
              body.skipBack({']'})
              body.skipBack(Digits)
              while body[Digits]:
                body.back()

              if str['%']:
                body.back()

              else:
                body.skipBack({'/'})
                body.skipBack(Digits)
                while body[Digits]:
                  body.back()

              body.skipBack({'['})

              let start = body.getPos(+1)

              headerTokens.add body.initTok(
                str.sliceBetween(start, finish), osCompletion)

              while body[' ']:
                body.back()

            block:
              let finish = body.getPos()
              body.goToSof()
              let start = body.getPos()

              headerTokens.add body.initTok(
                str.sliceBetween(start, finish), osText)




            result.add headerTokens.reversed()

          else:
            raise newUnexpectedCharError(str)

    block no_tag_subtree:
      let tokens = lexAll(varStr "* header", lexSubtree)
      check:
        matchdiff tokens, [
          (kind: osSubtreeStars, strVal: "*"),
          (kind: osText, strVal: "header")
        ]

    block tagged_subtree:
      let tokens = lexAll(varStr "* header :tag:", lexSubtree)
      check:
        matchdiff tokens, [
          (kind: osSubtreeStars, strVal: "*"),
          (kind: osText, strVal: "header"),
          (kind: osSubtreeTag, strVal: ":tag:")
        ]

    block tagged_subtree_with_completion_status:
      let tokens = lexAll(varStr "* header [0/10] :tag:", lexSubtree)
      check:
        matchdiff tokens, [
          (kind: osSubtreeStars, strVal: "*"),
          (kind: osText, strVal: "header"),
          (kind: osCompletion, strVal: "[0/10]"),
          (kind: osSubtreeTag, strVal: ":tag:")
        ]

import std/macros

suite "String scanning DSL":
  template testScan(s: string, args: varargs[untyped]): untyped =
    block:
      var str = varStr(s)
      unpackVarargs(scanSlice, str, args)

  test "scanSlice":
    check:
      testScan("0", '0').strVal() == "0"
      testScan("00", *'0').strVal() == "00"

  test "Scan escapes":
    check:
      testScan("a\n\n\nb", 'a', *\n, 'b').strVal() == "a\n\n\nb"
      testScan("<___>", '<', @{'>'}, '>').strVal() == "<___>"
      testScan("A", \N).strVal() == "A"

  test "Scan until":
    check:
      testScan("RADIO_TARGET>>>", @">>>").strVal() == "RADIO_TARGET"

testFileEnded()
