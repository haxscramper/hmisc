import
  hmisc/preludes/unittest,
  hmisc/algo/hparse_pegs

import std/[strutils, strtabs]

suite "Base feature tests":
  test "Compile & runtime tests":
    proc pegsTest() =
      assert escapePeg("abc''def'") == r"'abc'\x27\x27'def'\x27"
      assert match("(a b c)", peg"'(' @ ')'")
      assert match("W_HI_Le", peg"\y 'while'")
      assert(not match("W_HI_L", peg"\y 'while'"))
      assert(not match("W_HI_Le", peg"\y v'while'"))
      assert match("W_HI_Le", peg"y'while'")

      assert($ +digits == $peg"\d+")
      assert "0158787".match(peg"\d+")
      assert "ABC 0232".match(peg"\w+\s+\d+")
      assert "ABC".match(peg"\d+ / \w+")

      var accum: seq[string] = @[]
      for word in split("00232this02939is39an22example111", peg"\d+"):
        accum.add(word)
      assert(accum == @["this", "is", "an", "example"])

      assert matchLen("key", ident) == 3

      var pattern = sequence(ident, *whitespace, term('='), *whitespace, ident)
      assert matchLen("key1=  cal9", pattern) == 11

      var ws = newNonTerminal("ws", 1, 1)
      ws.rule = *whitespace

      var expr = newNonTerminal("expr", 1, 1)
      expr.rule = sequence(capture(ident), *sequence(
                    nonterminal(ws), term('+'), nonterminal(ws), nonterminal(expr)))

      var c: Captures
      var s = "a+b +  c +d+e+f"
      assert rawMatch(s, expr.rule, 0, c) == len(s)
      var a = ""
      for i in 0..c.ml-1:
        a.add(substr(s, c.matches[i][0], c.matches[i][1]))
      assert a == "abcdef"
      #echo expr.rule

      #const filename = "lib/devel/peg/grammar.txt"
      #var grammar = parsePeg(newFileStream(filename, fmRead), filename)
      #echo "a <- [abc]*?".match(grammar)
      assert find("_____abc_______", term("abc"), start = 2) == 5
      assert match("_______ana", peg"A <- 'ana' / . A")
      assert match("abcs%%%", peg"A <- ..A / .A / '%'")

      var matches: PegMatches
      if "abc" =~ peg"{'a'}'bc' 'xyz' / {\ident}":
        assert matches[0] == "abc"
      else:
        assert false

      var g2 = peg"""S <- A B / C D
                     A <- 'a'+
                     B <- 'b'+
                     C <- 'c'+
                     D <- 'd'+
                  """
      assert($g2 == "((A B) / (C D))")
      assert match("cccccdddddd", g2)
      assert("var1=key; var2=key2".replacef(peg"{\ident}'='{\ident}", "$1<-$2$2") ==
             "var1<-keykey; var2<-key2key2")
      assert("var1=key; var2=key2".replace(peg"{\ident}'='{\ident}", "$1<-$2$2") ==
             "$1<-$2$2; $1<-$2$2")
      assert "var1=key; var2=key2".endsWith(peg"{\ident}'='{\ident}")

      if "aaaaaa" =~ peg"'aa' !. / ({'a'})+":
        assert matches[0] == "a"
      else:
        assert false

      if match("abcdefg", peg"c {d} ef {g}", matches, start = 2):
        assert matches[0] == "d"
        assert matches[1] == "g"
      else:
        assert false

      accum = @[]
      for x in findAll("abcdef", peg".", start = 3):
        accum.add(x)
      assert(accum == @["d", "e", "f"])

      for x in findAll("abcdef", peg"^{.}", start = 3):
        assert x == "d"

      if "f(a, b)" =~ peg"{[0-9]+} / ({\ident} '(' {@} ')')":
        assert matches[0] == "f"
        assert matches[1] == "a, b"
      else:
        assert false

      assert match("eine übersicht und außerdem", peg"(\letter \white*)+")
      assert match("eine übersicht und auerdem", peg"(\lower \white*)+")
      assert match("EINE ÜBERSICHT UND AUSSERDEM", peg"(\upper \white*)+")
      assert(not match("456678", peg"(\letter)+"))

      assert("var1 = key; var2 = key2".replacef(
        peg"\skip(\s*) {\ident}'='{\ident}", "$1<-$2$2") ==
             "var1<-keykey;var2<-key2key2")

      assert match("prefix/start", peg"^start$", start = 7)

      if "foo" =~ peg"{'a'}?.*":
        assert matches[0].len == 0

      else:
        assert false

      if "foo" =~ peg"{''}.*":
        assert matches[0] == ""

      else:
        assert false

      if "foo" =~ peg"{'foo'}":
        assert matches[0] == "foo"

      else:
        assert false

      let empty_test = peg"^\d*"
      let str = "XYZ"

      assert(str.find(empty_test) == 0)
      assert(str.match(empty_test))

      proc handleMatches(m: int, n: int, c: PegMatches): string =
        if m > 0:
          result.add ", "

        result.add case n:
          of 2: toLowerAscii(c[0]) & ": '" & c[1] & "'"
          of 1: toLowerAscii(c[0]) & ": ''"
          else: ""

      assert("Var1=key1;var2=Key2;   VAR3".
        replace(peg"{\ident}('='{\ident})* ';'* \s*",
        handleMatches) == "var1: 'key1', var2: 'Key2', var3: ''")


      doAssert "test1".match(peg"""{@}$""")
      doAssert "test2".match(peg"""{(!$ .)*} $""")
      doAssert "a".replace(peg"'a'", "C") == "C"
      doAssert "aa".replace(peg"'a'", "C") == "CC"

      if "key=value" =~ peg"\s* {\w+} \s* '=' \s* {\w+}":
        # matches a key=value pair:
        assert matches[0] == "key"
        assert matches[1] == "value"

      else:
        assert false


    pegsTest()
    static:
      pegsTest()

  test "Injected matches":
    if injectMatch(
      "ABA",
      peg"{_}'B'${firstParamType}",
      proc(s: string): string =
        case s:
          of "firstParamType": "A"
          else: raise newUnexpectedKindError(s)
    ):

      assert matches[0] == "A"

  test "Interpolated replacement":
    check replaceInterpol(
      "A_B",
      peg"{_}_{_}",
      "${toLower}+${toUpper}") == "a+B"

    proc impl(
      str, pattern, res: string,
      env: openarray[(string, string)] = @[]
    ): string =

      let env = newStringTable(env)
      replaceInterpol(
        str,
        peg(pattern),
        res,
        env = proc(s: string): string = env[s])

    check:
      impl("A", "'A'", "B") == "B"
      impl("wave_context", "${lib} '_'* {_*}", "$1", {"lib": "wave"}) == "context"

      impl(
        "git_commit_graph_needs_refresh",
        "${lib} '_'* (${arg0:1} / ${arg0:0}) '_'* {_*}",
        "$1", {
          "lib": "git",
          "arg0:0": "commit",
          "arg0:1": "commit_graph"
        }) == "needs_refresh"

      impl(
        "git_commit_graph_needs_refresh",
        "${lib} '_'* (Y${arg0:1} / Y${arg0:0}) '_'* {_*}",
        "$1", {
          "lib": "git",
          "arg0:0": "Commit",
          "arg0:1": "CommitGraph"
        }) == "needs_refresh"



suite "Event parser DSL":
  test "documentation example":
    let
      pegAst = peg"""
Expr    <- Sum
Sum     <- Product (('+' / '-')Product)*
Product <- Value (('*' / '/')Value)*
Value   <- [0-9]+ / '(' Expr ')'"""
      txt = "(5+3)/2-7*22"

    var
      pStack: seq[string] = @[]
      valStack: seq[float] = @[]
      opStack = ""

    let
      parseArithExpr = pegAst.eventParser:
        pkNonTerminal:
          enter:
            pStack.add p.nt.name
          leave:
            pStack.setLen pStack.high
            if length > 0:
              let matchStr = s.substr(start, start+length-1)
              case p.nt.name:
                of "Value":
                  try:
                    valStack.add matchStr.parseFloat
                  except ValueError:
                    discard

                of "Sum", "Product":
                  try:
                    let val = matchStr.parseFloat
                  except ValueError:
                    if valStack.len > 1 and opStack.len > 0:
                      valStack[^2] = case opStack[^1]
                      of '+': valStack[^2] + valStack[^1]
                      of '-': valStack[^2] - valStack[^1]
                      of '*': valStack[^2] * valStack[^1]
                      else: valStack[^2] / valStack[^1]
                      valStack.setLen valStack.high
                      opStack.setLen opStack.high

        pkChar:
          leave:
            if length == 1 and "Value" != pStack[^1]:
              let matchChar = s[start]
              opStack.add matchChar

    let pLen = parseArithExpr(txt, nil)
