import std/[unittest]

import hmisc/algo/clformat

template `==`(rhs, lhs: untyped): untyped =
  assertEq(rhs, lhs)


suite "Clformat colored formatting":
  test "hshow string":
    echo hshow("")
    echo hshow("test")
    echo hshow("\n\n\n\n")
    echo hshow("test\n\n\n\n")

when false:
  suite "Parser correctness":
    test "String literals":
      assert hfmt("{\"--\"}") == "--"
      assert hfmt("{\"''\"}") == "''"
      assert hfmt("{\":\":<2}") == ": "

    test "Expressions":
      assert hfmt("{12 + 2:<4}") == "14  "

    test "Format specifier arguments":
      assert hfmt("{12:wrap=[]}") == "[12]"
      assert hfmt("{12:wrap=()}") == "(12)"
      assert hfmt("{12: wrap = ()}") == "(12)"

      expect CodeError:
        parseHFormat "{12: wrap=( )}"


      assert hfmt("{12: wrap = ('(', '}') }") == "(12}"

  suite "Simple variable interpolation":
    test "From scope":
      let nice = "88"
      assert hfmt("{nice}") == "88"
      assert hfmt("-{nice}-") == "-88-"
      assert hfmt("-{{nice}}-") == "-{nice}-"
      ## `"9"` is a string literal with value `9`
      assert hfmt("""--{"9"}--""") == "--9--"

    test "Non-strings":
      let val = 88
      assert hfmt("{nice}") == "88"
      assert hfmt("{12 + 2}") == "14"
      assert hfmt("{(121, 22)}") == "(121, 22)"

    test "Formatting":
      assert hfmt("{12:<4}") == "12  "
      assert hfmt("{12:>4}") == "  12"
      assert hfmt("{12:^4}") == " 12 "


  suite "Interpolation from JSON":
    test "Multiple arguments":
      assert hjfmt("{}, {}", %12, %14) == "12, 14"
      assert hjfmt("{$1} {$1}", %12) == "12 12"
      assert hjfmt("{$2}, {$1}", %90, %88) == "88, 90"
      assert hjfmt("{{$1}} {{$90}}", %"--", %"--") == "{$1} {$90}"

    test "From array":
      hjfmt("{[0]} {[1]}", %[12, 2]) == "12 2"


  suite "Conditional interpolation":
    assert hfmt("{if! {12 == 12}{\"yes\"}{\"no\"} }") == "yes"
    assert hfmt("{if! {false}{12}{20}}") == "20"
    assert hfmt("""{if! {false}}
  ---
  {else!}
  ===
  {endif!}""") == "---\n"

  suite "CL formatting directives":
    test "Single value justification":
      ## Justify left 4 chars
      assert hfmt("{12:<4}") == "12  "
      ## Justifty right 4 chars
      assert hfmt("{12:>4}") == "  12"

      ## Interpolate varable, justify 4 chars
      let val = "00"
      assert hfmt("{val:<4}") == "00  "

    test "Multiple value list justification":
        ## Evenly distribute list of three elements, justify elements to
        ## nine chars totally, each element is justified left
        assert hfmt("{[{1}, {1 + 1}, {1 + 1 + 1}]:9<}") == "1  2  3  "
        assert hfmt("{[{1}, {2}, {3}]:<9}") == "1  2  3  "

        ## Interpolate sequence and justify each element
        let seqv = @[1, 2, 3]
        assert hfmt("{{each! {seqv}}:<9}") == "1  2  3  "

        ## Justify first element right to four characters and all others left

        let val = hfmt("{seqv[0]:>4} =|= {{each! seqv[1..2]}:<6}")
        assert cval == "   1 =|= 2  3  "

        ## The string above is not really readable (what a surprise!),
        ## but it is possible to insert arbirary number of newlines and
        ## spaces inside 'control' part of the string as well as build
        ## it using concatenation, so following templates should
        ## generate the same string

        assert hfmt(
          "{seqv[0]:>4} =|= " &
            "{{each! seqv[1..2]}:<6}") == val

        ## Though ' =|= ' part cannot be but on it's own line without
        ## additional quoting:
        assert hfmt("""
      {seqv[0]:>4} =|= {
        {each! seqv[1..2]}:<6
      }
      """) == val


        ## wow, such readability, much declarative! (FFS, at this point
        ## you should probably use regular nim loop anyway)
        assert hfmt ("""
      {
        seqv[0]:>4
      }{
        " =|= "
      }{
        {each! seqv[1..2]}:<6
      }
      """) == val

    test "Integer formatting":
      assert hfmt("{1000::}") == "1.000"
      assert hfmt("{1000:@}") == "+1000"
      assert hfmt("{1000::@}") == "+1.000"
      assert hftm("{1000:pos-sign,sep}") == hftm("1000::@")
      assert hftm("{1000:sep='#'}") == "1#000"
      assert hftm("{1000:<6, sep='-', pad='_'}") == "_1-000"
      assert hftm("{1000::@<6}") == " +1.000"
      assert hftm("{10:roman}") == "X"
      assert hftm("{1000:engineering}") == "1K"
      assert hftm("{1000:scientific}") == "1+3E" # ?
      assert hftm("{1000:plural=file, text}") == "one thousand files"
      assert hftm("{1000:engineering, plural=file}") == "1K files"
      let data = @[1,2]
      assert hftm("{join! {each! data {:plural=element, text}}{\"; \"}}") ==
        "one element; two elements"

      assert data.mapIt(hftm("{it:plural=element, text}")).join("; ") ==
        "one element; two elements"

    test "Text formatting":
      let str = "Hello woRld"
      assert hfmt("{str:camel}") == "helloWorld"
      assert hfmt("{str:dashed}") == "hello-world"
      assert hfmt("{str:snake}") == "hello_world"
      assert hfmt("{str:pascal}") == "HelloWorld"
      assert hfmt("{str:camel}") == "helloWorld"
      assert hfmt("{str:upper}") == "HELLO WORLD"
      assert hfmt("{str:lower}") == "hello world"
      assert hftm("{str:joined}") == "HelloWoRld"
      assert hftm("{str:capitalized}") == "Hello World"
      assert hftm("{str:dashed, upper}") == "HELLO-WORLD"

    test "Character formatting":
      assert hfmt("{']':named}") == "Right Square Brace"
      assert hfmt("{']':short-named}") == "RBrack"
      assert hfmt("{\"---\":short-named,lower,nojoin}") == "triple Dash"
      assert hftm("{\"--\":short-named,camel,joined}") == "doubleDash"

    test "String wrapping":
      assert hfmt("{\"some string\":wrap=[]}") == "[some string]"

  suite "Json field access":
    test "String formatting":
      assert hjfmt("{:camel}", %"hello world") == "helloWorld"
      assert hjfmt("{:dashed}", %"hello woRLD") == "hello-world"

    test "Array formatting":
      assert hjformat("|{each!:<9|", %["1", "2", "3"]) == "|1  2  3  |"
      assert hjformat("[{each!:^9}]", %["1", "2", "3"]) == "[ 1  2  3 ]"
      assert hjformat("[{each!:^11, sep='|'}]", %["1", "2", "3"]) ==
        "[ 1 | 2 | 3 ]"

      let jdata = %*[[1,2,3], [4,5,6], [7,8,9]]
      let val = hjformat("{each!: {each!: ['[', {:^3}, ']'] sep='|'}}", jdata)

      assert val  == """
[ 1 | 2 | 3 ]
[ 4 | 5 | 6 ]
[ 7 | 8 | 9 ]
"""
      assert val == jdata.mapIt(it.mapIt(&"[{it:^3}]"))


  suite "Colored & unicode fancy output":
    assert hfmt("{90:fg-red}") == "\033[39m90\033[0m"
    hfmt("{\"some-value\":fg-green}")
    hfmt("{\"--\":fg-green,bg-yellow,attr-bold}")
