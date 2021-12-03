import hmisc/preludes/unittest
import std/[strformat, sequtils, strutils]

testFileStarted()

import
  hmisc/algo/[clformat, clformat_interpolate]

suite "Clformat colored formatting":
  test "hshow string":
    show:
      hshow("")
      hshow("test")
      hshow("\n\n\n\n")
      hshow("test\n\n\n\n")

  test "Colored text formatting":
    let text = clfmt("""
-- {"12\n12":,indent,broadcast}
?? {"12\n12\n12":,indent}
""", true)

    show text

suite "Parser correctness":
  test "String literals":
    check:
      $clfmt("{\"--\"}") == "--"
      $clfmt("{\"''\"}") == "''"
      $clfmt("{\"?\":<2}") == "? "
      fmt("{12 + 2:<4}") == "14  "
      $clfmt("{12 + 2:<4}") == "14  "

suite "Simple variable interpolation":
  test "From scope":
    let nice = "88"
    check:
      $clfmt("{nice}") == "88"
      $clfmt("-{nice}-") == "-88-"
      $clfmt("-{{nice}}-") == "-{nice}-"
      ## `"9"` is a string literal with value `9`
      $clfmt("""--{"9"}--""") == "--9--"

    test "Non-strings":
      let val = 88
      check:
        $clfmt("{nice}") == "88"
        $clfmt("{12 + 2}") == "14"
        $clfmt("{(121, 22)}") == "(121, 22)"

    test "Formatting":
      check:
        $clfmt("{12:<4}") == "12  "
        $clfmt("{12:>4}") == "  12"
        $clfmt("{12:^4}") == " 12 "

suite "CL formatting directives":
  test "Single value justification":
    ## Justify left 4 chars
    assert $clfmt("{12:<4}") == "12  "
    ## Justifty right 4 chars
    assert $clfmt("{12:>4}") == "  12"

    ## Interpolate varable, justify 4 chars
    let val = "00"
    assert $clfmt("{val:<4}") == "00  "

  test "Integer formatting":
    skip()
    check:
      # $clfmt("{1000:,sep='#'}")             == "1#000"
      # $clfmt("{1000:<6, sep='-', pad='_'}") == "_1-000"
      # $clfmt("{1000:<7, sep='.', sign}")    == " +1.000"
      $clfmt("{10:,roman}")                 == "X"
      $clfmt("{1000:,eng}")                 == "1K"
      # $clfmt("{1000:,scientific}")          == "1+3E" # ?
      $clfmt("{1000:,word=file, text}")     == "one thousand files"
      $clfmt("{12:,text,word=test}")             == "twelve tests"
      $clfmt("{1000:,eng, word=file}")      == "1K files"
      mapIt(@[1,2], $clfmt("{it:,word=element, text}")).join("; ") ==
        "one element; two elements"

  test "Text formatting":
    let str = "Hello woRld"
    check:
      $clfmt("{str:,dashed, lower}") == "hello-world"
      $clfmt("{str:,snake, lower}")  == "hello_world"
      $clfmt("{str:,upper}")         == "HELLO WORLD"
      $clfmt("{str:,lower}")         == "hello world"
      $clfmt("{str:,dashed, upper}") == "HELLO-WORLD"

  test "Alignments":
    let fmt = $clfmt("""
[ {   12:<,word=test}] executed
[ {    9:<,word=test}] passed
[ {    3:<,word=test}] failed""")

    check:
      $clfmt("{12:<}") == "12    "
      strdiff(fmt, """
[ 12 tests           ] executed
[ 9 tests            ] passed
[ 3 tests            ] failed""")

  test "Character formatting":
    check:
      $clfmt("{']':,named}") == "right square bracket"
      $clfmt("{']':,short-named}") == "rBrack"
      $clfmt("{\"--\":,short-named,camel,joined}") == "doubleMinus"

suite "Colored & unicode fancy output":
  show:
    $clfmt("{90:,fg-red}")
    $clfmt("{\"some-value\":,fg-green}")
    $clfmt("{\"--\":,fg-green,bg-yellow,attr-bold}")

testFileEnded()
