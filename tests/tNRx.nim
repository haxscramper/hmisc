import std/[sugar, strutils, sequtils, strformat, streams]
import hmisc/other/[rx]
import hmisc/algo/hlex_base
import hmisc/preludes/unittest


import hmisc/preludes/unittest

testFileStarted()

suite "NRX":
  test "test":
    check toStr(*nrx("hello")) == r"hello*"
    check toStr(*nrx({'0' .. '9'})) == r"[0123456789]*"
    check toStr(*nrx('0', '9')) == r"[0-9]*"
    let r1 = nrx("wer") | nrx("wor")
    check toStr(?r1) == "(wer|wor)?"
    show:
      treeRepr(?r1)
      lispRepr(r1)

      toConstStr(nrx("Hello"))

      toStr(group(
        nrx("Hello"),
        group(nrx('a') | nrx('b'))
      ))

    block:
      show "aa" =~ group(nrx("aa"))
      check matches[0] == ""

    block:
      show "aa" =~ capture(nrx("aa"))
      check matches[0] == "aa"

    block:
      const r = toConstStr(nrx("text"))
      # const r = re(toConstStr(nrx("text")))


suite "NRX with `PosStr`":
  test "match expression":
    doAssert initPosStr("test") =~ re"test"
    doAssert initPosStr("zzz") =~ (nrx("test") | nrx("zzz"))

  test "Cut from input":
    let str = "123--456"
    for str1 in [initPosStr(str), initPosStr(newStringStream(str))]:
      var str1 = str1
      doAssert str1.cut(*nrx('0', '9')) == "123"
      doAssert str1["--"]
      doAssert str1.cut(re"..") == "--"
      doAssert str1.cut(*nrx('0', '9')) == "456"


testFileEnded()
