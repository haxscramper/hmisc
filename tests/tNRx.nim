import std/[sugar, strutils, sequtils, strformat]
import hmisc/other/rx

import unittest

suite "NRX":
  test "test":
    echo toStr(*nrx("hello"))
    echo toStr(*nrx({'0' .. '9'}))
    echo toStr(*nrx('0', '9'))
    let r1 = nrx("wer") | nrx("wor")
    echo toStr(?r1)
    echo treeRepr(?r1)
    echo lispRepr(r1)

    echo toConstStr(nrx("Hello"))

    echo toStr(group(
      nrx("Hello"),
      group(nrx('a') | nrx('b'))
    ))

    block:
      echo "aa" =~ group(nrx("aa"))
      check matches[0] == ""

    block:
      echo "aa" =~ capture(nrx("aa"))
      check matches[0] == "aa"
