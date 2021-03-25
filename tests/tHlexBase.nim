import std/[sugar, strutils, sequtils, strformat]
import std/[unittest]
import hmisc/algo/hlex_base

suite "Hlex base":
  test "test":
    var str = initPosStr("""
of true:
  sliceIdx*: int
  baseString*: ptr string
  slices*: seq[PosStrSlice]
""")

    str.skipToEol()
    let indent = str.getIndent()
    echo indent
    while str.hasIndent(indent):
      str.advance(indent)
      str.startSlice()
      str.skipToEol()
      str.finishSlice()

    var subStr = initPosStr(str)

    echo subStr
