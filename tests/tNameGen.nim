import std/[sugar, strutils, sequtils, strformat]
import hmisc/algo/namegen
import hmisc/core/all
import hmisc/preludes/unittest

startHax()

suite "Name generation":
  test "Repeated identifier fix":
    var c: StringNameCache

    check fixIdentName("iTem", "f", c, true) == "fITem"
    check fixIdentName("iTem", "f", c, true) == "fITem"
    check fixIdentName("item", "f", c, true) == "ffItem"
    check fixIdentName("item", "f", c, true) == "ffItem"
