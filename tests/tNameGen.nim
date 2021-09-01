
import hmisc/preludes/unittest

testFileStarted()

import std/[sugar, strutils, sequtils, strformat]
import hmisc/algo/namegen
import hmisc/core/all


suite "Name generation":
  test "Repeated identifier fix":
    var c: StringNameCache

    check fixIdentName("iTem", "f", c, true) == "fITem"
    check fixIdentName("iTem", "f", c, true) == "fITem"
    check fixIdentName("item", "f", c, true) == "ffItem"
    check fixIdentName("item", "f", c, true) == "ffItem"

  test "Name gen":
    var c: StringNameCache
    check:
      fixIdentName("__pthread_unwind_buf_t", "", c) == "pthread_unwind_buf_t"
      fixIdentName("_pthread_unwind_buf_t", "f", c) == "fpthread_unwind_buf_t"

    expect ArgumentError as err:
      discard fixIdentName("___pthread_unwind_buf_t", "", c)

    check:
      "'pthread_unwind_buf_t'" in err.msg


    check:
      fixIdentName("___pthread_unwind_buf_t", "f", c) ==
        "ffpthread_unwind_buf_t"

testFileEnded()
