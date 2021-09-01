
import hmisc/preludes/unittest

testFileStarted()

import std/[sugar, strutils, sequtils, strformat]
import hmisc/algo/namegen
import hmisc/core/all


suite "Name generation":
  test "Nim norm":
    check:
      nimNorm("____a") == "a"
      nimNorm("_A") == "A"
      nimNorm("Aa__aaaA") == "Aaaaaa"

  test "Repeated identifier fix":
    var c: StringNameCache

    check:
      fixIdentName("iTem", "f", c, true) == "fITem"
      fixIdentName("iTem", "f", c, true) == "fITem"
      fixIdentName("item", "f", c, true) == "ffItem"
      fixIdentName("item", "f", c, true) == "ffItem"

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

  test "New name":
    var c: StringNameCache
    check:
      c.newName("test") == "test"
      c.newName("tesT") == "tesT1"

      ## Explicitly asking to contruct new name each time, count will be
      ## incremented.
      c.newName("tEst") == "tEst2"
      c.newName("tEst") == "tEst3"

      c.getName("test") == "test"
      c.getName("tesT") == "tesT1"

      ## Asking to *get* new name for an ident - fist time new one will be
      ## constructed, subsequent calls will use the same results.
      c.getName("teSt") == "teSt4"
      c.getName("teSt") == "teSt4"

testFileEnded()
