
import hmisc/preludes/unittest

testFileStarted()

import std/[sugar, strutils, sequtils, strformat]
import hmisc/algo/[namegen, hstring_algo]
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
      c.fixIdentName("iTem", "f", true) == "fITem"
      c.fixIdentName("iTem", "f", true) == "fITem"
      c.fixIdentName("item", "f", true) == "ffItem"
      c.fixIdentName("item", "f", true) == "ffItem"

  test "Leading/trailing underscores":
    var c: StringNameCache
    check:
      c.fixTypeName("S", "f") == "S"
      c.fixTypeName("_S", "f") == "FS"
      c.fixTypeName("s", "f") == "FFS"

  test "Lowercase prefix":
    var c: StringNameCache
    check:
      c.fixTypeName("s", "cxx") == "S"
      c.fixTypeName("_s", "cxx") == "CxxS"

      c.fixIdentName("S", "cxx") == "s"
      c.fixIdentName("_S", "cxx") == "cxxs"

  test "Known generated":
    var c: StringNameCache
    check:
      c.fixTypeName("S") == "S"
      c.knownGenerated("S")

      c.fixTypeName("_S") == "S1"
      c.knownGenerated("S1")

  test "Repeated encounters":
    var c: StringNameCache
    check:
      c.fixTypeName("S") == "S"
      c.fixTypeName("_S") == "S1"
      c.fixTypeName("__S") == "S2"

  test "Name gen":
    var c: StringNameCache
    check:
      c.fixIdentName("__pthread_unwind_buf_t", "") == "pthreadUnwindBufT"
      c.fixIdentName("_pthread_unwind_buf_t", "f") == "fpthreadUnwindBufT"

    expect ArgumentError as err:
      discard c.fixIdentName("___pthread_unwind_buf_t", "")

    check:
      "'pthreadUnwindBufT' ident has already been generated" in err.msg


    check:
      c.fixIdentName("___pthread_unwind_buf_t", "f") ==
        "ffpthreadUnwindBufT"

  test "Namegen with custom fixup logic":
    var c: StringNameCache
    proc fix(str: string, isType: bool): string =
      str.dropPrefix("git_").dropSuffix("_t")

    check:
      c.fixTypeName("git_submodule", fix) == "Submodule"
      c.fixTypeName("git_submodule", fix) == "Submodule"
      c.fixTypeName("git_submodule", fix) == "Submodule"

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
