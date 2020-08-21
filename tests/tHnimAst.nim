import sugar, strutils, sequtils, strformat
import ../src/hmisc/types/hnim_ast
import ../src/hmisc/helpers

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "HNimAst":
  test "{enumPref} :macro:":
    type
      En = enum
        en1 = 2

    assertEq enumPref(En), "en"
    let val = en1
    assertEq enumPref(val), "en"
    assertEq enumPref(en1), "en"

    proc gen[T](a: T, pr: string): void = assertEq enumPref(a), pr

    gen(en1, "en")

    type Alias = En

    var alias: Alias
    gen(alias, "en")

  test "{enumNames} :macro:":
    type
      En = enum
        en1 = "hello"
        en2 = "world"

    assertEq enumNames(En), @["en1", "en2"]
    assertEq enumNames(en1), @["en1", "en2"]
    let val = en1
    assertEq enumNames(val), @["en1", "en2"]
