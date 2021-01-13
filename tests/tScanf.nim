import unittest
import hmisc/algo/tscanf
import hmisc/helpers

import parseutils, strutils, sequtils

suite "tscanf test":
  test "Parse integer":
    if tscanf("12", "$i"):
      doAssert ml[0] is int
      assertEq ml[0], 12
    else:
      doAssert false, "Failed to parse"

  test "Parse string":
    if tscanf("****", "$+"):
      doAssert ml[0] is string
      assertEq ml[0], "****"
    else:
      doAssert false, "Failed to parse"

  test "Declared types assertion":
    if tscanf("-1, ))-", "-$i, $+-"):
      doAssert ml[0] is int
      doAssert ml[1] is string
      assertEq ml[0], 1
      assertEq ml[1], "))"
    else:
      static: doAssert not declared(ml)

  test "Parse string sequence":
    proc matcher1(s: string, arg: var seq[int], start: int): int =
      var tok: string
      let endpos = parseWhile(s, tok, {'0' .. '9', ' ', ','}, start)
      arg = tok
        .split({',', ' '})
        .filterIt(it.len != 0)
        .mapIt(
          it.strip(chars = {',', '0'}).parseInt()
        )

      return endpos

    if tscanf("12x12---%1,    1,1,1", "$ix$i$+%${matcher1}"):
      doAssert declared(ml)
      doAssert type(ml[3]) is seq[int]
      assertEq ml[3], @[1, 1, 1, 1]
    else:
      doAssert not declared(ml)
      echo "does not match"

    doAssert not declared(ml)
