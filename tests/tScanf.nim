import unittest
import hmisc/algo/tscanf
import hmisc/helpers

import parseutils, strutils, sequtils

suite "tscanf test":
  test "Parse integer":
    if tscanf("12", "$i"):
      assert ml[0] is int
      assertEq ml[0], 12
    else:
      assert false, "Failed to parse"

  test "Parse string":
    if tscanf("****", "$+"):
      assert ml[0] is string
      assertEq ml[0], "****"
    else:
      assert false, "Failed to parse"

  test "Declared types assertion":
    if tscanf("-1, ))-", "-$i, $+-"):
      assert ml[0] is int
      assert ml[1] is string
      assertEq ml[0], 1
      assertEq ml[1], "))"
    else:
      static: assert not declared(ml)

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
      assert declared(ml)
      assert type(ml[3]) is seq[int]
      assertEq ml[3], @[1, 1, 1, 1]
    else:
      assert not declared(ml)
      echo "does not match"

    assert not declared(ml)
