import sugar, strutils, sequtils, strformat, algorithm
import hmisc/[helpers, hpprint]
import hmisc/types/htrie
import hmisc/algo/halgorithm

#================================  tests  ================================#

import unittest

suite "Main":
  test "Add path to trie :object:":
    var tr: Trie[int, int]
    tr[[1, 2, 3]] = 190
    assertEq tr.paths, @[@[1, 2, 3]]

  test "Multiple trie paths :object:":
    var tr: Trie[int, char]
    tr[[2, 4, 90, 9]] = '9'
    tr[[8, 2, 3, 3]] = '%'
    tr[[1, 2, 5, 6]] = '&'
    assert tr.paths.len() == 3

  test "Has value assertions :object:":
    var tr: Trie[int, int]
    tr[[2, 3, 4]] = 9
    assert tr.prefixHasValue([2, 3, 4])
    assert tr.prefixHasValue([2, 3, 4, 5])
    assert not tr.prefixHasValue([1, 2, 3])
    assert not tr.prefixHasValue([2, 3])

  test "{prefixHasValue} :object:generic:proc:":
    var tr: Trie[int, int]
    tr[[0, 0]] = 999
    tr[[1, 2]] = 888
    assert not tr.prefixHasValue([0, 1, 2])
    tr[[0, 1, 2]] = 777
    assert tr.prefixHasValue([0, 1, 2])


  test "{paths} Set value on path using array :value:":
    var tr: Trie[int, int]
    tr[[0, 2]] = 1488
    assertEq tr.paths(), @[@[0, 2]]
    assertEq tr.paths().len, 1
    assertEq tr[@[0, 2]], 1488

  test "{paths} Path of length 1 :value:":
    var tr: Trie[int, int]
    tr[@[0]] = 1488
    # pprint tr
    assertEq tr[@[0]], 1488
    assertEq tr.paths(), @[@[0]]
    assertEq tr.paths().len, 1

  test "{merge} Empty trie with filled :value:":
    var tr1: Trie[int, int]
    var tr2: Trie[int, int]

    tr2[[1, 2, 3]] = 888
    tr2[[2, 2, 3]] = 888
    tr1.merge tr2

    assertEq tr1[[1, 2, 3]], 888
    assertEq tr1[[2, 2, 3]], 888


  test "{merge} two trees :value:":
    var tr1: Trie[int, int]
    var tr2: Trie[int, int]

    tr2[[1, 2, 3]] = 888
    tr2[[2, 2, 3]] = 898
    tr1[[1, 9]] = 8
    tr1.merge tr2

    assertEq tr1[[1, 2, 3]], 888
    assertEq tr1[[2, 2, 3]], 898
    assertEq tr1[[1, 9]], 8

  test "{prefixedValues}":
    var tr: Trie[int, int]
    tr[[0, 1, 2]] = 9
    tr[[0, 1]] = 8
    tr[[0]] = 7

    assertEq toSeq(tr.prefixedValues([0, 1, 2])), @[7, 8, 9]
    assertEq toSeq(
      tr.prefixedValues([0, 1, 2], topDown = false)), @[9, 8, 7]
