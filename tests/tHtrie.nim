discard """
joinable: false
"""

import hmisc/preludes/unittest

testFileStarted()


import
  std/[sugar, strutils, sequtils, strformat, algorithm]

import
  hmisc/preludes/unittest,
  hmisc/types/htrie,
  hmisc/algo/halgorithm


suite "Main":
  test "Add path to trie :object:":
    var tr: Trie[int, int]
    tr[[1, 2, 3]] = 190
    check tr.paths == @[@[1, 2, 3]]

  test "Multiple trie paths :object:":
    var tr: Trie[int, char]
    tr[[2, 4, 90, 9]] = '9'
    tr[[8, 2, 3, 3]] = '%'
    tr[[1, 2, 5, 6]] = '&'
    check tr.paths.len() == 3

  test "Has value checkions :object:":
    var tr: Trie[int, int]
    tr[[2, 3, 4]] = 9
    check tr.prefixHasValue([2, 3, 4])
    check tr.prefixHasValue([2, 3, 4, 5])
    check not tr.prefixHasValue([1, 2, 3])
    check not tr.prefixHasValue([2, 3])

  test "{prefixHasValue} :object:generic:proc:":
    var tr: Trie[int, int]
    tr[[0, 0]] = 999
    tr[[1, 2]] = 888
    check not tr.prefixHasValue([0, 1, 2])
    tr[[0, 1, 2]] = 777
    check tr.prefixHasValue([0, 1, 2])


  test "{paths} Set value on path using array :value:":
    var tr: Trie[int, int]
    tr[[0, 2]] = 1488
    check tr.paths() == @[@[0, 2]]
    check tr.paths().len == 1
    check tr[@[0, 2]] == 1488

  test "{paths} Path of length 1 :value:":
    var tr: Trie[int, int]
    tr[@[0]] = 1488
    check tr[@[0]] == 1488
    check tr.paths() == @[@[0]]
    check tr.paths().len == 1

  test "{merge} Empty trie with filled :value:":
    var tr1: Trie[int, int]
    var tr2: Trie[int, int]

    tr2[[1, 2, 3]] = 888
    tr2[[2, 2, 3]] = 888
    tr1.merge tr2

    check tr1[[1, 2, 3]] == 888
    check tr1[[2, 2, 3]] == 888


  test "{merge} two trees :value:":
    var tr1: Trie[int, int]
    var tr2: Trie[int, int]

    tr2[[1, 2, 3]] = 888
    tr2[[2, 2, 3]] = 898
    tr1[[1, 9]] = 8
    tr1.merge tr2

    check tr1[[1, 2, 3]] == 888
    check tr1[[2, 2, 3]] == 898
    check tr1[[1, 9]] == 8

  test "{prefixedValues}":
    var tr: Trie[int, int]
    tr[[0, 1, 2]] = 9
    tr[[0, 1]] = 8
    tr[[0]] = 7

    check toSeq(tr.prefixedValues([0, 1, 2])) == @[7, 8, 9]
    check toSeq(tr.prefixedValues([0, 1, 2], topDown = false)) == @[9, 8, 7]


testFileEnded()