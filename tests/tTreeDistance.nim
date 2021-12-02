import hmisc/preludes/unittest

testFileStarted()

import std/[parsesql]

iterator items(node: SqlNode): SqlNode =
  for idx in 0 ..< len(node):
    yield node[idx]

import
  hmisc/extra/treediff/[
    treediff_main,
    lcs_matcher,
    edit_generator],
  hmisc/other/htree_draw

suite "Diff SQL":
  test "simple compare":
    let sql1 = parseSql("select * from table1;")
    let sql2 = parseSql("select * from table2;")

    proc isToken(sq: SqlNode): bool = sq.kind in {nkIdent}
    proc getLabel(sq: SqlNode): string = $sq


    let tree1 = treeFromRefAst(sql1, isToken, getLabel)
    let tree2 = treeFromRefAst(sql2, isToken, getLabel)

    show tree1.treeRepr()
    show tree2.treeRepr()

    let matcher = newLcsMatcher()

    let edit = diff(
      tree1, tree2, matcher, newEditScriptGenerator())

    for action in edit.editScript:
      show action


testFileEnded()
