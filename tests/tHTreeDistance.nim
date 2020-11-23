import std/[xmlparser, xmltree]
import unittest
import hmisc/algo/[htree_distance, hseq_distance]
import hmisc/hdebug_misc


type
  Tree = object
    label: int
    value: string
    subn: seq[Tree]

func len(t: Tree): int = t.subn.len
func `[]`(t: Tree, idx: int): Tree = t.subn[idx]


suite "Tree diff":
  test "Custom type":
    startHax()
    let tree1 = Tree(value: "TREE-HEAD", label: 12, subn: @[
      Tree(value: "LEAF-1"),
      Tree(value: "LEAF-2"),
      Tree(value: "LEAF-OLD-VAL"),
    ])

    let tree2 = Tree(value: "TREE-HEAD", label: 12, subn: @[
      Tree(value: "LEAF-2"),
      Tree(value: "LEAF-1"),
      Tree(value: "LEAF-NEW-VAL"),
    ])

    let sourceIndex = makeIndex[Tree, int, string](
      tree1,
      true,
      getLabel = (proc(n: Tree): int = n.label),
      getValue = (proc(n: Tree): string = n.value)
    )

    let targetIndex = makeIndex[Tree, int, string](
      tree2,
      false,
      getLabel = (proc(n: Tree): int = n.label),
      getValue = (proc(n: Tree): string = n.value)
    )

    let
      root1       = sourceIndex.root()
      root2       = targetIndex.root()

    let mapping2 = simpleMatch(root1, root2) do(a, b: string) -> int:
      let res = int(100 * (
        longestCommonSubsequence(a, b)[0].matches.len /
        max(a.len, b.len)
      ))

      res

    for k, v in mapping2:
      echov (k, v)

    let script = mapping2.editScript(root1, root2)

  test "XML diff":
    let source = parseXML("<a>hello world</a>")
    let target = parseXML("<a>hello</a>")
