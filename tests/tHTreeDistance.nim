import std/[xmlparser, xmltree]
import unittest
import hmisc/algo/[htree_distance, hseq_distance, halgorithm]
import hmisc/hdebug_misc


type
  Tree = object
    label: int
    value: string
    subn: seq[Tree]

func len(t: Tree): int = t.subn.len
func `[]`(t: Tree, idx: int): Tree = t.subn[idx]

proc `==`(x1, x2: XmlNode): bool =
  result = (x1.kind == x2.kind) and (x1.len == x2.len)
  if result:
    for i in 0 ..< x1.len:
      result = x1[i] == x2[i]
      if not result:
        return false




suite "Tree diff":
  test "Custom type":
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

    let script = mapping2.editScript(root1, root2)


  proc makeIndex(xml: XmlNode, isSource: bool): TreeIndex[string, string] =
    makeIndex[XmlNode, string, string](
      xml, isSource,
      getLabel = (
        proc(x: XmlNode): string =
          if x.kind == xnElement: x.tag else: ""
      ),
      getValue = (
        proc(x: XmlNode): string = x.innerText
      )
    )



  test "XML diff":
    let source = parseXML("<a>at least some part of words sould match</a>")
    let target = parseXML("<a>at least one word should match</a>")
    let
      sourceIndex = makeIndex(source, true)
      targetIndex = makeIndex(target, false)

    let mapping = simpleMatch(
      sourceIndex.root,
      targetIndex.root,
      similarityTreshold = 30,
      valueScore = proc(text1, text2: string): int =
        let score = byWordSimilarityScore(text1, text2).int
        score
    )

    let res = editScript(mapping, sourceIndex.root, targetIndex.root)
    assert res.script.len == 1
    assert res.script[0].kind == ekUpd

  test "Simple tree diff xml":

    proc diff(x1, x2: string): EditScript[string, string] =
      simpleTreeDiff(
        parseXml(x1), parseXml(x2),
        similarityTreshold = 30,
        getLabel = (
          proc(x: XmlNode): string =
            if x.kind == xnElement: x.tag else: ""
        ),
        getValue = (
          proc(x: XmlNode): string =
            if x.kind == xnText: x.text else: ""
        ),
        valueScore = (
          proc(text1, text2: string): int =
            byCharSimilarityScore(text1, text2).int
        )
      )

    proc apply(x: var XmlNode, cmd: EditCmd[string, string]) =
      apply(
        x,
        cmd,
        setValue = (
          proc(x: var XmlNode, v: string) =
            x.text = v
        ),
        newTree = (
          proc(l: string, v: string): XmlNode =
            if l.len == 0:
              newText(v)
            else:
              newElement(l)
        ),
        setSubnode = (
          proc(x: var XmlNode, idx: int, node: XmlNode) =
            if idx == x.len:
              x.add node
            elif idx < x.len:
              # There is no `[]=` overload, so poitner magic it is
              (addr x[idx])[] = node
            else:
              raiseAssert("Cannot set node to index")
        )
      )


    block:
      let res = diff("<a>hello</a>", "<a>hallo</a>")

      assertEq res.len, 1
      assertEq res[0].kind, ekUpd
      assertEq res[0].updValue, "hallo"

    block:
      let res = diff(
        "<root></root>",
        "<root><first>Some text more</first></root>"
      )

      assertEq res.len, 2
      assertEq res[0].kind, ekIns
      assertEq res[0].insLabel, "first"
      assertEq res[1].insLabel, ""
      assertEq res[1].insValue, "Some text more"

      var tree = parseXml("<root></root>")

      tree.apply(res[0])
      assertEq tree, parseXml("<root><first></first></root>")


      tree.apply(res[1])
      let target = parseXml("<root><first>Some text more</first></root>")
      assertEq target.tag, tree.tag
      assertEq target.kind, tree.kind
      assertEq target[0].kind, tree[0].kind
      assertEq target[0][0].kind, tree[0][0].kind

      assertEq tree, target
