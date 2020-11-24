import std/[xmlparser, xmltree, strformat]
import unittest
import hmisc/algo/[htree_distance, hseq_distance, halgorithm]
import hmisc/hdebug_misc

template canImport(x: untyped): untyped =
  compiles:
    import x

when canImport(hasts/graphviz_ast):
  import hasts/graphviz_ast
  const hasGraphviz = true
else:
  const hasGraphviz = false

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



proc diff(x1, x2: string): auto =
  return simpleTreeDiff(
    parseXml(x1),
    parseXml(x2),
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
          raiseAssert(
            &"Cannot set subnode '{node}' to index {idx} - " &
            &"'{x}' only has {x.len} subnodes")
    ),
    delSubnode = (
      proc(x: var XmlNode, idx: int) =
        x.delete(idx)
    )
  )



proc diffApply(str1, str2: string): XmlNode =
  var source = str1.parseXml()
  let script = diff(str1, str2)
  for cmd in script[0]:
    echov cmd

  for cmd in script[0]:
    source.apply(cmd)

  return source


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

  test "Value update":
    let res = diff("<a>hello</a>", "<a>hallo</a>")[0]

    assertEq res.len, 1
    assertEq res[0].kind, ekUpd
    assertEq res[0].updValue, "hallo"

  test "Subnode insert":
    let res = diff(
      "<root></root>",
      "<root><first>Some text more</first></root>"
    )[0]

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

  test "Subnode deletion":
    let res = diffApply("<root><first /></root>", "<root />")
    assertEq res.tag, "root"
    assertEq res.len, 0

  test "Subnode move":
    block:
      let res = diffApply("<a><b/><c/></a>", "<a><c/><b/></a>")
      assertEq res.tag, "a"
      assertEq res[0].tag, "c"
      assertEq res[1].tag, "b"

    block:
      startHax()
      let res = diff(
        "<root><foo>bar</foo><foo>first</foo></root>",
        "<root><foo>first</foo><foo>bar</foo></root>"
      )
