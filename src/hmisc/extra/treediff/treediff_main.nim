import std/[sequtils, with, tables, hashes, sets]
import
  hmisc/core/all


import
  ./tree,
  ./jcommon,
  ./edit_generator,
  ./mapping_store,
  ./matcher,
  ./edit_generator

{.this: this.}

type
  Diff* = ref object
    src*: Tree
    dst*: Tree
    mappings*: MappingStore
    editScript*: EditScript

proc newDiff*(src: Tree; dst: Tree; mappings: MappingStore;
              editScript: EditScript): Diff =

  new(result)
  with result:
    src = src
    dst = dst
    mappings = mappings
    editScript = editScript

proc diff*(
    src, dst: Tree;
    matcher: Matcher,
    generator: EditScriptGenerator
  ): Diff =

  var mappings: MappingStore = matcher.match(
    src.getRoot(), dst.getRoot(), newMappingStore(src, dst))

  echo mappings
  assertRef(mappings)
  var editScript: EditScript = generator.computeActions(mappings)
  return newDiff(src, dst, mappings, editScript)


import
  ./lcs_matcher,
  ./rted_matcher,
  ./bottom_up_matcher,
  ./zs_matcher,
  ./subtree_matcher


import
  hmisc/other/[hpprint, hlogger]

proc heightPriority(tree: Tree): int =
  tree.getMetrics().height

when isMainModule:
  import hnimast

  proc treeFromPNode*(node: PNode): Tree =
    proc aux(node: PNode, parent: Tree): Tree =
      result = newTree(newTreeType(($node.kind)[2 ..^ 1]), parent)
      case node.kind:
        of nkTokenKinds:
          result.setLabel($node)

        else:
          for subnode in node:
            result.addChild aux(subnode, result)

      result.setMetadata("orig", cast[pointer](node))


    result = aux(node, nil)
    result.updateMetrics()

  for matcher in [
    newLcsMatcher(),
    # ,
    # newGreedyBottomUpMatcher(),
    # newZsMatcher()
    # newRtedMatcher(),
    newCompositeMatcher(
      newGreedySubtreeMatcher(heightPriority),
      newCompleteBottomUpMatcher()
    )
    # newHungarianSubtreeMatcher(heightPriority),
  ]:
    for (srcCode, dstCode) in {
      # "1": "2"
      "echo(1)": "echo(2, [[[3]]])",
      """
# proc writeIoEffect() {.tags: [ReadIoEffect].} =
#   discard

# proc changeSideEffect() = discard

# proc changeRaiseAnnotation() = discard

proc changeImplementation() = discard

proc main() =
  changeSideEffect()
  changeRaiseAnnotation()
  changeImplementation()
""":

      """
# proc writeIoEffect() {.tags: [ReadIoEffect].} =
#   discard

# proc changeSideEffect() =
#   writeIoEffect()
#   echo 12

# proc changeRaiseAnnotation() =
#   raise newException(OsError, "w23423")

proc changeImplementation() =
  for i in [0, 1, 3]:
    discard i

proc main() =
  changeSideEffect()
  changeRaiseAnnotation()
  changeImplementation()
"""
    }:
      let
        srcTree = srcCode.parsePNodeStr().treeFromPNode()
        dstTree = dstCode.parsePNodeStr().treeFromPNode()

      echov matcher.name
      echo srcTree.treeRepr(fullIdent = true)
      echo dstTree.treeRepr(fullIdent = true)
      try:
        let edit = diff(
          srcTree,
          dstTree,
          matcher,
          newEditScriptGenerator()
        )

        echo "ok"

        for action in edit.editScript:
          echo action
        # pprint edit.editScript, conf = pconf(
        #   stringPaths = matchField("node", "parent", "first", "second"),
        #   ignorePaths = matchField("src", "dst"))


      except Exception as e:
        newTermLogger().logStackTrace(e)
        quit 1


  echo "ALL OK"
