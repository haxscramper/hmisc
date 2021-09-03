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

  assertRef(mappings)
  var editScript: EditScript = generator.computeActions(mappings)
  return newDiff(src, dst, mappings, editScript)

proc heightPriority*(tree: Tree): int =
  tree.getMetrics().height

import
  ./lcs_matcher,
  ./rted_matcher,
  ./bottom_up_matcher,
  ./zs_matcher,
  ./subtree_matcher

export tree

proc treeFromRefAst*[N: ref object](
    node: N,
    isToken: proc(n: N): bool,
    getLabel: proc(n: N): string
  ): Tree =

  let impl = TreeImpl(
    reprType: proc(t: Type): string = $((typeof node.kind)(t.name)))

  proc aux(node: N, parent: Tree): Tree =
    result = newTree(newTreeType(node.kind.int), parent, impl)
    result.setNodeRef(node)

    if isToken(node):
      result.setLabel(getLabel(node))

    else:
      for subnode in node:
        result.addChild aux(subnode, result)


  result = aux(node, nil)
  result.updateMetrics()
