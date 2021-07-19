# This file is part of GumTree.
#
# GumTree is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GumTree is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with GumTree.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright 2011-2015 Jean-Rémy Falleri <jr.falleri@gmail.com>
# Copyright 2011-2015 Floréal Morandat <florealm@gmail.com>

# Nim implementation have been automatically generated, and /mostly/ based on
# the following files from the original project:
#
# - Move.java
# - Delete.java
# - Update.java
# - Addition.java
# - Insert.java
# - TreeInsert.java
# - TreeAddition.java
# - TreeDelete.java
# - TreeAction.java
# - Action.java
# - EditScriptGenerator.java
# - ChawatheScriptGenerator.java
# - InsertDeleteChawatheScriptGenerator.java
# - SimplifiedChawatheScriptGenerator.java

import ./jcommon, ./tree, ./mapping_store
import ./tree, ./jcommon
import hmisc/extra/hdrawing/term_buf

{.this: this.}

type
  EditScript* = ref object
    actions*: seq[Action]

  ActionKind* = enum
    akAddition
    akInsert
    akUpdate
    akMove
    akDelete
    akTreeInsert
    akTreeDelete

  Action* = ref object
    node*: Tree
    case kind*: ActionKind
      of akAddition, akInsert, akMove, akTreeInsert:
        parent*: Tree
        pos*: int

      of akUpdate:
        value*: string

      of akDelete, akTreeDelete:
        discard

  EditScriptGenerator* = ref object
    origSrc*: Tree
    cpySrc*: Tree
    origDst*: Tree
    origMappings*: MappingStore
    cpyMappings*: MappingStore
    dstInOrder*: HashSet[Tree]
    srcInOrder*: HashSet[Tree]
    actions*: EditScript
    origToCopy*: Table[Tree, Tree]
    copyToOrig*: Table[Tree, Tree]

proc newEditScriptGenerator*(): EditScriptGenerator = EditScriptGenerator()

proc newDelete*(node: Tree): Action = 
  Action(kind: akDelete, node: node)

proc newTreeDelete*(node: Tree): Action = 
  Action(kind: akTreeDelete, node: node)

proc newAddition*(node: Tree; parent: Tree; pos: int): Action =
  Action(kind: akAddition, node: node, parent: parent, pos: pos)

proc newMove*(node: Tree; parent: Tree; pos: int): Action =
  Action(kind: akMove, node: node, parent: parent, pos: pos)

proc newTreeInsert*(node: Tree; parent: Tree; pos: int): Action =
  Action(kind: akTreeInsert, node: node, parent: parent, pos: pos)

proc newInsert*(node: Tree; parent: Tree; pos: int): Action =
  Action(kind: akInsert, node: node, parent: parent, pos: pos)

proc newUpdate*(node: Tree, value: string): Action = 
  Action(kind: akUpdate, value: value, node: node)

proc getName*(this: Action): string =
  case this.kind:
    of akInsert: "\"insert-node\""
    of akAddition: ""
    of akUpdate: "\"update-node\""
    of akMove: "\"move-tree\""
    of akDelete: "\"delete-node\""
    of akTreeInsert: "\"insert-tree\""
    of akTreeDelete: "\"delete-tree\""

proc `$`*(action: Action): string = 
  withBufStr:
    var pos = action.getName().len() + 1

    buf[0, 0] = action.getName()

    if action.kind in {akAddition, akInsert, akMove, akTreeInsert}:
      buf[0, pos] = "@" & $action.pos
      pos += 3

    buf[0, pos] = $action.node

    case action.kind:
      of {akInsert, akTreeInsert}:
        buf[1, 2] = action.node.treeRepr()

      else:
        discard


proc getNode*(this: Action): Tree =
  return node

proc setNode*(this: Action; node: Tree): void =
  this.node = node

proc getParent*(this: Action): Tree =
  return parent

proc getPosition*(this: Action): int =
  return pos

proc toString*(this: Action): string =
  let name = if parent.isNil(): "\"root\"" else: parent.toString()
  return "\"===\\n{getName()}\\n---\\n{node.toString()}\\nto\\n{name}\\nat {pos}\""



proc newEditScript*(): EditScript =
  EditScript()

iterator items*(this: EditScript): Action =
  for act in actions:
    yield act

proc add*(this: EditScript; action: Action): void =
  actions.add(action)

proc add*(this: EditScript; index: int; action: Action): void =
  actions[index] = action

proc get*(this: EditScript; index: int): Action =
  return actions[index]

proc size*(this: EditScript): int =
  return actions.len()

proc remove*(this: EditScript; action: Action): bool =
  return actions.remove(action)

proc remove*(this: EditScript; index: int): Action =
  return actions.remove(index)

proc asList*(this: EditScript): seq[Action] =
  return actions

proc lastIndexOf*(this: EditScript; action: Action): int =
  return actions.rfind(action)


proc initWith*(this: EditScriptGenerator; ms: MappingStore): void =
  this.origSrc = ms.src
  this.cpySrc = this.origSrc.deepCopy()
  this.origDst = ms.dst
  this.origMappings = ms

  for (origTree, cpyTree) in zip(preOrder(origSrc).toSeq(), preOrder(cpySrc).toSeq()):
    origToCopy[origTree] = cpyTree
    copyToOrig[cpyTree] = origTree

  cpyMappings = newMappingStore(ms.src, ms.dst)
  for m in origMappings:
    cpyMappings.addMapping(origToCopy[m.first], m.second)



proc findPos*(this: EditScriptGenerator; x: Tree): int =
  var y = x.getParent()
  var siblings = y.getChildren()
  for c in siblings:
    if (dstInOrder.contains(c)):
      if (c == x):
        return 0

      else:
        break

  var xpos = x.positionInParent()
  var v: Tree = nil

  for c in siblings:
    if (dstInOrder.contains(c)):
      v = c

  if isNil(v):
    return 0

  var u = cpyMappings.getSrcForDst(v)
  echov u.getParent().treeRepr()
  echov x.treeRepr()
  var upos = u.positionInParent()
  return upos + 1



proc lcs*(this: EditScriptGenerator; x: seq[Tree]; y: seq[Tree]): 
  seq[Mapping] =

  var m = x.len()
  var n = y.len()
  var lcs: seq[Mapping]
  var opt: seq[seq[int]] = newSeqWith(m + 1, newSeqWith(n + 1, 0))
  for i in 0 .. m - 1:
    for j in 0 .. n - 1:
      if (cpyMappings.getSrcForDst(y[j]) == x[i]):
        opt[i][j] = opt[i + 1][j + 1] + 1
      else:
        opt[i][j] = max(opt[i + 1][j], opt[i][j + 1])

  var i = 0
  var j = 0
  while (i < m and j < n):
    if (cpyMappings.getSrcForDst(y[j]) == x[i]):
      lcs.add(newMapping(x[i], y[j]))
      postInc(i)
      postInc(j)
    else:
      if (opt[i + 1][j] >= opt[i][j + 1]):
        postInc(i)
      else:
        postInc(j)
  return lcs


proc alignChildren*(this: EditScriptGenerator; w: Tree; x: Tree): void =
  srcInOrder.excl w.getChildren().toHashSet()
  dstInOrder.excl x.getChildren().toHashSet()
  var s1: seq[Tree]
  for c in w.getChildren():
    if (cpyMappings.isSrcMapped(c)):
      if (x.getChildren().contains(cpyMappings.getDstForSrc(c))):
        s1.add(c)

  var s2: seq[Tree]
  for c in x.getChildren():
    if (cpyMappings.isDstMapped(c)):
      if (w.getChildren().contains(cpyMappings.getSrcForDst(c))):
        s2.add(c)

  var lcs: seq[Mapping] = lcs(s1, s2)
  for m in lcs:
    srcInOrder.incl m.first
    dstInOrder.incl m.second

  for b in s2:
    for a in s1:
      if (cpyMappings.has(a, b)):
        if (not(lcs.contains(newMapping(a, b)))):
          a.getParent().getChildren().remove(a)
          var k: int = findPos(b)
          var mv: Action = newMove(copyToOrig[a], copyToOrig[w], k)
          actions.add(mv)
          w.getChildren()[k] = a
          a.setParent(w)
          srcInOrder.incl a
          dstInOrder.incl b

proc generate*(this: EditScriptGenerator) =
  var srcFakeRoot: Tree = newFakeTree("SRC-FAKE-ROOT", cpySrc)
  var dstFakeRoot: Tree = newFakeTree("DST-FAKE-ROOT", origDst)
  cpySrc.setParent(srcFakeRoot)
  origDst.setParent(dstFakeRoot)
  actions = newEditScript()
  cpyMappings.addMapping(srcFakeRoot, dstFakeRoot)
  for targetTree in breadthFirst(origDst):
    # Iterate all nodes in tree in BFS order
    var sourceTree: Tree # Newly constructed node or mapping counterpart for the target node
    var targetParent = targetTree.getParent() # parent node in right tree
    var mappedSourceParent = cpyMappings.getSrcForDst(targetParent) # Partner of the target subnode
    
    if (not(cpyMappings.isDstMapped(targetTree))):
      # if current node does not have a corresponding partner in mapping
      # insert it (add `ins` action to edit script)

      var k = findPos(targetTree)
      actions.add newInsert(targetTree, copyToOrig[mappedSourceParent], k)

      # Create new fake tree to insert
      sourceTree = newFakeTree("NEW-INSERT")
      
      copyToOrig[sourceTree] = targetTree

      # Add new mapping for the node
      cpyMappings.addMapping(sourceTree, targetTree)

      # Position of the node to insert under + IndetargetTree of newly inserted node. It
      # is assumed that `sourceTree` has been transformed to have the same
      # structure as `targetTree`.
      mappedSourceParent.insertChild(sourceTree, k)

    else:
      sourceTree = cpyMappings.getSrcForDst(targetTree)
      if targetTree != origDst:
        var v: Tree = sourceTree.getParent()
        if sourceTree.getLabel() != targetTree.getLabel():
          # Different labesl on the source and targe tree, create and apply
          # update command
          actions.add newUpdate(copyToOrig[sourceTree], targetTree.getLabel())
          sourceTree.setLabel(targetTree.getLabel())

        if notNil(v) and mappedSourceParent != v:
          # Current source tree has a parent, and it is different from one
          # provided by mapping. 
          var k: int = findPos(targetTree)
          actions.add newMove(copyToOrig[sourceTree], copyToOrig[mappedSourceParent], k)
          
          var oldk: int = sourceTree.positionInParent()

          # Remove node from the old parent and insert it to the new one
          sourceTree.getParent().getChildren().remove(oldk)
          mappedSourceParent.insertChild(sourceTree, k)

    srcInOrder.incl(sourceTree)
    dstInOrder.incl(targetTree)

    # Align subnodes for the `targetTree` and it's counterpart (that might have been
    # genenrated just now via `insert` operation)
    alignChildren(sourceTree, targetTree)

  for w in cpySrc.postOrder():
    if (not(cpyMappings.isSrcMapped(w))):
      if w in copyToOrig: # HACK was not present in original algorithm, idk if this is 
                          # realy needed for nim version.
        actions.add newDelete(copyToOrig[w])



proc simplify*(this: EditScriptGenerator) =
  var addedTrees: Table[Tree, Action]
  var deletedTrees: Table[Tree, Action]
  for a in actions:
    case a.kind:
      of akInsert:
        addedTrees[a.getNode()] = a

      of akDelete:
        deletedTrees[a.getNode()] = a

      else:
        discard

  for t, _ in addedTrees:
    if (
      addedTrees.contains(t.getParent()) and
      t.getParent().getDescendants().allIt(it in addedTrees)
    ):

      discard actions.remove(addedTrees[t])

    elif (
      t.getChildren().len() > 0 and
      t.getDescendants().allIt(it in addedTrees)
    ):
      var originalAction = addedTrees[t]
      var ti  = newTreeInsert(originalAction.getNode(),
          originalAction.getParent(), originalAction.getPosition())
      var index = actions.lastIndexOf(originalAction)

      
      actions.add(index, ti)
      discard actions.remove(index + 1)

  for t, _ in deletedTrees:
    let parent = t.getParent()
    if (
      notNil(parent) and
      deletedTrees.contains(parent) and
      parent.getDescendants().allIt(it in deletedTrees)
    ):
      discard actions.remove(deletedTrees[t])

    elif (
      t.getChildren().len() > 0 and
      t.getDescendants().allIt(it in deletedTrees)
    ):
      var originalAction = deletedTrees[t]
      var ti = newTreeDelete(originalAction.getNode())
      var index: int = actions.lastIndexOf(originalAction)
      actions.add(index, ti)
      discard actions.remove(index + 1)

proc removeMovesAndUpdates*(this: EditScriptGenerator): EditScript =
  var actionsCpy: EditScript
  for a in actions:
    case a.kind:
      of akUpdate:
        var src: Tree = a.getNode()
        var dst: Tree = origMappings.getDstForSrc(src)
        actionsCpy.add(newInsert(dst, dst.getParent(), dst.positionInParent()))
        actionsCpy.add(newDelete(a.getNode()))

      of akMove:
        var m = a
        var src: Tree = a.getNode()
        var dst: Tree = origMappings.getDstForSrc(src)
        actionsCpy.add(newTreeInsert(dst, dst.getParent(), m.getPosition()))
        actionsCpy.add(newTreeDelete(a.getNode()))

      else:
        actionsCpy.add(a)

  return actionsCpy

proc computeActions*(this: EditScriptGenerator; ms: MappingStore): EditScript =
  assertRef this
  assertRef ms
  initWith(ms)
  generate()
  simplify()

  return actions


