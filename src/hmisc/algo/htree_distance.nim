import sugar, strutils, sequtils, strformat, heapqueue, tables,
       macros, algorithm

import halgorithm

type
  NodeId = distinct int
  Mapping = object
    table: Table[NodeId, seq[NodeId]]

  NodeQue = HeapQueue[NodeId]

  Label = int
  Value = string

  Tree = object
    label: Label
    value: Value
    subn: seq[Tree]

  TreeIndex = object

  EditCmdKind = enum
    ekIns
    ekDel
    ekMov
    ekUpd

  EditCmd = object
    discard

  EditScript = object
    cmds: seq[EditCmd]

proc add(es: var EditScript, ec: EditCmd): void =
  discard

proc makeMove(
  nodeId, newParent: NodeId,
  newIdx: int
): EditCmd =
  discard

proc makeIns(
  newnode: tuple[leafId: NodeId, label: Label, value: Value],
  parent: NodeId,
  idx: int
): EditCmd =
  discard

proc makeUpd(id: NodeId, val: Value): EditCmd =
  discard

proc makeDel(id: NodeId): EditCmd =
  discard


proc matched(n: NodeId): bool =
  discard

proc inOrder(b: NodeId): bool =
  discard

proc `inOrder=`(b: NodeId, val: bool) =
  ## mark node as "in order"
  discard

proc idx(n: NodeId): int =
  ## get node index in sequence of parent's subnodes
  discard

proc candidate(n: NodeId, m: Mapping): NodeId =
  discard

proc partner(n: NodeId, m: Mapping): NodeId =
  discard

proc siblings(n: NodeId): tuple[left, right: seq[NodeId]] =
  ## return sibling nodes to the left and right of node
  discard

proc leftmostInOrder(n: NodeId): NodeId =
  ## return leftmost node that is marked as "in order"
  discard

proc `==`(n: NodeId, other: typeof(nil)): bool =
  discard

proc opt(t1, t2: NodeId): Mapping =
  ## ? Wtf is this shit
  discard

iterator items(m: Mapping): (NodeId, NodeId) =
  ## iterate over all pairs in mapping
  discard

proc sort(m: var Mapping, cmp: proc(a, b: NodeId): bool) =
  discard

proc LCS(lhs, rhs: seq[NodeId],
         eq: proc(a, b: NodeId): bool): seq[(NodeId, NodeId)] =
  discard

func add(m: var Mapping, t: (NodeId, NodeId)) =
  ## Add mapping to mapping
  discard

proc bfsIterate(tree: Tree, cb: proc(n: NodeId)) =
  discard

proc dfsIteratePost(tree: Tree, cb: proc(n: NodeId)) =
  discard

proc applyLast(script: EditScript, tree: var Tree): void =
  discard

proc contains(parent, subnode: NodeId): bool =
  discard

proc contains(mapping: Mapping, pair: (NodeId, NodeId)): bool =
  discard

macro `notin`(lhs: untyped, rhs: (Mapping, Mapping)): untyped =
  quote do:
    true

macro `notin`(lhs: untyped, rhs: Mapping): untyped =
  quote do:
    false

proc label(n: NodeId): int =
  discard

proc value(n: NodeId): string =
  discard

proc isRoot(n: NodeId): bool =
  discard

func `==`(a, b: NodeId): bool = a.int == b.int

iterator items(id: NodeId): NodeId =
  ## Iterate over subnodes for node pointed to by `id`
  discard

iterator items(tr: Tree): NodeId =
  ## iterate over all subnodes of tree root node
  discard

func s(t: NodeId): seq[NodeId] =
  ## Return list of subnodes
  discard

func parent(t: NodeId): NodeId =
  ## Get parent node for node
  discard

func size(a: Mapping): int =
  ## get number of items in mapping
  discard

func remove(a: var Mapping, `?`: int): (NodeId, NodeId) =
  ## remove something from mapping. Now idea what ? Should actually be.
  discard

template delItIf(a: var Mapping, expr: untyped): untyped =
  ## delete all mapping pairs that satisfy predicate expression
  discard

proc children(node: NodeId): seq[NodeId] =
  ## Get list of children for node `node`
  discard

proc peekMax(que: NodeQue): int =
  ## Return greatest height of node in the list
  discard

proc pop(que: var NodeQue): seq[NodeId] =
  ## Pop all nodes having height of `peekMax`
  discard

proc push(node: NodeId, que: var NodeQue) =
  ## Insert node `node` in que
  discard

proc open(node: NodeId, que: var NodeQue) =
  ## Insert all children of `t` into `l`
  discard

iterator carthesian[T](a, b: seq[T]): (T, T) =
  ## iterate over carthesian product of two sequences
  for valA in a:
    for valB in b:
      yield (valA, valB)

proc isomorphic(t1, t2: NodeId): bool =
  ## Check if two trees are isomorphic
  discard

proc root(t: Tree): NodeId =
  ## Get root node for tree
  discard

proc dice(t1, t2: NodeId, m: Mapping): float =
  ## get fraction of equal subnodes for two trees relative to total size of trees.
  discard

proc topDown(
  srcTree, targetTree: Tree, minHeight: int, minDice: float): Mapping =
  var
    srcQue: NodeQue
    targetQue: NodeQue
    A, map: Mapping

  push(root(srcTree), srcQue) ## store root node
  push(root(targetTree), targetQue) ## for each input

  ## until subtree of necessary height hasn't been reached
  while min(peekMax(srcQue), peekMax(targetQue)) > minHeight:
    ## if two top subtrees don't have equal height
    if peekMax(srcQue) != peekMax(targetQue):
      ## insert all nodes from tallest subforest
      if peekMax(srcQue) > peekMax(targetQue):
        for t in pop(srcQue):
          open(t, srcQue)
      else:
        for t in pop(targetQue):
          open(t, targetQue)

    else:
      ## otherwise get two subforest of equal height
      let
        srcTops = pop(srcQue)
        targetTops = pop(targetQue)

      ## for each combination of Therese is these forests
      for (src, target) in carthesian(srcTops, targetTops):
        ## if pair of trees is isomorphic
        if isomorphic(src, target):
          ## if any of the child nodes for target is isomorphic to src
          ## itself (and vice-versa)
          if target.anyOfIt(isomorphic(src, it) and it != target) or
             src.anyOfIt(isomorphic(it, target) and it != src):
            ## add both src and target to mapping
            add(A, (src, target))

          else:
            ## otherwise iterate over all pairs of child nodes
            for (is1, is2) in carthesian(s(src), s(target)):
              ## and determine they are isomorphic
              if isomorphic(is1, is2):
                add(map, (is1, is2))

      ## so we basically determine if there is any isomorphic mapping
      ## between either (1) roots two highest subforests or (2) root
      ## and subnodes of a root in other tree

      for src in srcTops:
        ## if there is unmatched forest root in first forest
        if (src, _) notin (A, map):
          ## insert it's subnodes
          open(src, srcQue)

      for target in targetTops:
        ## do the same for other forest
        if (_, target) notin (A, map):
          open(target, targetQue)

  A.sort() do(src, target: NodeId) -> bool:
    dice(parent(src), parent(target), map) > minDice

  while size(A) > 0:
    let (src, target)  = remove(A, 0)
    ## TODO Add all pairs of isomprhic nodes of `s(src)` and `s(target)` to m
    A.delItIf(it[0] == src)
    A.delItIf(it[1] == target)

proc bottomUp(
  srcTree, targetTree: Tree, M: Mapping, minDice: float, maxSize: int): Mapping =
  var M = M
  for src in srcTree:
    ## for all nodes in left, if node itself is not matched, but
    ## has any children matched
    if not src.matched() and src.children.anyOfIt(it.matched()):
      # get candidate node
      let target = candidate(src, M)
      ## if it is a valid candidate and matches criteria for
      ## minimum number of shares subnodes
      if target != nil and dice(src, target, M) > minDice:
        ## add node to mapping
        add(M, (src, target))
        ## if max of number of subnodes does not exceed threshold
        if max(s(src).len, s(target).len) < maxSize:
          let R = opt(src, target)
          for (ta, tb) in R:
            if ((ta, tb) notin M) and (label(ta) == label(tb)):
              add(M, (ta, tb))


proc findPos(curr: NodeId, map: Mapping): int =
  let
    currPar = parent(curr)
    w = curr.partner(map)

  if currPar.leftmostInOrder() == curr:
    return 1

  var v: NodeId
  for node in curr.siblings().left.reversed():
    if node.inOrder:
      v = node


  return v.partner(map).idx + 1

proc editScript(map: Mapping, srcTree, targetTree: Tree): EditScript =
  var srcTree = srcTree
  var E: EditScript

  proc alignChildren(other, curr: NodeId) =
    ## generate optimal sequence of moves that will align
    ## child nodes of w and x

    ## map all subnodes for
    for ch in other:
      ch.inOrder = false

    for ch in curr:
      ch.inOrder = false

    let
      S1 = collect(newSeq):
        for ch in other:
          if ch.partner(map) in curr:
            ch

      S2 = collect(newSeq):
        for ch in curr:
          if ch.partner(map) in other:
            ch

      S = LCS(S1, S2) do(a, b: NodeId) -> bool:
        ## left and right subnodes are considered equal if
        ## this is a pair which already exists in mapping.
        (a, b) in map

    for (a, b) in carthesian(S1, S2):
      if ((a, b) in map) and ((a, b) notin S):
        E.add makeMove(a, other, b.findPos(map))
        E.applyLast(srcTree)

        a.inOrder = true
        b.inOrder = true

  targetTree.bfsIterate() do(curr: NodeId):
    ## iderate all nodes in tree in BFS order
    let
      currPar = curr.parent ## parent node in right tree
      otherPar = currPar.partner(map) ## partner of right parent tree.
      ## left parent tree.

    if otherPar == nil:
      ## if current node's parent does not have a corresponding
      ## partner in mapping
      let currPos = findPos(curr, map)
      E.add makeIns(
        (partner(curr, map), label(curr), value(curr)),
        otherPar, currPos)

      E.applyLast(srcTree)
    elif not curr.isRoot:
      ## if node parent has partner and the node itself
      ## is not root
      let
        other = curr.partner(map) ## get partner of current node
        otherPar = parent(other) ## parent of the partner

      ## I'd node and partner have different values
      if value(other) != value(curr):
        ## add update to edit script
        E.add makeUpd(other, value(curr))
        E.applyLast(srcTree)


      ## if mapping current node and it's
      ## partner are not in mapping
      if (currPar, otherPar) notin map:
        let
          otherPar = currPar.partner(map)
          k = curr.findPos(map)

        E.add makeMove(other, otherPar, k)
        E.applyLast(srcTree)


    ## align subnodes for current node and it's counterpart
    alignChildren(curr.partner(map), curr)

  srcTree.dfsIteratePost do(curr: NodeId):
    ## for each node in post order traversal of left tree
    if curr.partner(map) == nil:
      ## if current node does not have a parent, remove it
      ## deletion will happen from leaves to roots
      E.add makeDel(curr)
      E.applyLast(srcTree)
