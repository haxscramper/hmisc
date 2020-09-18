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
  T1, T2: Tree, minHeight: int, minDice: float): Mapping =
  var
    L1: NodeQue
    L2: NodeQue
    A, M: Mapping

  push(root(T1), L1) ## store root node
  push(root(T2), L2) ## for each input

  ## until subtree of necessary height hasn't been reached
  while min(peekMax(L1), peekMax(L2)) > minHeight:
    ## if two top subtrees don't have equal height
    if peekMax(L1) != peekMax(L2):
      ## insert all nodes from tallest subforest
      if peekMax(L1) > peekMax(L2):
        for t in pop(L1):
          open(t, L1)
      else:
        for t in pop(L2):
          open(t, L2)

    else:
      ## otherwise get two subforest of equal height
      let
        H1 = pop(L1)
        H2 = pop(L2)

      ## for each combination of Therese is these forests
      for (t1, t2) in carthesian(H1, H2):
        ## if pair of trees is isomorphic
        if isomorphic(t1, t2):
          ## if any of the child nodes for t2 is isomorphic to t1 itself (and vice-versa)
          if t2.anyOfIt(isomorphic(t1, it) and it != t2) or
             t1.anyOfIt(isomorphic(it, t2) and it != t1):
            ## add both t1 and t2 to mapping
            add(A, (t1, t2))

          else:
            ## otherwise iterate over all pairs of child nodes
            for (is1, is2) in carthesian(s(t1), s(t2)):
              ## and determine they are isomorphic
              if isomorphic(is1, is2):
                add(M, (is1, is2))

      ## so we basically determine if there is any isomorphic mapping
      ## between either (1) roots two highest subforests or (2) root
      ## and subnodes of a root in other tree

      for t1 in H1:
        ## if there is unmatched forest root in first forest
        if (t1, _) notin (A, M):
          ## insert it's subnodes
          open(t1, L1)

      for t2 in H2:
        ## do the same for other forest
        if (_, t2) notin (A, M):
          open(t2, L2)

  A.sort() do(t1, t2: NodeId) -> bool:
    dice(parent(t1), parent(t2), M) > minDice

  while size(A) > 0:
    let (t1, t2)  = remove(A, 0)
    ## TODO Add all pairs of isomprhic nodes of `s(t1)` and `s(t2)` to m
    A.delItIf(it[0] == t1)
    A.delItIf(it[1] == t2)

proc bottomUp(
  T1, T2: Tree, M: Mapping, minDice: float, maxSize: int): Mapping =
  var M = M
  for t1 in T1:
    ## for all nodes in left, if node itself is not matched, but
    ## has any children matched
    if not t1.matched() and t1.children.anyOfIt(it.matched()):
      # get candidate node
      let t2 = candidate(t1, M)
      ## if it is a valid candidate and matches criteria for
      ## minimum number of shares subnodes
      if t2 != nil and dice(t1, t2, M) > minDice:
        ## add node to mapping
        add(M, (t1, t2))
        ## if max of number of subnodes does not exceed threshold
        if max(s(t1).len, s(t2).len) < maxSize:
          let R = opt(t1, t2)
          for (ta, tb) in R:
            if ((ta, tb) notin M) and (label(ta) == label(tb)):
              add(M, (ta, tb))


proc findPos(x: NodeId, M: Mapping): int =
  let
    y = parent(x)
    w = x.partner(M)

  if y.leftmostInOrder() == x:
    return 1

  var v: NodeId
  for node in x.siblings().left.reversed():
    if node.inOrder:
      v = node


  return v.partner(M).idx + 1

proc editScript(M: Mapping, T1, T2: Tree): EditScript =
  var T1 = T1
  var E: EditScript

  proc alignChildren(w, x: NodeId) =
    ## generate optimal sequence of moves that will align
    ## child nodes of w and x

    ## map all subnodes for
    for ch in w:
      ch.inOrder = false

    for ch in x:
      ch.inOrder = false

    let
      S1 = collect(newSeq):
        for ch in w:
          if ch.partner(M) in x:
            ch

      S2 = collect(newSeq):
        for ch in x:
          if ch.partner(M) in w:
            ch

      S = LCS(S1, S2) do(a, b: NodeId) -> bool:
        ## left and right subnodes are considered equal if
        ## this is a pair which already exists in mapping.
        (a, b) in M

    for (a, b) in carthesian(S1, S2):
      if ((a, b) in M) and ((a, b) notin S):
        let k = b.findPos(M)
        E.add makeMove(a, w, k)
        E.applyLast(T1)

        a.inOrder = true
        b.inOrder = true

  T2.bfsIterate() do(x: NodeId):
    ## iderate all nodes in tree in BFS order
    let
      y = x.parent ## parent node in right tree
      z = y.partner(M) ## partner of right parent tree.
      ## left parent tree.

    if z == nil:
      ## if current node's parent does not have a corresponding
      ## partner in mapping
      let k = findPos(x, M)
      E.add makeIns((partner(x, M), label(x), value(x)), z, k)
      E.applyLast(T1)
    elif not x.isRoot:
      ## if node parent has partner and the node itself
      ## is not root
      let
        w = x.partner(M) ## get partner of current node
        v = parent(w) ## parent of the partner

      ## I'd node and partner have different values
      if value(w) != value(x):
        ## add update to edit script
        E.add makeUpd(w, value(x))
        E.applyLast(T1)


      ## if mapping current node and it's
      ## partner are not in mapping
      if (y, v) notin M:
        let
          z = y.partner(M)
          k = x.findPos(M)

        E.add makeMove(w, z, k)
        E.applyLast(T1)


    ## align subnodes for current node and it's counterpart
    alignChildren(x.partner(M), x)

  T1.dfsIteratePost do(w: NodeId):
    ## for each node in post order traversal of left tree
    if w.partner(M) == nil:
      ## if current node does not have a parent, remove it
      ## deletion will happen from leaves to roots
      E.add makeDel(w)
      E.applyLast(T1)
