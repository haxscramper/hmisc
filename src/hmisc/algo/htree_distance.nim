import sugar, strutils, sequtils, strformat, heapqueue, tables

type
  NodeId = distinct int
  Mapping = object
    table: Table[NodeId, seq[NodeId]]

  NodeQue = HeapQueue[NodeId]

  TreeIndex = object


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
  for valA in a:
    for valB in b:
      yield (valA, valB)

proc isomprhic(t1, t2: Tree): bool =
  ## Check if two trees are isomorphic
  discard

proc root(t: Tree): NodeId =
  ## Get root node for tree
  discard


proc topDown(T1, T2: Tree, minHeight: int = 2): Mapping =
  var
    L1: NodeQue
    L2: NodeQue

  push(root(T1), L1)
  push(root(T2), L2)

  while min(peekMax(L1), peekMax(L2)) > minHeight:
    if peekMax(L1) != peekMax(L2):
      if peekMax(L1) > peekMax(L2):
        for t in pop(L1):
          open(t, L1)
      else:
        for t in pop(L2):
          open(t, L2)

    else:
      let
        H1 = pop(L1)
        H2 = pop(L2)

      for (t1, t2) in carthesian(H1, H2):
        if isomorphic(t1, t2):
          if t2.anyOfIt(isomorphic(t1, it) and it != t2) or
             t1.anyOfIt(isomorphic(it, t2) and it != t1):
            add(A, (t1, t2))

          else:
            for (is1, is2) in carthesian(s(t1), s(t2)):
              if isomorphic(is1, is2):
                add(M, (is1, is2))

    for t1 in h1:
      if (t1, _) notin A + M:
        open(t1, l1)

   for t2 in h2:
     if (_, t2) notin A + M:
       open(t2, l2)

  A.sort() do(t1, t2: NodeId) -> bool:
    dice(parent(t1), parent(t2), m)

  while size(A) > 0:
    let (t1, t2)  = remove(A, 0)
    # TODO Add all pairs of isomprhic nodes of `s(t1)` and `s(t2)` to m
    A = A \ A.filterIt(it[0] == t1)
    A = A \ A.filterIt(it[1] == t2)

proc bottomUp(
  t1, t2: Tree, m: Mapping, minDice: float, maxSize: int): Mapping =
  for t1 in T2:
    if not t1.matched() and t1.children.anyOfIt(it.matched()):
      let t2 = candidate(t1, M)
      if t2 != nil and dice(t1, t2, M) > minDice:
        add(M, (t1, t2))
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

  var w: NodeId
  for node in s.siblings().left().reversed():
    if node.inOrder:
      w = node


  return w.partner(M).idx + 1

proc editScript(M: Matching, T1, T2: Tree): EditScript =
  var E: EditScript

  proc alignChildren(w, x: NodeId) =
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
        (a, b) in M

    for (a, b) in carthesian(S1, S2):
      if ((a, b) in M) and (not (a, b) in S):
        let k = b.findPos()
        E.add makeMove(a, w, k)
        E.apply(T1)

        a.inOrder = true
        b.inOrder = true

  for x in T2.dfsIterate:
    let
      y = x.parent
      z = y.partner(M)

    if z == nil:
      let k = findPos(x)
      E.add makeIns((w, a, value(x)), z, k)
      E.applyLast(T1)
    elif not x.isRoot:
      let
        w = x.partner(M)
        v = parent(w, T1)

      if value(w) != value(x):
        E.add makeUpd(w, value(x))
        E.applyLast(T1)

      if (y, v) notin M:
        let
          z = y.partner(M)
          k = x.findPos()

        E.add makeMove(w, z, k)
        E.applyLast(T1)

    alignChildren(w, x)

  for w in T1.postDFS:
    if w.partner(M) == nil:
      E.add makeDel(w)
      E.applyLast(T1)
