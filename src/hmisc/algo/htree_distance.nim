import sugar, strutils, sequtils, strformat, heapqueue, tables,
       macros, algorithm, hashes, deques, sets

import halgorithm
import hpprint

import ../hdebug_misc


type
  NodeId = object
    id {.requiresinit.}: int
    isSrc {.requiresinit.}: bool
    index {.requiresinit.}: TreeIndex
    level {.requiresinit.}: int

  Mapping = object
    table: Table[NodeId, NodeId]

  NodeQue = HeapQueue[NodeId]

  Label = int
  Value = string

  Tree = object
    label: Label
    value: Value
    subn: seq[Tree]

  TreeIndex = ref object
    hashes: Table[NodeId, Hash] ## Hash for each node - used for
                                ## efficient O(1) tree isomprhism test

    subnodes: Table[NodeId, seq[NodeId]] ## Table of subnodes for each
                                         ## node

    parentTable: Table[NodeId, NodeId] ## node-parent

    idVals: Table[NodeId, Value]
    idLabel: Table[NodeId, Label]
    inordNodes: HashSet[NodeId]
    maxId: int

  EditCmdKind = enum
    ekIns
    ekDel
    ekMov
    ekUpd

  EditCmd = object
    newIdx: int
    case kind: EditCmdKind
      of ekIns:
        leafId: NodeId
        label: Label
        value: Value
        parent: NodeID
      of ekDel:
        delId: NodeID
      of ekMov:
        movId, newParent: NodeId
      of ekUpd:
        updId: NodeId
        newVal: Value

  EditScript = object
    cmds: seq[EditCmd]

func hash(id: NodeId): Hash =
  !$(hash(id.id) !& hash(id.isSrc))

func `$`(id: NodeID): string =
  var val: string
  if id in id.index.idVals:
    val = id.index.idVals[id]
  else:
    val = "NO-VALUE"


  (if id.isSrc: "s" else: "t") & "@" & $id.id &
    " (" & $(id.level) & ") - " & val

func `<`(id1, id2: NodeId): bool =
  id1.level < id2.level

proc makeIndex(tree: Tree, isSrc: bool): TreeIndex =
  var index = TreeIndex()
  proc fillIndex(
    tree: Tree, isSrc: bool, parentId: NodeId
       ): tuple[id: NodeId, hash: Hash] {.discardable.} =
    ## Fill tree index with nodes from `tree`
    result.id = NodeId(
      id: index.maxId, isSrc: isSrc, index: index, level: 0)
    # index.parentTable[result.id] = parentId
    inc index.maxId

    var
      id: seq[NodeId]
      hashes: seq[Hash]
      depths: seq[int]

    for sub in tree.subn:
      let (nId, hash) = fillIndex(sub, isSrc, result.id)
      id.add nId
      hashes.add hash
      depths.add nId.level

    result.id.level = depths.max(0) + 1
    for node in id:
      index.parentTable[node] = result.id

    index.idVals[result.id] = tree.value
    index.idLabel[result.id] = tree.label

    index.subnodes[result.id] = id
    var h: Hash = 0
    h = h !& hash(tree.label) !& hash(tree.value)
    for subh in hashes:
      h = h !& subh

    index.hashes[result.id] = !$h


  let parent = NodeId(
    index: index, id: index.maxId, isSrc: isSrc, level: 0)
  discard fillIndex(tree, isSrc, parent)
  return index



proc add(es: var EditScript, ec: EditCmd): void =
  es.cmds.add ec

proc makeMove(nodeId, newParent: NodeId, newIdx: int): EditCmd =
  EditCmd(
    kind: ekMov, movId: nodeId, newParent: newParent, newIdx: newIdx)

proc makeIns(
  newnode: tuple[leafId: NodeId, label: Label, value: Value],
  parent: NodeId, idx: int): EditCmd =
  EditCmd(newIdx: idx, parent: parent, kind: ekIns,
          leafId: newnode.leafId,
          label: newnode.label,
          value: newnode.value
  )

proc makeUpd(id: NodeId, val: Value): EditCmd =
  EditCmd(kind: ekUpd, updId: id, newVal: val)

proc makeDel(id: NodeId): EditCmd =
  EditCmd(kind: ekDel, delId: id)


proc matched(n: NodeId): bool =
  discard

proc getInOrder(index: TreeIndex, b: NodeId): bool =
  b in index.inordNodes

proc setInOrder(index: var TreeIndex, b: NodeId, val: bool) =
  ## mark node as "in order"
  index.inordNodes.incl b

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

iterator bfsIterate(tree: NodeId, index: TreeIndex): NodeId =
  ## Yield each node id in tree in BFS order
  discard

iterator dfsIteratePost(tree: NodeId, index: TreeIndex): NodeId =
  ## Yield each node in tree in post-order DFS traversal
  discard

proc applyLast(script: EditScript, tree: var TreeIndex): void =
  ## Apply last added script element to tree `tree`
  case script.cmds[^1].kind:
    of ekIns:
      discard
    of ekDel:
      discard
    of ekMov:
      discard
    of ekUpd:
      discard

proc contains(
  index: TreeIndex, pair: tuple[parent, subnode: NodeId]): bool =
  ## Returne tree if `parent` node contains `subnode` as one of it's
  ## children
  discard

proc contains(mapping: Mapping, pair: (NodeId, NodeId)): bool =
  ## Return true if mapping between `pair[0]` and `pair[1]` exists in
  ## tree
  discard

proc notinImpl(lhs, rhs: NimNode): NimNode =
  if lhs.kind == nnkIdent:
    result = quote do:
      `lhs` notin `rhs`.table
  elif lhs.kind == nnkPar:
    if lhs[0] == ident "_":
      let lhIn = lhs[1]
      result = quote do:
        var found = false
        for lh, rh in `rhs`.table: # NOTE `O(n)` complexity
          if rh == `lhIn`:
            found = true
            break

        not found
    elif lhs[1] == ident "_":
      let lh = lhs[0]
      result = quote do:
        `lh` notin `rhs`.table
    else:
      let lh1 = lhs[0]
      let lh2 = lhs[1]
      result = quote do:
        `lh1` notin `rhs`.table or `rhs`.table[`lh2`] != `lh2`


macro `notin`(lhs: untyped, rhs: (Mapping, Mapping)): untyped =
  ## For lhs in form of `(src, _)` determine if there is a pair if any
  ## of two mappings that maps `src` to something. For `(_, target)`
  ## check if anything maps to `target`.
  result = nnkInfix.newTree(
    ident "and",
    notinImpl(lhs, rhs[0]),
    notinImpl(lhs, rhs[1])
  )

macro `notin`(lhs: untyped, rhs: Mapping): untyped =
  ## Same as `notin` for two mappings
  return notinImpl(lhs, rhs)

proc label(n: NodeId): Label =
  ## Get label associated with node
  n.index.idLabel[n]

proc value(n: NodeId): Value =
  ## Get value associated with node
  n.index.idVals[n]

proc isRoot(n: NodeId): bool =
  ## Return true if node is a root node
  n in n.index.parentTable and n.index.parentTable[n] != n

func `==`(a, b: NodeId): bool =
  (a.id == b.id) and (a.isSrc == b.isSrc)

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

proc len(id: NodeId): int =
  ## Get number of subnodes for node `id`
  id.index.subnodes[id].len

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
  que[que.len - 1].level

proc pop(que: var NodeQue): seq[NodeId] =
  ## Pop all nodes having height of `peekMax`
  discard

proc push(node: NodeId, que: var NodeQue) =
  ## Insert node `node` in que
  que.push(node)

proc open(node: NodeId, que: var NodeQue) =
  ## Insert all children of `t` into `l`
  for node in node.index.subnodes[node]:
    node.push(que)

iterator carthesian[T](a, b: seq[T]): (T, T) =
  ## iterate over carthesian product of two sequences
  for valA in a:
    for valB in b:
      yield (valA, valB)

proc isomorphic(t1, t2: NodeId): bool =
  ## Check if two trees are isomorphic
  t1.index.hashes[t1] == t2.index.hashes[t2]

proc root(id: NodeId): NodeId =
  ## Get root node for tree
  if id.id == 0 or id.index.parentTable[id] == id:
    id
  else:
    root(id.index.parentTable[id])

proc root(idx: TreeIndex): NodeId =
  for node, _ in idx.parentTable:
    # Just return root for any node
    return node.root

proc dice(t1, t2: NodeId, m: Mapping): float =
  ## ratio of common descendants between two nodes given a set of
  ## mappings M
  when false:
    2 .× ⟦ {t₁ .∈ s(t₁) | (t₁, t₂) .∈ M} ⟧
    ---------------------------------------
             ⟦ s(t₂) ⟧ + ⟦ s(t₁) ⟧

  var cntTop: int
  for node in t1:
    if (t1, t2) in m:
      inc cntTop

  result = (
    2 * (
      float(cntTop)
    )
  ) / (
    float(t1.len + t2.len)
  )

proc topDown(
  srcTree: NodeId,
  targetTree: NodeId,
  minHeight: int, minDice: float): Mapping =
  var
    srcQue: NodeQue
    targetQue: NodeQue
    A, map: Mapping

  push(root(srcTree), srcQue) ## store root node
  echov root(srcTree)
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
  srcTree: NodeId,
  targetTree: NodeId,
  map: Mapping, minDice: float, maxSize: int): Mapping =
  var map = map
  for src in srcTree:
    ## for all nodes in left, if node itself is not matched, but
    ## has any children matched
    if not src.matched() and src.children.anyOfIt(it.matched()):
      # get candidate node
      let target = candidate(src, map)
      ## if it is a valid candidate and matches criteria for
      ## minimum number of shares subnodes
      if target != nil and dice(src, target, map) > minDice:
        ## add node to mapping
        add(map, (src, target))
        ## if max of number of subnodes does not exceed threshold
        if max(s(src).len, s(target).len) < maxSize:
          let R = opt(src, target)
          for (ta, tb) in R:
            if ((ta, tb) notin map) and (label(ta) == label(tb)):
              add(map, (ta, tb))


proc findPos(index: TreeIndex, curr: NodeId, map: Mapping): int =
  let
    currPar = parent(curr)
    w = curr.partner(map)

  if currPar.leftmostInOrder() == curr:
    return 1

  for node in curr.siblings().left.reversed():
    if index.getInOrder(node):
      return node.partner(map).idx + 1

proc editScript(
  map: Mapping,
  srcTree: NodeId,
  targetTree: NodeId,
     ): EditScript =
  var srcTree = srcTree
  var script: EditScript
  var srcIndex = srcTree.index
  var targetIndex = targetTree.index

  proc alignChildren(
    other, curr: NodeId, index: var TreeIndex) =
    ## generate optimal sequence of moves that will align
    ## child nodes of w and x

    ## map all subnodes for both indices as out-of-order
    for ch in other:
      targetIndex.setInOrder(ch, false)

    for ch in curr:
      srcIndex.setInOrder(ch, false)

    let
      srcSubn: seq[NodeId] = collect(newSeq):
        for ch in other:
          if (curr, ch.partner(map)) in index:
            ch

      targetSubn: seq[NodeId] = collect(newSeq):
        for ch in curr:
          if (other, ch.partner(map)) in index:
            ch

      matches: seq[(NodeId, NodeId)] =
                LCS(srcSubn, targetSubn) do(a, b: NodeId) -> bool:
        ## left and right subnodes are considered equal if
        ## this is a pair which already exists in mapping.
        (a, b) in map

    for (a, b) in carthesian(srcSubn, targetSubn):
      ## For every pair of notes in carthesian produc of two lists
      ## (for source and target parent nodes)
      if ((a, b) in map) and ((a, b) notin matches):
        ## If `a-b` mapping exists, but is not in LCS mapirings for
        ## subnode lists - move this node to target index
        # QUESTION - might not be correct. Why do we check for pairing
        # inside `matches` in the first place?
        script.add makeMove(a, other, targetIndex.findPos(b, map))
        ## And apply movement to the source index
        script.applyLast(srcIndex)

        ## Mark both nodes as 'in order'. Changes not need to be
        ## propagated into callsite (?) since 'on order' propery
        ## exists only for subnode alignment.
        # REFACTOR maybe just get from `inOrder` property altogether
        # and keep hashset of on/out-of order nodes on callsite?
        srcIndex.setInOrder(a, true)
        targetIndex.setInOrder(b, true)

  for curr in targetTree.bfsIterate(targetIndex):
    ## iderate all nodes in tree in BFS order
    var tmp = curr.index
    let
      currPar = curr.parent ## parent node in right tree
      otherPar = currPar.partner(map) ## partner of right parent tree.
      ## left parent tree.

    if otherPar == nil:
      ## if current node's parent does not have a corresponding
      ## partner in mapping
      let currPos = srcIndex.findPos(curr, map)
      script.add makeIns(
        (partner(curr, map), label(curr), value(curr)),
        otherPar, currPos)

      script.applyLast(tmp)
    elif not isRoot(curr):
      ## if node parent has partner and the node itself
      ## is not root
      let
        other = curr.partner(map) ## get partner of current node
        otherPar = parent(other) ## parent of the partner

      ## I'd node and partner have different values
      if value(other) != value(curr):
        ## add update to edit script
        script.add makeUpd(other, value(curr))
        script.applyLast(tmp)


      ## if mapping current node and it's
      ## partner are not in mapping
      if (currPar, otherPar) notin map:
        let
          otherPar = currPar.partner(map)
          k = srcIndex.findPos(curr, map)

        script.add makeMove(other, otherPar, k)
        script.applyLast(srcIndex)


    ## align subnodes for current node and it's counterpart
    alignChildren(curr.partner(map), curr, srcIndex)

  for curr in srcTree.dfsIteratePost(srcIndex):
    ## for each node in post order traversal of left tree
    if curr.partner(map) == nil:
      ## if current node does not have a parent, remove it
      ## deletion will happen from leaves to roots
      script.add makeDel(curr)
      script.applyLast(srcIndex)

  return script

when isMainModule:
  let tree1 = Tree(value: "TREE-1-HEAD", label: 12, subn: @[
    Tree(value: "LEAF-1", label: 222),
    Tree(value: "LEAF-2", label: 333),
  ])

  let tree2 = Tree(value: "TREE-2-HEAD", label: 12, subn: @[
    Tree(value: "LEAF-1"),
    Tree(value: "LEAF-2"),
    Tree(value: "LEAF-3"),
  ])

  startHax()

  let srcIndex = makeIndex(tree1, true)

  let targetIndex = makeIndex(tree2, false)

  let root1 = srcIndex.root()
  let root2 = targetIndex.root()

  echov root1

  let mapping1 = topDown(root1, root2, minHeight = 2, minDice = 0.7)

  let mapping2 = bottomUp(
    root1, root2, map = mapping1, minDice = 0.7, maxSize = 100)

  let script = mapping2.editScript(root1, root2)

  pprint srcIndex
  pprint mapping1
  pprint mapping2

  pprint script

  echo "done"



