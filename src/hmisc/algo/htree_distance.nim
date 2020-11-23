import sugar, strutils, sequtils, strformat, heapqueue, tables,
       macros, algorithm, hashes, deques, sets, std/[enumerate, options]

import halgorithm
import hpprint
import hseq_distance
import htree_mapping

import ../hdebug_misc
import ../types/hprimitives


type
  NodeId = object
    id {.requiresinit.}: int
    isSource {.requiresinit.}: bool
    index {.requiresinit.}: TreeIndex
    path {.requiresinit.}: TreePath

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
    idLevels: Table[NodeId, int]
    inordNodes: HashSet[NodeId]
    deletedNodes: HashSet[NodeId]
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
        insertPath: TreePath
        label: Label
        value: Value
      of ekDel:
        delPath: TreePath
      of ekMov:
        oldMovPath, newMovPath: TreePath
      of ekUpd:
        updPath: TreePath
        newVal: Value

  EditScript = object
    cmds: seq[EditCmd]

func `$`(cmd: EditCmd): string =
  case cmd.kind:
    of ekIns: &"INS(({cmd.label}, {cmd.value}), {cmd.insertPath})"
    of ekDel: &"DEL({cmd.delPath})"
    of ekUpd: &"UPD({cmd.newVal}, {cmd.updPath})"
    of ekMov: &"MOV({cmd.oldMovPath}, {cmd.newMovPath})"


func hash(id: NodeId): Hash =
  !$(hash(id.id) !& hash(id.isSource))

func level(id: NodeId): int =
  if id.id == -1:
    int(10e9)
  else:
    if id notin id.index.idLevels:
      int(-999)
    else:
      id.index.idLevels[id]

func `$`(id: NodeId): string =
  var val: string
  if id.index.isNil:
    result = (if id.isSource: "s" else: "t") & "@" & $id.id & " # \e[91mNIL-INDEX\e[39m"
  else:
    if id in id.index.idVals:
      val = id.index.idVals[id]
    else:
      val = "NO-VALUE"


    result = (if id.isSource: "s" else: "t") & "@" & $id.id &
      " (" & $(id.level) & ") - " & val & " # " & $id.path

  result = &"'{result}'"

func `<`(id1, id2: NodeId): bool =
  id1.level < id2.level

proc makeIndex(tree: Tree, isSource: bool): TreeIndex =
  var index = TreeIndex()
  proc fillIndex(
    tree: Tree, isSource: bool, parentId: NodeId, subnIdx: int
       ): tuple[id: NodeId, hash: Hash, level: int] {.discardable.} =
    ## Fill tree index with nodes from `tree`
    result.id = NodeId(
      id: index.maxId,
      isSource: isSource,
      index: index,
      path: parentId.path & subnIdx
    )

    inc index.maxId

    var
      id: seq[NodeId]
      hashes: seq[Hash]
      depths: seq[int]

    for idx, sub in tree.subn:
      let (nId, hash, level) = fillIndex(sub, isSource, result.id, idx)
      id.add nId
      hashes.add hash
      depths.add level

    for node in id:
      index.parentTable[node] = result.id

    result.level = depths.max(0) + 1
    index.idVals[result.id] = tree.value
    index.idLabel[result.id] = tree.label
    index.idLevels[result.id] = result.level
    index.subnodes[result.id] = id

    var h: Hash = 0
    h = h !& hash(tree.label) !& hash(tree.value)
    for subh in hashes:
      h = h !& subh

    index.hashes[result.id] = !$h


  var parent = NodeId(
    index: index,
    id: index.maxId,
    isSource: isSource,
    path: @[]
  )

  let (id, hash, level) = fillIndex(tree, isSource, parent, 0)

  parent.path = rootTreePath
  index.parentTable[parent] = parent
  index.idLevels[parent] = level
  return index



proc add(es: var EditScript, ec: EditCmd): void =
  es.cmds.add ec

proc makeMove(oldPath, newPath: TreePath): EditCmd =
  EditCmd(
    kind: ekMov, oldMovPath: oldPath, newMovPath: newPath)

proc makeIns(
  newnode: tuple[label: Label, value: Value],
  insertPath: TreePath
     ): EditCmd =

  EditCmd(kind: ekIns,
          label: newnode.label,
          value: newnode.value,
          insertPath: insertPath)


proc makeUpd(path: TreePath, val: Value): EditCmd =
  EditCmd(kind: ekUpd, updPath: path, newVal: val)

proc makeDel(path: TreePath): EditCmd =
  EditCmd(kind: ekDel, delPath: path)


proc matched(n: NodeId): bool =
  discard

proc getInOrder(index: TreeIndex, b: NodeId): bool =
  b in index.inordNodes

proc setInOrder(b: NodeId, val: bool) =
  ## mark node as "in order"
  b.index.inordNodes.incl b

proc idx(n: NodeId): int =
  ## get node index in sequence of parent's subnodes
  discard

proc candidate(n: NodeId, m: Mapping): NodeId =
  discard

proc sourcePartner(n: NodeId, m: Mapping): NodeId =
  ## Get **node in source tree**. Node partner in source tree. `n` is a
  ## target tree node, result will be in source tree
  assert not n.isSource
  result = NodeId(id: -2, isSource: true, index: nil, path: @[])
  for key, val in m.table:
    if val == n:
      result = key
      break

  assert result.isSource

proc targetPartner(n: NodeId, m: Mapping): NodeId =
  ## Get **node in source tree**. Node partner in target tree. `n` is a
  ## source tree node, result will be in target tree
  assert n.isSource
  if n notin m.table:
    result = NodeId(id: -2, isSource: false, index: nil, path: @[])
  else:
    result = m.table[n]

  assert not result.isSource


proc `==`(n: NodeId, other: typeof(nil)): bool =
  n.id < 0

proc opt(t1, t2: NodeId): Mapping =
  ## ? Wtf is this shit
  discard

iterator items(m: Mapping): (NodeId, NodeId) =
  ## iterate over all pairs in mapping
  for key, val in m.table:
    yield (key, val)


# proc sort(m: var Mapping, cmp: proc(a, b: NodeId): bool) =
#   discard

proc lcs(lhs, rhs: seq[NodeId],
         eq: proc(a, b: NodeId): bool): seq[(NodeId, NodeId)] =
  let lcsres = longestCommonSubsequence(lhs, rhs, eq)
  echov lhs
  echov rhs
  echov lcsres
  let (_, lhsIdx, rhsIdx) = lcsres[0]

  for (lIdx, rIdx) in zip(lhsIdx, rhsIdx):
    result.add((lhs[lIdx], rhs[rIdx]))

func add(m: var Mapping, t: (NodeId, NodeId)) =
  ## Add mapping to mapping
  assert t[0] notin m.table
  m.table[t[0]] = t[1]

iterator items(id: NodeId): NodeId =
  ## Iterate over subnodes for node pointed to by `id`
  assert not id.index.isNil, $id
  if id in id.index.deletedNodes:
    raiseAssert("Attempt to iterate over deleted node subnodes")

  for node in id.index.subnodes[id]:
    yield node

proc contains(
  index: TreeIndex, pair: tuple[parent, subnode: NodeId]): bool =
  ## Returne tree if `parent` node contains `subnode` as one of it's
  ## subnodes
  for node in pair.parent:
    if node == pair.subnode:
      return true

proc contains(mapping: Mapping, pair: (NodeId, NodeId)): bool =
  ## Return true if mapping between `pair[0]` and `pair[1]` exists in
  ## tree
  pair[0] in mapping.table and mapping.table[pair[0]] == pair[1]

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
        `lh1` notin `rhs`.table or `rhs`.table[`lh1`] != `lh2`


macro `notin`(lhs: untyped, rhs: (Mapping, Mapping)): untyped =
  ## For lhs in form of `(source, _)` determine if there is a pair if any
  ## of two mappings that maps `source` to something. For `(_, target)`
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
  result = n.id == 0
  if result:
    assert n.path == rootTreePath, &"'{n}' is a root, but has invalid path"

func `==`(a, b: NodeId): bool =
  (a.id == b.id) and (a.isSource == b.isSource)

proc parent(t: NodeId): NodeId =
  ## Get parent node for node
  assert t.index != nil
  if t.isRoot():
    ## For root node create dummy 'supper root'
    result = t
    result.id = -1
  else:
    result = t.index.parentTable[t]


proc siblings(n: NodeId): tuple[left, right: seq[NodeId]] =
  ## return sibling nodes to the left and right of node
  var foundNode: bool = false
  for node in n.parent:
    if node == n:
      foundNode = true
    elif foundNode:
      result.right.add node
    else:
      result.left.add node


proc leftmostInOrder(n: NodeId): NodeId =
  ## return leftmost node that is marked as "in order"
  discard



func size(a: Mapping): int =
  ## get number of items in mapping
  a.table.len

proc len(id: NodeId): int =
  ## Get number of subnodes for node `id`
  id.index.subnodes[id].len

func remove(a: var Mapping, `?`: int): (NodeId, NodeId) =
  ## remove something from mapping. Now idea what ? Should actually be.
  discard

proc subnodes(node: NodeId): seq[NodeId] =
  ## Get list of children for node `node`
  node.index.subnodes[node]

proc `[]`(node: NodeId, idx: int): NodeId =
  node.index.subnodes[node][idx]

iterator bfsIterate(tree: NodeId): NodeId =
  ## Yield each node id in tree in BFS order
  iterateItBFS(tree, toSeq(it), true):
    yield it

iterator dfsIteratePost(tree: NodeId): NodeId =
  ## Yield each node in tree in post-order DFS traversal
  iterateItDFS(tree, toSeq(it), true, dfsPostorder):
    yield it

proc peekMax(que: NodeQue): int =
  ## Return greatest height of node in the list
  if que.len > 0:
    que[que.len - 1].level
  else:
    -1

proc pop(que: var NodeQue): seq[NodeId] =
  ## Pop all nodes having height of `peekMax`
  if que.len > 0:
    let maxH = que[que.len - 1].level
    while que.len > 0 and que[que.len - 1].level == maxH:
      result.add que[que.len - 1]
      que.del(que.len - 1)

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
    assert id.path == rootTreePath, $id
    result = id
  else:
    result = root(id.index.parentTable[id])

  assert result.path == rootTreePath, $result

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
  sourceTree: NodeId,
  targetTree: NodeId,
  minHeight: int, minDice: float): Mapping =
  var
    sourceQue: NodeQue
    targetQue: NodeQue
    tmpMap, resMap: Mapping

  push(root(sourceTree), sourceQue) ## store root node
  push(root(targetTree), targetQue) ## for each input
  echov sourceQue
  echov targetQue

  ## until subtree of necessary height hasn't been reached
  while min(peekMax(sourceQue), peekMax(targetQue)) > minHeight:
    ## if two top subtrees don't have equal height
    if peekMax(sourceQue) != peekMax(targetQue):
      ## insert all nodes from tallest subforest
      if peekMax(sourceQue) > peekMax(targetQue):
        for t in pop(sourceQue):
          open(t, sourceQue)
      else:
        for t in pop(targetQue):
          open(t, targetQue)

    else:
      ## otherwise get two subforest of equal height
      let
        sourceTops = pop(sourceQue)
        targetTops = pop(targetQue)

      ## for each combination of Therese is these forests
      for (source, target) in carthesian(sourceTops, targetTops):
        ## if pair of trees is isomorphic
        if isomorphic(source, target):
          ## If `source` node has more than one isomorphic pair in target
          ## index (or `target` has pair in source index)
          if true:
            # target.anyOfIt(isomorphic(source, it) and it != target) or
            # source.anyOfIt(isomorphic(it, target) and it != source)
            raiseAssert("#[ IMPLEMENT ]#")
            ## add both source and target to candidate list that will be
            ## processed later
            add(tmpMap, (source, target))

          else:
            ## otherwise (only single mapping from `source` to `target`)
            ## iterate over all pairs of child nodes,
            for (is1, is2) in carthesian(source.subnodes, target.subnodes):
              ## determine if they are isomorphic
              if isomorphic(is1, is2):
                ## and add matching ones to resulting tree
                add(resMap, (is1, is2))

                block footnote_1:
                  ## QUESTION for cases when we compare nodes like
                  when false: # hash 111
                    if expr: discard # hash - 01
                    if expr: discard # hash - 01
                    while expr: discard # hash - 10

                  when false: # hash 111
                    while expr: discard # hash - 01
                    if expr: discard # hash - 10
                    if expr: discard # hash - 10

                  ## which both have the same hash - (for `when false:`).
                  ## But there might be more than one pair of nodes for
                  ## carthesian produc. E.g. there is two pairs of
                  ## `if expr:`, which means that totatlly, there are
                  ## four pairs of nodes that can match to each other.



      ## so we basically determine if there is any isomorphic mapping
      ## between either (1) roots two highest subforests or (2) root
      ## and subnodes of a root in other tree

      for source in sourceTops:
        ## if there is unmatched forest root in first forest
        if (source, _) notin (tmpMap, resMap):
          ## insert it's subnodes
          open(source, sourceQue)

      for target in targetTops:
        ## do the same for other forest
        if (_, target) notin (tmpMap, resMap):
          open(target, targetQue)

  ## Order list of candicate mappings using `dice` function. Potential
  ## mappings are sorted using ration of similarity between their parent
  ## nodes. For two mappings, one with higher `dice` is located in tree are
  ## where surrounding nodes have better 'overall mapping'
  let orderedMap = sorted(toSeq(tmpMap)) do(
    it1, it2: (NodeId, NodeId)) -> int:
    # Paper says 'sort (t₁, t₂) ∈ A using dice(parent(t₁), parent(t₂),
    # M)' which is not a lot
    raiseAssert("#[ IMPLEMENT ]#")

  while size(tmpMap) > 0:
    let (sourceId, targetId)  = remove(tmpMap, 0)
    ## Add all pairs of isomprhic nodes of `s(source)` and `s(target)` to
    ## resulting mapping
    if isomorphic(sourceId, targetId):
      add(resMap, (sourceId, targetId))

    ## Remove mappings from temporary list `A`
    for source, tar in tmpMap.table:
      if source == sourceId or tar == targetId:
        tmpMap.table.del(source)

  return resMap

proc bottomUp(
  sourceTree: NodeId,
  targetTree: NodeId,
  map: Mapping, minDice: float, maxSize: int): Mapping =
  var map = map
  for source in sourceTree:
    ## for all nodes in left, if node itself is not matched, but
    ## has any children matched
    if not source.matched() and source.subnodes.anyOfIt(it.matched()):
      # get candidate node
      let target = candidate(source, map)
      ## if it is a valid candidate and matches criteria for
      ## minimum number of shares subnodes
      if target != nil and dice(source, target, map) > minDice:
        ## add node to mapping
        add(map, (source, target))
        ## if max of number of subnodes does not exceed threshold
        if max(source.len, target.len) < maxSize:
          let R = opt(source, target)
          for (ta, tb) in R:
            if ((ta, tb) notin map) and (label(ta) == label(tb)):
              add(map, (ta, tb))


proc findPos(curr: NodeId): int =
  let currPar = parent(curr)
  for idx, node in enumerate(currPar):
    if node == curr:
      return idx

  #   w = curr.sourcePartner(map) # WARNING

  # if currPar.leftmostInOrder() == curr:
  #   return 1

  # for node in curr.siblings().left.reversed():
  #   if index.getInOrder(node):
  #     return node.sourcePartner(#[ WARNING - potentially incorrect
  #                               *Partner` used ]# map).idx + 1


proc applyLast(script: EditScript, index: var TreeIndex): Option[NodeId] =
  ## Apply last added script element to tree `tree`. Return `NodeId` if any
  ## created
  let cmd = script.cmds[^1]
  echov cmd
  case cmd.kind:
    of ekIns:
      let node = NodeId(
        id: index.maxId,
        path: cmd.insertPath,
        index: index,
        isSource: true
      )

      index.idVals[node] = cmd.value
      index.idLabel[node] = cmd.label
      index.subnodes[node] = @[]

      inc index.maxId
      echo "Inserted node ", node
      return some(node)
    of ekDel:
      var node = index.root().followPath(cmd.delPath)
      index.subnodes[node.parent].del(cmd.delPath[^1])
      # TEST checl for repeated deletion - might need to swap iteration
      # order (although it is DFS-post now, so this is probably fine)
      echov index.subnodes[node.parent]

      index.idVals.del(node)
      index.idLabel.del(node)
      index.parentTable.del(node)
      index.subnodes.del(node)
      index.inordNodes.excl(node)
      index.idLevels.del(node)
      index.hashes.del(node)

      # index.deletedNodes.incl(node)
    of ekMov:
      discard
    of ekUpd:
      discard




proc alignSubn(
  sourceNode, targetNode: NodeId,
  index: var TreeIndex,
  map: Mapping,
  script: var EditScript) =
  assert not targetNode.isSource
  assert sourceNode.isSource
  ## generate optimal sequence of moves that will align
  ## child nodes of w and x

  ## map all subnodes for both indices as out-of-order
  for node in targetNode:
    setInOrder(node, false)

  for node in sourceNode:
    setInOrder(node, false)

  let
    sourceSubnodes: seq[NodeId] = collect(newSeq):
      for node in targetNode:
        let partner = node.sourcePartner(map)
        if partner in sourceNode.subnodes:
          partner

    targetSubnodes: seq[NodeId] = collect(newSeq):
      for node in sourceNode:
        let partner = node.targetPartner(map)
        if partner in targetNode.subnodes:
          partner

    matches: seq[(NodeId, NodeId)] =
              lcs(sourceSubnodes, targetSubnodes) do(a, b: NodeId) -> bool:
      ## left and right subnodes are considered equal if
      ## this is a pair which already exists in mapping.
      # echov (a, b)
      echov (a, b) in map
      echov (a, b)
      (a, b) in map

  ## For each `a ∈ S₁, b ∈ S₂` such that `(a, b) ∈ M` but `(a, b) ∉ S`
  for (sourceNode, targetNode) in carthesian(sourceSubnodes, targetSubnodes):
    ## For every pair of notes in carthesian produc of two lists
    ## (for source and target parent nodes)
    if ((sourceNode, targetNode) in map) and
       ((sourceNode, targetNode) notin matches):
      ## If `a-b` mapping exists, but is not in lcs mapirings for
      ## subnode lists - move this node to target index
      # QUESTION - might not be correct. Why do we check for pairing
      # inside `matches` in the first place?
      script.add makeMove(
        sourceNode.path,
        targetNode.path & targetNode.findPos()
      )

      ## And apply movement to the source index
      discard script.applyLast(index)

      setInOrder(sourceNode, true)
      setInOrder(targetNode, true)



proc editScript(
  map: Mapping,
  sourceTree: NodeId,
  targetTree: NodeId,
     ): tuple[script: EditScript, mapping: Mapping, updatedSource: TreeIndex] =
  var script: EditScript

  var sourceTree  = sourceTree
  var map         = map
  var sourceIndex = sourceTree.index
  var targetIndex = targetTree.index

  for targetSubn in targetTree.bfsIterate():
    ## Iterate all nodes in tree in BFS order
    let
      targetParent = targetSubn.parent ## parent node in right tree
      sourceSubn = targetSubn.sourcePartner(map) ## Partner of the target subnode
      ## to determine if `targetSubn` has matching.

    if sourceSubn == nil:
      ## if current node does not have a corresponding partner in mapping
      ## insert it (add `ins` action to edit script)

      ## Find the parent's partner, if there is one.
      let sourceParent = targetParent.sourcePartner(map)

      ## We are visiting nodes from the root down in BFS order, and parent
      ## should exist. If not, something unexpected happened earlier (and
      ## this is a bug).
      assert sourceParent != nil, &"No pair mapping for {targetParent}"

      # NOTE should work the same as python's xml index
      let sourcePos = findPos(targetSubn)

      script.add makeIns( ## Create insert action
        (
          label(targetSubn), ## Label of new inserted node
          value(targetSubn), ## and it's value
        ),
        targetParent.path & sourcePos, ## Position of the node to insert
        ## under + Index of newly inserted node. It is assumed that
        ## `sourceTree` has been transformed to have the same structure as
        ## `targetTree`.
      )

      let newNode = script.applyLast(sourceIndex).get()
      map.add((newNode, targetSubn))

    elif targetParent != nil:
      ## if node parent has partner and the node itself
      ## is not root
      let
        sourceSubn = targetSubn.sourcePartner(map) ## parent of the partner
        sourceParent = sourceSubn.parent

      ## I'd node and partner have different values
      if value(sourceSubn) != value(targetSubn):
        ## add update to edit script
        script.add makeUpd(
          targetSubn.path,
          targetSubn.value
        )

        ## Again - it is assumed tree has been transformed into same shape
        ## by earlier actions.
        assert sourceSubn.path == targetSubn.path


        discard script.applyLast(sourceIndex)


      ## if mapping current node and it's partner are not in mapping
      if (sourceParent, targetParent) notin map:
        let matchingTargetParent = sourceParent.targetPartner(map)
          # k = sourceIndex.findPos(targetSubn, map)

        script.add makeMove(
          # Move subnode from start index
          sourceSubn.path,
          # To position in the target tree, under matching parent using
          # index gived by `findPos`
          matchingTargetParent.path & findPos(targetParent)
        )

        discard script.applyLast(sourceIndex)


    ## align subnodes for current node and it's counterpart
    echo &"Aligning childred for {targetSubn.sourcePartner(map)} and {targetSubn}"
    alignSubn(targetSubn.sourcePartner(map), targetSubn, sourceIndex, map, script)

  for sourceNode in dfsIteratePost(sourceTree):
    ## for each node in post order traversal of left tree
    if sourceNode.targetPartner(map) == nil:
      ## if current node does not have a parent, remove it
      ## deletion will happen from leaves to roots
      script.add makeDel(sourceNode.path)
      discard script.applyLast(sourceIndex)

  return (script, map, sourceIndex)

proc simpleMatch*(sourceNode, targetNode: NodeId): Mapping =
  if sourceNode.path != targetNode.path:
    return

  var
    sourceNodes = @[sourceNode]
    targetNodes = @[targetNode]

  while sourceNodes.len > 0 and targetNodes.len > 0:
    for sourceNode in sourceNodes:
      for targetNode in targetNodes:
        if sourceNode.value == targetNode.value:
          echov (sourceNode.value, targetNode.value)
          result.add((sourceNode, targetNode))

    sourceNodes = concat: collect(newSeq):
      for node in sourceNodes:
        node.subnodes

    targetNodes = concat: collect(newSeq):
      for node in targetNodes:
        node.subnodes





when isMainModule:
  startHax()
  let tree1 = Tree(value: "TREE-HEAD", label: 12, subn: @[
    Tree(value: "LEAF-1", label: 222),
    Tree(value: "LEAF-2", label: 333),
    Tree(value: "LEAF-3"),
  ])

  let tree2 = Tree(value: "TREE-HEAD", label: 12, subn: @[
    Tree(value: "LEAF-1"),
    Tree(value: "LEAF-2"),
  ])


  let
    sourceIndex = makeIndex(tree1, true)
    targetIndex = makeIndex(tree2, false)
    root1       = sourceIndex.root()
    root2       = targetIndex.root()

  let mapping2 = simpleMatch(root1, root2)
  for k, v in mapping2.table:
    echov (k, v)

  let script = mapping2.editScript(root1, root2)

  pprint script.script
  pprint script.updatedSource
  pprint script.mapping

  echo "done"
