import sugar, strutils, sequtils, strformat, heapqueue, tables,
       macros, algorithm, hashes, deques, sets, std/[enumerate, options]

import hmisc/algo/[halgorithm, hseq_distance, htree_mapping]
import hmisc/hdebug_misc
import hmisc/types/hprimitives

export hash, Hash

type
  NodeId*[L, V] = object
    id {.requiresinit.}: int
    isSource {.requiresinit.}: bool
    index {.requiresinit.}: TreeIndex[L, V]
    path {.requiresinit.}: TreePath

  Mapping*[L, V] = object
    table: Table[NodeId[L, V], NodeId[L, V]]

  NodeQue[L, V] = HeapQueue[NodeId[L, V]]

  TreeIndex*[L, V] = ref object
    hashes: Table[NodeId[L, V], Hash]
    ## Hash for each node - used for efficient O(1) tree isomprhism test


    parentTable: Table[NodeId[L, V], NodeId[L, V]] ## node-parent

    idValues: Table[NodeId[L, V], V]
    idLabels: Table[NodeId[L, V], L]
    idLevels: Table[NodeId[L, V], int]
    inordNodes: HashSet[NodeId[L, V]]
    deletedNodes: HashSet[NodeId[L, V]]
    subnodes: Table[NodeId[L, V], seq[NodeId[L, V]]]
    ## Table of subnodes for each node

    maxId: int

  EditCmdKind* = enum
    ekIns
    ekDel
    ekMov
    ekUpd

  EditCmd*[L, V] = object
    newIdx: int
    case kind*: EditCmdKind
      of ekIns:
        insPath*: TreePath
        insLabel*: L
        insValue*: V
      of ekDel:
        delPath*: TreePath
      of ekMov:
        oldMovPath*, newMovPath*: TreePath
      of ekUpd:
        updPath*: TreePath
        updValue*: V

  EditScript*[L, V] = object
    cmds: seq[EditCmd[L, V]]


proc makeInvalidNode[L, V](isSource: bool): NodeId[L, V] =
  NodeId[L, V](id: -1, index: nil, path: @[], isSource: isSource)


func `$`*[L, V](cmd: EditCmd[L, V]): string =
  case cmd.kind:
    of ekIns: &"INS(('{cmd.insLabel}', '{cmd.insValue}'), {cmd.insPath})"
    of ekDel: &"DEL({cmd.delPath})"
    of ekUpd: &"UPD({cmd.updValue}, {cmd.updPath})"
    of ekMov: &"MOV({cmd.oldMovPath}, {cmd.newMovPath})"

iterator items*[L, V](script: EditScript[L, V]): EditCmd[L, V] =
  for cmd in script.cmds:
    yield cmd

func len*[L, V](script: EditScript[L, V]): int = script.cmds.len
func `[]`*[L, V](script: EditScript[L, V], idx: int): EditCmd[L, V] =
  script.cmds[idx]

iterator pairs*[L, V](matching: Mapping[L, V]): tuple[source, target: NodeId[L, V]] =
  for k, v in pairs(matching.table):
    yield (k, v)

func hash*[L, V](id: NodeId[L, V]): Hash =
  !$(hash(id.id) !& hash(id.isSource))

func level[L, V](id: NodeId[L, V]): int =
  if id.id == -1:
    int(10e9)
  else:
    if id notin id.index.idLevels:
      int(-999)
    else:
      id.index.idLevels[id]

func `$`*[L, V](id: NodeId[L, V]): string =
  if id == makeInvalidNode[L, V](id.isSource):
    if id.isSource:
      result = "\e[33mINVALID-SOURCE\e[39m"
    else:
      result = "\e[33mINVALID-TARGET\e[39m"
  else:
    if id.index.isNil:
      result = (if id.isSource: "s" else: "t") & "@" & $id.id & " # \e[91mNIL-INDEX\e[39m"
    else:
      let value = if id in id.index.idValues: $id.index.idValues[id] else: "NO-VALUE"
      let label = if id in id.index.idLabels: $id.index.idLabels[id] else: "NO-LABEL"


      result = (if id.isSource: "s" else: "t") & "@" & $id.id &
        " [" & label & "/" & value & "] " & $id.path

  result = &"'{result}'"

func `<`[L, V](id1, id2: NodeId[L, V]): bool =
  id1.level < id2.level

proc makeIndex*[T, L, V](
  tree: T,
  isSource: bool,
  getValue: proc(t: T): V,
  getLabel: proc(t: T): L,
                      ): TreeIndex[L, V] =
  var index = TreeIndex[L, V]()
  var m: Table[NodeId[L, V], seq[NodeId[L, V]]]
  if false: echo m.len
  proc fillIndex(
    tree: T, isSource: bool, parentId: NodeId[L, V], subnIdx: int
       ): tuple[id: NodeId[L, V], hash: Hash, level: int] {.discardable.} =
    ## Fill tree index with nodes from `tree`
    result.id = NodeId[L, V](
      id: index.maxId,
      isSource: isSource,
      index: index,
      path: parentId.path & subnIdx
    )

    inc index.maxId

    var
      id: seq[NodeId[L, V]]
      hashes: seq[Hash]
      depths: seq[int]

    mixin len
    for idx in 0 ..< tree.len:
      let sub = tree[idx]
      let (nId, hash, level) = fillIndex(sub, isSource, result.id, idx)
      id.add nId
      hashes.add hash
      depths.add level

    for node in id:
      index.parentTable[node] = result.id

    result.level = depths.max(0) + 1
    index.idValues[result.id] = getValue(tree)
    index.idLabels[result.id] = getLabel(tree)
    index.idLevels[result.id] = result.level
    # static:
    #   echo typeof(index.subnodes)
    #   echo typeof(result.id)
    #   echo typeof(id)

    index.subnodes[result.id] = id

    var h: Hash = 0
    h = h !& hash(getLabel(tree)) !& hash(getValue(tree))
    for subh in hashes:
      h = h !& subh

    index.hashes[result.id] = !$h


  var parent = NodeId[L, V](
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



proc add[L, V](es: var EditScript[L, V], ec: EditCmd[L, V]): void =
  es.cmds.add ec

proc makeMove[L, V](oldPath, newPath: TreePath): EditCmd[L, V] =
  EditCmd[L, V](
    kind: ekMov, oldMovPath: oldPath, newMovPath: newPath)

proc makeIns[L, V](
    newnode: tuple[label: L, value: V], insertPath: TreePath
  ): EditCmd[L, V] =

  EditCmd[L, V](kind: ekIns,
          insLabel: newnode.label,
          insValue: newnode.value,
          insPath: insertPath)


proc makeUpd[L, V](path: TreePath, val: V): EditCmd[L, V] =
  EditCmd[L, V](kind: ekUpd, updPath: path, updValue: val)


proc makeDel[L, V](path: TreePath): EditCmd[L, V] =
  EditCmd[L, V](kind: ekDel, delPath: path)


proc matched[L, V](n: NodeId[L, V]): bool =
  discard

proc getInOrder[L, V](index: TreeIndex, b: NodeId[L, V]): bool =
  b in index.inordNodes

proc setInOrder[L, V](b: NodeId[L, V], val: bool) =
  ## mark node as "in order"
  b.index.inordNodes.incl b

proc idx[L, V](n: NodeId[L, V]): int =
  ## get node index in sequence of parent's subnodes
  discard

proc candidate[L, V](n: NodeId[L, V], m: Mapping): NodeId[L, V] =
  discard

proc sourcePartner[L, V](n: NodeId[L, V], m: Mapping): NodeId[L, V] =
  ## Get **node in source tree**. Node partner in source tree. `n` is a
  ## target tree node, result will be in source tree
  assert not n.isSource
  result = makeInvalidNode[L, V](isSource = true)
  for key, val in pairs(m.table):
    if val == n:
      result = key
      break

  assert result.isSource

proc targetPartner[L, V](n: NodeId[L, V], m: Mapping): NodeId[L, V] =
  ## Get **node in source tree**. Node partner in target tree. `n` is a
  ## source tree node, result will be in target tree
  assert n.isSource
  if n notin m.table:
    result = makeInvalidNode[L, V](isSource = false)
  else:
    result = m.table[n]

  assert not result.isSource


proc `==`[L, V](n: NodeId[L, V], other: typeof(nil)): bool =
  n.id < 0

proc opt[L, V](t1, t2: NodeId[L, V]): Mapping =
  ## ? Wtf is this shit
  discard

iterator items[L, V](m: Mapping): (NodeId[L, V], NodeId[L, V]) =
  ## iterate over all pairs in mapping
  for key, val in m.table:
    yield (key, val)


# proc sort(m: var Mapping, cmp: proc(a, b: NodeId[L, V]): bool) =
#   discard

proc lcs[L, V](lhs, rhs: seq[NodeId[L, V]],
         eq: proc(a, b: NodeId[L, V]): bool): seq[(NodeId[L, V], NodeId[L, V])] =
  let lcsres = longestCommonSubsequence(lhs, rhs, eq)
  let (_, lhsIdx, rhsIdx) = lcsres[0]

  for (lIdx, rIdx) in zip(lhsIdx, rhsIdx):
    result.add((lhs[lIdx], rhs[rIdx]))

func add[L, V](m: var Mapping, t: (NodeId[L, V], NodeId[L, V])) =
  ## Add mapping to mapping
  assert t[0] notin m.table
  m.table[t[0]] = t[1]

iterator items[L, V](id: NodeId[L, V]): NodeId[L, V] =
  ## Iterate over subnodes for node pointed to by `id`
  assert not id.index.isNil, $id
  if id in id.index.deletedNodes:
    raiseAssert("Attempt to iterate over deleted node subnodes")

  for node in id.index.subnodes[id]:
    yield node

proc contains*[L, V](
  index: TreeIndex, pair: tuple[parent, subnode: NodeId[L, V]]): bool =
  ## Returne tree if `parent` node contains `subnode` as one of it's
  ## subnodes
  for node in pair.parent:
    if node == pair.subnode:
      return true

proc contains*[L, V](mapping: Mapping, pair: (NodeId[L, V], NodeId[L, V])): bool =
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
        (not contains(`rhs`.table, `lh1`)) or
        not(`rhs`.table[`lh1`] == `lh2`)
        # `lh1` notin `rhs`.table or `rhs`.table[`lh1`] != `lh2`


macro `notin`[L, V](lhs: untyped, rhs: (Mapping[L, V], Mapping[L, V])): untyped =
  ## For lhs in form of `(source, _)` determine if there is a pair if any
  ## of two mappings that maps `source` to something. For `(_, target)`
  ## check if anything maps to `target`.
  result = nnkInfix.newTree(
    ident "and",
    notinImpl(lhs, rhs[0]),
    notinImpl(lhs, rhs[1])
  )

macro `notin`[L, V](lhs: untyped, rhs: Mapping[L, V]): untyped =
  ## Same as `notin` for two mappings
  return notinImpl(lhs, rhs)

proc label[L, V](n: NodeId[L, V]): L =
  ## Get label associated with node
  n.index.idLabels[n]

proc value[L, V](n: NodeId[L, V]): V =
  ## Get value associated with node
  n.index.idValues[n]

proc isRoot[L, V](n: NodeId[L, V]): bool =
  ## Return true if node is a root node
  result = n.id == 0
  if result:
    assert n.path == rootTreePath, &"'{n}' is a root, but has invalid path"

func `==`[L, V](a, b: NodeId[L, V]): bool =
  (a.id == b.id) and (a.isSource == b.isSource)

proc parent[L, V](t: NodeId[L, V]): NodeId[L, V] =
  ## Get parent node for node
  assert t.index != nil
  if t.isRoot():
    ## For root node create dummy 'supper root'
    result = makeInvalidNode[L, V](t.isSource)
  else:
    result = t.index.parentTable[t]


proc siblings[L, V](n: NodeId[L, V]): tuple[left, right: seq[NodeId[L, V]]] =
  ## return sibling nodes to the left and right of node
  var foundNode: bool = false
  for node in n.parent:
    if node == n:
      foundNode = true
    elif foundNode:
      result.right.add node
    else:
      result.left.add node


proc leftmostInOrder[L, V](n: NodeId[L, V]): NodeId[L, V] =
  ## return leftmost node that is marked as "in order"
  discard



func size[L, V](a: Mapping[L, V]): int =
  ## get number of items in mapping
  a.table.len

proc len[L, V](id: NodeId[L, V]): int =
  ## Get number of subnodes for node `id`
  id.index.subnodes[id].len

func remove[L, V](a: var Mapping[L, V], `?`: int): (NodeId[L, V], NodeId[L, V]) =
  ## remove something from mapping. Now idea what ? Should actually be.
  discard

proc subnodes[L, V](node: NodeId[L, V]): seq[NodeId[L, V]] =
  ## Get list of children for node `node`
  node.index.subnodes[node]

proc `[]`*[L, V](node: NodeId[L, V], idx: int): NodeId[L, V] =
  node.index.subnodes[node][idx]

func followPath*[L, V](node: NodeId[L, V], path: TreePath): NodeId[L, V] =
  var res: NodeId[L, V] = node
  for step in path.pathTail():
    res = node.index.subnodes[node][step] # node[step]

  res



iterator bfsIterate[L, V](tree: NodeId[L, V]): NodeId[L, V] =
  ## Yield each node id in tree in BFS order
  iterateItBFS(tree, toSeq(items(it)), true):
    yield it

iterator dfsIteratePost[L, V](tree: NodeId[L, V]): NodeId[L, V] =
  ## Yield each node in tree in post-order DFS traversal
  iterateItDFS(tree, toSeq(items(it)), true, dfsPostorder):
    yield it

proc peekMax[L, V](que: NodeQue[L, V]): int =
  ## Return greatest height of node in the list
  if que.len > 0:
    que[que.len - 1].level
  else:
    -1

proc pop[L, V](que: var NodeQue[L, V]): seq[NodeId[L, V]] =
  ## Pop all nodes having height of `peekMax`
  if que.len > 0:
    let maxH = que[que.len - 1].level
    while que.len > 0 and que[que.len - 1].level == maxH:
      result.add que[que.len - 1]
      que.del(que.len - 1)

proc push[L, V](node: NodeId[L, V], que: var NodeQue[L, V]) =
  ## Insert node `node` in que
  que.push(node)

proc open[L, V](node: NodeId[L, V], que: var NodeQue[L, V]) =
  ## Insert all children of `t` into `l`
  for node in node.index.subnodes[node]:
    node.push(que)

iterator carthesian[T](a, b: seq[T]): (T, T) =
  ## iterate over carthesian product of two sequences
  for valA in a:
    for valB in b:
      yield (valA, valB)

proc isomorphic[L, V](t1, t2: NodeId[L, V]): bool =
  ## Check if two trees are isomorphic
  t1.index.hashes[t1] == t2.index.hashes[t2]

proc root*[L, V](id: NodeId[L, V]): NodeId[L, V] =
  ## Get root node for tree
  if id.id == 0 or id.index.parentTable[id] == id:
    assert id.path == rootTreePath, $id
    result = id
  else:
    result = root(id.index.parentTable[id])

  assert result.path == rootTreePath, $result

proc root*[L, V](idx: TreeIndex[L, V]): NodeId[L, V] =
  for node, _ in pairs(idx.parentTable):
    # Just return root for any node
    return node.root

proc dice[L, V](t1, t2: NodeId[L, V], m: Mapping[L, V]): float =
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

proc topDown[L, V](
  sourceTree: NodeId[L, V],
  targetTree: NodeId[L, V],
  minHeight: int, minDice: float): Mapping[L, V] =
  var
    sourceQue: NodeQue[L, V]
    targetQue: NodeQue[L, V]
    tmpMap, resMap: Mapping[L, V]

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
    it1, it2: (NodeId[L, V], NodeId[L, V])) -> int:
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

proc bottomUp[L, V](
  sourceTree: NodeId[L, V],
  targetTree: NodeId[L, V],
  map: Mapping[L, V], minDice: float, maxSize: int): Mapping[L, V] =
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


proc findPos[L, V](curr: NodeId[L, V]): int =
  let currPar = parent(curr)
  for idx, node in enumerate(items(currPar)):
    if node == curr:
      return idx

  #   w = curr.sourcePartner(map) # WARNING

  # if currPar.leftmostInOrder() == curr:
  #   return 1

  # for node in curr.siblings().left.reversed():
  #   if index.getInOrder(node):
  #     return node.sourcePartner(#[ WARNING - potentially incorrect
  #                               *Partner` used ]# map).idx + 1


proc applyLast[L, V](
    script: EditScript[L, V], index: var TreeIndex[L, V]
  ): Option[NodeId[L, V]] =
  ## Apply last added script element to tree `tree`. Return `NodeId[L, V]` if any
  ## created
  let cmd = script.cmds[^1]
  case cmd.kind:
    of ekIns:
      let node = NodeId[L, V](
        id: index.maxId,
        path: cmd.insPath,
        index: index,
        isSource: true
      )

      index.idValues[node] = cmd.insValue
      index.idLabels[node] = cmd.insLabel
      index.subnodes[node] = @[]

      inc index.maxId
      return some(node)
    of ekDel:
      bind `[]`
      var node = followPath(index.root(), cmd.delPath)
      index.subnodes[node.parent].del(cmd.delPath[^1])
      # TEST checl for repeated deletion - might need to swap iteration
      # order (although it is DFS-post now, so this is probably fine)

      index.idValues.del(node)
      index.idLabels.del(node)
      index.parentTable.del(node)
      index.subnodes.del(node)
      index.inordNodes.excl(node)
      index.idLevels.del(node)
      index.hashes.del(node)

      # index.deletedNodes.incl(node)
    of ekMov:
      discard
      # raiseAssert("#[ IMPLEMENT ]#")
    of ekUpd:
      var node = index.root().followPath(cmd.updPath)
      index.idValues[node] = cmd.updValue
      # raiseAssert("#[ IMPLEMENT ]#")

proc apply*[T, L, V](
  tree: var T,
  cmd: EditCmd[L, V],
  setValue: proc(t: var T, value: V),
  newTree: proc(label: L, value: V): T,
  setSubnode: proc(t: var T, index: int, subnode: T),
  delSubnode: proc(t: var T, index: int)): void =
  case cmd.kind:
    of ekIns:
      let nodePtr = tree.followPathPtr(cmd.insPath[0 .. ^2])
      setSubnode(
        nodePtr[],
        cmd.insPath[^1],
        newTree(
          label = cmd.insLabel,
          value = cmd.insValue
        )
      )
    of ekDel:
      var nodePtr = tree.followPathPtr(cmd.delPath[0 ..^ 2])
      delSubnode(nodePtr[], cmd.delPath[^1])
    else:
      var
        sourceParent: ptr T = tree.followPathPtr(cmd.oldMovPath[0 ..^ 2])
        targetParent: ptr T = tree.followPathPtr(cmd.newMovPath[0 ..^ 2])

      # if cmd.oldMovPath[0..^2] == cmd.newMovPath[0..^2]:
      #   swap(sourceParent[cmd.oldMovPath[^1]], targetParent)

      let node = sourceParent[][cmd.oldMovPath[^1]]
      delSubnode(sourceParent[], cmd.oldMovPath[^1])
      setSubnode(targetParent[], cmd.newMovPath[^1], node)





proc alignSubn[L, V](
    sourceNode, targetNode: NodeId[L, V],
    index: var TreeIndex[L, V],
    map: Mapping[L, V],
    script: var EditScript[L, V]
  ) =
  assert not targetNode.isSource
  assert sourceNode.isSource
  ## generate optimal sequence of moves that will align
  ## child nodes of w and x

  ## map all subnodes for both indices as out-of-order
  for node in items(targetNode):
    setInOrder(node, false)

  for node in items(sourceNode):
    setInOrder(node, false)

  let
    sourceSubnodes: seq[NodeId[L, V]] = collect(newSeq):
      for node in items(targetNode):
        let partner = node.sourcePartner(map)
        if partner in sourceNode.subnodes:
          partner

    targetSubnodes: seq[NodeId[L, V]] = collect(newSeq):
      for node in items(sourceNode):
        let partner = node.targetPartner(map)
        if partner in targetNode.subnodes:
          partner

    matches: seq[(NodeId[L, V], NodeId[L, V])] =
              lcs(sourceSubnodes, targetSubnodes) do(a, b: NodeId[L, V]) -> bool:
      ## left and right subnodes are considered equal if this is a pair
      ## which already exists in mapping.
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
      script.add makeMove[L, V](
        sourceNode.path,
        targetNode.path[0..^2] & targetNode.findPos()
      )

      ## And apply movement to the source index
      discard script.applyLast(index)

      setInOrder(sourceNode, true)
      setInOrder(targetNode, true)



proc editScript*[L, V](
    map: Mapping[L, V],
    sourceTree: NodeId[L, V],
    targetTree: NodeId[L, V],
  ): tuple[
    script: EditScript[L, V],
    mapping: Mapping[L, V],
    updatedSource: TreeIndex[L, V]
  ] =

  var script: EditScript[L, V]

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
      if targetSubn.isRoot():
        ## Paper says that it is possible to assume that two root nodes are
        ## matched without loss of generality, by inserting dummy roots,
        ## but I find it easier to just edge-case this and create new root
        ## directly, if there is no match for `targetRoot`
        script.add makeIns(
          (
            label(targetSubn),
            value(targetSubn),
          ),
          @[0], ## Directly insert as root node for source tree
        )

      else:
        ## if current node does not have a corresponding partner in mapping
        ## insert it (add `ins` action to edit script)

        ## Find the parent's partner, if there is one.
        let sourceParent = targetParent.sourcePartner(map)

        ## We are visiting nodes from the root down in BFS order, and parent
        ## should exist. If not, something unexpected happened earlier (and
        ## this is a bug).
        assert not(sourceParent == nil), &"No pair mapping for {targetParent}"

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

    elif not(targetParent == nil):
      ## Node parent has partner
      let
        sourceSubn = targetSubn.sourcePartner(map) ## parent of the partner
        sourceParent = sourceSubn.parent

      ## I'd node and partner have different values
      if value(sourceSubn) != value(targetSubn):
        ## add update to edit script
        script.add makeUpd[L, V](
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

        script.add makeMove[L, V](
          # Move subnode from start index
          sourceSubn.path,
          # To position in the target tree, under matching parent using
          # index gived by `findPos`
          matchingTargetParent.path & findPos(targetParent)
        )

        discard script.applyLast(sourceIndex)


    ## align subnodes for current node and it's counterpart
    alignSubn(targetSubn.sourcePartner(map), targetSubn, sourceIndex, map, script)

  for sourceNode in dfsIteratePost(sourceTree):
    ## for each node in post order traversal of left tree
    if sourceNode.targetPartner(map) == nil:
      ## if current node does not have a parent, remove it
      ## deletion will happen from leaves to roots
      script.add makeDel[L, V](sourceNode.path)
      discard script.applyLast(sourceIndex)

  return (script, map, sourceIndex)

proc simpleMatch*[L, V](
  sourceNode, targetNode: NodeId[L, V],
  valueScore: ScoreCmpProc[V],
  similarityTreshold: int = 60): Mapping[L, V] =
  if sourceNode.path != targetNode.path:
    return

  var
    sourceNodes = @[sourceNode]
    targetNodes = @[targetNode]

  while sourceNodes.len > 0 and targetNodes.len > 0:
    var bestPairs: OrderedTable[NodeId[L, V], tuple[node: NodeId[L, V], score: int]]

    for sourceNode in sourceNodes:
      for targetNode in targetNodes:
        let score = valueScore(sourceNode.value, targetNode.value)
        if (sourceNode.label == targetNode.label) and
           (score > similarityTreshold)
          :
          if (not contains(bestPairs, sourceNode) or
             score > bestPairs[sourceNode].score):
            var hasVal = false
            for k, v in pairs(bestPairs):
              if v.node == targetNode:
                hasVal = true

            if not hasVal:
              # Avoid matching nodes with identical labels and values.
              bestPairs[sourceNode] = (targetNode, score)

    for source, target in pairs(bestPairs):
      result.add (source, target.node)

    sourceNodes = concat: collect(newSeq):
      for node in sourceNodes:
        node.subnodes

    targetNodes = concat: collect(newSeq):
      for node in targetNodes:
        node.subnodes

proc simpleTreeDiff*[T, L, V](
    source, target: T,
    getLabel: proc(t: T): L,
    getValue: proc(t: T): V,
    valueScore: ScoreCmpProc[V],
    similarityTreshold: int = 60,
    initialMapping: bool = true
  ): auto =
  # static:
  #   echo "\e[41m*===\e[49m  ===============  \e[41m====*\e[49m"

  discard makeIndex[T, L, V](target, false, getValue, getLabel)
  var m: Mapping[L, V]
  let
    sourceIndex: TreeIndex[L, V] = makeIndex[T, L, V](
      source, true, getValue, getLabel)

  let
    targetIndex: TreeIndex[L, V] = makeIndex[T, L, V](
      target, false, getValue, getLabel)

  let
    mapping: Mapping[L, V] = simpleMatch[L, V](
      sourceIndex.root,
      targetIndex.root,
      similarityTreshold = similarityTreshold,
      valueScore = valueScore
    )

  for k, v in mapping:
    echov (k, v)

  let res = editScript(mapping, sourceIndex.root, targetIndex.root)
  if initialMapping:
    return (res.script, mapping)
  else:
    return (res.script, res.mapping)
