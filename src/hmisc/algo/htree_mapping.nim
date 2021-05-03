import options, macros, deques
# export deques

#=================================  BFS  =================================#
# IDEA `itemsBFS` and `itemsDFS` to iterate over nodes in tree
# IDEA `pairsBFS` and `pairsDFS` to iterate over paths
#      (similar to index?) + nodes in tree
# IDEA it is possible to support DAG by computing hash of each node
# TODO Use `iterateItBFS` for `mapItBFStoSeq` implementation
# TODO iterate over *mutable* tree nodes in BFS/DFS order

template toSeqImpl(op: untyped): untyped =
  when compiles((for item in items(op): discard)):
    var res: seq[typeof(items(op))]
    for i in items(op):
      res.add i
    res

  elif compiles((discard op.len; discard op[0])):
    var res: seq[typeof(op[0])]
    for i in 0 ..< op.len:
      res.add op[0]
    res

  else:
    static:
      error "Neither `items` nor `len`, `[]` are implemented for " &
        $typeof(op) & " and this type cannot be user for tree iteration."


template mapItBFStoSeq*(
  topNode: typed,
  subNode: untyped,
  op: untyped,
  hasSubnodes: untyped = true): untyped =
  # IDEA maybe add better static type checking? Something like
  # boost's concepts: "the top node type must satisfy requirement
  # is-tree-on-$topNode". And I will check that `subNode` is indeed
  # contains objects of the Node type.
  ##[

Do BFS iteration on recursive data type and map results into sequence

## Parameters

:topNode: First node in tree
:subNode: Expression to get subnodes
:op: Expression to get result.
  If `typeof(op)` is `Option` `none` results are discarded
:hasSubnode: Check if particular node can have subnodes

## Injected variables

:it: Current node in tree
:lv: level of current node in original tree

  ]##
  # TODO replace `lv` with `path`
  # TODO assert correct types for `topNode...` and `subNodes...`
  type OutType = type((
    block:
      var tmp: ref type(topNode)
      var it {.inject.}: type(topNode) = tmp[]
      var lv {.inject.}: int
      op))

  when OutType is Option:
    type ResType = type((var res: OutType; get(res)))
  else:
    type ResType = OutType

  type VertType = typeof((topNode))
  var result: seq[ResType] = @[]

  mixin addLast, popFirst
  var q = initDeque[(VertType, int)]()
  q.addLast((topNode, 0))

  while q.len != 0:
    var tmp = q.popFirst()
    let it {.inject.}: VertType = tmp[0]
    let lv {.inject.}: int = tmp[1]

    when OutType is Option:
      let tmpRes: OutType = op
      if tmpRes.isSome():
        result.add tmpRes.get()
    else:
      result.add(op)

    if hasSubnodes:
      for sub in toSeqImpl(subNode):
        static:
          assert sub is VertType,
            "Mismatch between type of the subnodes and root tree - subnode is " &
            $typeof(sub) & ", while `topNode` is " & $typeof(topNode)

        q.addLast((sub, lv + 1))

  result


template iterateItBFS*(
  topNode: typed,
  subNode: untyped,
  hasSubnodes: untyped,
  body: untyped): untyped =
  ##[

Loop over tree nodes in BFS order

  ]##
  mixin addLast
  mixin popFirst
  type VertType = typeof((topNode))
  var q = initDeque[VertType]()
  q.addLast(topNode)

  while q.len != 0:
    let it {.inject.} = q.popFirst()
    let subt {.inject.} =
      if hasSubnodes:
        toSeqImpl(subNode)
      else:
        @[]

    block:
      body

    if hasSubnodes:
       for sub in toSeqImpl(subt):
        static:
          assert sub is VertType,
            "Mismatch between type of the subnodes and root tree" &
            "- subnode is " & $typeof(sub) & ", while `topNode` is " &
            $typeof(topNode)

        q.addLast(sub)


template iterateItBFS*(tree, body: untyped): untyped =
  iterateItBFS(tree, it, true, body)


#*************************************************************************#
#*********************************  DFS  *********************************#
#*************************************************************************#

#==========================  recursive mapper  ===========================#

proc mapDFSpost*[InTree, OutTree, CbRes](
  tree: InTree,
  map: proc(
    n: InTree,
    path: seq[int],
    subn: seq[OutTree],
    inSubn: seq[InTree]
  ): CbRes,
  getSubnodes: proc(tree: InTree): seq[InTree],
  hasSubnodes: proc(it: InTree): bool = (proc(it: InTree): bool = true),
  path: seq[int] = @[0]): CbRes =
  ##[

Map one tree into another

Convert one tree type into another using post-order DFS traversal.
Tree is iterated in bottom-up manner. After leaving each node `map` is
called to generate new result. `subn` is a result of previous.

For tree like this `map` will first be executed for `IN1.1` through
`IN1.3`. Results of the map will be passed as `subn`.

.. code::
    +-------+     +-------+     +-------+
    | IN1.3 | <-- |  IN1  | --> | IN1.1 |
    +-------+     +-------+     +-------+
                    |
                    |
                    v
                  +-------+
                  | IN1.2 |
                  +-------+


## Parameters

:tree: Input tree
:map: Procedure for converting one tree into another.

  `CbRes` can be either `OutTree` or `Option[OutTree]`. In latter case
  `none()` results will be filtered out from the `subn`

  :n: Input subtree
  :path: Path of the current subtree in original node
  :subn: Evaluation results from child nodes.
  :inSubn: List of child nodes for original node.

  Elements in `subn` and `inSubn` are ordered identically: `subn[i] ==
  map(inSubn[i], ...)`

:getSubnodes: Get subnodes from current tree
:hasSubnodes: Check if input node can have subnodes

  ]##

  # IDEA if `OutTree` is a sequence perform recursive concatenation of
  # the items instead of joining them in tree.
  static:
    assert (CbRes is OutTree) or (CbRes is Option[OutTree])

  var subnodes: seq[InTree]
  let nodeRes: seq[OutTree] =
    if hasSubnodes(tree):
      var tmp: seq[OutTree]
      subnodes = getSubnodes(tree)
      for idx, node in subnodes:
        var res = mapDFSpost(
          node, map, getSubnodes, hasSubnodes,  path & @[idx])

        when CbRes is Option[OutTree]:
          if res.isSome():
            tmp.add res.get()
        else:
          tmp.add res

      tmp
    else:
      @[]

  return map(tree, path, nodeRes, subnodes)


proc mapDFSpost*[InTree, OutTree](
    tree: InTree,
    map: proc(n: InTree, subn: seq[OutTree]): Option[OutTree],
    getSubnodes: proc(tree: InTree): seq[InTree],
    hasSubnodes: proc(it: InTree): bool = (proc(it: InTree): bool = true),
    path: seq[int] = @[0]
  ): Option[OutTree] =
  ##[

Map @param{InTree} to @param{OutTree} using @arg{map} for node conversion.


- @arg{tree} :: Input tree structure
- @arg{map} :: Conversion proc for subnodes.
  - @arg{n} :: Input tree node
  - @arg{subn} :: Sequence of already converted subnodes
  - @ret{} :: Return new node with all subnodes added. If original @arg{n}
    does not map into any node in new tree might return `none()`
- @arg{getSubnodes} :: Return sequence of child nodes for @arg{tree}
- @arg{hasSubnodes} :: Check if tree node has subnodes. Defaults to
  always-true. Override is necessary for cases where @arg{getSubnodes} can
  raise on invalid node kind (instead of returning empty sequence)
- @arg[internal]{path} :: Path @arg{tree} in original tree.

]##

  ## Overload without `path` for `map`
  # TODO DOC
  return mapDFSpost(
    tree = tree,
    map = proc(
      n: InTree,
      path: seq[int],
      subn: seq[OutTree],
      inSubn: seq[InTree]
    ): Option[OutTree] = map(n, subn),
    getSubnodes = getSubnodes,
    hasSubnodes = hasSubnodes,
    path = path
  )


proc mapDFSpost*[InTree, OutTree](
  tree: InTree,
  map: proc(n: InTree, subn: seq[OutTree]): OutTree,
  getSubnodes: proc(tree: InTree): seq[InTree],
  hasSubnodes: proc(it: InTree): bool = (proc(it: InTree): bool = true),
  path: seq[int] = @[0]): OutTree =
  ## Overload without `path` for `map`
  # TODO DOC
  mapDFSpost(
    tree,
    proc(
      n: InTree,
      path: seq[int],
      subn: seq[OutTree],
      inSubn: seq[InTree]
    ): OutTree = map(n, subn),
    getSubnodes,
    hasSubnodes,
    path
  )

#==========================  Iterative mapper  ===========================#

type
  DfsFrame[T, R] = object
    idx: int
    inSubt: seq[T]
    path: seq[int]
    subt: seq[R]
    inordVisited: set[int8]

  DfsStack[T, R] = seq[DfsFrame[T, R]]

  DfsTraversalOrder* = enum
    ## Order of traversal for DFS iteration. Nodes are ordered from
    ## left to right. This enum controls order in which elements will
    ## be 'acessed' (`body` callback evaluated)
    dfsPreorder ## Preorder DFS traversal - first visit root node, and
                ## then all it's descendants.
    dfsPostorder ## First visit all child nodes and then root node
    dfsInorder ## Visit 'left' nodes, root and then 'right'.

func makeDfsFrame[T, R](elems: seq[T], path: seq[int]): DfsFrame[T, R] =
  DfsFrame[T, R](idx: 0, inSubt: elems, path: path, subt: @[])

template iterateItDFS*(
  inTree, getSubnodes, hasSubnodes: untyped,
  order: DfsTraversalOrder, body: untyped): untyped =
  ##[

Perform DFS iteration of the `inTree` using `getSubnodes` to get list of
subnodes and `hasSubnodes` to determine if particular node has
subnodes.

* Parameters

:inTree: First node in tree
:getSubnodes: Expression to get list of subnodes
:hasSubnodes: Expression to check if current node has list of subnodes
:order: Order of traversal
:body: Body to evaluate for each node

* Injected variables

:it: Current node
:path: Path to current node starting from the tree root
:subt: List of subnodes for current node
:childIndex: Index of child node. Can be used for inorder traversal to
  determine correct split on 'right' and 'left' nodes. For postorder
  traversal (and 'child' part of inorder) is equal to child count.
  For preorder - equal to 0.

* Notes

- NOTE :: inorder traversal is only well-defined for binary trees (e.g.
  when partitioning on 'left' and 'right' can be performed easyly). For all
  other cases you can use injected `childIndex` - determine when 'middle'
  if the child list is reached and execute callback yourself. So most
  likely, for inorder traversal body should have form of

  ```nim
    if childIndex == it.len div 2:
      # execute actual callback
  ```

or something similar (when last/first child is reached). Otherwise body
will be executed each time traversal triggers 'between child nodes'.

* TODO

- TODO :: Support iteration over mutable tree using
  pointers/references/view types or anything of that sort.

  ]##
  type InTree = typeof(inTree)
  var stack: seq[DfsFrame[InTree, bool]]
  stack.add makeDfsFrame[InTree, bool](@[inTree], @[])
  block dfsLoop:
    while true:
      if stack[^1].idx == stack[^1].inSubt.len:
        let top = stack.pop
        if order == dfsPostorder or
           (order == dfsInorder and
            stack[^1].idx.int8 notin stack[^1].inordVisited
            # Node has not been visited during inorder traversal.
           )
          :
          let
            it {.inject.} = stack[^1].inSubt[stack[^1].idx]
            path {.inject.} = top.path
            subt {.inject.} = top.subt
            childIndex {.inject.} = stack[^1].inSubt.len - 1

          body

        if order == dfsInorder and
           stack.len > 1 and
           stack[^1].idx == 0:
          let
            it {.inject.} = stack[^2].inSubt[stack[^2].idx]
            path {.inject.} = stack[^2].path
            subt {.inject.} = stack[^2].path
            childIndex {.inject.} = stack[^1].idx

          stack[^2].inordVisited.incl int8(stack[^2].idx)

          body

        if stack.len == 1:
          break dfsLoop
        else:
          inc stack[^1].idx
      else:
        var elems: seq[InTree]
        block:
          var it {.inject.} = stack[^1].inSubt[stack[^1].idx]
          if order == dfsPreorder:
            let
              path {.inject.} = stack[^1].path
              subt {.inject.} = stack[^1].subt
              childIndex {.inject.} = 0

            body

          if hasSubnodes:
            elems = toSeqImpl(getSubnodes)

        stack.add makeDfsFrame[InTree, bool](
          elems, stack[^1].path & @[stack[^1].idx]
        )

template iterateItDFS*(tree, orderOrGetKind, body: untyped): untyped =
  ## Perform iterative traversal of `tree`, invoking `body` for each node.
  ##
  ##
  ## - @arg{tree} :: Input tree
  ## - @arg{orderOrGetKind} :: Either `bool` or `DfsTraversalOrder`
  ##   expression. `when is bool`, used as predicate for checking
  ##   if @injected{it} has subnodes. `dfsPreorder` is then used
  ##   as iteration order. When argument is of type `DfsTraversalOrder`
  ##   it is used as traversal order parameter, and `true` is used for
  ##   predicate.
  ## - @arg{body} :: Code block invoked on each node

  static:
    assert (orderOrGetKind is DfsTraversalOrder) or (orderOrGetKind is bool)

  when orderOrGetKind is bool:
    iterateItDFS(tree, it, orderOrGetKind, dfsPreorder, body)

  else:
    iterateItDFS(tree, it, true, orderOrGetKind, body)



template mapItDFSImpl*[InTree, OutTree](
  inTree: InTree,
  subnodeCall: untyped,
  op: untyped,
  hasSubnodes: untyped = true): untyped =
  # NOTE Too lazy to check for already implemented features. I guess
  # most oft he things are implemented? #idea #software##emacs write
  # helper to jump to closest todo in the code. #todo parse todo items
  # in comments.

  # TODO add proc for checking if futher recursion is not needed (trim
  # down arbitrary branches from tree)

  # TODO return `Option[OutTree]` from map function. Support both
  # versions: return-all and return-option (NOTE: can be determined
  # using typeof `op`)

  # NOTE `subnodeCall` does not feel intuitive - injecting current
  # node into scope will be better.
  # TEST - either write unit tests or chec if existing ones cover this
  #      explicitly

  # IDEA TEST write example/test for mapping tree to DFS sequence

  # TODO predicate to check if item has subnodes or not.
  # TEST predicated for subnodes checking
  # REVIEW TODO STYLE rename `outType` into `intermediateType` ?

  # IDEA store pointers intead of full objects. Maybe add
  # configuration option for this one, but should be possible (and
  # useful) in theory.

  var stack: seq[DfsFrame[InTree, OutTree]]
  var res: OutTree
  stack.add makeDfsFrame[InTree, OutTree](@[intree], @[])

  block dfsLoop:
    while true:
      if stack[^1].idx == stack[^1].inSubt.len:
        # Current toplevel frame reached the end
        let top = stack.pop
        let foldRes = (
          block:
            let
              it {.inject.} = stack[^1].inSubt[stack[^1].idx]
              path {.inject.} = top.path
              subt {.inject.} = top.subt
              inSubt {.inject.} = top.inSubt

            op
        )

        if not (foldRes is OutTree) or (foldREs is Option[OutTree]):
          let pos = instantiationInfo().line
          raiseAssert(
            "Invalid type for expression result: expected either `" & $typeof(OutTree) &
             "` or " & "`Option[" & $typeof(OutTree) &
             "]` in `mapItTreeDfs` on line " & $`pos` & ", but `op` is " &
            $typeof(foldRes)
          )

        if stack.len == 1:
          when foldRes is Option[OutTree]:
            res = foldRes.get() # NOTE
          else:
            res = foldRes

          break dfsLoop
        else:
          inc stack[^1].idx
          when foldRes is Option[OutTree]:
            if foldRes.isSome():
              stack[^1].subt.add foldRes.get()
          else:
            stack[^1].subt.add foldRes

      else:
        stack.add makeDfsFrame[InTree, OutTree](
          block:
            let it {.inject.} = stack[^1].inSubt[stack[^1].idx]
            if hasSubnodes:
              toSeq(subnodeCall)
            else:
              var tmp: seq[InTree]
              tmp
          ,
          stack[^1].path & @[stack[^1].idx]
        )

  res

macro mapItDFS*(
  inTree, subnodeCall, outType, hasSubnodes, op: untyped): untyped =

  ##[

Convert one tree type into another.

Conversion is perfomed in bottom-up manner - first child nodes are
evaluated, then results are supplied to parent nodes and so on.

## Parameters

:subnodeCall: Expression to get child nodes
:outType: Result type
:inTree: Tree to convert
:op: Expression for converting objects.

## Injected variables

:it: current tree node
:path: path of the current node in original tree
:subt: already converted subnodes
:inSubt: current input subnodes

## Example

For examples of use look into `tests/tHalgorithm.nim`, 'Tree mapping
suite'.

  ]##


  result = quote do:
    mapItDFSImpl[typeof(`inTree`), `outType`](
      `inTree`,
      `subnodeCall`,
      `op`,
      `hasSubnodes`
    )
