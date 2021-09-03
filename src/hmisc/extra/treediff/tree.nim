import ./jcommon

import hmisc/algo/htree_mapping

import std/[math, strutils, strformat, algorithm]

type
  TreeMetrics* = object
    size*: int ## The number of nodes in the subtree rooted at the node.
    height*: int ## The size of the longer branch in the subtree rooted at the node.
    hash*: Hash ## The hashcode of the subtree rooted at the node.
    structureHash*: Hash ## The hashcode of the subtree rooted at the node, excluding labels.
    depth*: int ## The number of ancestors of a node.
    position*: int ## An absolute position for the node. Usually computed via the postfix order.


  Tree* = ref object
    impl: TreeImpl
    nodeRef: pointer

    parent: Tree
    metadata: Table[string, pointer]
    children: seq[Tree]
    label: string
    treeType: Type
    pos: int
    length: int
    metrics: TreeMetrics
    fake: bool

  Type* = object
    name*: int

  TreeImpl* = ref object
    reprType*: proc(t: Type): string

const
  NO_LABEL = ""
  NO_TYPE = Type()

{.this: this.}

proc newTreeType*(name: int): Type = Type(name: name)

proc newTree*(
    treeType: Type, parent: Tree,
    impl: TreeImpl,
    metadata: openarray[(string, pointer)] = @[]
  ): Tree =
  Tree(impl: impl, treeType: treeType, parent: parent, metadata: toTable(metadata))


proc newFakeTree*(name: string, trees: varargs[Tree]): Tree =
  Tree(children: toSeq(trees), fake: true, label: "FAKE-TREE-" & name)

proc newFakeTree*(trees: varargs[Tree]): Tree =
  Tree(children: toSeq(trees), fake: true, label: "FAKE-TREE")

proc preOrder*(this: Tree): Iterable[Tree] =
  return iterator(): Tree =
    iterateItDfs(this, it.children, true, dfsPreorder):
      yield it


proc postOrder*(this: Tree): Iterable[Tree] =
  return iterator(): Tree =
    iterateItDfs(this, it.children, true, dfsPostorder):
      yield it


proc breadthFirst*(this: Tree): Iterable[Tree] =
  return iterator(): Tree =
    iterateItBfs(this, it.children, true):
      yield it


proc addChild*(this: Tree; t: Tree) =
  assertRef t
  children.add t



proc setChildren*(this: Tree; children: seq[Tree]) =
  assertRef this
  this.children = children


proc getChildren*(this: Tree): var seq[Tree] =
  assertRef this
  result = children

iterator items*(tree: Tree): Tree =
  assertRef tree
  for sub in tree.children:
    yield sub

iterator pairs*(tree: Tree): (int, Tree) =
  assertRef tree
  for it in pairs(tree.children):
    yield it

func high*(tree: Tree): int = high(tree.children)


proc getParent*(this: Tree): Tree =
  parent

proc isRoot*(this: Tree): bool =
  return getParent().isNil()

proc getParents*(this: Tree): seq[Tree] =
  var parents: seq[Tree]
  if (getParent().isNil()):
    return parents
  else:
    parents.add(getParent())
    parents.add(getParent().getParents())
  return parents


proc `$`*(this: Tree): string =
  if isNil(this):
    result = "<nil tree>"

  else:
    let parents = getParents()
    result.add "<"
    for idx, parent in parents.reversed():
      if idx > 0: result.add "/"
      with result:
        add this.impl.reprType(parent.treeType)
        add "["
        add $parent.pos
        add "]"

    with result:
      add "/"
      add this.impl.reprType(treeType)
      add "["
      add $pos
      add "]::"
      add label


    if fake:
      result.add "[fake]>"
    else:
      result.add ">"

proc treeRepr*(
    tree: Tree, fullIdent: bool = false,
    maxdepth: int = high(int)
  ): string =
  proc aux(node: Tree, res: var string, level: int, parent: Tree) =
    if fullIdent:
      with res:
        addIndent(level)
        add $node
        add " "

    else:
      with res:
        addIndent(level)
        add tree.impl.reprType(node.treeType)
        add " "

      if notNil(parent) and
         notNil(node.getParent()) and
         node.getParent() != parent:
        res.add "INVALID_PARENT_SET"

    if node.label.len > 0:
      with res:
        add "'"
        add node.label
        add "' "

    res.add &"h{node.metrics.height}s{node.metrics.size} "


    if level + 1 < maxdepth:
      for idx, sub in node:
        res.add "\n"
        aux(sub, res, level + 1, node)

  aux(tree, result, 0, nil)

proc insertChild*(this: Tree; t: Tree; position: int) =
  assertRef this
  assertRef t
  children.insert t, position
  children[position].parent = this

proc getChildPosition*(this: Tree; child: Tree): int = getChildren().find(child)
proc getChild*(this: Tree; position: int): Tree = getChildren()[position]
proc getPos*(this: Tree): int = this.pos
proc setPos*(this: Tree; pos: int) = this.pos = pos
proc getLength*(this: Tree): int = this.length
proc setLength*(this: Tree; length: int) = this.length = length
proc getLabel*(this: Tree): string = this.label
proc hasLabel*(this: Tree): bool = not(NO_LABEL == getLabel())
proc setLabel*(this: Tree; label: string) = this.label = label
proc getEndPos*(this: Tree): int = getPos() + getLength()
proc getType*(this: Tree): Type = this.treeType
proc setType*(this: Tree; jtype: Type) = this.treeType = jtype
proc hasSameType*(this: Tree; t: Tree): bool = getType() == t.getType()
proc hash*(tree: Tree): Hash = tree.metrics.hash


proc hasSameTypeAndLabel*(this: Tree; t: Tree): bool =
  return hasSameType(t) and getLabel() == t.getLabel()





proc deepCopy*(this: Tree): Tree =
  new(result)
  result[] = this[]
  for sub in mitems(result.children):
    sub = deepCopy(sub)
    sub.parent = result

  assert result.label == this.label





proc `==`*(tree1, tree2: Tree): bool =
  assertRef tree1
  assertRef tree2
  # echov "check for equality"
  # echov $tree1, $tree2
  hasSameTypeAndLabel(tree1, tree2)


proc getTreesBetweenPositions*(
  this: Tree; position: int; endPosition: int): seq[Tree] =
  var trees: seq[Tree]
  for t in this.preOrder():
    if (t.getPos() >= position and t.getEndPos() <= endPosition):
      trees.add(t)
  return trees

proc isLeaf*(this: Tree): bool =
  return getChildren().isEmpty()


proc newTreeMetrics*(size: int; height: int; hash: int; structureHash: int;
                     depth: int; position: int): TreeMetrics =

  with result:
    size = size
    height = height
    hash = hash
    structureHash = structureHash
    depth = depth
    position = position

proc getMetrics*(this: Tree): TreeMetrics = metrics

proc setMetrics*(this: Tree; metrics: TreeMetrics) =
  this.metrics = metrics

proc getMetadata*(this: Tree; key: string): pointer =
  metadata[key]

proc setMetadata*(this: Tree; key: string; value: pointer) =
  metadata[key] = value

proc getMetadata*(this: Tree): Table[string, pointer] =
  metadata


proc isIsomorphicTo*(this: Tree; tree: Tree): bool =
  if (not(hasSameTypeAndLabel(tree))):
    return false
  if (getChildren().len() != tree.getChildren().len()):
    return false
  block:
    var i: int = 0
    while i < getChildren().len():
      var isChildrenIsomophic: bool = getChild(i).isIsomorphicTo(
          tree.getChild(i))
      if (not(isChildrenIsomophic)):
        return false
      postInc(i)
  return true

proc isIsoStructuralTo*(this: Tree; tree: Tree): bool =
  if (this.getType() != tree.getType()):
    return false
  if (getChildren().len() != tree.getChildren().len()):
    return false
  block:
    var i: int = 0
    while i < getChildren().len():
      var isChildrenStructural: bool = getChild(i).isIsoStructuralTo(
          tree.getChild(i))
      if (not(isChildrenStructural)):
        return false
      postInc(i)
  return true

proc searchSubtree*(this: Tree; subtree: Tree): seq[Tree] =
  var results: seq[Tree]
  for candidate in this.preOrder():
    if (candidate.getMetrics().hash == subtree.getMetrics().hash):
      if (candidate.isIsomorphicTo(subtree)):
        results.add(candidate)
  return results

proc getDescendants*(this: Tree): seq[Tree] =
  assertRef this
  for tree in preOrder(this):
    result.add tree

  result.del 0

proc setParent*(this: Tree; parent: Tree) =
  assertRef this
  this.parent = parent

proc setNodeRef*[N: ref object](this: Tree, node: N) =
  assertRef this
  assertRef node
  this.nodeRef = cast[pointer](node)

proc setParentAndUpdateChildren*(this: Tree; parent: Tree) =
  raise newImplementError()


proc getRoot*(this: Tree): Tree =
  if getParent().isNil():
    return this

  else:
    getParents()[^1]

proc positionInParent*(this: Tree): int =
  if (parent.isNil()):
    return -(1)

  else:
    return parent.getChildren().find(this)

proc toString*(this: Tree): string = ""

# proc deepCopy*(this: FakeTree): Tree =
#   var copy: Tree = newFakeTree()
#   for child in getChildren():
#     copy.addChild(child.deepCopy())
#   return copy

# proc getLabel*(this: FakeTree): string =
#   return NO_LABEL

# proc getLength*(this: FakeTree): int =
#   return getEndPos() - getPos()

# proc getPos*(this: FakeTree): int =
#   return getMinIt(this.children, it.getPos()).getPos()

# proc getEndPos*(this: FakeTree): int =
#   return getMaxIt(this.children, it.getPos()).getEndPos()

# proc getType*(this: FakeTree): Type =
#   return NO_TYPE

# proc setLabel*(this: FakeTree; label: string) =
#   raise unsupportedOperation()

# proc setLength*(this: FakeTree; length: int) =
#   raise unsupportedOperation()

# proc setPos*(this: FakeTree; pos: int) =
#   raise unsupportedOperation()

# proc setType*(this: FakeTree; jtype: Type) =
#   raise unsupportedOperation()

# proc toString*(this: FakeTree): string =
#   return "\"FakeTree\""

# proc getMetadata*(this: FakeTree; key: string): pointer =
#   return nil

# proc setMetadata*(this: FakeTree; key: string; value: pointer): pointer =
#   raise unsupportedOperation()

# proc getMetadata*(this: FakeTree): Table[string, pointer] =
#   discard



type
  TreeMetricComputer* = ref object
    ENTER*: string
    LEAVE*: string
    BASE*: int
    currentDepth*: int
    currentPosition*: int

proc newTreeMetricComputer(
    enter: string = "enter",
    leave: string = "leave",
    base: int = 33
  ): TreeMetricComputer =

  TreeMetricComputer(ENTER: enter, LEAVE: leave, BASE: base)


proc `+%=`[T: SomeInteger](val: var T, add: T) = val = val +% add
proc `*%=`[T: SomeInteger](val: var T, add: T) = val = val *% add

# proc `^%`[T: float|int](base: T; exp: int): T =
#   var (base, exp) = (base, exp)
#   result = 1

#   if exp < 0:
#     when T is int:
#       if base *% base != 1: return 0
#       elif (exp and 1) == 0: return 1
#       else: return base

#     else:
#       base = 1.0 / base
#       exp = -exp

#   while exp != 0:
#     if (exp and 1) != 0:
#       result *%= base
#     exp = exp shr 1
#     base *%= base

proc fastExponentiation*(base: int; exponent: int): int =
  var (base, exponent) = (base, exponent)
  if (exponent == 0):
    return 1

  if (exponent == 1):
    return base

  result = 1

  while (exponent > 0):
    if ((exponent and 1) != 0):
      result *%= base
    exponent = exponent shr 1
    base *%= base

proc hashFactor*(this: TreeMetricComputer; exponent: int): int =
  return fastExponentiation(BASE, exponent)

proc startInnerNode*(this: TreeMetricComputer; tree: Tree) =
  postInc(currentDepth)

proc hash*(t: Type): Hash = hash(t.name)

proc innerNodeStructureHash*(
    this: TreeMetricComputer;
    tree: Tree;
    size: int;
    middleHash: Hash
  ): Hash =

  return (
    (hash(tree.getType()) !& hash(ENTER)) +%
    (hash(tree.getType()) !& hash(LEAVE)) +%
    middleHash
  ) *% hashFactor(size)

proc innerNodeHash*(this: TreeMetricComputer; tree: Tree; size: int;
                    middleHash: int): int =

  return (
    (hash(tree.getType()) !& hash(ENTER) !& hash(tree.getLabel())) +%
    (hash(tree.getType()) !& hash(LEAVE) !& hash(tree.getLabel())) +%
    middleHash
  ) *% hashFactor(size)

proc leafHash*(this: TreeMetricComputer; tree: Tree): int =
  return innerNodeHash(tree, 1, 0)

proc leafStructureHash*(this: TreeMetricComputer; tree: Tree): int =
  return innerNodeStructureHash(tree, 1, 0)

proc visitLeaf*(this: TreeMetricComputer; tree: Tree) =
  tree.setMetrics(
    newTreeMetrics(
      1,
      0,
      leafHash(tree),
      leafStructureHash(tree),
      currentDepth,
      currentPosition))

  postInc(currentPosition)

proc endInnerNode*(this: TreeMetricComputer; tree: Tree) =
  postDec(currentDepth)
  var sumSize: int = 0
  var maxHeight: int = 0
  var currentHash: int = 0
  var currentStructureHash: int = 0
  for child in tree:
    var metrics: TreeMetrics = child.getMetrics()
    var exponent: int = 2 * sumSize + 1
    currentHash +%= metrics.hash *% hashFactor(exponent)
    currentStructureHash +%= metrics.structureHash *% hashFactor(exponent)
    sumSize += metrics.size
    if (metrics.height > maxHeight):
      maxHeight = metrics.height
  tree.setMetrics(newTreeMetrics(
    sumSize + 1,
    maxHeight + 1,
    innerNodeHash(tree, 2 * sumSize + 1, currentHash),
    innerNodeStructureHash(tree, 2 * sumSize + 1, currentStructureHash),
    currentDepth,
    currentPosition))

  postInc(currentPosition)


proc updateMetrics*(root: Tree) =
  let computer = newTreeMetricComputer()

  proc aux(tree: Tree, pos: int) =
    tree.pos = pos
    if tree.isLeaf():
      computer.visitLeaf(tree)

    else:
      computer.startInnerNode(root)

      var pos = 0
      for subnode in tree:
        aux(subnode, pos)
        inc pos

      computer.endInnerNode(tree)

  aux(root, 0)

proc newMetrics*(tree: Tree): TreeMetrics =
  updateMetrics(tree)
  return tree.metrics
