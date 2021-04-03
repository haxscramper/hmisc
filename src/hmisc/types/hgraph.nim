## Generic implementation of graph data structure

import std/[tables, deques, hashes, random,
            strformat, strutils, sequtils, algorithm]

import ../algo/htemplates

#[
checklist

- All implementations work both at compile and run-time
- Works on JS target
- All iterator algorithms have both mutable and immutable implementations
- As little side effects as possible
- Make node and edge ID's distinct integers to avoid messing them up

- IDEA :: Add `->` and `<-` as operators that would return `TempEdge`
  object that can be added via `graph.add source -> target`.

]#

import std/[intsets]

type
  HNodeId* = distinct int
  HEdgeId* = distinct int

  HNodeSet* = object
    intSet: IntSet

  HEdgeSet* = object
    intSet: IntSet

  HNode* = object
    id: HNodeId

  HEdge* = object
    id: HEdgeId

  HGraphProperty = enum
    gpDirected
    gpUndirected
    gpAllowSelfLoops


  HGraphStructure* = ref object
    properties: set[HGraphProperty]
    maxId: int
    edgeMap: Table[HEdgeId, (HNode, HNode)]
    ingoingIndex: Table[HNodeId, HEdgeSet]
    outgoingIndex: Table[HNodeId, HEdgeSet]


  HNodePropertyMap*[N] = ref object
    valueMap: Table[HNode, N]
    case keepReverseIndex: bool
      of true:
        reverseMap: Table[N, seq[HNode]]

      of false:
        discard

  HEdgePropertyMap*[E] = ref object
    valueMap: Table[HEdge, E]
    case keepReverseIndex: bool
      of true:
        reverseMap: Table[E, seq[HEdge]]

      of false:
        discard

  HGraph*[N, E] = object
    structure*: HGraphStructure
    edgeMap*: HEdgePropertyMap[E]
    nodeMap*: HNodePropertyMap[N]

  HGraphPath* = seq[HEdge]


type
  HGraphError* = ref object of CatchableError

  HGraphCyclesError* = ref object of HGraphError


func newHEdgePropertyMap*[E](keepReverseIndex: bool = true):
  HEdgePropertyMap[E] =

  return HEdgePropertyMap[E](keepReverseIndex: keepReverseIndex)


func newHNodePropertyMap*[E](keepReverseIndex: bool = true):
  HNodePropertyMap[E] =

  return HNodePropertyMap[E](keepReverseIndex: keepReverseIndex)

func initHNode*(id: int): HNode = HNode(id: HNodeId(id))
func initHEdge*(id: int): HEdge = HEdge(id: HEdgeId(id))

func newHGraph*[N, E](
    properties: set[HGraphProperty] = {gpDirected, gpAllowSelfLoops}
  ): HGraph[N, E] =

  HGraph[N, E](
    structure: HGraphStructure(properties: properties),
    nodeMap: newHNodePropertyMap[N](),
    edgeMap: newHEdgePropertyMap[E]()
  )

func copyStructureHGraph*[N, E](graph: HGraph[N, E]): HGraph[N, E] =
  HGraph[N, E](
    structure: deepCopy(graph.structure),
    edgeMap: graph.edgeMap,
    nodeMap: graph.nodeMap
  )

func withPropertyMap*[N1, N2, E](
    graph: HGraph[N1, E], map: HNodePropertyMap[N2]): HGraph[N2, E] =

  HGraph[N2, E](
    structure: graph.structure, nodeMap: map, edgeMap: graph.edgeMap)


func withPropertyMap*[N, E1, E2](
    graph: HGraph[N, E1], map: HEdgePropertyMap[E2]): HGraph[N, E2] =

  HGraph[N, E2](
    structure: graph.structure, nodeMap: graph.nodeMap, edgeMap: map)


func incl*(s: var HNodeSet, node: HNode) =
  s.intSet.incl node.id.int

func incl*(s: var HEdgeSet, edge: HEdge) =
  s.intSet.incl edge.id.int

func excl*(s: var HNodeSet, node: HNode) =
  s.intSet.excl node.id.int

func excl*(s: var HEdgeSet, edge: HEdge) =
  s.intSet.excl edge.id.int

iterator items*(s: HEdgeSet): HEdge =
  for item in s.intSet:
    yield HEdge(id: HEdgeId(item))

func contains*(s: HEdgeSet, id: HEdgeId): bool = id.int in s.intSet
func contains*(s: HNodeSet, id: HNodeId): bool = id.int in s.intSet
func contains*(s: HNodeSet, node: HNode): bool = node.id.int in s.intSet
func contains*(s: HEdgeSet, node: HEdge): bool = node.id.int in s.intSet
func len*(s: HNodeSet | HEdgeSet): int = s.intSet.len

func pop(iset: var IntSet): int =
  for i in iset:
    result = i
    break

  iset.excl result

func pop*(s: var HNodeSet): HNode = HNode(id: HNodeId(s.intSet.pop()))



iterator items*(s: HNodeSet): HNode =
  for item in items(s.intSet):
    yield HNode(id: HNodeId(item))

func `$`*(id: HNodeId|HEdgeId): string = $id.int
func `==`*(id1, id2: HNodeId | HEdgeId): bool = id1.int == id2.int
func hash*(id: HNodeId|HEdgeId): Hash = hash(id.int)
func hash*(node: HNode): Hash = hash(node.id)
func hash*(edge: HEdge): Hash = hash(edge.id)

# proc `$`*(node: HNode): string = $node.id
# proc `$`*(edge: HEdge): string = $edge.id

func `[]`*[N, E](g: var HGraph[N, E], node: HNode): N =
  g.nodeMap.valueMap[node]

func `[]`*[N, E](g: var HGraph[N, E], edge: HEdge): E =
  g.edgeMap.valueMap[edge]

func target*[N, E](g: HGraph[N, E], edge: HEdge): HNode =
  g.structure.edgeMap[edge.id][1]

func source*[N, E](g: HGraph[N, E], edge: HEdge): HNode =
  g.structure.edgeMap[edge.id][0]

func contains*[N](map: HNodePropertyMap[N], value: N): bool =
  value in map.reverseMap

func contains*[N](map: HNodePropertyMap[N], node: HNode): bool =
  node in map.valueMap

func len*[N](map: HEdgePropertyMap[N]): int = map.valueMap.len
func len*[E](map: HNodePropertyMap[E]): int = map.valueMap.len

func del*[N](map: var HEdgePropertyMap[N], edge: HEdge) =
  map.valueMap.del edge
  if map.keepReverseIndex:
    # IMPLEMENT remove reverse index
    discard

func `[]`*[N](map: HNodePropertyMap[N], value: N): HNode =
  ## Return fist node matching value `value`
  map.reverseMap[value][0]

func `[]`*[N](map: HNodePropertyMap[N], node: HNode): N =
  map.valueMap[node]

func `[]=`*[N](map: var HNodePropertyMap[N], node: HNode, value: N) =
  map.valueMap[node] = value
  # IMPLEMENT how to handle revese index?


iterator pairs*[N](map: HNodePropertyMap[N]): (HNode, N) =
  for node, value in pairs(map.valueMap):
    yield (node, value)


iterator pairs*[N](map: HEdgePropertyMap[N]): (HEdge, N) =
  for edge, value in pairs(map.valueMap):
    yield (edge, value)

func `$`*[N](map: HNodePropertyMap[N]): string =
  result = "{"
  for key, value in pairs(map):
    result &= $key & ": " & $value & ", "

  result &= "}"


func add*[N](map: var HNodePropertyMap[N], value: N, node: HNode) =
  map.valueMap[node] = value
  if map.keepReverseIndex:
    map.reverseMap.mgetOrPut(value, @[]).add node

func add*[E](map: var HEdgePropertyMap[E], value: E, edge: HEdge) =
  map.valueMap[edge] = value
  if map.keepReverseIndex:
    map.reverseMap.mgetOrPut(value, @[]).add edge


func edgeCount*[N, E](graph: HGraph[N, E]): int = graph.edgeMap.len
func nodeCount*[N, E](graph: HGraph[N, E]): int = graph.nodeMap.len

func inDeg*[N, E](graph: HGraph[N, E], node: HNode): int {.inline.} =
  ## Return in degree for @arg{node}
  if node.id in graph.structure.ingoingIndex:
    result = len(graph.structure.ingoingIndex[node.id])

func outDeg*[N, E](graph: HGraph[N, E], node: HNode): int =
  ## Return out degree for @arg{node}. IMPLEMENT
  discard

func newId[N, E](graph: var HGraph[N, E]): int {.inline.} =
  result = graph.structure.maxId
  inc graph.structure.maxId

func isDirected*[N, E](graph: HGraph[N, E]): bool {.inline.} =
  gpDirected in graph.structure.properties

proc addNode*[N, E](graph: var HGraph[N, E], value: N):
  HNode {.discardable.} =
  ## Add new node with `value` to graph and return resulting node

  result = HNode(id: HNodeId(graph.newId()))
  graph.nodeMap.add(value, result)

proc first(intSet: IntSet): int =
  assert intSet.len > 0
  for value in intSet:
    return value


proc addOrGetNode*[N, E](graph: var HGraph[N, E], value: N):
  HNode {.discardable.} =
  ## Add new node with given value to graph or return first matching node
  ## for that value

  if value in graph.nodeMap:
    return graph.nodeMap[value]

  else:
    return graph.addNode(value)

template addEdgeImpl(N, E, post: untyped): untyped {.dirty.} =
  result = HEdge(id: HEdgeId(graph.newId()))

  post

  graph.structure.edgeMap[result.id] = (source, target)
  graph.structure.outgoingIndex.mgetOrPut(source.id, HEdgeSet()).incl result

  if not graph.isDirected():
    graph.structure.ingoingIndex.mgetOrPut(
      target.id, HEdgeSet()).incl result



proc addEdge*[N, E](
    graph: var HGraph[N, E], source, target: HNode, value: E):
  HEdge {.discardable.} =
  ## Add new edge with @arg{value} between @arg{source} and @arg{target}
  ## nodes, return resulting edge

  addEdgeImpl(N, E):
    graph.edgeMap.add(value, result)


proc addEdge*[N](
    graph: var HGraph[N, void], source, target: HNode):
  HEdge {.discardable.} =

  addEdgeImpl(N, void):
    discard


proc addEdge*[N, E](
    graph: var HGraph[N, E], sourceValue, targetValue: N, edgeValue: E):
  HEdge {.discardable.} =

  return graph.addEdge(
    graph.addNode(sourceValue),
    graph.addNode(targetValue),
    edgeValue,
  )

proc addOrGetEdge*[N, E](
    graph: var HGraph[N, E], sourceValue, targetValue: N, edgeValue: E):
  HEdge {.discardable.} =

  return graph.addEdge(
    graph.addOrGetNode(sourceValue),
    graph.addOrGetNode(targetValue),
    edgeValue
  )

proc addOrGetEdge*[N, E](
    graph: var HGraph[N, E],
    edgePairs: openarray[tuple[edgePair:
      tuple[sourceValue, targetValue: N], edgeValue: E]]):
  seq[HEdge] {.discardable.} =

  for (valuePair, edge) in edgePairs:
    result.add graph.addEdge(
      graph.addOrGetNode(valuePair.sourceValue),
      graph.addOrGetNode(valuePair.targetValue),
      edge
    )



proc removeNode*[N, E](graph: var HGraph[N, E], node: HNode) =
  ## Remove `node` from graph.
  ##
  ## Note that related edges will be removed too, if they completely loose
  ## all related nodes. So calling `removeHNode("a -> b", "b")` will not
  ## only remove node `"b"`, but also an edge.
  for targets in mitems(graph.edges[node.id]):
    targets.excl node.id

  graph.edges.del node.id


proc removeEdge*[N, E](graph: var HGraph[N, E], edge: HEdge) =
  ## Remove `edge` from graph. Note that starting/ending nodes will not be
  ## removed.
  graph.edgeMap.del edge
  # FIXME
  graph.structure.ingoingIndex.del graph.target(edge).id

proc getNode*[N, E](graph: HGraph[N, E], value: N): HNode =
  ## Return node associated with given value
  graph.nodeMap[value]

proc getNodeId*(node: HNode): HNodeId =
  node.id

proc getNodeById*[N, E](graph: HGraph[N, E], id: int): HNode =
  graph.nodeIdMap[id]

proc isAdjacent*[N, E](graph: HGraph[N, E], node1, node2: HNode): bool =
  ## Tests whether there is an edge from the vertex x to the vertex y;
  discard

iterator outEdges*[N, E](graph: HGraph[N, E], source: HNode): HEdge =
  ## Iterate over outgoing edges for `source`
  if source.id in graph.structure.outgoingIndex:
    for edgeId in items(graph.structure.outgoingIndex[source.id]):
      yield edgeId

iterator inEdges*[N, E](graph: HGraph[N, E], target: HNode): HEdge =
  ## Iterate over ingoing edges for `target`
  if target.id in graph.structure.ingoingIndex:
    for edgeId in items(graph.structure.ingoingIndex[target.id]):
      yield edgeId

iterator outNodes*[N, E](graph: HGraph[N, E], source: HNode): HNode =
  for edge in outEdges(graph, source):
    yield graph.target(edge)

iterator inNodes*[N, E](graph: HGraph[N, E], source: HNode): HNode =

  for edge in inEdges(graph, source):
    yield graph.source(edge)

iterator adjacent*[N, E](graph: HGraph[N, E], node: HNode): HNode =
  ## Iterate over all adjacent nodes for `node`
  var yielded: HNodeSet
  for node in outNodes(graph, node):
    yield node
    yielded.incl node

  for node in inNodes(graph, node):
    if node notin yielded:
      yield node
      yielded.incl node



iterator edges*[N, E](graph: HGraph[N, E]): HEdge =
  ## Iterate over all edges in graph
  for edge, value in pairs(graph.edgeMap):
    yield edge

iterator nodes*[N, E](graph: HGraph[N, E]): HNode =
  ## Iterate over all nodes in graph
  for node, value in pairs(graph.nodeMap):
    yield node

iterator nodesId*[N, E](graph: HGraph[N, E]): int =
  for id, _ in pairs(graph.nodeMap):
    yield id

template depthFirstAux(): untyped {.dirty.} =
  discard

iterator depthFirst*[N, E](
    graph: HGraph[N, E], root: HNode, preorderYield: bool = true
  ): HNode =
  ## Perform depth-first iteration of **immutable** nodes in graph,
  ## starting from `node`
  discard



iterator depthFirst*[N, E](
    graph: var HGraph[N, E], root: HNode, preorderYield: bool = true
  ): HNode =
  ## Perform depth-first iteration of **mutable** nodes in graph, starting
  ## from `node`
  var
    visited: HNodeSet
    stack: seq[seq[HNode]]

  stack.add @[root]


  while stack.len > 0 and stack[^1].len > 0:
    let top = stack[^1].pop

    yield top

    var buf: seq[HNode]
    for node in graph.outNodes(top):
      if node notin visited:
        visited.incl node
        buf.add node

    if buf.len > 0:
      stack.add buf

iterator breadthFirst*[N, E](graph: HGraph[N, E], root: HNode):
  HNode =

  ## Perform breadth-first iteration of parent graph, starting from `node`
  discard

iterator breadthFirst*[N, E](graph: var HGraph[N, E], root: HNode):
  var HNode =

  ## Perform breadth-first iteration of parent graph, starting from `node`
  var que = initDeque[HNode]()
  que.addLast(root)

  var visited: IntSet

  while que.len > 0:
    for outHEdge in graph.outgoing(root):
      if outHEdge.target.id notin visited:
        visited.incl outHEdge.target.id
        que.add outHEdge.target


proc topologicalOrdering*[N, E](graph: HGraph[N, E]): seq[HNode] =
  ## Return graph nodes in topological ordering if possible. Otherwise (if
  ## graph contains cycles raise `HGraphCyclesError`)
  var noincoming: HNodeSet
  for node in graph.nodes:
    if graph.inDeg(node) == 0:
      noincoming.incl node

  var graph = graph # WARNING use `copyGraphStructure`

  while len(noincoming) > 0:
    let node = noincoming.pop
    result.add node
    for outEdge in graph.outEdges(node):
      graph.removeEdge(outEdge)
      let target = graph.target(outEdge)
      if graph.inDeg(target) == 0:
        noincoming.incl target

  if graph.edgeCount > 0:
    raise HGraphCyclesError(
      msg: "Topological sorting is impossible, graph contains cycles")




proc dependencyOrdering*[N, E](
    graph: HGraph[N, E],
    root: HNode, leafFirst: bool = true
  ): seq[HNode] =
  ## Return graph nodes in dependencies for `node`. IMPLEMENT

  discard

proc colorizeDSatur*[N, E](graph: HGraph[N, E]):
  tuple[maxColors: int, colorMap: HNodePropertyMap[int]] =
  ## Color graph nodes using DSatur algorithm

  type
    DColor = int16
    DSatNode = ref object
      baseNode: HNode
      color: DColor
      dsat: int
      degree: int
      adjacent: seq[DSatNode]

  let noColor: DColor = -1
  func `<`(n1, n2: DSatNode): bool =
    if n1.dsat == n2.dsat:
      n1.degree < n2.degree

    else:
      n1.dsat < n2.dsat

  func popMaxDSat(nodes: var seq[DSatNode]): DSatNode =
    let idx = findMaxIt(nodes, it.dsat)
    result = nodes[idx]
    nodes.delete(idx)

  func nextMinColor(node: DSatNode):
    tuple[nextMin: DColor, distinctAdjacent: int] =

    var used: set[DColor]
    for node in node.adjacent:
      used.incl node.color

    for color in int16(0) .. DColor.high:
      if color notin used:
        return (color, used.len())

  func updateDSat(node: DSatNode) =
    for adjacent in node.adjacent:
      if adjacent.color == noColor:
        adjacent.dsat = adjacent.nextMinColor().distinctAdjacent

  var nodes: seq[DSatNode]

  var dsatMap = newHnodePropertyMap[DSatNode](false)

  for node in nodes(graph):
    let dsatur = DSatNode(
      degree: graph.outDeg(node),
      color: noColor,
      baseNode: node
    )

    nodes.add dsatur
    dsatMap[node] = dsatur

  for dsat in mitems(nodes):
    for node in graph.outNodes(dsat.baseNode):
      dsat.adjacent.add dsatMap[node]

  result.colorMap = newHNodePropertyMap[int](false)

  while nodes.len > 0:
    let node = nodes.popMaxDSat()
    let (color, nextMin) = node.nextMinColor()
    node.color = color
    result.maxColors = nextMin + 1
    node.updateDSat()
    result.colorMap[node.baseNode] = node.color.int






proc colorizeRLF*[N, E](graph: HGraph[N, E]) =
  ## Color graph using RLF algorithm. IMPLEMENT
  discard

proc minmalSpanningTree*[N, E](graph: HGraph[N, E]): HGraph[N, E] =
  ## Return minimal spanning tree for graph. IMPLEMENT
  discard

proc findCycles*[N, E](graph: HGraph[N, E]): seq[HGraphPath] =
  discard

proc connectedComponents*[N, E](graph: HGraph[N, E]): seq[seq[HNode]] =
  ## Return nodes forming strongly connected components
  type
    Node = HNode
    IdTable = HNodePropertyMap[int]

  var time: int

  proc aux(
    u: Node, disc: var IdTable, low: var IdTable,
    st: var seq[Node], stackMember: var HNodeSet,
    components: var seq[seq[Node]]
  ) =

    inc time
    disc[u] = time
    low[u] = time
    st.add u
    stackMember.incl u

    for v in graph.adjacent(u):
      if v notin disc:
        aux(v, disc, low, st, stackMember, components)
        low[u] = min(low[u], low[v])

      elif v in stackMember:
        low[u] = min(low[u], disc[v])

    if (low[u] == disc[u]):
      components.add @[]
      while st[^1] != u:
        let w = st.pop()
        components[^1].add w
        stackMember.excl w

      let w = st.pop()
      components[^1].add w
      stackMember.excl w

  var
    disc = newHNodePropertyMap[int](false)
    low = newHNodePropertyMap[int](false)
    stackMember: HNodeSet
    st = newSeq[Node]()

  for node in nodes(graph):
    if node notin disc:
      aux(node, disc, low, st, stackMember, result)

proc graphvizRepr*[N, E](
    graph: HGraph[N, E],
    nodeDotAttrs: proc(node: HNode): string = nil,
    edgeDotAttrs: proc(edge: HEdge): string = nil,
    nodeFormatting: seq[tuple[key, value: string]] = @[],
    edgeFormatting: seq[tuple[key, value: string]] = @[],
    graphFormatting: seq[tuple[key, value: string]] = @[],
    globalFormatting: seq[tuple[key, value: string]] = @[],
  ): string =

  ## Return `dot` representation for graph

  result.add "digraph G {\n"

  template toIdStr(node: HNode): string =
    "n" & replace($node.id, "-", "neg")

  for node in graph.nodes:
    result.add "  " & toIdStr(node)
    if not isNil(nodeDotAttrs):
      result.add nodeDotAttrs(node)

    result.add ";\n"

  for edge in graph.edges:
    result.add  "  " & toIdStr(graph.source(edge))
    if graph.isDirected():
      result.add " -> "

    else:
      result.add " -- "

    result.add toIdStr(graph.target(edge))

    if not isNil(edgeDotAttrs):
      result.add edgeDotAttrs(edge)

    result.add ";\n"


  result.add "}"
