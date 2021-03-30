## Generic implementation of graph data structure

import std/[tables, deques, hashes, random, strformat, strutils]

#[
checklist

- All implementations work both at compile and run-time
- Works on JS target
- All iterator algorithms have both mutable and immutable implementations
- As little side effects as possible
- Make node and edge ID's distinct integers to avoid messing them up

]#

import std/[intsets]

type
  Node[N, E] = ref NodeObj[N, E]
  NodeObj[N, E] = object
    id: int
    nodeValue: N

  Edge[N, E] = ref EdgeObj[N, E]
  EdgeObj[N, E] = object
    id: int
    source*: Node[N, E]
    target*: Node[N, E]
    edgeValue: E

  GraphProperty = enum
    gpDirected
    gpUndirected
    gpAllowSelfLoops

  Graph[N, E] = ref GraphObj[N, E]

  GraphObj[N, E] = object
    nodeMap: Table[Hash, Node[N, E]]
    nodeIdMap: Table[int, Node[N, E]]
    properties: set[GraphProperty]
    maxId: int
    edgeMap: Table[(int, int), Edge[N, E]]
    ingoingIndex: Table[int, IntSet]
    outgoingIndex: Table[int, IntSet]

  GraphPath[N, E] = seq[Edge[N, E]]


type
  GraphError = ref object of CatchableError

  GraphCyclesError = ref object of GraphError



func newGraph*[N, E](
    properties: set[GraphProperty] = {gpDirected, gpAllowSelfLoops}
  ): Graph[N, E] =

  Graph[N, E](properties: properties)

func value*[N, E](node: Node[N, E]): N {.inline.} = node.nodeValue

func value*[N, E](edge: Edge[N, E]): E {.inline.} = edge.edgeValue

func `value=`*[N, E](node: var Node[N, E], value: N) {.inline.} =
  node.nodeValue = value

func `value=`*[N, E](edge: var Node[N, E], value: E) {.inline.} =
  edge.edgeValue = value

func lenEdges*[N, E](graph: Graph[N, E]): int = graph.edgeMap.len

func inDeg*[N, E](graph: Graph[N, E], node: Node[N, E]): int {.inline.} =
  if node.id in graph.ingoingIndex:
    result = len(graph.ingoingIndex[node.id])

func newId[N, E](graph: var Graph[N, E]): int {.inline.} =
  result = graph.maxId
  inc graph.maxId

func isDirected*[N, E](graph: Graph[N, E]): bool {.inline.} =
  gpDirected in graph.properties

proc addNode*[N, E](graph: var Graph[N, E], value: N): Node[N, E] =
  result = Node[N, E](id: graph.newId, nodeValue: value)
  graph.nodeIdMap[result.id] = result
  graph.nodeMap[hash(value)] = result

proc addEdge*[N, E](
    graph: var Graph[N, E],
    source, target: Node[N, E], value: E): Edge[N, E] =

  result = Edge[N, E](
    id: graph.maxId,
    edgeValue: value,
    source: source,
    target: target
  )

  graph.edgeMap[(source.id, target.id)] = result

  graph.outgoingIndex.mgetOrPut(source.id, IntSet()).incl target.id

  if not graph.isDirected():
    graph.ingoingIndex.mgetOrPut(target.id, IntSet()).incl source.id


proc removeNode*[N, E](graph: var Graph[N, E], node: Node[N, E]) =
  ## Remove `node` from graph.
  ##
  ## Note that related edges will be removed too, if they completely loose
  ## all related nodes. So calling `removeNode("a -> b", "b")` will not
  ## only remove node `"b"`, but also an edge.
  for targets in mitems(graph.edges[node.id]):
    targets.excl node.id

  graph.edges.del node.id


proc removeEdge*[N, E](graph: var Graph[N, E], edge: Edge[N, E]) =
  ## Remove `edge` from graph. Note that starting/ending nodes will not be
  ## removed.
  graph.edgeMap.del (edge.source.id, edge.target.id)
  # FIXME
  graph.ingoingIndex.del edge.target.id

proc getNode*[N, E](graph: Graph[N, E], value: N): Node[N, E] =
  ## Return node associated with given value
  graph.nodeMap[hash(value)]

proc getNodeById*[N, E](graph: Graph[N, E], id: int): Node[N, E] =
  graph.nodeIdMap[id]

proc adjacent*[N, E](graph: Graph[N, E], node1, node2: Node[N, E]): bool =
  ## Tests whether there is an edge from the vertex x to the vertex y;
  discard

iterator outgoing*[N, E](graph: Graph[N, E], node: Node[N, E]): Edge[N, E] =
  ## Iterate over outgoing edges for `node`
  if node.id in graph.outgoingIndex:
    for targetId in graph.outgoingIndex[node.id]:
      yield graph.edgeMap[(node.id, targetId)]

iterator ingoing*[N, E](graph: Graph[N, E], node: Node[N, E]): Edge[N, E] =
  ## Iterate over ingoing edges for `node`
  for sourceId in items(graph.ingoingIndex[node.id]):
    yield graph[node.id][sourceId]

iterator edges*[N, E](graph: Graph[N, E]): Edge[N, E] =
  ## Iterate over all edges in graph
  for _, edge in pairs(graph.edgeMap):
    yield edge

iterator nodes*[N, E](graph: Graph[N, E]): Node[N, E] =
  ## Iterate over all nodes in graph
  for _, node in pairs(graph.nodeMap):
    yield node

template depthFirstAux(): untyped {.dirty.} =
  discard

iterator depthFirst*[N, E](
    graph: Graph[N, E], root: Node[N, E], preorderYield: bool = true
  ): Node[N, E] =
  ## Perform depth-first iteration of **immutable** nodes in graph,
  ## starting from `node`
  discard



iterator depthFirst*[N, E](
    graph: var Graph[N, E], root: Node[N, E], preorderYield: bool = true
  ): Node[N, E] =
  ## Perform depth-first iteration of **mutable** nodes in graph, starting
  ## from `node`
  var
    visited: IntSet
    stack: seq[seq[Node[N, E]]]

  stack.add @[root]


  while stack.len > 0 and stack[^1].len > 0:
    let top = stack[^1].pop

    yield top

    var buf: seq[Node[N, E]]
    for outEdge in graph.outgoing(top):
      if outEdge.target.id notin visited:
        visited.incl outEdge.target.id
        buf.add outEdge.target

    if buf.len > 0:
      stack.add buf

iterator breadthFirst*[N, E](graph: Graph[N, E], root: Node[N, E]): Node[N, E] =
  ## Perform breadth-first iteration of parent graph, starting from `node`
  discard

iterator breadthFirst*[N, E](graph: var Graph[N, E], root: Node[N, E]): var Node[N, E] =
  ## Perform breadth-first iteration of parent graph, starting from `node`
  var que = initDeque[Node[N, E]]()
  que.addLast(root)

  var visited: IntSet

  while que.len > 0:
    for outEdge in graph.outgoing(root):
      if outEdge.target.id notin visited:
        visited.incl outEdge.target.id
        que.add outEdge.target

proc pop(iset: var IntSet): int =
  for i in iset:
    result = i
    break

  iset.excl result

proc topologicalOrdering*[N, E](graph: Graph[N, E]): seq[Node[N, E]] =
  ## Return graph nodes in topological ordering if possible. Otherwise (if
  ## graph contains cycles raise `GraphCyclesError`)
  var noincoming: IntSet
  for node in graph.nodes:
    if graph.inDeg(node) == 0:
      noincoming.incl node.id

  var graph = graph

  while len(noincoming) > 0:
    let node = noincoming.pop
    result.add graph.getNodeById(node)
    for outEdge in graph.outgoing(graph.getNodeById(node)):
      graph.removeEdge(outEdge)
      if graph.inDeg(outEdge.target) == 0:
        noincoming.incl outEdge.target.id

  if graph.lenEdges > 0:
    raise GraphCyclesError(
      msg: "Topological sorting is impossible, graph contains cycles")




proc dependencyOrdering*[N, E](
    graph: Graph[N, E],
    root: Node[N, N], leafFirst: bool = true): seq[Node[N, E]] =
  ## Return graph nodes in dependencies for `node`.

  discard

proc findCycles*[N, E](graph: Graph[N, E]): seq[GraphPath[N, E]] =
  discard

proc graphvizRepr*[N, E](
    graph: Graph[N, E],
    nodeDotAttrs: proc(node: Node[N, E]): string = nil,
    edgeDotAttrs: proc(edge: Edge[N, E]): string = nil,
    nodeFormatting: seq[tuple[key, value: string]] = @[],
    edgeFormatting: seq[tuple[key, value: string]] = @[],
    graphFormatting: seq[tuple[key, value: string]] = @[],
    globalFormatting: seq[tuple[key, value: string]] = @[],
  ): string =

  ## Return `dot` representation for graph

  result.add "digraph G {\n"

  template toIdStr(node: Node[N, E]): string =
    "n" & replace($node.id, "-", "neg")

  for node in graph.nodes:
    result.add "  " & toIdStr(node)
    if not isNil(nodeDotAttrs):
      result.add nodeDotAttrs(node)

    result.add ";\n"

  for edge in graph.edges:
    result.add  "  " & toIdStr(edge.source)
    if graph.isDirected():
      result.add " -> "

    else:
      result.add " -- "

    result.add toIdStr(edge.target)

    if not isNil(edgeDotAttrs):
      result.add edgeDotAttrs(edge)

    result.add ";\n"


  result.add "}"


import std/[times]

template timeIt(name: string, body: untyped): untyped =
  block:
    let start = cpuTime()
    body
    let total {.inject.} = cpuTime() - start
    echo &"  {int(total * 1000):<5} ms ", name


proc main() =
  var graph = newGraph[string, int]()

  let node1 = graph.addNode("test1")
  let node2 = graph.addNode("test2")
  let edge = graph.addEdge(node1, node2, 190)

  echo isNil(edge)

  for node in graph.depthFirst(node1):
    echo node.value

  for node in graph.topologicalOrdering():
    echo node.value

  let dotRepr = graph.graphvizRepr() do (node: Node[string, int]) -> string:
    "[shape=box]"

  echo dotRepr

  var rand = initRand(228)

  for i in [10, 100, 1000, 10000]:
    var graph = newGraph[int, int]()
    echo "Processing \e[31m", i, "\e[39m items"
    timeIt "Adding nodes":
      for val in 0 ..< i:
        discard graph.addNode(val)


    timeIt "Adding edges":
      for val in 0 ..< i:
        discard graph.addEdge(
          graph.getNode(rand.rand 0 ..< i),
          graph.getNode(rand.rand 0 ..< i),
          0
        )

    timeIt "Iterate over nodes":
      for node in graph.nodes:
        discard node.value

    timeIt "Iterate over all outging for each node":
      for node in graph.nodes:
        for outNode in graph.outgoing(node):
          discard outNode.value

when isMainModule:
  main()
