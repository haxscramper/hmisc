import hmisc/types/hgraph
import hmisc/algo/hgraph_db
import hmisc/hdebug_misc
import benchy

import std/[unittest, sequtils, algorithm, random, strformat]

startHax()

suite "Graph API":
  test "Connected components":
    var graph = newHGraph[int, string]()
    discard graph.addOrGetEdge({
      (0, 1) : "e1",
      (1, 2) : "e2",
      (2, 3) : "e3",
      (2, 4) : "e4",
      (3, 0) : "e5",
      (4, 2) : "e6",
    })

    doAssert graph.connectedComponents()[0].mapIt(graph[it]).sorted() ==
      @[0, 1, 2, 3, 4]

  test "Iterations, graphviz repr":
    var graph = newHGraph[string, int]()

    let node1 = graph.addNode("test1")
    let node2 = graph.addNode("test2")
    let edge = graph.addEdge(node1, node2, 190)

    for node in graph.depthFirst(node1):
      echo graph[node]

    for node in graph.topologicalOrdering():
      echo graph[node]

    let dotRepr = graph.graphvizRepr() do (node: HNode) -> string:
      "[shape=box]"

    echo dotRepr

  test "Graph coloring":
    var graph = newHGraph[int, int]()
    graph.addOrGetEdge({
      (0, 1): 0,
      (1, 2): 0,
      (2, 0): 0
    })

    let colorMap = graph.colorizeDSatur()
    echo $colorMap.colorMap

  test "Graph microbenchmark":
    var rand = initRand(228)
    var graph = newHGraph[int, int]()
    timeIt "Adding nodes":
      discard graph.addNode(rand.rand(0 .. 400))

    let max = graph.nodeCount()

    timeIt "Adding edges":
      discard graph.addEdge(
        initHNode(rand.rand 0 ..< max),
        initHNode(rand.rand 0 ..< max),
        0
      )

    timeIt &"Iterate over {max} nodes":
      for node in nodes(graph):
        discard graph[node]

    timeIt "Iterate over all outging for each node":
      for node in graph.nodes:
        for outHNode in graph.outNodes(node):
          discard graph[outHNode]

suite "Graph DB API":
  var graph = newHGraph[Value, string]()
  let start = graph.addNode("start")
  graph.addEdge(start, graph.addNode("out-node-1"), "->")
  graph.addEdge(start, graph.addNode("out-node-2"), "->")

  var query = newQuery(graph)
  let resultNodes = query.start(start).outNodes().run()
  for res in resultNodes:
    echo res.kind
