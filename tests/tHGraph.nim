import hmisc/types/hgraph
import hmisc/algo/[hgraph_db, hseq_mapping]
import hmisc/other/hshell
import hmisc/hdebug_misc
import hmisc/hasts/graphviz_ast
import benchy

import std/[unittest, sequtils, algorithm, random, strformat,
            hashes]

startHax()

type
  RefT = ref object
    subnodes: seq[RefT]

func hash(t: RefT): Hash =
  hashes.hash(unsafeAddr t)


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

  test "Connected components other nodes":
    var graph = newHGraph[int, string]()
    discard graph.addOrGetEdge({
      (0, 1): "0 -> 1",
      (1, 0): "1 -> 0",
      (1, 2): "1 -> 2",
      (1, 1): "1 -> 1"
    })

    let components = graph.connectedComponents()
    echo $components

    startHax()
    let cycles = graph.findCycles(ignoreSelf = true)
    stopHax()
    echo cycles

  test "Iterations, graphviz repr":
    var graph = newHGraph[string, int]()

    let node1 = graph.addNode("test1")
    let node2 = graph.addNode("test2")
    let edge = graph.addEdge(node1, node2, 190)

    for node in graph.depthFirst(node1):
      echo graph[node]

    for node in graph.topologicalOrdering():
      echo graph[node]

    let dotRepr = graph.dotRepr()
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

  test "Build graph from ref type":
    let graph = newHGraphForRef(RefT(
      subnodes: @[RefT(), RefT()]
    ))

  test "Ortho layout":
    let graph = newHGraph[int, int]()
    let data = orthoLayout(graph)


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

  var query = newQuery()
  let resultNodes = query.start(start).outNodes().run(graph)
  for res in resultNodes:
    echo res.kind

suite "Graph sets":
  test "Hashing":
    check hash(HNodeSet()) == hash(HNodeSet())

  test "Outgoing extension":
    var graph = newHGraph[int, int]()
    graph.addOrGetEdge({
      (0, 1): 0,
      (1, 2): 0,
      (2, 0): 0,
      (2, 4): 0
    })

    for part in graph.connectedComponents():
      let extended = graph.extendOutgoing(part)
      echo extended

    echo graph.dotRepr()

  test "Extend outgoing with cluster merging":
     var graph = newHGraph[int, int]()
     graph.addOrGetEdge({
       # First connected component
       (0, 1): 0,
       (1, 2): 1,
       (2, 0): 2,

       # Second connected component
       (3, 4): 3,
       (4, 5): 4,
       (5, 3): 5,

       # Link between first and second component
       (2, 4): 6
     })

     var components: seq[(HNodeSet, bool)]
     for component in graph.connectedComponents():
       components.add((component, false))

     let idx = components.findIt(graph[0] in it[0])
     let extended = graph.extendOutgoing(
       components[idx][0], components)

     check components.len == 0

     if hasCmd shellCmd(dot):
       graph.dotRepr().toPng(
         getAppTempFile("extendedMerger.png"))
