import hmisc/preludes/unittest

testFileStarted()

import
  hmisc/types/hgraph,
  hmisc/algo/[hgraph_db, hseq_mapping],
  hmisc/other/hshell,
  hmisc/hasts/graphviz_ast,
  hmisc/preludes/unittest

import std/[
  sequtils, algorithm, random, strformat,
  hashes, options, macros
]

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

    check:
      graph.connectedComponents()[0].mapIt(graph[it]).sorted() == @[0, 1, 2, 3, 4]

  test "Connected components other nodes":
    var graph = newHGraph[int, string]()
    discard graph.addOrGetEdge({
      (0, 1): "0 -> 1",
      (1, 0): "1 -> 0",
      (1, 2): "1 -> 2",
      (1, 1): "1 -> 1"
    })

    let
      components = graph.connectedComponents()
      cycles = graph.findCycles(ignoreSelf = true)

    show:
      $components
      cycles

  test "Find cycles with multiedges":
    var graph = newHGraph[string, string]()
    discard graph.addOrGetEdge(mapIt({
      "codecvt":   "initializer_list",
      "codecvt":   "ios",
      "codecvt":   "iosfwd",
      "codecvt":   "istream",
      "codecvt":   "iterator",
      "codecvt":   "memory",
      "codecvt":   "streambuf",
      "codecvt":   "string",
      "codecvt":   "x86_64_pc_linux_gnu/bits/cppconfig",
      "codecvt":   "x86_64_pc_linux_gnu/bits/cpplocale",
      "ios":       "codecvt",
      "ios":       "initializer_list",
      "ios":       "iosfwd",
      "ios":       "istream",
      "ios":       "iterator",
      "ios":       "memory",
      "ios":       "streambuf",
      "ios":       "string",
      "ios":       "x86_64_pc_linux_gnu/bits/cppconfig",
      "ios":       "x86_64_pc_linux_gnu/bits/cpplocale",
      "iosfwd":    "codecvt",
      "iosfwd":    "initializer_list",
      "iosfwd":    "ios",
      "iosfwd":    "istream",
      "iosfwd":    "iterator",
      "iosfwd":    "memory",
      "iosfwd":    "streambuf",
      "iosfwd":    "string",
      "iosfwd":    "x86_64_pc_linux_gnu/bits/cppconfig",
      "iosfwd":    "x86_64_pc_linux_gnu/bits/cpplocale",
      "istream":   "codecvt",
      "istream":   "initializer_list",
      "istream":   "ios",
      "istream":   "iosfwd",
      "istream":   "iterator",
      "istream":   "memory",
      "istream":   "streambuf",
      "istream":   "string",
      "istream":   "x86_64_pc_linux_gnu/bits/cppconfig",
      "istream":   "x86_64_pc_linux_gnu/bits/cpplocale",
      "iterator":  "codecvt",
      "iterator":  "initializer_list",
      "iterator":  "ios",
      "iterator":  "iosfwd",
      "iterator":  "istream",
      "iterator":  "iterator",
      "iterator":  "memory",
      "iterator":  "streambuf",
      "iterator":  "string",
      "iterator":  "x86_64_pc_linux_gnu/bits/cppconfig",
      "iterator":  "x86_64_pc_linux_gnu/bits/cpplocale",
      "memory":    "codecvt",
      "memory":    "initializer_list",
      "memory":    "ios",
      "memory":    "iosfwd",
      "memory":    "istream",
      "memory":    "iterator",
      "memory":    "streambuf",
      "memory":    "string",
      "memory":    "x86_64_pc_linux_gnu/bits/cppconfig",
      "memory":    "x86_64_pc_linux_gnu/bits/cpplocale",
      "streambuf": "codecvt",
      "streambuf": "initializer_list",
      "streambuf": "ios",
      "streambuf": "iosfwd",
      "streambuf": "istream",
      "streambuf": "iterator",
      "streambuf": "string",
      "streambuf": "x86_64_pc_linux_gnu/bits/cppconfig",
      "streambuf": "x86_64_pc_linux_gnu/bits/cpplocale",
      "string":    "codecvt",
      "string":    "initializer_list",
      "string":    "ios",
      "string":    "iosfwd",
      "string":    "istream",
      "string":    "iterator",
      "string":    "memory",
      "string":    "streambuf",
      "string":    "x86_64_pc_linux_gnu/bits/cppconfig",
      "string":    "x86_64_pc_linux_gnu/bits/cpplocale",
    }, ((it[0], it[1]), &"{it[0]} -> {it[1]}")))

    let cycles = graph.findCycles()
    show cycles.len()
    graph.
      dotRepr(
        baseGraph = some makeDotGraph(dgpRecords),
        clusters = cycles.mergeCycleSets().mapIt((it, ""))).
      toPng(getAppTempDir() /. "cycles2.png")

  test "Iterations, graphviz repr":
    var graph = newHGraph[string, int]()

    let node1 = graph.addNode("test1")
    let node2 = graph.addNode("test2")
    let edge = graph.addEdge(node1, node2, 190)

    show graph

    for node in graph.depthFirst(node1):
      show graph[node]

    for node in graph.topologicalOrdering():
      show graph[node]

    let dotRepr = graph.dotRepr()
    show dotRepr

  test "Graph coloring":
    var graph = newHGraph[int, int]()
    graph.addOrGetEdge({
      (0, 1): 0,
      (1, 2): 0,
      (2, 0): 0
    })

    let colorMap = graph.colorizeDSatur()
    show $colorMap.colorMap

  test "Build graph from ref type":
    let graph = newHGraphForRef(RefT(
      subnodes: @[RefT(), RefT()]
    ))

  test "Ortho layout":
    let graph = newHGraph[int, int]()
    let data = orthoLayout(graph)


  test "Graph microbenchmark":
    parametrizeOnValue count, [10, 200, 400]:
      var rand = initRand(228)
      var graph = newHGraph[int, int]()
      timeIt "Adding nodes":
        discard graph.addNode(rand.rand(0 .. count))

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
    show res.kind

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
      show extended

    show graph.dotRepr()

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

testFileEnded()
