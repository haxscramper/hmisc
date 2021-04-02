import hmisc/types/hgraph
import hmisc/algo/hgraph_db
import hmisc/hdebug_misc

import std/[unittest]

startHax()

suite "Graph API":
  discard

suite "Graph DB API":
  var graph = newHGraph[Value, string]()
  let start = graph.addNode("start")
  graph.addEdge(start, graph.addNode("out-node-1"), "->")
  graph.addEdge(start, graph.addNode("out-node-2"), "->")

  var query = newQuery(graph)
  let resultNodes = query.start(start).outNodes().run()
  for res in resultNodes:
    echo res.kind
