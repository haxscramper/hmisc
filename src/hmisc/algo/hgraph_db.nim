import ../types/hgraph
import ../hdebug_misc
import ../base_errors

import std/[options, intsets, sequtils, tables]

type
  Value* = string
  DGraph = HGraph[Value, string]

  QueryResultKind = enum
    qrkNone
    qrkValue
    qrkVertex


  QueryResult = object
    case kind*: QueryResultKind
      of qrkNone:
        discard

      of qrkValue:
        value: Value

      of qrkVertex:
        vertex: HNode

  Query = object
    program: seq[Pipe]

  OptGremlinKind = enum
    ogkPull
    ogkPush
    ogkGremlin
    ogkDone
    ogkFalse

  Gremlin = object
    value*: Option[Value]
    vertex*: Option[HNode]
    asMap: Option[Table[string, HNode]]

  VertexFilterKind = enum
    vfkNone
    vfkIdSet
    vfkProperty
    vfkPredicate

  VertexFilter = object
    case kind*: VertexFilterKind
      of vfkNone:
        discard

      of vfkIdSet:
        idSet: HNodeSet

      of vfkPredicate:
        predicate: proc(vertex: HNode): bool

      of vfkProperty:
        property: Value

  OptGremlin = object
    case kind*: OptGremlinKind
      of ogkPull, ogkPush, ogkFalse, ogkDone:
        discard

      of ogkGremlin:
        gremlin*: Gremlin

  PipeKind = enum
    pkProperty
    pkUnique
    pkFilter
    pkOutNodes
    pkInNodes
    pkTakeN
    pkAsTagged ## Tag pipe results
    pkMergeTagged ## Merge tagged pipe results
    pkBack


  Pipe = object
    case kind*: PipeKind
      of pkProperty:
        property: string

      of pkUnique:
        seen: HNodeSet

      of pkOutNodes, pkInNodes:
        edges: seq[HEdge]
        gremlin: Option[Gremlin]

      of pkFilter:
        init: bool
        vertices: seq[HNode]
        filter: VertexFilter

      of pkTakeN:
        takeNum: int

      of pkAsTagged:
        asName: string

      of pkMergeTagged:
        mergeNames: seq[string]

      of pkBack:
        backName: string


proc run(pipe: var Pipe, gremlin: Option[Gremlin], graph: DGraph): OptGremlin

proc newQuery*(): Query = Query()
proc makePull*(): OptGremlin = OptGremlin(kind: ogkPull)
proc makeYield*(gremlin: Gremlin): OptGremlin =
  OptGremlin(kind: ogkGremlin, gremlin: gremlin)

proc makeDone*(): OptGremlin = OptGremlin(kind: ogkDone)
proc makeFalse*(): OptGremlin = OptGremlin(kind: ogkFalse)


proc run*(query: var Query, graph: DGraph): seq[QueryResult] =
  let max = query.program.high()
  var
    results: seq[Gremlin]
    optGremlin = makeFalse()
    done = -1
    pc = max

  while done < max:
    let arg =
      if optGremlin.kind == ogkGremlin:
        some(optGremlin.gremlin)

      else:
        none(Gremlin)

    optGremlin = run(query.program[pc], arg, graph)
    if optGremlin.kind == ogkPull:
      # Current pipe needs more input
      optGremlin = makeFalse()
      if pc - 1 > done:
        # Try previous filter - decrement PC and continue evaluation
        dec pc
        continue

      else:
        done = pc

    if optGremlin.kind == ogkDone:
      optGremlin = makeFalse()
      done = pc

    inc pc

    if pc > max:
      if optGremlin.kind == ogkGremlin:
        results.add optGremlin.gremlin

      optGremlin = makeFalse()
      dec pc

  for gremlin in results:
    if gremlin.value.isSome():
      result.add QueryResult(kind: qrkValue, value: gremlin.value.get())

    elif gremlin.vertex.isSome():
      result.add QueryResult(kind: qrkVertex, vertex: gremlin.vertex.get())

    else:
      result.add QueryResult(kind: qrkNone)



proc unique*(query: var Query): var Query =
  query.program.add Pipe(kind: pkUnique)
  return query

proc filter*(query: var Query, filter: VertexFilter): var Query =
  query.program.add Pipe(kind: pkFilter, filter: filter)
  return query

proc start*(query: var Query, node: HNode): var Query =
  var filter = VertexFilter(kind: vfkIdSet)
  filter.idSet.incl node
  return query.filter(filter)

proc inNodes*(query: var Query): var Query =
  query.program.add Pipe(kind: pkInNodes)
  return query

proc outNodes*(query: var Query): var Query =
  query.program.add Pipe(kind: pkOutNodes)
  return query

proc asTag*(query: var Query, name: string): var Query =
  query.program.add Pipe(kind: pkAsTagged, asName: name)
  return query

proc merge*(query: var Query, toMerge: varargs[string]): var Query =
  query.program.add Pipe(kind: pkMergeTagged, mergeNames: toSeq(toMerge))
  return query

proc runUniquePipe(
    pipe: var Pipe, gremlin: Option[Gremlin], graph: DGraph): OptGremlin =

  if gremlin.isNone() or
     gremlin.get().vertex.isNone() or
     gremlin.get().vertex.get() in pipe.seen:
    return makePull()

  else:
    return makeYield(gremlin.get())

proc getEdges(pipe: Pipe, graph: DGraph): seq[HEdge] =
  case pipe.kind:
    of pkOutNodes:
      for edge in graph.outEdges(pipe.gremlin.get().vertex.get()):
        result.add edge

    of pkInNodes:
      for edge in graph.inEdges(pipe.gremlin.get().vertex.get()):
        result.add edge

    else:
      raiseUnexpectedKindError(pipe)


proc runTraversalPipe(
    pipe: var Pipe, gremlin: Option[Gremlin], graph: DGraph): OptGremlin =

  if gremlin.isNone() and pipe.edges.len() == 0:
    return makePull()

  if pipe.edges.len() == 0:
    pipe.gremlin = gremlin
    pipe.edges.add getEdges(pipe, graph)

  if pipe.edges.len() == 0:
    return makePull()

  let vertex = pipe.edges.pop()

  return makeYield(Gremlin(vertex: some(graph.target(vertex))))

proc optGet[K, V](table: Table[K, V], key: K): Option[V] =
  if key in table:
    return some table[key]

proc runBackPipe(
    pipe: var Pipe, gremlin: Option[Gremlin], graph: DGraph): OptGremlin =

  if gremlin.isNone(): return makePull()

  let asName: Option[HNode] = gremlin.get().
    asMap.get().optGet(pipe.backName) # [pipe.backName]

  return makeYield(Gremlin(vertex: asName))

proc evalFilterPipe(
    pipe: var Pipe, gremlin: Option[Gremlin], graph: DGraph): OptGremlin =

  case pipe.filter.kind:
    of vfkIdSet:
      if not pipe.init:
        pipe.init = true
        for id in pipe.filter.idSet:
          pipe.vertices.add id

      if pipe.vertices.len > 0:
        return makeYield Gremlin(vertex: some(pipe.vertices.pop()))

      else:
        # Not found any new vertices, returning
        return makeDone()


    else:
      raiseImplementKindError(pipe.filter)

proc run(
    pipe: var Pipe, gremlin: Option[Gremlin], graph: DGraph): OptGremlin =

  case pipe.kind:
    of pkUnique:
      return runUniquePipe(pipe, gremlin, graph)

    of pkOutNodes, pkInNodes:
      return runTraversalPipe(pipe, gremlin, graph)

    of pkFilter:
      return evalFilterPipe(pipe, gremlin, graph)

    of pkBack:
      return runBackPipe(pipe, gremlin, graph)

    else:
      raiseImplementKindError(pipe)
