import ../types/hgraph
import std/[options, intsets]
import ../hdebug_misc
import ../base_errors

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
    graph: DGraph
    program: seq[Pipe]

  MaybeGremlinKind = enum
    mgkPull
    mgkPush
    mgkGremlin
    mgkDone
    mgkFalse

  Gremlin = object
    value*: Option[Value]
    vertex*: Option[HNode]

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

  MaybeGremlin = object
    case kind*: MaybeGremlinKind
      of mgkPull, mgkPush, mgkFalse, mgkDone:
        discard

      of mgkGremlin:
        gremlin*: Gremlin

  PipeKind = enum
    pkProperty
    pkUnique
    pkFilter
    pkOutNodes

  Pipe = object
    graph: DGraph # {.deprecated: "Pass graph in `run` for main pipe".}
    case kind*: PipeKind
      of pkProperty:
        property: string

      of pkUnique:
        seen: HNodeSet

      of pkOutNodes:
        edges: seq[HEdge]
        gremlin: Option[Gremlin]

      of pkFilter:
        init: bool
        vertices: seq[HNode]
        filter: VertexFilter


proc run(pipe: var Pipe, gremlin: Option[Gremlin]): MaybeGremlin

proc newQuery*(graph: HGraph): Query =
  Query(graph: graph)

proc run*(query: var Query): seq[QueryResult] =
  let max = query.program.high()
  var
    results: seq[Gremlin]
    maybeGremlin = MaybeGremlin(kind: mgkFalse)
    done = -1
    pc = max

  while done < max:
    maybeGremlin = run(
      query.program[pc],
      if maybeGremlin.kind == mgkGremlin:
        some(maybeGremlin.gremlin)

      else:
        none(Gremlin)
    )

    if maybeGremlin.kind == mgkPull:
      # Current pipe needs more input
      maybeGremlin = MaybeGremlin(kind: mgkFalse)
      if pc - 1 > done:
        # Try previous filter - decrement PC and continue evaluation
        dec pc
        continue

      else:
        done = pc

    if maybeGremlin.kind == mgkDone:
      maybeGremlin = MaybeGremlin(kind: mgkFalse)
      done = pc

    inc pc

    if pc > max:
      if maybeGremlin.kind == mgkGremlin:
        results.add maybeGremlin.gremlin

      maybeGremlin = MaybeGremlin(kind: mgkFalse)
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
  query.program.add Pipe(kind: pkFilter, filter: filter, graph: query.graph)
  return query

proc start*(query: var Query, node: HNode): var Query =
  var filter = VertexFilter(kind: vfkIdSet)
  filter.idSet.incl node
  return query.filter(filter)

proc outNodes*(query: var Query): var Query =
  query.program.add Pipe(kind: pkOutNodes, graph: query.graph)
  return query


proc runUniquePipe(pipe: var Pipe, gremlin: Option[Gremlin]): MaybeGremlin =
  if gremlin.isNone() or
     gremlin.get().vertex.isNone() or
     gremlin.get().vertex.get() in pipe.seen:
    return MaybeGremlin(kind: mgkPull)

  else:
    return MaybeGremlin(kind: mgkGremlin, gremlin: gremlin.get())

proc getEdges(pipe: Pipe): seq[HEdge] =
  case pipe.kind:
    of pkOutNodes:
      for edge in pipe.graph.outEdges(pipe.gremlin.get().vertex.get()):
        result.add edge

    else:
      raiseUnexpectedKindError(pipe)


proc runTraversalPipe(pipe: var Pipe, gremlin: Option[Gremlin]): MaybeGremlin =
  if gremlin.isNone() and pipe.edges.len() == 0:
    return MaybeGremlin(kind: mgkPull)

  if pipe.edges.len() == 0:
    pipe.gremlin = gremlin
    pipe.edges.add getEdges(pipe)

  if pipe.edges.len() == 0:
    return MaybeGremlin(kind: mgkPull)

  let vertex = pipe.edges.pop()

  return MaybeGremlin(kind: mgkGremlin, gremlin: Gremlin(
    vertex: some(pipe.graph.target(vertex))
  ))


proc evalFilterPipe(pipe: var Pipe, gremlin: Option[Gremlin]): MaybeGremlin =
  case pipe.filter.kind:
    of vfkIdSet:
      if not pipe.init:
        pipe.init = true
        for id in pipe.filter.idSet:
          pipe.vertices.add id

      if pipe.vertices.len > 0:
        return MaybeGremlin(kind: mgkGremlin, gremlin: Gremlin(
          vertex: some(pipe.vertices.pop()),
        ))

      else:
        # Not found any new vertices, returning
        return MaybeGremlin(kind: mgkDone)


    else:
      raiseImplementKindError(pipe.filter)

proc run(pipe: var Pipe, gremlin: Option[Gremlin]): MaybeGremlin =
  case pipe.kind:
    of pkUnique:
      return runUniquePipe(pipe, gremlin)

    of pkOutNodes:
      return runTraversalPipe(pipe, gremlin)

    of pkFilter:
      return evalFilterPipe(pipe, gremlin)

    else:
      raiseImplementKindError(pipe)
