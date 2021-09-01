# Spec: https://www.w3.org/TR/css3-selectors/
## Based on https://github.com/GULPF/nimquery

import std/[xmltree, strutils, strtabs, unicode, math, deques]
import hmisc/core/all

type
  ParseError* = object of ValueError
  TokenKind* = enum
    tkInvalid

    tkBracketStart, tkBracketEnd
    tkParam
    tkComma

    tkIdentifier, tkString

    tkClass, tkId, tkElement

    tkCombinatorDescendents, tkCombinatorChildren
    tkCombinatorNextSibling, tkCombinatorSiblings

    tkPseudoNthChild, tkPseudoNthLastChild
    tkPseudoNthOfType, tkPseudoNthLastOfType

    tkPseudoFirstOfType, tkPseudoLastOfType
    tkPseudoOnlyChild, tkPseudoOnlyOfType, tkPseudoEmpty
    tkPseudoFirstChild, tkPseudoLastChild

    tkPseudoNot

    tkPredicate

    tkEoi # End of input

  Token = object
    kind: TokenKind
    value: string

const NthKinds = {
  tkPseudoNthChild, tkPseudoNthLastChild,
  tkPseudoNthOfType, tkPseudoNthLastOfType
}

type
  Demand[N, K] = object
    case kind: Tokenkind
      of tkPredicate:
        predicate: proc(node: N): bool

      of NthKinds:
        a, b: int

      of tkPseudoNot:
        notQuery: QueryPart[N, K]

      of tkElement:
        element: K

      else:
        discard

  NodeWithParent[N, K] = object
    parent: N
    index: int
    elementIndex: int

  Combinator* = enum
    cmDescendants = tkCombinatorDescendents
    cmChildren = tkCombinatorChildren
    cmNextSibling = tkCombinatorNextSibling
    cmSiblings = tkCombinatorSiblings
    cmLeaf # Special case for the last query

  QueryOption* = enum
    optUniqueIds          ## Assume unique id's or not
    optUnicodeIdentifiers ## Allow non-ascii in identifiers (e.g `#exämple`)
    optSimpleNot          ## Only allow simple selectors as the argument
                          ## for ":not". Combinators and/or commas are not
                          ## allowed even if this option is excluded.

  Query*[N, K] = object ## Represents a parsed query.
    queries: seq[seq[QueryPart[N, K]]]
    options: set[QueryOption]

  QueryCtx*[N, K] = object
    wrapNode*: proc(node: N): N
    eqElement*: proc(node: N, element: K): bool
    eqKind*: proc(node1, node2: N): bool

  QueryPart[N, K] = object
    demands: seq[Demand[N, K]]
    combinator: Combinator

const
  DefaultQueryOptions* = {optUniqueIds, optUnicodeIdentifiers, optSimpleNot}
  Identifiers = Letters + Digits + {'-', '_', '\\'}
  CssWhitespace = {'\x20', '\x09', '\x0A', '\x0D', '\x0C'}
  Combinators = CssWhitespace + {'+', '~', '>'}

  PseudoNoParamsKinds = {
    tkPseudoFirstOfType, tkPseudoLastOfType,
    tkPseudoOnlyChild, tkPseudoOnlyOfType,
    tkPseudoEmpty, tkPseudoFirstChild,
    tkPseudoLastChild
  }

  PseudoParamsKinds = NthKinds + {tkPseudoNot}

  CombinatorKinds = {
    tkCombinatorChildren, tkCombinatorDescendents,
    tkCombinatorNextSibling, tkCombinatorSiblings
  }

func safeCharCompare(str: string, idx: int, cs: set[char]): bool {.inline.} =
  idx in 0 .. high(str) and str[idx] in cs

func safeCharCompare(str: string, idx: int, c: char): bool {.inline.} =
  return str.safeCharCompare(idx, {c})

func node[N, K](pair: NodeWithParent[N, K]): N =
  return pair.parent[pair.index]

func initNotDemand[N, K](notQuery: QueryPart[N, K]): Demand[N, K] =
  result = Demand[N, K](kind: tkPseudoNot, notQuery: notQuery)

func initElementDemand*[N, K](element: K): Demand[N, K] =
  result = Demand[N, K](kind: tkElement, element: element)

func initPseudoDemand*[N, K](kind: TokenKind): Demand[N, K] =
  result = Demand[N, K](kind: kind)

func initPredicateDemand*[N, K](predicate: proc(node: N): bool): Demand[N, K] =
  Demand[N, K](kind: tkPredicate, predicate: predicate)

template predicate*[N, K](ctx: QueryCtx[N, K], predicate: untyped): untyped =
  initPredicateDemand[N, K](proc(it {.inject.}: N): bool = predicate)


func initQueryPart*[N, K](
    demands: seq[Demand[N, K]], combinator: Combinator): QueryPart[N, K] =
  return QueryPart[N, K](demands: demands, combinator: combinator)

func initQuery*[N, K](
    parts: seq[QueryPart[N, K]],
    options: set[QueryOption] = DefaultQueryOptions): Query[N, K] =

  Query[N, K](queries: @[parts], options: options)

func initQueryCtx*[N, K](
    wrapNode: proc(node: N): N,
    eqElement: proc(node: N, element: K): bool,
    eqKind: proc(node1, node2: N): bool
  ): QueryCtx[N, K] =

  QueryCtx[N, K](wrapNode: wrapNode, eqElement: eqElement, eqKind: eqKind)

func initWithParent*[N, K](
    parent: N, index: int, elementIndex: int): NodeWithParent[N, K] =

  NodeWithParent[N, K](parent: parent, index: index, elementIndex: elementIndex)



func query*[N, K](demand: Demand[N, K]): Query[N, K] =
  initQuery(@[initQueryPart(@[demand], cmLeaf)])

func initNthChildDemand*[N, K](kind: TokenKind, a, b: int): Demand[N, K] =
  case kind
    of NthKinds:
      result = Demand[N, K](kind: kind, a: a, b: b)

    else:
      raiseAssert "invalid kind: " & $kind

func `$`*[N, K](demand: Demand[N, K]): string =
  case demand.kind:
    of tkPseudoNot:
      result = ":" & $demand.kind & "(" & $demand.notQuery & ")"

    of NthKinds:
      result = ":" & $demand.kind & "(" & $demand.a & "n, " & $demand.b & ")"

    of PseudoNoParamsKinds:
      result = ":" & $demand.kind

    of tkElement:
      result = $demand.element

    else:
      result = $demand.kind

func `==`*(d1, d2: Demand): bool =
  if d1.kind != d2.kind:
    return false

  else:
    case d1.kind:
      of tkPredicate:
        return d1.predicate == d2.predicate

      of NthKinds:
        return d1.a == d2.b

      of tkPseudoNot:
        return d1.notQuery == d2.notQuery

      of tkElement:
        return d1.element == d2.element

      else:
        raise newException(Exception, "Invalid demand kind: " & $d1.kind)

iterator children[N, K](
    node: N,
    offset: NodeWithParent[N, K] =
      NodeWithParent[N, K](parent: nil, index: -1, elementIndex: -1)
  ): NodeWithParent[N, K] =

  var idx = offset.index + 1
  var elIdx = offset.elementIndex + 1
  while idx < len(node):
    let el = node[idx]
    let ok = (when N is XmlNode: el.kind == xnElement else: true)

    if ok:
      yield NodeWithParent[N, K](
        parent: el, index: idx, elementIndex: elIdx)

      inc elIdx

    inc idx


func initToken(kind: TokenKind, value: string = ""): Token =
  return Token(kind: kind, value: value)

func canFindMultiple(q: Querypart, comb: Combinator,
                     options: set[QueryOption]): bool =
  for demand in q.demands:
    if comb in {cmChildren, cmSiblings} and demand.kind in {
      tkPseudoFirstOfType, tkPseudoLastOfType,
      tkPseudoFirstChild, tkPseudoLastChild, tkPseudoOnlyOfType
    }:
      return false

  return true

# func `$`*(q: Query): string =
#   result = q.queryStr

func isValidNotQuery(q: Query, options: set[QueryOption]): bool =
  return
    q.queries.len == 1 and
    q.queries[0].len == 1 and
    (q.queries[0][0].demands.len == 1 or not (optSimpleNot in options))


func initPseudoToken(str: string): Token =
    let kind = case str:
      of ":empty":            tkPseudoEmpty
      of ":only-child":       tkPseudoOnlyChild
      of ":only-of-type":     tkPseudoOnlyOfType
      of ":first-child":      tkPseudoFirstChild
      of ":last-child":       tkPseudoLastChild
      of ":last-of-type":     tkPseudoLastOfType
      of ":first-of-type":    tkPseudoFirstOfType
      of ":not":              tkPseudoNot
      of ":nth-child":        tkPseudoNthChild
      of ":nth-last-child":   tkPseudoNthLastChild
      of ":nth-of-type":      tkPseudoNthOfType
      of ":nth-last-of-type": tkPseudoNthLastOfType
      else:
        raise newException(ParseError, "Unknown pseudo selector: " & str)

    result = initToken(kind)

func isFinishedSimpleSelector(prev: Token, prevPrev: Token): bool =
  if prev.kind in {tkBracketEnd, tkParam, tkElement} + PseudoNoParamsKinds:
    return true

  if prev.kind == tkIdentifier and prevPrev.kind in {tkClass, tkId}:
    return true

func hasAttr[N, K](node: N, attr: string): bool {.inline.} =
  return not node.attrs.isNil and node.attrs.hasKey(attr)

func validateNth(a, b, nSiblings: int): bool =
    if a == 0:
      return nSiblings == b - 1

    let n = (nSiblings - (b - 1)) / a
    return n.floor == n and n >= 0

proc satisfies[N, K](
    pair: NodeWithParent[N, K],
    demands: seq[Demand[N, K]],
    ctx: QueryCtx[N, K]
  ): bool

proc satisfies[N, K](
    pair: NodeWithParent[N, K],
    demand: Demand[N, K],
    ctx: QueryCtx[N, K]
  ): bool =
  let node = pair.node

  case demand.kind:
    of tkPredicate:
      return demand.predicate(node)

    of tkElement:
      return ctx.eqElement(node, demand.element)

    of tkPseudoEmpty:
      return node.len == 0

    of tkPseudoOnlyChild:
      for siblingPair in children[N, K](pair.parent):
        if siblingPair.node != node:
          return false

      return true

    of tkPseudoOnlyOfType:
      for siblingPair in children[N, K](pair.parent):
        if siblingPair.node != node and
           ctx.eqKind(siblingPair.node, node):

          return false

      return true

    of tkPseudoFirstChild:
      return pair.elementIndex == 0

    of tkPseudoLastChild:
      for siblingPair in pair.parent.children(offset = pair):
        return false

      return true

    of tkPseudoFirstOfType:
      for siblingPair in children[N, K](pair.parent):
        if ctx.eqKind(siblingPair.node, node):
          return siblingPair.node == node

    of tkPseudoLastOfType:
      for siblingPair in children[N, K](pair.parent, offset = pair):
        if ctx.eqKind(siblingPair.node, node):
          return false

      return true

    of tkPseudoNot:
      return not pair.satisfies(demand.notQuery.demands, ctx)

    of tkPseudoNthChild:
      return validateNth(demand.a, demand.b, pair.elementIndex)

    of tkPseudoNthLastChild:
      var nSiblingsAfter = 0
      for siblingPair in pair.parent.children(offset = pair):
        nSiblingsAfter.inc
      return validateNth(demand.a, demand.b, nSiblingsAfter)

    of tkPseudoNthOfType:
      var nSiblingsOfTypeBefore = 0
      for siblingPair in children[N, K](pair.parent):
        if siblingPair.node == node:
          break

        elif ctx.eqKind(siblingPair.node, node):
          nSiblingsOfTypeBefore.inc

      return validateNth(demand.a, demand.b, nSiblingsOfTypeBefore)

    of tkPseudoNthLastOfType:
      var nSiblingsOfTypeAfter = 0
      for siblingPair in pair.parent.children(offset = pair):
        if ctx.eqKind(siblingPair.node, node):
          nSiblingsOfTypeAfter.inc

        return validateNth(demand.a, demand.b, nSiblingsOfTypeAfter)

    else:
        raiseAssert "Invalid demand: " & $demand

proc satisfies[N, K](
    pair: NodeWithParent[N, K],
    demands: seq[Demand[N, K]],
    ctx: QueryCtx[N, K]
  ): bool =

  for demand in demands:
    if not pair.satisfies(demand, ctx):
      return false

  return true

iterator searchDescendants[N, K](
    queryPart: QueryPart[N, K],
    position: NodeWithParent[N, K],
    ctx: QueryCtx[N, K]
  ): NodeWithParent[N, K] =

  var queue = initDeque[NodeWithParent[N, K]]()
  for nodeData in children[N, K](position.node):
    queue.addLast(initWithParent[N, K](
      position.node, nodeData.index, nodeData.elementIndex))

  while queue.len > 0:
    let pair = queue.popFirst()
    if pair.satisfies(queryPart.demands, ctx):
      yield pair

    for nodeData in children[N, K](pair.node):
      queue.addLast(initWithParent[N, K](
        pair.node, nodeData.index, nodeData.elementIndex))

iterator searchChildren[N, K](
    queryPart: QueryPart[N, K],
    position: NodeWithParent[N, K],
    ctx: QueryCtx[N, K]
  ): NodeWithParent[N, K] =

  for pair in children[N, K](position.node):
    if pair.satisfies(queryPart.demands, ctx):
      yield pair

iterator searchSiblings[N, K](
    queryPart: QueryPart[N, K],
    position: NodeWithParent[N, K],
    ctx: QueryCtx[N, K]
  ): NodeWithParent[N, K] =

  for pair in children[N, K](position.parent, offset = position):
    if pair.satisfies(queryPart.demands, ctx):
      yield pair

iterator searchNextSibling[N, K](
    queryPart: QueryPart[N, K],
    position: NodeWithParent[N, K],
    ctx: QueryCtx[N, K]
  ): NodeWithParent[N, K] =

  for pair in position.parent.children(offset = position):
    if pair.satisfies(queryPart.demands, ctx):
      yield pair
    break

type SearchIterator[N, K] =
  iterator(
    q: QueryPart[N, K],
    p: NodeWithParent[N, K],
    ctx: QueryCtx[N, K]
  ): NodeWithParent[N, K] {.inline.}

proc exec[N, K](
    parts: seq[QueryPart],
    root: NodeWithParent[N, K],
    single: bool,
    options: set[QueryOption],
    result: var seq[N],
    ctx: QueryCtx[N, K]
  ) =

  var combinator = cmDescendants
  var buffer = initDeque[NodeWithParent[N, K]]()
  var partIndex = 0
  buffer.addLast root

  template search(position: NodeWithParent[N, K], itr: SearchIterator) =
    for next in itr(parts[partIndex], position, ctx):
      if partIndex == high(parts):
        result.add next.node
        if single:
          return

      else:
        buffer.addLast next

      if not canFindMultiple(parts[partIndex], combinator, options):
        break

  while buffer.len > 0:
    for _ in 0..<buffer.len:
      let position = buffer.popFirst
      case combinator
        of cmDescendants: search(position, searchDescendants)
        of cmChildren:    search(position, searchChildren)
        of cmSiblings:    search(position, searchSiblings)
        of cmNextSibling: search(position, searchNextSibling)
        of cmLeaf: discard

    combinator = parts[partIndex].combinator
    partIndex.inc

proc exec*[N, K](
    query: Query[N, K],
    root: N,
    ctx: QueryCtx[N, K]
  ): seq[N] =

  let wrapper = ctx.wrapNode(root)
  let wrapperRoot = ctx.wrapNode(wrapper)

  result = newSeq[N, K]()
  let wRoot = NodeWithParent[N, K](
    parent: wrapperRoot, index: 0, elementIndex: 0)
  for parts in query.queries:
    parts.exec(wRoot, false, query.options, result, ctx)

template execWithCtx*[N, K](
    tree: N,
    inCtx: QueryCtx[N, K], body: untyped): untyped =

  let ctx {.inject.} = inCtx
  exec(query(body), tree, ctx)
