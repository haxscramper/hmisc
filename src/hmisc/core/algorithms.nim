import
  std/[algorithm, sequtils, options]

import
  ./gold

const
  Utf8Any*            = { '\x80' .. '\xFF' }
  Utf8Continuations*  = { char(0b1000_0000) .. char(0b1011_1111) } ##
                        ## Utf8 continuation
  Utf8Starts2*        = { char(0b1100_0000) .. char(0b1101_1111) }
                        ## Start of the two-byte utf8 rune
  Utf8Starts3*        = { char(0b1110_0000) .. char(0b1110_1111) }
                        ## Start of the three-byte utf8 rune
  Utf8Starts4*        = { char(0b1111_0000) .. char(0b1111_0111) }
                        ## Start of the four-byte utf8 rune
  Utf8Starts*         = Utf8Starts2 + Utf8Starts3 + Utf8Starts4
                        ## Start of any utf8 rune



template mergeUniqByIt*(sequence, operation: untyped): untyped =
  ## For each element in sequence apply `operation` and compare
  ## results. Consequent items from `sequence` with equal results will
  ## be added into single subsequence
  runnableExamples:
    assert @[1,2,3,4,4,5].mergeUniqByIt(it) ==
           @[@[1], @[2], @[3], @[4, 4], @[5]]

    assert @[(1,2), (1,2), (1,4), (2,3)].mergeUniqByIt(it[0]) ==
           @[@[(1, 2), (1, 2), (1, 4)], @[(2, 3)]]

  {.line: instantiationInfo(fullPaths = true).}:
    let s = sequence
    var prev =
      block:
        let it {.inject.} = s[0]
        operation

    var equal: seq[type(s[0])] = @[s[0]]
    var result: seq[type(equal)]

    for i in 1..<s.len:
      let it {.inject.} = s[i]
      let new = operation

      if new == prev:
        equal.add it
      else:
        result.add equal
        equal = @[it]

      prev = new

    result.add equal
    result

template deduplicateIt*[T](
  inseq: seq[T], op: untyped, isSorted: bool = false): seq[T] =
  ## Deduplicate values in sequence. If not `isSorted` and value of op
  ## cannot be compared (no operator `<` is defined) naive O(n^2)
  ## deduplication is used. Otherwise items are sorted and
  ## deduplicated in O(nlogn)
  type OpType = typeof((var it {.inject.}: T; op))
  var res: seq[T] = @[]
  if isSorted or compiles(( var a: OpType; a < a )):
    let s =
      if isSorted:
        inseq
      else:
        when compiles(( var a: OpType; a < a )):
          inseq.sortedByIt(op)
        else:
          @[]

    if s.len > 0:
      var prev: OpType = ((let it {.inject.} = s[0]; op))
      res.add(s[0])
      for i in 1..s.high:
        let opres: OpType = ((let it {.inject.} = s[i]; op))
        if opres != prev:
          prev = opres
          res.add(s[i - 1])
  else:
    var opres: seq[OpType]
    for itm in items(inseq):
      let it {.inject.} = itm
      let opval = op
      if not opres.contains(opval):
        opres.add opval
        res.add itm

  res

template sortIt*[T](sequence: var seq[T], expr: untyped): untyped =
  sort(sequence,
       proc(item1, item2: T): int =
         var it {.inject.} = item1
         let it1 = expr
         it = item2
         let it2 = expr
         return cmp(it1, it2))


template groupByIt*(sequence, op: untyped): untyped =
  var res: seq[typeof(sequence)]
  for item in sequence:
    for i in 0 .. res.len:
      if i == res.len:
        res.add @[item]

      else:
        if ((block:
               let it {.inject.} = res[i][0]; op)) ==
           ((block:
               let it {.inject.} = item; op)):
          res[i].add item
          break

  res


template sweepGroupByIt*(sequence, op: untyped): untyped =
  var res: seq[typeof(sequence)]
  var i = 0
  for item in sequence:
    if i == 0:
      res.add @[item]

    else:
      if ((block:
             let it {.inject.} = res[^1][0]; op)) ==
         ((block:
             let it {.inject.} = item; op)):
        res[^1].add item

      else:
        res.add @[item]

    inc i

  res



template twoPassSortByIt*(
  sequence, operation1, operation2: untyped): untyped =
  ## Sort input sequence using firt `operation1`, then group into
  ## 2d-sequence based on result of `operation1` and sort each
  ## subsequence using `operation2`
  # TODO support ascending and descending order. Possible
  # implementation - depending on type of the expression. If it is a
  # tuple `(_, SortOrder)` use it for comparison.

  let s = sequence
  var secondSorted: seq[type(@[s[0]])]

  if s.len > 0:
    let firstSorted = sortedByIt(sequence, operation1)

    for equal in firstSorted.mergeUniqByIt(operation1):
      secondSorted.add(equal.sortedByIt(operation2))

  secondSorted

template subnodesEq*(lhs, rhs, field: untyped): untyped =
  ## Check if two objects `lhs` and `rhs` has identical field `field`
  ## by comparing all items in the field. Check if two object's fields
  ## have identical lengths too.
  bind zip, allIt
  lhs.field.len() == rhs.field.len() and
  zip(lhs.field, rhs.field).allIt(it[0] == it[1])

template findIt*(s: typed, op: untyped): int =
  ##[ Find first element of the sequence for which `op` evaluates as
  true and return it's index. If no such element is found return -1
  ]##

  var result = -1
  for idx, it {.inject.} in s:
    if op: result = idx; break

  result


template byaddr1*(lhs, typ, ex) =
  when typ is typeof(nil):
    when compiles(addr(ex)):
      let tmp = addr(ex)

    else:
      let tmp = unsafeAddr(ex)

  else:
    when compiles(addr(ex)):
      let tmp: ptr typ = addr(ex)

    else:
      let tmp: ptr typ = unsafeaddr(ex)

  template lhs: untyped = tmp[]

func takesOnlyMutable*[T](v: var T) = discard
template isMutable*(v: typed): untyped = compiles(takesOnlyMutable(v))

# template timeIt*(name: string, body: untyped): untyped =
#   block:
#     let start = cpuTime()
#     body
#     let total {.inject.} = cpuTime() - start
#     echo &"  {total:<5} ms ", name



proc toString*(x: enum): string {.magic: "EnumToStr", noSideEffect.}

func toMapArray*[K, V](map: openarray[(K, V)]): array[K, V] =
  for (k, v) in map:
    result[k] = v

func toMapArray*[K, V1, V2](
    conv: proc(v1: V1): V2, map: openarray[(K, V1)]): array[K, V2] =

  for (k, v) in map:
    result[k] = conv(v)

func toRevMapArray*[K, V](map: openarray[(K, V)]): array[V, K] =
  for (k, v) in map:
    result[v] = k

func toMapArray*[K, V](map: openarray[(set[K], V)]): array[K, V] =
  for (keySet, v) in map:
    for k in items(keySet):
      result[k] = v

func toKeySet*[K, V](map: openarray[(K, V)]): set[K] =
  for (k, v) in map:
    result.incl k

func toValSet*[K, V](map: openarray[(K, V)]): set[V] =
  for (k, v) in map:
    result.incl v

func toArrayKeys*[K, V](
    map: openarray[(K, V)], skipDefault: bool = true): seq[K] =
  const def = default(K)
  for (k, v) in map:
    if not skipDefault or k != def:
      result.add k

template eachIt*(main, expr: untyped) =
  for it {.inject.} in main:
    expr

func width*(s: string): int =
  var cnt = 0
  for ch in s:
    if ch == '\n':
      result = max(cnt, result)
      cnt = 0

    else:
      inc cnt

  if result == 0:
    result = cnt
