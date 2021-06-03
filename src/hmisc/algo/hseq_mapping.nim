import sequtils, strutils, macros
import algorithm
export algorithm

proc nthType1*[T1, T2](a: (T1, T2)): T1 =
  ## Helper proc to get first type from tuple. Used as workaround for
  ## `pairs` iterator
  discard

proc nthType2*[T1, T2](a: (T1, T2)): T2 =
  ## Helper proc to get second type from tuple. Used as workaround for
  ## `pairs` iterator
  discard


#========================  sorting and filtering  ========================#

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

template twoPassSortByIt*(
  sequence, operation1, operation2: untyped): untyped =
  ## Sort input sequence using firt `operation1`, then group into
  ## 2d-sequence based on result of `operation1` and sort each
  ## subsequence using `operation2`
  # TODO support ascending and descending order. Possible
  # implementation - depending on type of the expression. If it is a
  # tuple `(_, SortOrder)` use it for comparison.
  runnableExamples:
    # Sort by first field and then by second
    assert @[(1,2), (1,9), (4,32), (1,3)].twoPassSortByIt(it[0], it[1]) ==
           @[@[(1, 2), (1, 3), (1, 9)], @[(4, 32)]]


  let s = sequence
  var secondSorted: seq[type(@[s[0]])]

  if s.len > 0:
    let firstSorted = sortedByIt(sequence, operation1)

    for equal in firstSorted.mergeUniqByIt(operation1):
      secondSorted.add(equal.sortedByIt(operation2))

  secondSorted

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

#===========================  transformation  ============================#

macro mapPairs*(inseq: untyped, op: untyped, injectNames: untyped): untyped =
  ## `mapIt` for object with `pairs`. `lhs`, `rhs` and `idx` are
  ## injected into scope
  assert injectNames.kind in {nnkPar, nnkTupleConstr}
  var inj: tuple[lhs, rhs, idx: string] = ("lhs", "rhs", "idx")
  for pair in injectNames:
    case $pair[0]:
      of "lhs": inj.lhs = $pair[1]
      of "rhs": inj.rhs = $pair[1]
      of "idx": inj.idx = $pair[1]

  let
    lhsId = ident(inj.lhs)
    rhsId = ident(inj.rhs)
    idxId = ident(inj.idx)

  quote do:
    block:
      const openarrPairs = ((`inseq` is array) or (`inseq` is seq) or (`inseq` is openarray))

      when openarrPairs:
        when `inseq`[0] is tuple:
          type TLhs = type((`inseq`[0][0]))
          type TRhs = type((`inseq`[0][1]))
        else:
          type TLhs = int
          type TRhs = type((`inseq`[0]))
      else:
        when compiles(for k, v in pairs(`inseq`): discard):
          type TLhs = typeof((
            block:
              var tmp: typeof pairs(`inseq`)
              tmp[0]
          ))
          type TRhs = typeof((
            block:
              var tmp: typeof pairs(`inseq`)
              tmp[1]
          ))
        else:
          type TLhs = int
          type TRhs = type((items(`inseq`)))

      var `idxId` {.inject.}: int = 0
      type TRes = type((
        block:
          var lhsRef: ref TLhs; var `lhsId` {.inject.} = lhsRef[]
          var rhsRef: ref TRhs; var `rhsId` {.inject.} = rhsRef[]
          `op`))

      var res: seq[TRes]

      when openarrPairs:
        when `inseq`[0] is tuple:
          for (`lhsId`, `rhsId`) in items(`inseq`):
            res.add `op`
            inc `idxId`

        else:
          for `lhsId`, `rhsId` in pairs(`inseq`):
            res.add `op`
            inc `idxId`

      else:
        when compiles(for k, v in pairs(`inseq`): discard):
          for `lhsId`, `rhsId` in pairs(`inseq`):
            res.add `op`
            inc `idxId`
        else:
          var lhs {.inject.}: int = 0
          for `rhsId` in items(`inseq`):
            res.add `op`
            inc `lhsId`
            inc `idxId`

      res


template mapPairs*(inseq: untyped, op: untyped): untyped =
  mixin nthType1, nthType2
  mapPairs(inseq, op, (lhs: lhs, rhs: rhs, idx: idx))

template mutMapIt*(s: typed, op: untyped): untyped =
  type OutType = typeof((
    block:
      var it{.inject.}: typeof(mitems(s), typeOfIter);
      op), typeOfProc)

  var i = 0
  var result = newSeq[OutType](s.len)
  for it {.inject.} in mitems(s):
    result[i] = op
    i += 1
  result


template foldlTuple*(sequence, operation: untyped): untyped =
  let s = sequence
  assert s.len > 0, "Can't fold empty sequences"
  static:
    assert s[0] is tuple, $typeof(s[0]) & " sequence of (" &
      $typeof(s) & ") "

  var result: tuple[opres: type(s[0][0]), side: seq[type(s[0][1])]]
  result.opres = s[0][0]
  result.side.add s[0][1]

  for i in 1 ..< s.len:
    let
      a {.inject.} = result.opres
      b {.inject.} = s[i][0]

    result.opres = operation
    result.side.add s[i][1]

  result

func concatSide*[A, B](arg: (A, seq[seq[B]])): (A, seq[B]) =
  result[1] = arg[1].concat()
  result[0] = arg[0]



#==============================  searching  ==============================#

template findIt*(s: typed, op: untyped): int =
  ##[ Find first element of the sequence for which `op` evaluates as
  true and return it's index. If no such element is found return -1
  ]##

  var result = -1
  for idx, it {.inject.} in s:
    if op: result = idx; break

  result

template findItFirst*(s: typed, op: untyped): untyped =
  var res: typeof(s[0])
  var found: bool = false
  for it {.inject.} in s:
    if op:
      res = it
      found = true
      break

  if not found:
    raiseArgumentError("Item not found in sequence " & astToStr(op) & ((
      block:
        if s.len == 0:
          "; no elements in sequence"
        else:
          ""
    )) & " " & $instantiationInfo())

  res


template findItFirstOpt*(s: typed, op: untyped): untyped =
  var res: Option[typeof(s[0])]
  for it {.inject.} in s:
    if op:
      res = some(it)
      break

  res
