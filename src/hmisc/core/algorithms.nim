import
  std/[algorithm]

import
  ./gold



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
