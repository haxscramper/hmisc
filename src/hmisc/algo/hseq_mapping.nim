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

template rfindIt*(s: typed, op: untyped): untyped =
  var res = -1
  for idx, it {.inject.} in rpairs(s):
    if op:
      res = idx
      break

  res


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
