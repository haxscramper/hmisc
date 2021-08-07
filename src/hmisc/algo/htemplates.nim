import sugar, sequtils, macros

# TODO avoid unnecessary copying with these templates by checking whether
# `let value = expr` can be optimized to `it[]` hack.


template orElse*(
  value: untyped, predicate: bool, fallback: untyped): untyped =
  if predicate: value
  else: fallback

template setIf*(lhs: untyped, predicate: bool, value: untyped): untyped =
  if predicate: lhs = value


template splitOnIt*[T](s: seq[T], op: untyped): tuple[
  before, after: seq[T]] =
  var res: tuple[before, after: seq[T]]
  var found: bool = false
  for it {.inject.} in s:
    if found:
      res.after.add it
    else:
      if op:
        found = true
      else:
        res.before.add it

  res

template eachIt*(ins, op: untyped): untyped =
  for it {.inject.} in ins:
    op

template anyOfIt*(sequence: typed, predicate: untyped): bool =
  ## Return `true` if for any of the items in sequence `predicate`
  ## evaluates as `true`. Otherwise return false.
  var result = false
  for it {.inject.} in sequence:
    if predicate:
      result = true
      break

  result

template allOfIt*(s: untyped, op: untyped): bool =
  ## True if for all items in `s` predicate `op` returns true.
  mixin anyOfIt
  not s.anyOfIt(not op)

template getIterOpType*(s, op: untyped): untyped =
  typeof((
    block:
      var it {.inject.} = default(typeof(items(s), typeOfIter))
      op
    ), typeOfProc
  )

template mapIt1*(s, op: untyped): untyped =
  type OutType = getIterOpType(s, op)
  var res: seq[OutType]
  for it {.inject.} in s:
    res.add op

  res


template maxIt*(s: untyped, op: untyped): untyped =
  ## Maximize value for all elements in sequence
  type OutType = getIterOpType(s, op)
  var res: OutType = low(OutType)
  for it {.inject.} in s:
    let val = op
    if val > res:
      res = val
  res


template minIt*(s: untyped, op: untyped): untyped =
  ## Maximize value for all elements in sequence
  type OutType = getIterOpType(s, op)
  var res: OutType = high(OutType)
  for it {.inject.} in s:
    let val = op
    if val < res:
      res = val
  res


template findMaxIt*(s: untyped, op: untyped): untyped =
  var res: int = 0
  var idx = 0
  var prev: typeof((let it {.inject.} = s[0]; op))

  for it {.inject.} in s:
    let val = op
    if val > prev:
      res = idx

    inc idx

  res

template getMaxIt*(s: untyped, op: untyped): untyped =
  ## Maximize value for all elements in sequence
  let idx = findMaxIt(s, op)
  s[idx]



template findMinIt*(s: untyped, op: untyped): untyped =
  var res: int = 0
  var idx {.inject.} = 0
  var prevMin: getIterOpType(s, op)
  for it {.inject.} in s:
    let val = op
    if idx == 0:
      prevMin = val

    else:
      if val < prevMin:
        prevMin = val
        res = idx

    inc idx

  res

template getMinIt*(s: untyped, op: untyped): untyped =
  ## Maximize value for all elements in sequence
  let idx = findMinIt(s, op)
  s[idx]

template sumIt*(s: untyped, op: untyped): untyped =
  type OutType = getIterOpType(s, op)
  var res: OutType = default(OutType)
  for it {.inject.} in s:
    res = res + op

  res


template noneOfIt*(s: untyped, op: untyped): bool =
  ## True if for all items in `s` predicate `op` returns true.
  mixin anyOfIt
  not s.anyOfIt(op)
