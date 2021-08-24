import std/[
  math, strutils, sequtils, random, macros, options, strformat,
  parseutils, algorithm, sugar, wordwrap, tables
]


import htemplates
export htemplates

import hseq_distance
import hstring_algo
export hstring_algo

import ../types/hprimitives
import ../core/all

import hmath
export hmath

#=======================  small helper templates  ========================#

template expectEqualTypes(a, b: untyped): untyped =
  assert (a is typeof(b)), "Mismatch between types: first is `" &
    $typeof(a) & "` and second is `" & $typeof(b) & "`"

macro disjointIterImpl(x: typed): untyped =
  var values: seq[NimNode]
  for value in x.getTypeImpl[1..^1]:
    values.add newIdentNode($value.tostrlit)

  result = nnkStmtList.newTree(
    nnkPrefix.newTree(
      newIdentNode("@"),
      nnkBracket.newTree(values)))

macro disjointIter*(x: typed): untyped =
  nnkBracket.newTree(x.getType[1][1..^1])


#========================  sequence operations  ==========================#

type
  LenIndexable*[T] = concept x
    x.len is int
    x[0] is T

iterator rmpairs*[T](s: var LenIndexable[T]): (int, var T) =
  ## Iterate over mutable sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield (idx, s[idx])

iterator ritems*[T](s: var LenIndexable[T]): var T =
  ## Iterate over mutable sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield s[idx]

iterator ritems*(slice: Slice[int]): int =
  assert slice.a <= slice.b
  var idx = slice.b
  while slice.a <= idx:
    yield idx
    dec idx

iterator rpairs*[T](s: T): auto =
  ## Iterate over sequence starting from the rightk
  for idx in countdown(s.len - 1, 0):
    yield (idx, s[idx])

proc rfind*[T, Q](s: T, item: Q): int =
  for idx, pos in rpairs(s):
    if pos == item:
      return idx

  return -1

proc rfindByKind*[T; K: enum](s: T, kind: K | set[K]): auto =
  for idx in ritems(0 ..< s.len):
    when kind is set:
      if s[idx].kind in kind:
        return (idx, s[idx])

    else:
      if s[idx].kind == kind:
        return (idx, s[idx])

iterator ritems*[T](s: openarray[T]): T =
  ## Iterate over sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield s[idx]

iterator rmitems*[T](s: var seq[T]): var T =
  ## Iterate over sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield s[idx]


iterator itemsIsFirst*[T](s: T): auto =
  mixin `[]`
  for idx in 0 .. s.len - 1:
    yield (idx == 0, s[idx])

iterator mitemsIsFirst*[T](s: var seq[T]): (bool, var T) =
  mixin `[]`
  for idx in 0 .. s.len - 1:
    yield (idx == 0, s[idx])

iterator mitemsIsLast*[T](s: var seq[T]): (bool, var T) =
  mixin `[]`
  for idx in 0 .. s.len - 1:
    yield (idx == s.high, s[idx])

iterator itemsIsLast*[T](s: T): auto =
  let sLen = s.len - 1
  var idx = 0

  mixin items
  for item in items(s):
    yield (idx == sLen, item)
    inc idx


iterator itemsIsFirstLast*[T](
  s: LenIndexable[T]): tuple[isFirst, isLast: bool, val: T] =
  let sLen = s.len - 1
  mixin `[]`
  for idx in 0 .. sLen:
    yield (idx == 0, idx == sLen, s[idx])

iterator zip*[T1, T2, T3, T4, T5](
    s1: LenIndexable[T1],
    s2: LenIndexable[T2],
    s3: LenIndexable[T3],
    s4: LenIndexable[T4],
    s5: LenIndexable[T5]
  ): tuple[v1: T1, v2: T2, v3: T3, v4: T4, v5: T5] =

  for idx in 0 ..< min([s1.len, s2.len, s3.len, s4.len, s5.len]):
    yield (s1[idx], s2[idx], s3[idx], s4[idx], s5[idx])


func emptySeq*[T](): seq[T] = discard
proc enumerate*[T](s: openArray[T]): seq[(int, T)] =
  ## Return enumerated sequence of items
  for idx, item in s:
    result.add((idx, item))

func splitList*[T](s: openarray[T]): (T, seq[T]) =
  ## Return head and tail of the list
  if s.len == 0:
    raise newArgumentError("Cannot split empty list")

  (s[0], s[1..^1])


template last*[T](stack: var seq[T]): var T = stack[^1]
template last*[T](stack: seq[T]): T = stack[^1]

template last2*[T](stack: var seq[seq[T]]): var T = stack[^1][^1]
template last2*[T](stack: seq[seq[T]]): T = stack[^1][^1]

# TODO use static hashtable instead of searching whole list each time.
proc matchWith*[K, V](
  val: K,
  tbl: seq[tuple[k: seq[K], v: V]]): Option[V] =
  ## Search `seq[seq[Key], Val]` for entry that has matching `Key` and
  ## return corresponding `Val`. If nothing found return `none(V)`

  for tupl in tbl:
    if val in tupl.k:
      return some(tupl.v)

    result = none(V)

#=======================  single item operations  ========================#

func isSubseq*[T](subseq, inseq: openarray[T]): bool =
  var inPos, subPos: int
  while subPos < subseq.len:
    # debugecho inseq[inPos .. ^1], " ", subseq[subPos]
    let inPos = inseq[inPos .. ^1].find(subseq[subPos])
    # debugecho inPos
    if inPos == -1:
      return false
    else:
      inc subPos
      result = true


func dropSubseq*[T](inseq, subseq: openarray[T]): seq[T] =
  ## Drop all non-overlapping occurencies of `subseq` in `inseq`
  var i = 0
  if subseq.len == 0:
    result = toSeq(inseq)
    return


  var prev = -1
  while i < inseq.len:
    # # debugecho i, " ", inseq[i..^1], " ", subseq
    # if prev == i:
    #   raiseAssert("#[ IMPLEMENT ]#")
    var matches: bool = true
    for shift in 0 ..< subseq.len:
      if (i + shift < inseq.len):
        if (inseq[i + shift] != subseq[shift]):
          matches = false
      else:
        matches = false


    prev = i
    # debugecho "@ ", inseq[i], " matches"
    if not matches:
      result.add inseq[i]
      inc i
    else:
      i += subseq.len


func dropLongestSubseq*[T](inseq: seq[T], subseqs: seq[seq[T]]): seq[T] =
  ## Sort `subseq` by lenght and try to drop each from `inseq`. First
  ## first drop attempt that changes result length is returned.
  let subseqs = subseqs.sortedByIt(-it.len)
  result = inseq
  for sub in subseqs:
    let dropped = inseq.dropSubseq(sub)
    if dropped.len != inseq.len:
      result = dropped
      break


func dropLongestSubseq*(inseq: string, inseqs: seq[string]): string =
  ## Sort `subseq` by lenght and try to drop each from `inseq`. First
  ## first drop attempt that changes result length is returned.
  let inseqs = collect(newSeq):
    for str in inseqs:
      str.mapIt(it)

  dropLongestSubseq(inseq.mapIt(it), inseqs).join("")

func dropSubstr*(instr, substr: string): string =
  ## Drop all occurencies of `substr` in `instr`
  instr.dropSubseq(substr).join("")

func dropLowerPrefix*(str: sink string): string =
  result = str[str.skipWhile({'a' .. 'z'}) .. ^1]



#===============================  options  ===============================#
proc `==`*[T](opt: Option[T],val: T): bool =
  ## Compare option with value for equatilty
  if opt.isNone: false
  else: opt.get() == val

proc `==`*[A, B](tpl: (Option[A], Option[B]), tpl1: (A, B)): bool =
  ## Compare tuple of optional values for equality
  tpl[0] == tpl1[0] and tpl[1] == tpl1[1]

template ifSomeIt*[T](opt: Option[T], predicate: untyped): bool =
  when not compiles(opt.isSome()):
    static: error "ifSomeIt denends on options module. " &
      "Add `import std/options` to fix this error"
  else:
    ((
      block:
      (opt.isSome() and ((let it {.inject.} = opt.get(); predicate)))
    ))



template getSome*[T](opt: Option[T], injected: untyped): bool =
  let expr = opt

  expr.isSome() and (
    let injected {.inject.} = expr.get();
    mixin injected;
    true
  )


template mapSomeIt*[T](opt: Option[T], expr: untyped): untyped =
  var result: Option[typeof(
    block:
      var it {.inject.}: T
      expr
  )]

  if opt.isSome():
    let it {.inject.} = opt.get()
    result = some(expr)

  result


#================================  tests  ================================#



type
  MarkTable*[K, M] = object
    used: set[M]
    table: Table[K, M]

func nextVal*[E](used: var set[E]): E =
  var allowed: set[E] = { low(E) .. high(E) } - used
  result = pop(allowed)
  used.incl result

func getMark*[K, E](marks: var MarkTable[K, E], value: K): E =
  if value notin marks.table:
    marks.table[value] = nextVal(marks.used)

  result = marks.table[value]

proc nextRandVal*[E](used: var set[E]): E =
  var allowed: set[E] = { low(E) .. high(E) } - used
  result = sample(allowed)
  used.incl result

proc getRandMark*[K, E](marks: var MarkTable[K, E], value: K): E =
  if value notin marks.table:
    marks.table[value] = nextRandVal(marks.used)

  result = marks.table[value]




func mapChar*[Cat: enum](
  ch: char, map: static[openarray[tuple[key: char, val: Cat]]]): Cat =

  const
    chars = toKeySet(map)
    map = toMapArray(map)

  if ch notin chars:
    raise newArgumentError(
      &"Unexpected input char: got '{ch}', but expected {chars}")

  return map[ch]

type
  StrNormalizationKind* = enum
    snkNoNormalization
    snkNimNormalize
    snkFullNormalize
    snkCaseNormalize

  EnumParseError* = object of ParseError



func normalize*(str: string, kind: StrNormalizationKind): string =
  case kind:
    of snkNoNormalization:
      result = str

    of snkNimNormalize, snkFullNormalize:
      var start = 0
      if kind == snkNimNormalize:
        result.add str[0]
        inc start

      for ch in str[start ..^ 1]:
        case ch:
          of '_', '-': discard
          of 'a'..'z': result.add ch
          of 'A'..'Z': result.add char(ch.uint8 - 32)
          else: result.add ch

    of snkCaseNormalize:
      for ch in str:
        case ch:
          of 'A'..'Z': result.add char(ch.uint8 - 32)
          else: result.add ch


func parseEnum*[E: enum](
    map: array[E, string],
    str: string,
    normalize: StrNormalizationKind = snkNimNormalize,
    optionalPrefix: bool = true
  ): E =
  if optionalPrefix:
    let normalized = str.dropLowerPrefix().normalize(normalize)

    for (key, val) in pairs(map):
      if val == normalized:
        return key

    raise newException(
      EnumParseError,
      &"Could not parse enum value for {$typeof(E)} from '{str}' (normalized to {normalized})")

  let normalized = str.normalize(normalize)

  for (key, val) in pairs(map):
    if val == normalized:
      return key

  raise newException(
    EnumParseError,
    &"Could not parse enum value for {$typeof(E)} from '{str}'")



# func `&`*[T](s: seq[T], v: T): seq[T] = s & @[v]

#=========================  functional helpers  ==========================#

func curry1*[A, B, C](arg: proc(a: A, b: B): C, a: A): proc(b: B): C =
  return proc(b: B): C = arg(a, b)

func curry2*[A, B, C](arg: proc(a: A, b: B): C, b: B): proc(a: A): C =
  return proc(a: A): C = arg(a, b)

template matchProc1*[A, B](pr: untyped): proc(a: A): B =
  block:
    proc tmp(a: A): B = pr(a)
    tmp

template matchProc2*[A, B, C](pr: untyped): proc(a: A, b: B): C =
  block:
    proc tmp(a: A, b: B): C = pr(a, b)
    tmp

template matchCurry2*[B](tA: typed, val: B, pr: untyped): untyped =
  block:
    type ResT = typeof((var a: tA; pr(a, val)))
    proc tmp(a: tA): ResT = pr(a, val)
    tmp


when isMainModule:
  proc t(a, b: int): string = $a & $b
  proc t(a, b: string): string = a & b
  assert matchProc2[int, int, string](t).curry1(9)(3) == "93"
  assert matchProc2[int, int, string](t).curry2(9)(3) == "39"
  assert matchCurry2(int, 9, t)(3) == "39"
