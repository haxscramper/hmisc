import macros, strutils

import algo/[halgorithm, hseq_mapping]
export halgorithm
export hseq_mapping
import sequtils
import hdebug_misc
export hdebug_misc
import strformat
import strutils
export strutils

import base_errors
export base_errors

type
  SliceTypes* = Slice[int] | Slice[BackwardsIndex] | HSlice[int, BackwardsIndex]
  IndexTypes* = int | BackwardsIndex


proc startFor*(slice: SliceTypes, base: int): int =
  when slice.a is int:
    result = slice.a

  else:
    result = base - slice.a.int + 1

proc endFor*(slice: SliceTypes, base: int): int =
  when slice.b is int:
    result = slice.b

  else:
    result = base - slice.b.int + 1

func intersect*(slice1, slice2: Slice[int]): Slice[int] =
  max(slice1.a, slice2.a) .. min(slice1.b, slice2.b)

proc clamp*(slice: SliceTypes, base: int): Slice[int] =
  if base == 0:
    result = 0 .. -1

  else:
    result =
      clamp(startFor(slice, base), 0, base) ..
      clamp(endFor(slice, base), 0, base)

  # if base < result.a and base < result.b:
  #   result = 0 .. -1

proc clamp*(slice: SliceTypes, base: Slice[int]): Slice[int] =
  let (s, e) = (startFor(slice, base.b), endFor(slice, base.b))

  if intersect(s .. e, base).len == 0:
    result = 0 .. -1

  else:
    result = clamp(s, base.a, base.b) .. clamp(e, base.a, base.b)

func getClamped*[T](s: seq[T], idx: IndexTypes): T =
  assert s.len > 0
  when idx is BackwardsIndex:
    let pos = clamp(s.len - idx.int + 1, 0, s.high)

  else:
    let pos = clamp(idx, 0, s.high)

  return s[pos]

func `@`*(slice: Slice[int]): seq[int] =
  for idx in slice:
    result.add idx

func clampIdx*(val: int): int = clamp(val, 0, high(int))


template subnodesEq*(lhs, rhs, field: untyped): untyped =
  ## Check if two objects `lhs` and `rhs` has identical field `field`
  ## by comparing all items in the field. Check if two object's fields
  ## have identical lengths too.
  lhs.field.len() == rhs.field.len() and
  zip(lhs.field, rhs.field).allOfIt(it[0] == it[1])

template fail*(msg: string): untyped {.deprecated.} =
  debugecho "Fail on ", instantiationInfo()
  raiseAssert(msg)

template nnil*(): untyped {.deprecated.} =
  defer:
    let iinfo = instantiationInfo()
    when result is seq:
      for idx, val in result:
        when val is ref:
          assert (val != nil)
        else:
          for name, fld in val.fieldPairs():
            when fld is ref:
              if fld.isNil:
                raiseAssert("Non-nil return assert on line " &
                  $iinfo.line & ". Error idx: " & $idx & " fld name: " &
                  name & ". Item type is " & $typeof(val)
                )
    else:
      assert (result != nil)


type
  SingleIt*[T] {.deprecated.} = object
    it: seq[T]

func getIt*[T](it: SingleIt[T]): T = it.it[0]
func setIt*[T](it: var SingleIt[T], val: T): void = (it.it[0] = val)
func getIt*[T](it: var SingleIt[T]): var T = it.it[0]
func newIt*[T](it: T): SingleIt[T] = SingleIt[T](it: @[it])
converter toT*[T](it: SingleIt[T]): T = it.it[0]

func takesOnlyMutable*[T](v: var T) = discard
template isMutable*(v: typed): untyped = compiles(takesOnlyMutable(v))

macro dumpStr*(body: untyped): untyped {.deprecated.} =
  newCall(ident "echo", newLit(body.treeRepr()))

template notNil*(arg: untyped): bool = not isNil(arg)

func nor*(args: varargs[bool]): bool =
  for arg in args:
    result = arg or result

  result = not result

func nand*(args: varargs[bool]): bool =
  result = true
  for arg in args:
    result = arg and result

  result = not result

func `or`*(args: varargs[bool]): bool =
  for arg in args:
    result = arg and result

func `and`*(args: varargs[bool]): bool =
  result = true
  for arg in args:
    result = arg and result

{.push inline.}

func `-`*[E](s1: set[E], s2: E): set[E] = s1 - {s2}
func `-=`*[E](s1: var set[E], v: E | set[E]) = (s1 = s1 - {v})

{.pop.}

import std/[options, times]

proc add*[T](s: var seq[T], opt: Option[T]) =
  if opt.isSome():
    s.add opt.get()

proc `&`*[T](elements: openarray[seq[T]]): seq[T] =
  for element in elements:
    result &= element

proc `&`*(strings: openarray[string]): string =
  for str in strings:
    result &= str

proc `&=`*(target: var string, args: openarray[string]) =
  for arg in args:
    target &= arg

template timeIt*(name: string, body: untyped): untyped =
  block:
    let start = cpuTime()
    body
    let total {.inject.} = cpuTime() - start
    echo &"  {total:<5} ms ", name

proc toString*(x: enum): string {.magic: "EnumToStr", noSideEffect.}

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

proc toRef*[T](t: T): ref T =
  new(result)
  result[] = t

template currIInfo*(): untyped =
  instantiationInfo(fullpaths = true)
