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


template subnodesEq*(lhs, rhs, field: untyped): untyped =
  ## Check if two objects `lhs` and `rhs` has identical field `field`
  ## by comparing all items in the field. Check if two object's fields
  ## have identical lengths too.
  lhs.field.len() == rhs.field.len() and
  zip(lhs.field, rhs.field).allOfIt(it[0] == it[1])

type
  SingleIt*[T] = object
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


{.push inline.}

func `-`*[E](s1: set[E], s2: E): set[E] = s1 - {s2}
func `-=`*[E](s1: var set[E], v: E | set[E]) = (s1 = s1 - {v})

{.pop.}

import std/[options, times]


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
