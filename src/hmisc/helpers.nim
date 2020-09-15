import macros, strutils

import algo/[halgorithm, hseq_mapping]
export halgorithm
export hseq_mapping
import sequtils
import hdebug_misc
export hdebug_misc
import strformat


template subnodesEq*(lhs, rhs, field: untyped): untyped =
  ## Check if two objects `lhs` and `rhs` has identical field `field`
  ## by comparing all items in the field. Check if two object's fields
  ## have identical lengths too.
  lhs.field.len() == rhs.field.len() and
  zip(lhs.field, rhs.field).allOfIt(it[0] == it[1])

template fail*(msg: string): untyped =
  raiseAssert(msg)
template nnil*(): untyped =
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
  SingleIt*[T] = object
    it: seq[T]

func getIt*[T](it: SingleIt[T]): T = it.it[0]
func setIt*[T](it: var SingleIt[T], val: T): void = (it.it[0] = val)
func getIt*[T](it: var SingleIt[T]): var T = it.it[0]
func newIt*[T](it: T): SingleIt[T] = SingleIt[T](it: @[it])
converter toT*[T](it: SingleIt[T]): T = it.it[0]

func takesOnlyMutable*[T](v: var T) = discard
template isMutable*(v: typed): untyped = compiles(takesOnlyMutable(v))

macro dumpStr*(body: untyped): untyped =
  newCall(ident "echo", newLit(body.treeRepr()))
