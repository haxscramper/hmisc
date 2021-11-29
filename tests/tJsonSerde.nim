import
  hmisc/preludes/unittest

import std/[intsets]

import
  hmisc/hasts/json_serde

import
  ./assets/example_types

type
  ImCyclic* = ref object
    used*: seq[ImCyclic]
    field: int

jsonSerdeFor(ImDist, loadJsonDistinct, writeJsonDistinct)

proc cyclic*(sub: varargs[ImCyclic]): ImCyclic =
  ImCyclic(used: @sub)

proc add*(cy: var ImCyclic, other: ImCyclic) =
  cy.used.add other


proc writeJson*(writer: var JsonSerializer, target: IntSet) =
  writeJsonItems(writer, target)

proc loadJson*(reader: var JsonDeserializer, target: var IntSet) =
  var tmp: int
  loadJsonItems[int, IntSet](reader, target):
    target.incl tmp

proc round[T](obj: T): T =
  let tmp = obj.toJson()
  return tmp.fromJson(T)

suite "Write basic types":
  test "Serialize primitives":
    echo toJson(12)

  test "Serialize tuples":
    echo toJson((1, 2, "3"))

  test "Variant objects":
    type
      Var = object
        case kind*: bool
          of false:
            f1: int

          of true:
            f2: float

        str: string

        case kind1*: bool
          of false:
            f12: int

          of true:
            f23: float

    echo toJson(Var(kind: true))


  test "Serialize objects":
    type
      Obj = object
        f1: int
        f2: float

    let start = Obj(f1: 123, f2: 0.12)
    echo start.toJson()
    check:
      structdiff start, start.round()

  test "Imported object serialization":
    let start = ImObj(x: 12)
    check:
      structdiff start, start.round()

  test "Cyclic":
    var one = cyclic()
    # var two = cyclic(one, cyclic(), cyclic(), nil)
    var two = cyclic(one, cyclic(), cyclic(), nil)
    one.add two

    let wrote = one.toJson()
    echo wrote

    let target = wrote.fromJson(ImCyclic)

    echo target.toJson()

  test "Torture":
    let res = makeTorture().round()
    echo res.toJson()
