
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

suite "Basic types":
  test "Tuple":
    check:
      round((1, 2)) == (1, 2)
      round((a: 12, b: "sfad")) == (a: 12, b: "sfad")

  test "Object":
    type
      Obj = object
        f1: int
        f2: (int, int)

    check round(Obj()) == Obj()

suite "Roundtrip tests":
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

    echo withItWriter(it.writeJsonObject(res, multiline = true))
    # check structdiff(res, makeTorture())

  test "Writer vertical items":
    echo withItWriter(it.writeJsonItems(@[1, 2, 3, 4], true))

  test "Writer kv-pairs":
    echo withItWriter(it.writeJsonPairs(@[1,2,3,4], true, false))

  test "Mutliline object":
    echo withItWriter(it.writeJsonObject((a: 12, b: "123", c: [1,2,3,4]), false, true))

type
  HookObj = object
    field: int

proc jsonRenameField(obj: var HookObj, field: var string) =
  case field:
    of "___f": field = "field"

suite "User hooks":
  test "Rename field":
    let f = fromJson("{\"___f\": 12}", HookObj)

    check:
      f.field == 12


suite "Error reporting":
  test "Allow unknown fields":
    let res = fromJson("{\"a\": 12, \"q\": 300}", tuple[a, b: int])
    check res.a == 12

  test "Raise for unknown field":
    expect JsonSerdeUnknownFieldError as err:
      let res = fromJson(
        "{\"a\": 12, \"q\": 300}",
        tuple[a, b: int],
        options = {})

    check:
      err.field == "q"

  test "Invalid input data":
    expect JsonSerdeUnexpectedTypeError as err:
      let res = fromJson("[12]", (string,))

    check:
      err.whenUsing == "string"



# # from jsony import nil

# suite "Bench":
#   type
#     Obj = object
#       values: array[256, (int, int)]

#   timeIt "json_serde":
#     let text = json_serde.toJson(Obj())
#     let value = json_serde.fromJson(text, Obj)

#   timeIt "jsony":
#     let text = jsony.toJson(Obj())
#     let value = jsony.fromJson(text, Obj)
