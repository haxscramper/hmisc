import
  hmisc/preludes/unittest

import std/[intsets]

import
  hmisc/hasts/xml_serde,
  hmisc/algo/hstring_algo

import
  ./assets/example_types

type
  ImCyclic* = ref object
    used*: seq[ImCyclic]
    field: int

xmlSerdeFor(ImDist, loadXmlDistinct, writeXmlDistinct)

proc cyclic*(sub: varargs[ImCyclic]): ImCyclic =
  ImCyclic(used: @sub)

proc add*(cy: var ImCyclic, other: ImCyclic) =
  cy.used.add other


proc writeXml*(writer: var XmlSerializer, target: IntSet, tag: string) =
  writeXmlItems(writer, target, tag)

proc loadXml*(reader: var XmlDeserializer, target: var IntSet, tag: string) =
  var tmp: int
  loadXmlItems(reader, tmp, tag):
    target.incl tmp

proc fromXml[T](text: string, target: typedesc[T]): T =
  var reader = newXmlDeserializer(text)
  loadXml(reader, result, "main")

proc toXml[T](obj: T): string =
  var writer = newXmlSerializer()
  writeXml(writer, obj, "main")
  return writer.readAll()

proc round[T](obj: T): T =
  let tmp = obj.toXml()
  return tmp.fromXml(T)

suite "Write basic types":
  test "Serialize primitives":
    echo toXml(12)

  test "Serialize tuples":
    echo toXml((1, 2, "3"))

  test "Variant objects":
    type
      Var = object
        case kind*: bool
          of false:
            f1: int

          of true:
            f2: float


        case kind1*: bool
          of false:
            f12: int

          of true:
            f23: float

    echo toXml(Var(kind: true))


  test "Serialize objects":
    type
      Obj = object
        f1: int
        f2 {.Attr.}: float

    let start = Obj(f1: 123, f2: 0.12)
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

    let wrote = one.toXml()
    echo wrote

    let target = wrote.fromXml(ImCyclic)

    echo target.toXml()

suite "Torture test roundtrip":
  test "Torture":
    let res = makeTorture().toXml("q")
    echov res.len
    "/tmp/q.xml".writeFile(res)
    let parsed = res.fromXml(ImTorture, "q")
