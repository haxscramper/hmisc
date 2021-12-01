import
  hmisc/preludes/unittest

import std/[intsets, strutils]

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
  writeXmlItems(writer, target, tag, "item")

proc loadXml*(reader: var XmlDeserializer, target: var IntSet, tag: string) =
  var tmp: int
  loadXmlItems(reader, tmp, tag, "item"):
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
    check toXml(12) == "<main>12</main>\n"

  test "Serialize tuples":
    check strdiff(
      toXml((1, 2, "3")),
      """
<main>
  <Field0>1</Field0>
  <Field1>2</Field1>
  <Field2>3</Field2>
</main>
"""
    )

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

    check strdiff(
      toXml(Var(kind: true)),
      """
<main kind="true" kind1="false">
  <f2>0.0</f2>
  <f12>0</f12>
</main>
""")


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

    let target = wrote.fromXml(ImCyclic)
    check strdiff(wrote, target.toXml())

suite "Torture test roundtrip":
  test "8k+ roundtrip":
    let item = toXml(@[(12, 2)])
    check strdiff(item, """
<main>
  <item>
    <Field0>12</Field0>
    <Field1>2</Field1>
  </item>
</main>
""")

    let itemStr = """
<item>
  <Field0>12</Field0>
  <Field1>2</Field1>
</item>
"""

    block:
      let full = "<main>\n" & repeat(itemStr, 8200 div len(itemStr)) & "</main>"
      let values = fromXml(full, seq[(int, int)])

    # block:
    #   let full = "<main>\n" & repeat(itemStr, 8400 div len(itemStr)) & "</main>"
    #   # let tmp = full.len
    #   let values = fromXml(full, seq[(int, int)])


  test "Torture":
    let res = makeTorture().toXml("q")
    let tmp = res.len
    let parsed = res.fromXml(ImTorture, "q")
