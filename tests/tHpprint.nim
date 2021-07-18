{.passc: "-pg".}
{.passl: "-pg".}

import
  std/[
    unittest, sequtils, strutils, terminal, random,
    tables, json, macros
  ]

import
  hmisc/other/hpprint,
  hmisc/algo/halgorithm,
  hmisc/hdebug_misc

startHax()


proc ruler(step: int = 10) =
  for rep in countdown(
    ((terminalWidth() - 5) div step) * step, step, step):
    echo repeat("-", rep), "| ", rep

suite "PPrint simple":
  test "Get tree":
    let tree = toPPrintTree(12)

var op = defaultPPrintConf
op.colored = false

import benchy

suite "PPrint data":
  test "Simple types":
    pprint 12
    pprint "test"
    pprint 0 + 190
    pprint (1, 2)
    pprint @[1,2,3,4,4]
    pprint (a: 123)

  test "Layout selection":
    for num in [1, 2]:
      for margin in [4, 10]:
        echo "num: ", num, ", margin: ", margin
        pprint toSeq(0 .. num), margin

  test "Nested seq":
    let val = mapIt(
      0 .. 20,
      mapIt(1 .. sample({0 .. 5}), (it, $it)))

    let v = pstring val

suite "Simple configuration":
  test "integer":
    assertEq pstring(12, conf = op), "12"

  test "string":
    assertEq pstring("112", conf = op), "\"112\""

  test "Anonymous tuple":
    assertEq pstring((12, "sdf"), conf = op), "(12, \"sdf\")"

  test "Named tuple":
    assertEq pstring((a: "12", b: "222"), conf = op), "(a: \"12\", b: \"222\")"

  test "Narrow sequence":
    assertEq @[1, 2, 3, 4].pstring(6, conf = op), """
- 1
- 2
- 3
- 4"""

  test "Wide sequence":
    assertEq @[1, 2, 3].pstring(conf = op), "[1, 2, 3]"

  test "int-int table":
    assertEq {2: 3, 4: 5}.toOrderedTable().pstring(conf = op),
        "{ 2: 3, 4: 5"

  test "seq-seq table":
    let tbl = toOrderedTable {
      toSeq(0 .. 10): toSeq(0 .. 10),
      toSeq(0 .. 9): toSeq(0 .. 10),
      toSeq(0 .. 8): toSeq(0 .. 10),
    }

    let str = tbl.pstring(conf = op)
    assertEq str, """
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] =
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9] =
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
[0, 1, 2, 3, 4, 5, 6, 7, 8] =
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"""


  test "Sequence of tuples":
    assertEq @[(1, 3), (4, 5)].pstring(conf = op),
        "[(1, 3), (4, 5)]"

  type
    T = object
      f1: int

  test "Simple object":
    assertEq T(f1: 12).pstring(conf = op), "T(f1: 12)"

  test "Multiline constant":
    type
      Tt = object
        f2: string

    let str = Tt(f2: """
      Aliquam erat volutpat. Nunc sad asdfd
      non orci commodo lobortis. Proin nequ
      lobortis eget, lacus. Sed diam. Praes
      Nullam tempus. Mauris ac felis vel ve
      pede. Etiam vel neque nec dui digniss
      Phasellus neque orci, porta a, alique
      Phasellus purus. Pellentesque tristiq""".dedent()).pstring(conf = op)
    assertEq str, """
      Tt(f2: "Aliquam erat volutpat. Nunc sad asdfd
             non orci commodo lobortis. Proin nequ
             lobortis eget, lacus. Sed diam. Praes
             Nullam tempus. Mauris ac felis vel ve
             pede. Etiam vel neque nec dui digniss
             Phasellus neque orci, porta a, alique
             Phasellus purus. Pellentesque tristiq"""".dedent

  test "Sequence of objects":
    assertEq @[T(f1: 12), T(f1: -99)].pstring(conf = op),
        "[T(f1: 12), T(f1: -99)]"

  type
    C = object
      case kind: bool
      of true: f90: string
      of false: f09: (float, string)

  test "Case object":
    assertEq C(kind: true, f90: "12").pstring(conf = op),
         "C(kind: true, f90: \"12\")"
    assertEq C(kind: false, f09: (1.2, "12")).pstring(conf = op),
         "C(kind: false, f09: (1.2, \"12\"))"

suite "Colored printing":
  # test "Field annotation":
  #   type
  #     A = object
  #       f1: seq[int]
  #       f2: seq[A]

  #   var tree = toObjTree(A(
  #     f1: @[21],
  #     f2: @[
  #     A(f1: @[1, 2, 3, 4, 5, 6]),
  #     A(f1: @[1, 2, 3, 4, 5, 6]),
  #     A(f1: @[1, 2, 3, 4, 5, 6]),
  #     A(f1: @[1, 2, 3, 4, 5, 6])
  #   ]))

  #   tree.getAtPath(@[objAccs("f1"), seqAccs(0)]).annotate(" Hello".toRed())
  #   tree.getAtPath(@[objAccs("f2")]).annotate("Hello".toGreen())
  #   tree.getAtPath(@[objAccs("f2")]).stylize(initStyle(fgRed))

  #   tree.getAtPath(@[objAccs("f2"), seqAccs(1)]).stylize(
  #     initStyle(fgBlue))

  #   echo tree.pstringing()

  test "Base pprint":
    pprint 12
    pprint [1, 2, 3, 4]
    pprint ["hello"]
    pprint ("123", 0.3, nil)


suite "Deeply nested types":
  # test "8D sequence":
  #   let str = @[@[@[@[@[@[@[@[1]]]]]]]].pstring(conf = op)
  #   echo str
  #   assertEq str, "[[[[[[[[1]]]]]]]]"

  test "4x4 seq":
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstring(80, conf = op),
         "[[1, 2, 3, 4], [5, 6, 7, 8], [9, 1, 2, 3], [4, 5, 6, 7]]"


  test "Narrow 4x4 seq":
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstring(20, conf = op), """
      - [1, 2, 3, 4]
      - [5, 6, 7, 8]
      - [9, 1, 2, 3]
      - [4, 5, 6, 7]""".dedent


  test "Super narrow 2x2 seq":
    assertEq @[
      @[1, 2, 4],
      @[5, 6, 8],
    ].pstring(7, conf = op), """
      - - 1
        - 2
        - 4
      - - 5
        - 6
        - 8""".dedent

suite "Other tests":
  test "primitive types colored":
    pprint "hello"
    pprint 12
    pprint '1'
    pprint @["12", "2"]

  test "Tuples and json colored":
    pprint %["he", "llo"]
    pprint %12

  test "Larger types colored":
    pprint PPrintConf(), force = {
      matchField("format_policy"): forceStack()
    }, ignore = matchField("cpack")

  test "Cyclic objects":
    type
      A = ref object
        next: A

    var a = A()
    a.next = a

    pprint a

  test "Enum array":
    type
      En = enum
        en1 = "sdf"
        en2
        en3

    block:
      var arr: array[En, string]
      pprint(arr)

    block:
      var arr: array[en1 .. en2, string]
      pprint(arr)
