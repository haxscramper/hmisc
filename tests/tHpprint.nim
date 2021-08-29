{.passc: "-pg".}
{.passl: "-pg".}

import
  std/[
    sequtils, strutils, terminal, random,
    tables, json, macros
  ]

import
  hmisc/other/hpprint,
  hmisc/algo/halgorithm,
  hmisc/types/colorstring,
  hmisc/preludes/unittest

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
    check pstring(12, conf = op) == "12"

  test "string":
    check pstring("112", conf = op) == "\"112\""

  test "Anonymous tuple":
    check pstring((12, "sdf"), conf = op) == "(12, \"sdf\")"

  test "Named tuple":
    check pstring((a: "12", b: "222"), conf = op) == "(a: \"12\", b: \"222\")"

  test "Narrow sequence":
    check @[1, 2, 3, 4].pstring(6, conf = op) == """
- 1
- 2
- 3
- 4"""

  test "Wide sequence":
    check @[1, 2, 3].pstring(conf = op) == "[1, 2, 3]"

  test "int-int table":
    check {2: 3, 4: 5}.toOrderedTable().pstring(conf = op) ==
        "{ 2: 3, 4: 5"

  test "seq-seq table":
    let tbl = toOrderedTable {
      toSeq(0 .. 10): toSeq(0 .. 10),
      toSeq(0 .. 9): toSeq(0 .. 10),
      toSeq(0 .. 8): toSeq(0 .. 10),
    }

    let str = tbl.pstring(conf = op)
    check str == lit3"""
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] =
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] =
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      [0, 1, 2, 3, 4, 5, 6, 7, 8] =
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"""


  test "Sequence of tuples":
    check @[(1, 3), (4, 5)].pstring(conf = op) == "[(1, 3), (4, 5)]"

  type
    T = object
      f1: int

  test "Simple object":
    check T(f1: 12).pstring(conf = op) == "T(f1: 12)"

  test "Multiline constant":
    type
      Tt = object
        f2: string

    let str = Tt(f2: lit3"""
      Aliquam erat volutpat. Nunc sad asdfd
      non orci commodo lobortis. Proin nequ
      lobortis eget, lacus. Sed diam. Praes
      Nullam tempus. Mauris ac felis vel ve
      pede. Etiam vel neque nec dui digniss
      Phasellus neque orci, porta a, alique
      Phasellus purus. Pellentesque tristiq""").pstring(conf = op)

    check str == lit3"""
      Tt(f2: "Aliquam erat volutpat. Nunc sad asdfd
             non orci commodo lobortis. Proin nequ
             lobortis eget, lacus. Sed diam. Praes
             Nullam tempus. Mauris ac felis vel ve
             pede. Etiam vel neque nec dui digniss
             Phasellus neque orci, porta a, alique
             Phasellus purus. Pellentesque tristiq")"""

  test "Sequence of objects":
    check @[T(f1: 12), T(f1: -99)].pstring(conf = op) ==
        "[T(f1: 12), T(f1: -99)]"

  type
    C = object
      case kind: bool
      of true: f90: string
      of false: f09: (float, string)

  test "Case object":
    check C(kind: true, f90: "12").pstring(conf = op) ==
         "C(kind: true, f90: \"12\")"
    check C(kind: false, f09: (1.2, "12")).pstring(conf = op) ==
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
  #   check str, "[[[[[[[[1]]]]]]]]"

  test "4x4 seq":
    check @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstring(80, conf = op) ==
         "[[1, 2, 3, 4], [5, 6, 7, 8], [9, 1, 2, 3], [4, 5, 6, 7]]"


  test "Narrow 4x4 seq":
    check @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstring(20, conf = op) == lit3"""
      - [1, 2, 3, 4]
      - [5, 6, 7, 8]
      - [9, 1, 2, 3]
      - [4, 5, 6, 7]"""


  test "Super narrow 2x2 seq":
    check @[
      @[1, 2, 4],
      @[5, 6, 8],
    ].pstring(7, conf = op) == lit3"""
      - - 1
        - 2
        - 4
      - - 5
        - 6
        - 8"""

suite "Other tests":
  test "primitive types colored":
    show:
      pstring("hello")
      pstring(12)
      pstring('1')
      pstring(@["12", "2"])

  test "Tuples and json colored":
    show:
      pstring(%["he", "llo"])
      pstring(%12)

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

    show pstring(a)

  test "Enum array":
    type
      En = enum
        en1 = "sdf"
        en2
        en3

    block:
      var arr: array[En, string]
      show pstring(arr)

    block:
      var arr: array[en1 .. en2, string]
      show pstring(arr)

import hmisc/algo/[hparse_base, hlex_base]

suite "Extra features":
  test "extra fields":
    type
      ExtraType = object
        name: string

    pprint(
      ExtraType(),
      extraFields = @[
        pprintExtraField(ExtraType, "nameLen", newPPrintConst($it.name.len))
      ]
    )

  test "Token list":
    var str = asRef("10293480129480123")
    type En = enum ostCommandPrefix
    let tokens = @[
      HsTok[En](isSlice: true, baseStr: str, finish: 2),
      HsTok[En](isSlice: true, baseStr: str, finish: 2),
      HsTok[En](isSlice: true, baseStr: str, finish: 2),
      HsTok[En](isSlice: true, baseStr: str, finish: 2),
      HsTok[En](isSlice: true, baseStr: str, finish: 2),
      HsTok[En](isSlice: true, baseStr: str, finish: 2),
    ]

    pprint(
      tokens,
      ignore = matchField("baseStr"),
      force = { matchType"HsTok": forceLine() },
      extraFields = @[
        pprintExtraField(
          "HsTok", HsTok[En], "str",
          newPprintConst(
            "\"" & it.strVal() & "\"", fgYellow + bgDefault))
      ]
    )


testFileEnded()

suite "Tree pprint":
  test "Simple type":
    echo pptree(12).objectTreeRepr()

  test "With fields":
    echo pptree((a: 12, b: 12)).objectTreeRepr()
    echo pptree((a: 12, b: (c: 12, q: "string"))).objectTreeRepr()

  test "Sequence":
    echo pptree(@[(a: 12), (a: 24)]).objectTreeRepr()



when false:
  # TODO test this
  pprint(
    tokens,
    ignore = matchField("baseStr"),
    force = { matchType("HsTok"): forceLine() }
  )
