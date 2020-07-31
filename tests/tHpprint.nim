import unittest, shell, sugar, sequtils

import hmisc/[helpers]
import strutils

import hasts/[graphviz_ast, html_ast]
import hmisc/types/[
  block_grid,
  hdrawing,
  hnim_ast,
  htrie,
]

import hmisc/macros/[obj_field_macros]

import hmisc/[hpprint, hpprint_graphviz, hcommon_converters]
import hmisc/types/seq2d

suite "Block grid":
  test "{makeGrid} make string grid":
    var grid = makeGrid[StrBlock](3, 3, makeThinLineGridBorders())
    for (pos, cell) in grid.itercells():
      grid[pos] = makeGrid(
        @[
          @["[||||", "world\neee"],
          @["EEEE", "####"],
          @["eee"]
        ].toStrGrid(@[""].toStrBlock()),
        makeAsciiGridBorders()
      ).toCell()

    grid.addHeader(makeCell[StrBlock](
      @["! ANNOTATION !"].toStrBlock(),
      (3, 1)
    ))
    grid.addHeader(makeCell[StrBlock](
      @["! ANNOTATION 2 !"].toStrBlock(),
      (3, 1)
    ))

  test "Convert object tree into grid":
    echo toPGrid("Hello")
    echo toPGrid(("Hello", 12, 1.2))

  test "Block grid to html table":
    var grid = makeGrid(
      @[@["hello", "123"],
        @["world", "1234", "123"]])

    grid.setOrAdd(makeArrPos(0, 2), makeGrid(
      @[@["eee", "Ewer"], @["123", "123"]]))

    let text = grid.toHtml().toHtmlDoc()
    echo text

    "/tmp/grid.html".writeFile(text)


  test "Graphviz block grid":
    let grid = makeGrid(@[@["hell", "eolrsd"], @["", "123"]])
    let graph = makeDotGraph(nodes = @[makeNode(1, grid.toHtml())])
    graph.toPng("/tmp/hello.png")
    quit 0


suite "Block labeling":
  template testtmp(
    labels: untyped, chunkLines: seq[string] = @["[|||]"]): untyped =
    relativePosition(
      chunk = makeChunk(chunkLines), labels)

  test "Chunk label on left":
    assertEq $(testtmp(
      @{ rpTopLeftLeft : (text: "<>", offset: 0) })), "<>[|||]"

  test "Chunk label on top left":
    assertEq $(testtmp(
      @{ rpTopLeftAbove : (text: "<-->", offset: 2) })), "<-->\n  [|||]"

  test "Chunk label on bottom right":
    assertEq $(testtmp(
      @{ rpBottomRight : (text: "<>", offset: 2) })), "[|||]<>"

  test "Chunk label on bottom left":
    assertEq $(testtmp(
      @{ rpBottomLeft : (text: "<>", offset: 2) })), "  [|||]\n<>"

  test "Top-above & bottom left":
    assertEq $(testtmp(
      @{ rpTopLeftAbove : (text: "{{{", offset: 2),
         rpBottomLeft : (text: "}}}", offset: 2)})),
         """
         {{{
           [|||]
         }}}""".dedent

  test "Multiline block compact":
    assertEq $(testtmp(
      @{ rpBottomRight: (text: "}}", offset: 2),
         rpTopLeftLeft: (text: "{{", offset: 2)
         # Top left left offset should be ignored
       },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{[||||]
           [||||]}}""".dedent

  test "Multiline block expanded":
    assertEq $(testtmp(
      @{ rpBottomLeft: (text: "}}", offset: 2),
         rpTopLeftAbove: (text: "{{", offset: 2)
         # Top left left offset should be ignored
       },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{
           [||||]
           [||||]
         }}""".dedent

  test "Multiline block expanded with prefix":
    assertEq $(testtmp(
      @{ rpBottomLeft: (text: "}}", offset: 2),
         rpTopLeftAbove: (text: "{{", offset: 2),
         rpPrefix: (text: "- ", offset: 2)
         # Top left left offset should be ignored
       },
      chunkLines = @["[||||]", "[||||]"]
    )),
         """
         {{
         - [||||]
         - [||||]
         }}""".dedent

  test "Invalid prefix assertion":
    try:
      discard testtmp(@{
        rpTopLeftLeft: (text: "==", offset: 2),
        rpPrefix: (text: "--", offset: 2)
      })

      fail("Unreachable code")
    except AssertionError:
      assert getCurrentExceptionMsg().startsWith("Incompatible chunk labels")
    except:
      fail("Wrong exception")


suite "Case object field iteration":
  discard

  test "{makeFieldsLiteral} No case fields :macro:":
    type
      U = object
        f1: int

    let generated = U.makeFieldsLiteral()
    let expected = @[
      ValField(name: "f1", fldType: "int", isKind: false, isTuple: false)
    ]

    # echo $(ValField())
    # static:
    #   echo typeof(generated)
    #   echo typeof(expected)

    assert generated == expected

  test "{makeFieldsLiteral} Multiple fields on the same level :macro:":
    type
      U = object
        f1: int
        f2: float
        f3: char
        f4: string

    let generated = U.makeFieldsLiteral()
    let expected = @[
      ValField(name: "f1", fldType: "int", isKind: false, isTuple: false),
      ValField(name: "f2", fldType: "float", isKind: false, isTuple: false),
      ValField(name: "f3", fldType: "char", isKind: false, isTuple: false),
      ValField(name: "f4", fldType: "string", isKind: false, isTuple: false)
    ]

    if generated != expected:
      # "/tmp/generated.nim".writeFile(pstring generated)
      # "/tmp/expected.nim".writeFile(pstring expected)
      # shell:
      #   cwdiff /tmp/expected.nim /tmp/generated.nim

      quit 1

  test "{makeFieldsLiteral} Single case field :macro:":
    type
      U = object
        case kind: bool
          of true:
            f1: int
          of false:
            f2: float

    let lhs = U.makeFieldsLiteral()
    let rhs = @[
      ValField(fldType: "bool", name: "kind", isKind: true,
               isTuple: false, branches: @[
        ValFieldBranch(
          value: ValObjTree(
            kind: okConstant, constType: "bool", strLit: "true"),
          flds: @[ ValField(fldType: "int", isKind: false,
                            name: "f1", isTuple: false) ],
          isElse: false
        ),
        ValFieldBranch(
          value: ValObjTree(
            kind: okConstant, constType: "bool", strLit: "false"),
          flds: @[ ValField(
            fldType: "float", isKind: false, name: "f2", isTuple: false) ],
          isElse: false
        ),
      ]
    )]

    if lhs != rhs:
      raiseAssert "Fail"

  test "{makeFieldsLiteral} Multiple case fields :macro:":
    type
      U = object
        case kind1: bool
          of true: f11: int
          of false: f21: float

        case kind2: char
          of 'a':
            f12: int
          else:
            f22: float

    let generated = U.makeFieldsLiteral()
    let expected  = @[
      ValField(fldType: "bool", name: "kind1", isKind: true,
               isTuple: false, branches: @[
        ValFieldBranch(
          value: ValObjTree(
            kind: okConstant, constType: "bool", strLit: "true"),
          flds: @[ ValField(
            fldType: "int", isKind: false, name: "f11", isTuple: false) ],
          isElse: false
         ),
        ValFieldBranch(
          value: ValObjTree(
            kind: okConstant, constType: "bool", strLit: "false"),
          flds: @[ ValField(
            fldType: "float", isKind: false, name: "f21", isTuple: false) ],
          isElse: false
         ),
      ]),
      ValField(fldType: "char", name: "kind2", isKind: true,
               isTuple: false, branches: @[
        ValFieldBranch(
          value: ValObjTree(
            kind: okConstant, constType: "char", strLit: "'a'"),
          flds: @[ ValField(fldType: "int", isKind: false,
                            name: "f12", isTuple: false) ],
          isElse: false
        ),
        ValFieldBranch(
          value: ValObjTree(),
          flds: @[ ValField(fldType: "float", isKind: false,
                            name: "f22", isTuple: false) ],
          isElse: true
        ),
      ])
    ]


    if generated != expected:
      # raiseAssert "Fail"
      # "/tmp/generated.nim".writeFile(pstring generated)
      # "/tmp/expected.nim".writeFile(pstring expected)
      # shell:
      #   cwdiff /tmp/expected.nim /tmp/generated.nim
      quit 1

  test "{makeFieldsLiteral} Nested case fields :macro:":
    type
      U = object
        case kind1: bool
          of true: f11: int
          of false:
            case kind2: char
              of 'a':
                f12: int
              else:
                f22: float

    let generated = U.makeFieldsLiteral()
    let expected = @[
      ValField(fldType: "bool", name: "kind1", isKind: true,
               isTuple: false, branches: @[
        ValFieldBranch(
          value: ValObjTree(
            kind: okConstant, constType: "bool", strLit: "true"),
          flds: @[ ValField(fldType: "int", isKind: false,
                            name: "f11", isTuple: false) ],
          isElse: false
         ),
        ValFieldBranch(
          value: ValObjTree(
            kind: okConstant, constType: "bool", strLit: "false"),
          flds: @[
            ValField(
              fldType: "char", name: "kind2", isKind: true,
              isTuple: false, branches: @[
              ValFieldBranch(
                value: ValObjTree(
                  kind: okConstant, constType: "char", strLit: "'a'"),
                flds: @[ ValField(
                  fldType: "int", isKind: false, name: "f12",
                  isTuple: false) ],
                isElse: false
              ),
              ValFieldBranch(
                value: ValObjTree(),
                flds: @[ ValField(
                  fldType: "float", isKind: false,
                  name: "f22", isTuple: false) ],
                isElse: true
              ),
            ])
          ],
         isElse: false
         ),
      ]),
    ]

    if generated != expected:
      raiseAssert "Fail"

  test "{makeFieldsLiteral} Get fields inside of generic proc :macro:":
    proc generic[T](a: T): void =
      let generated = T.makeFieldsLiteral()
      let expected = @[
        ValField(name: "f1", fldType: "int",
                 isKind: false, isTuple: false),
        ValField(name: "f2", fldType: "char",
                 isKind: false, isTuple: false)
      ]

      if generated != expected:
        # "/tmp/generated.nim".writeFile(pstring generated)
        # "/tmp/expected.nim".writeFile(pstring expected)
        raiseAssert "Fail"


    type
      U = object
        f1: int
        f2: char

    generic(U())


  test "{makeFieldsLiteral} Get all kind fields :macro:":
    type
      U = object
        case kind1: bool
          of true: f11: int
          of false:
            case kind2: char
              of 'a':
                f12: int
              else:
                f22: float

        case kind3: bool
          of true:
            f31: float
          of false:
            f32: seq[seq[seq[seq[seq[set[char]]]]]]


    let generated = makeFieldsLiteral(U).getKindFields()
    let expected = @[
      ValField(
        name: "kind1", fldType: "bool", isKind: true,
        isTuple: false, branches: @[
          ValFieldBranch(
            value: ValObjTree(
              kind: okConstant, constType: "bool", strLit: "false"),
            flds: @[
              ValField(name: "kind2", fldType: "char",
                       isKind: true, isTuple: false)
            ]
          )
        ]
      ),
      ValField(name: "kind3", fldType: "bool", isKind: true, isTuple: false)
    ]

    if generated != expected:
      # "/tmp/generated.nim".writeFile(pstring generated)
      # "/tmp/expected.nim".writeFile(pstring expected)
      # shell:
      #   cwdiff /tmp/expected.nim /tmp/generated.nim

      quit 1


  type
    UTop = object
      case kind: bool
        of true:
          f1: char
        of false:
          f2: string

      f3: int

  template checkDeclaredFields(): untyped =
    if name == "kind":
      assert (lhs is bool) and (rhs is bool)
      assert isKind
    elif name == "f1":
      assert (lhs is char) and (rhs is char)
      assert not isKind
    elif (name == "f2") or (name == "f3"):
      discard
    else:
      fail()

  test "{parallelFieldPairs} from object constructor :macro:":
    parallelFieldPairs(
      UTop(kind: true, f1: '1'),
      UTop(kind: true, f1: '1')
    ):
      checkDeclaredFields()

  test "{parallelFieldPairs} from variable :macro:":
    let v1 = UTop()
    let v2 = UTop()
    parallelFieldPairs(v1, v2):
      checkDeclaredFields()

  test "{parallelFieldPairs} inside generic function :macro:generic:":
    var found = (kind: false, f2: false, f3: false)
    proc generic[T](lhsIn, rhsIn: T): void =
      parallelFieldPairs(lhsIn, rhsIn):
        checkDeclaredFields()

        if name == "kind":
          found.kind = true
        elif name == "f2":
          found.f2 = true
        elif name == "f3":
          found.f3 = true

    generic(UTop(), UTop())
    assert found.kind
    assert found.f2
    assert found.f3

  test "{parallelFieldPairs} iterate field/call :macro:":
    block:
      proc makeObj(): UTop =
        discard

      parallelFieldPairs(makeObj(), makeObj()):
        assert lhsObj is UTop
        checkDeclaredFields()

    block:
      let val = (f: UTop())

      parallelFieldPairs(val.f, val.f):
        assert lhsObj is UTop
        checkDeclaredFields()

type
  Obj1 = object
    f1: int
    f2: seq[int]
    f3: Table[int, string]

var conf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("(", multiline = true),
               makeDelim(")", multiline = false)),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": "
  # wrapLargerThan: 10
)

template pstr(arg: untyped, ident: int = 0): untyped =
  var counter = makeCounter()
  toSimpleTree(arg, counter).prettyString(conf, ident)

suite "Simple configuration":
  test "integer":
    assertEq pstr(12), "12"

  test "indentation":
    assertEq pstr(12, 3), "   12"

  test "string":
    assertEq pstr("112"), "\"112\""

  test "Anonymous tuple":
    assertEq pstr((12, "sdf")), "(12, \"sdf\")"

  test "Named tuple":
    assertEq pstr((a: "12", b: "222")), "(a: \"12\", b: \"222\")"

  test "Narrow sequence":
    conf.maxWidth = 6
    assertEq @[1,2,3,4].pstr(), "- 1\n- 2\n- 3\n- 4"

  test "Wide sequence":
    conf.maxWidth = 80
    assertEq @[1,2,3].pstr(), "[1, 2, 3]"

  test "int-int table":
    assertEq {2: 3, 4: 5}.toOrderedTable().pstr(), "{2: 3, 4: 5}"

  test "Sequence of tuples":
    assertEq @[(1, 3), (4, 5)].pstr(), "[(1, 3), (4, 5)]"

  type
    T = object
      f1: int

  test "Simple object":
    assertEq T(f1: 12).pstr(), "T(f1: 12)"

  test "Sequence of objects":
    assertEq @[T(f1: 12), T(f1: -99)].pstr(), "[T(f1: 12), T(f1: -99)]"

  type
    C = object
      case kind: bool
      of true: f90: string
      of false: f09: (float, string)

  test "Case object":
    assertEq C(kind: true, f90: "12").pstr(), "C(kind: true, f90: \"12\")"
    assertEq C(kind: false, f09: (1.2, "12")).pstr(),
         "C(kind: false, f09: (1.2, \"12\"))"

suite "Deeply nested types":
  test "8D sequence":
    assertEq @[@[@[@[@[@[@[@[1]]]]]]]].pstr(),
         "[[[[[[[[1]]]]]]]]"

  test "4x4 seq":
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstr(), "[[1, 2, 3, 4], [5, 6, 7, 8], [9, 1, 2, 3], [4, 5, 6, 7]]"


  test "Narrow 4x4 seq":
    conf.maxWidth = 20
    assertEq @[
      @[1, 2, 3, 4],
      @[5, 6, 7, 8],
      @[9, 1, 2, 3],
      @[4, 5, 6, 7],
    ].pstr(),  """
      - [1, 2, 3, 4]
      - [5, 6, 7, 8]
      - [9, 1, 2, 3]
      - [4, 5, 6, 7]""".dedent
    conf.maxWidth = 80


  test "Super narrow 2x2 seq":
    conf.maxWidth = 7
    assertEq @[
      @[1, 2, 4],
      @[5, 6, 8],
    ].pstr(), """
      - - 1
        - 2
        - 4
      - - 5
        - 6
        - 8""".dedent

    conf.maxWidth = 80

import json

suite "Printout json as object":
  test "Json named tuple":
    let jsonNode = parseJson("""{"key": 3.14}""")
    assertEq jsonNode.pstr(), "(key: 3.14)"

  test "Json array":
    assertEq parseJson("""{"key": [1, 2, 3]}""").pstr(),
        "(key: [1, 2, 3])"

  test "Json nested array":
    assertEq parseJson("""{"key": [[1, 2, 3], [1, 2, 3]]}""").pstr(),
        "(key: [[1, 2, 3], [1, 2, 3]])"


var jsonConf = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("{"), makeDelim("}")),
  fldNameWrapper: (makeDelim("\""), makeDelim("\"")),
  fldSeparator: ",",
  kvSeparator: ": ",
  # wrapLargerThan: 10,
  alignFieldsRight: true
)

template pjson(arg: untyped): untyped =
  var counter = makeCounter()
  toSimpleTree(arg, counter).prettyString(jsonConf)

suite "Json pretty printing":
  test "Reparse int":
    let jsonNode = parseJson("""{"key": 3.14}""")
    let pretty = jsonNode.pjson()
    let reparsed = pretty.parseJson()
    assertEq jsonNode, reparsed

  test "Nested JSON":
    let jsonNode = parseJson """
       {
        "name":"John",
        "age":30,
        "cars": {
          "car1":"Ford",
          "car2":"BMW",
          "car3":"Fiat"
        }
       }""".dedent()

    let formatted = jsonNode.pjson()
    assertEq formatted, """
        {
          "name": "John",
           "age": 30,
          "cars": {"car1": "Ford", "car2": "BMW", "car3": "Fiat"}
        }""".dedent()

  test "Large JSON reparse":
      let jsonNode = parseJson """
{"menu": {
  "id": "file",
  "value": "File",
  "popup": {
    "menuitem": [
      {"value": "New", "onclick": "CreateNewDoc()"},
      {"value": "Open", "onclick": "OpenDoc()"},
      {"value": "Close", "onclick": "CloseDoc()"}
    ]
  }
}}        """

      assertEq jsonNode, jsonNode.pjson().parseJson()


var treeConf = PPrintConf(
  maxWidth: 40,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  # wrapLargerThan: 10,
  nowrapMultiline: true
)

template treeStr(arg: untyped): untyped =
  var counter = makeCounter()
  toSimpleTree(arg, counter).prettyString(treeConf)

import hmisc/algo/hpprint_repr
suite "Repr pprint":
  test "Lisp repr":
    assertEq pptConst("12").lispRepr(), "12"
    assertEq pptSeq(pptConst("12")).lispRepr(), "'(12)"
    assertEq pptObj("Hello", {"we" : pptConst("12")}).lispRepr(),
      "(Hello :we 12)"
    assertEq pptObj("Hello", pptConst("12")).lispRepr(),
      "(Hello 12)"

  test "Tree repr":
    assertEq pptConst("12").treeRepr(), "12"
    assertEq pptSeq(pptConst("12"), pptConst("123123")).treeRepr(),
      "+-- 12\n+-- 123123"

    let obj = pptObj("Parent object",
                pptConst("12"),
                pptConst("12"),
                pptSeq(pptConst "Hello", pptConst "1231"),
                pptSeq("item", pptConst "Hello", pptConst "1231"),
                pptMap(("int", "float"), {
                  "Hello" : pptConst("Workd"),
                  "Nice" : pptSeq(pptConst("123"), pptConst("q234"))
                }),
                pptObj("Object name",
                  pptConst("12"),
                  pptConst("12"),
                  pptConst("12")))

    discard obj.treeRepr(maxlevel = 1)
    discard obj.treeRepr()

suite "Large object printout":
  test "Large JSON as treeRepr":
    let jsonNode = parseJson """
      {"widget": {
          "debug": "on",
          "window": {
              "title": "Sample Konfabulator Widget",
              "name": "main_window",
              "width": 500,
              "height": 500
          },
          "image": {
              "src": "Images/Sun.png",
              "name": "sun1",
              "hOffset": 250,
              "vOffset": 250,
              "alignment": "center"
          },
          "text": {
              "data": "Click Here",
              "size": 36,
              "style": "bold",
              "name": "text1",
              "hOffset": 250,
              "vOffset": 100,
              "alignment": "center",
              "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
          }
      }}"""

    assertEq jsonNode.treeStr(), """
      widget:
        debug:  "on"
        window:
          title:  "Sample Konfabulator Widget"
          name:   "main_window"
          width:  500
          height: 500
        image:
          src:       "Images/Sun.png"
          name:      "sun1"
          hOffset:   250
          vOffset:   250
          alignment: "center"
        text:
          data:      "Click Here"
          size:      36
          style:     "bold"
          name:      "text1"
          hOffset:   250
          vOffset:   100
          alignment: "center"
          onMouseUp: "sun1.opacity = (sun1.opacity / 100) * 90;"""".dedent()


suite "Object tree to dot graph":
  # TODO generate html page with original object, it's pretty-printed
  # tree and resulting graph. It is not possible to adequately assert
  # generated. NOTE `unittest` has support for `setup` and `teardown`
  # sections: it can be used to generate report files.

  # TODO genrate images and html files only when
  # `-d:haxTestGenerateFiles` is enabled to avoid pollution of the
  # filesystem.
  template testgraph(obj: untyped): untyped =
    let graph = toDotGraph(obj)
    try:
      graph.topng("/tmp/image.png")
    except ShellExecError:
      let e = cast[ShellExecError](getCurrentException())
      let str = $graph
      echo "   ----   message:"
      echo e.msg
      echo "   ----   error str:"
      echo e.errstr
      echo "   ----   graph text:"
      echo str

  test "Integer":
    discard
    # testgraph(12)

  test "Integer sequence":
    discard
    # testgraph(@[12])


import hmisc/[objdiff]

suite "Object diff":
  test "{parallelFieldPairs} field indexing :macro:":
    type
      Case = object
        f1: int                # fldIdx - `0`, valIdx - `0`
        case kind: bool        # fldIdx - `1`, valIdx - `1`
          of true: f2: float   # fldIdx - `2`, valIdx - `2`
          of false: f3: string # fldIdx - `3`, valIdx - `2`

          # Fields `f2` and `f3` have the same `valIdx` because they
          # are located in different branches and cannot be accessed
          # simotaneously.

    let v = Case(kind: false, f3: "Hello")
    parallelFieldPairs(v, v):
      if name == "f3":
        assert fldIdx == 3
        assert valIdx == 2

  test "diff integers":
    assertEq diff(1, 2).paths(), @[@[0]]

  # NOTE test diff with string sequece too
  test "{diff} seq":
    assertEq diff(@[1], @[2]).paths(), @[@[0, 0]]
    assertEq diff(@[1, 1], @[2, 1]).paths(), @[@[0, 0]]
    assertEq diff(@[1, 2], @[2, 1]).paths(), @[@[0, 0], @[0, 1]]
    assertEq diff(@[1], @[1]).paths(), emptySeq[seq[int]]()
    assertEq diff(@["hel"], @["`1`"]).paths(), @[@[0, 0]]

  test "{diff} Object field difference":
    type
      U = object
        f1: int

    assertEq diff(U(f1: 90), U(f1: 91)).paths(), @[@[0, 0]]

  test "{diff} Case object difference":
    type
      U = object
        case kind: bool
          of true:
            f1: char
          of false:
            f2: string

    block:
      let res = diff(
        U(kind: true, f1: '1'),
        U(kind: true, f1: '9')
      )

      assertEq res.paths, @[@[0, 1]]

    block:
      let res = diff(
        U(kind: true, f1: '9'),
        U(kind: false, f2: "hello")
      )

      assertEq res.paths, @[@[0]]
      assertEq res[[0]].kind, odkKind
      # Not testig for different fields since they will not be
      # iterated (different kinds)

  type
    AstKind = enum
      akFloatLit
      akIntLit
      akStrLit

      akIdent

      akInfix
      akStmtList
      akTypeDef
      akCall

    Ast = object
      case kind: AstKind
        of akFloatLit:
          floatVal: float
        of akIntLit:
          intVal: int
        of akStrLit, akIdent:
          strVal: string
        of akStmtList, akTypeDef, akInfix, akCall:
          subnodes: seq[Ast]


  proc newTree(kind: AstKind, subn: varargs[Ast]): Ast =
    result = Ast(kind: kind)
    result.subnodes = toSeq(subn)

  proc newLit(a: float): Ast = Ast(kind: akFloatLit, floatVal: a)
  proc newLit(a: int): Ast = Ast(kind: akIntLit, intVal: a)
  proc newLit(a: string): Ast = Ast(kind: akStrLit, strVal: a)
  proc newIdent(a: string): Ast = Ast(kind: akIdent, strVal: a)
  proc newStmtList(args: varargs[Ast]): Ast =
    Ast(kind: akStmtList, subnodes: toSeq(args))

  test "{diff} Ast tree diff":
    let lhs = newStmtList(newLit("hello"), newLit(1.22))
    let rhs = newStmtList(newLit("hello"), newLit(1.23))
    ppDiff(lhs, rhs)

  test "{diff}":
    ppDiff({
      "A" : newStmtList(newLit("hello"), newLit(1.22))
    }, {
      "A" : newStmtList(newLit("hello"), newLit(1.23))
    })
