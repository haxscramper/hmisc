# {.define(plainStdout).}

import unittest, strutils, unicode, macros
import sugar, json, sequtils, tables, strformat, options, terminal

import hmisc/types/[hvariant, colorstring]
import hmisc/[helpers, hexceptions]
import hmisc/algo/[halgorithm, htree_mapping, hseqdistance]

type
  InTest = object
    val: int
    sub: seq[InTest]

  OutTest = object
    val: string
    sub: seq[OutTest]

let inval = InTest(val: 91, sub: @[
  InTest(val: 22, sub: @[InTest(val: 900)]),
  InTest(val: 90, sub: @[InTest(val: 0), InTest(val: 888)])]
)

let outval = OutTest(
    val: "91--2",
    sub: @[
      OutTest(val: "22--1", sub: @[OutTest(val: "900--0")]),
      OutTest(val: "90--2", sub: @[
        OutTest(val: "0--0"),
        OutTest(val: "888--0")])])

type
  Ast = object
    name: string
    case isToken: bool
      of true:
        tok: string
      of false:
        subnodes: seq[Ast]

let astInval = Ast(isToken: false, subnodes: @[
  Ast(isToken: false, subnodes: @[
    Ast(isToken: true, tok: "opBrace"),
    Ast(isToken: true, tok: "ident"),
    Ast(isToken: true, tok: "closeBrace"),
  ]),
  Ast(isToken: false, subnodes: @[
    Ast(isToken: true, tok: "opBrace"),
    Ast(isToken: true, tok: "ident"),
    Ast(isToken: true, tok: "closeBrace"),
  ])
])

suite "Tree mapping":
  ## Test tempates/procs/macros for working on trees.
  # Tests are a little verbose
  test "{mapItBFStoSeq} :template:value:":
    type
      Tree = ref object
        name: string
        subs: seq[Tree]

    var tree =
      Tree(
        name: "test",
        subs: @[
          Tree(name: "test11"),
          Tree(name: "test12")
      ])

    assert tree.mapItBFStoSeq(
      it.subs,
      it.name & " on level " & $lv) == @[
        "test on level 0",
        "test11 on level 1",
        "test12 on level 1"]

  test "{iterateItBFS}":
    astInval.iterateItBFS(it.subnodes, not it.isToken):
      if it.isToken:
        discard it.tok

  test "{iterateItDFS}":
    astInval.iterateItDFS(it.subnodes, not it.isToken):
      if it.isToken:
        discard it.tok

  test "{mapDFSpost} value assetions :proc:generic:value:example:":
    assert inval.mapDFSpost(
      map =
        proc(
          it: InTest,
          path: seq[int],
          subt: seq[OutTest],
          inSubt: seq[InTest]
        ): OutTest =
          OutTest(val: $it.val & "--" & $(subt.len()), sub: subt),
      getSubnodes =
        proc(it: InTest): seq[InTest] = it.sub
    ) == outval


  test "{mapDFSpost} drop filter tree :proc:value:example":
    ## Iterate over nodes of the tree and leave only ones that have
    ## `val` greater or equal than 90. Discard their children nodes.
    let res = mapDFSpost[InTest, InTest](
      tree = inval,
      map = proc(it: InTest, subn: seq[InTest]): auto =
                if it.val >= 90:
                  some(InTest(val: it.val, sub: subn))
                else:
                  none(InTest),
      getSubnodes = proc(it: InTest): seq[InTest] = it.sub
    )

    assert res is Option[InTest]
    assert res.get().mapItBFStoSeq(it.sub, it.val) == @[91, 90, 888]

  test "{mapItBFStoSeq} check missing subnodes :template:example:":
    ## Get subnodes only from nodes that have subnodes
    # echo astInval.mapItBFStoSeq(
    #   it.subnodes,
    #   it.val,
    # )

  test "{mapItBFStoSeq} filter json node :template:example:":
    ## Map json tree to sequence.
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
      }}
      """

    let values = jsonNode.mapItBFStoSeq(
      # Get only values from JObject
      if it.kind == JObject:
        collect(newSeq):
          for key, val in it.getFields():
            val
      else:
        it.getElems()
      ,
      # Get `onclick` fields from all objects
      if it.kind == JObject and "onclick" in it:
        some(it["onclick"].getStr())
      else:
        none(string)
      ,
      # Only try to get values from objects and arrays
      it.kind in {JObject, JArray}
    )

    assert values == @["CreateNewDoc()", "OpenDoc()", "CloseDoc()"]

  test "{mapBFStoSeq} nested grid":
    type
      Cell = object
        case isVal: bool
          of true:
            val: string
          of false:
            grid: Grid

      Grid = object
        elems: seq[seq[Cell]]

    let grid: Grid = Grid(
      elems: @[@[
        Cell(isVal: true, val: "1"),
        Cell(isVal: true, val: "2")
      ], @[
        Cell(isVal: true, val: "3"),
        Cell(isVal: false, grid: Grid(
          elems: @[@[
            Cell(isVal: true, val: "4.1"),
            Cell(isVal: true, val: "4.2")
          ], @[
            Cell(isVal: true, val: "4.3"),
            Cell(isVal: true, val: "4.4")
          ]]
        ))
      ]]
    )

    func concatIdx[T](s: seq[seq[T]]): seq[((int, int), T)] =
      for rId, row in s:
        for cId, col in row:
          result.add ((rId, cId), col)

    template mapItTuples[T, A](ins: seq[(T, A)], op: untyped): untyped =
      var res: seq[(T, typeof((var it {.inject.}: A; op)))]
      for s in ins:
        let it {.inject.}: A = s[1]
        res.add (s[0], op)

      res

    let res = mapItBFStoSeq(
      # Tree is not entirely homogenous, it is necessary to use
      # variant in order to be able to process it
      topNode = ((0, 0), toVar2[Cell, Grid](grid)),
      subNode = mapItTuples[(int, int), Cell](
        (it[1].idx == 0).tern(
          it[1].get(Cell).grid.elems.concatIdx(),
          it[1].get(Grid).elems.concatIdx()),
        toVar2[Cell, Grid](it)
      ),
      op = (
        block:
          # Export cells from grid.
          assert it is ((int, int), Var2[Cell, Grid])
          it
      ),
      hasSubnodes = (
        it[1].idx == 1 or # Either grid node or cell with nested grid
        (it[1].idx == 0 and not it[1].get(Cell).isVal)
      )
    )

    assert res is seq[((int, int), Var2[Cell, Grid])]
    assert res[0][1].hasType(Grid) # First element
    for it in res[1..^1]:
      let cell = it[1].get(Cell)

  test "{mapDFSpost} check missing subnodes :proc:generic:example":
    ## Check if tree instance can have subnodes before trying to
    ## iterate over it's children

    let res: Option[Ast] = astInval.mapDFSpost(
      map = proc(it: Ast, subn: seq[Ast]): Option[Ast] =
                if not it.isToken:
                  some(Ast(isToken: false, subnodes: subn))
                elif it.isToken and it.tok == "ident":
                  some(Ast(isToken: true, tok: "ident"))
                else:
                  none(Ast)
      ,
      # Field is accessible only for non-token ast nodes
      getSubnodes = proc(it: Ast): seq[Ast] = it.subnodes
      ,
      hasSubnodes = proc(it: Ast): bool = not it.isToken
    )

    assert res.get().mapItBFStoSeq(
      it.subnodes,
      if it.isToken: some(it.tok) else: none(string),
      not it.isToken) == @["ident", "ident"]

  test "{mapItDFS} map to dot grap :proc:macro:example:":
    ## Convert ast to linear structure (graphviz document). Fold
    ## tree-like type into dot graph with only ~20 lines of code
    ## (mostly comments and formatting)
    type
      T = object
        val: string
        sub: seq[T]

    let val = T(val: "hello", sub: @[T(val: "world"), T(val: "!")])
    let res = val.mapItDFS(
      it.sub,
      seq[string],
      block:
        # Get name of the current subnode
        let currName = it.val
        # Get id for current node
        let currId = "_" & path.mapIt($it).join("_")
        # Get all subnode ids
        let subnodes: seq[string] = collect(newSeq):
          for idx, _ in it.sub:
            "_" & (path & @[idx]).mapIt($it).join("_")

        let edgeCode =
          if subnodes.len > 0:
            &"{currId} -> {{{subnodes.join(',')}}};"
          else:
            ""

        static: # Each node was folded into sequence of strings - list
                # of nodes is passed to upper node.
          assert subt is seq[seq[string]]

        @[
          &"{currId} [label=\"{currName}\"];",
          edgeCode
        ] & subt.concat()
    )

    let inner = res.filterIt(it.len > 0).mapIt("  " & it).join('\n')
    let final = &"digraph G {{\n{inner}\n}}"
    assertEq final, """
      digraph G {
        _0 [label="hello"];
        _0 -> {_0_0,_0_1};
        _0_0 [label="world"];
        _0_1 [label="!"];
      }""".dedent()

  # TODO macro type assertions for option

  test "{mapItDFS} type assertions :macro:type:example:":
    let res = InTest().mapItDFS(
      it.sub, OutTest,
      OutTest(val: $it.val & "+"))

    assert res is OutTest

  test "{mapItDFS} get subnodes using proc call :macro:value:":
    proc getSub(n: InTest): seq[InTest] = n.sub
    discard inval.mapItDFS(it.getSub(), string, "")

  test "{mapItDFS} value assertions :macro:value:":
    assert inval.mapItDFS(
      it.sub, OutTest,
      block:
        OutTest(
          val: $it.val & "--" & $(subt.len()),
          sub: subt
        )) == outval

import strutils

suite "Simple sequence templates":
  test "{maxIt}":
    assert @[1,2,3,4].maxIt(it) == 4
    var empty: seq[int]
    assert empty.maxIt(it) == 0

  test "{allOfIt} empty sequence :template:":
    var empty: seq[int]
    assert empty.allOfIt(false)

  test "{allOfIt} use example :template:example:":
    assert @[1, 2, 3].allOfIt(it > 0)

  test "{mapPairs} type assertion :template:type:":
    assert {1: "hello", 2: "222"}.mapPairs(
      $lhs & "--" & rhs
    ) is seq[string]

  test "{mapPairs} return value :template:value:":
    assert {1: 2, 3: 4}.mapPairs(
      lhs + rhs
    ) == @[3, 7]

  test "{mapPairs} map table values :template:example:":
    assert {1: 3, 4: 5}.toTable().mapPairs(
      max(lhs, rhs)
    ) ==  @[3, 5]

  test "{mapPairs} iterate indexed :template:example:":
    let res = @["a", "b"].mapPairs(
      block:
        assert lhs is int
        assert rhs is string
        $lhs & ":" & rhs
    )

    assert res is seq[string]
    assert res == @["0:a", "1:b"]

  test "{mapPairs} custom `pairs` :template:":
    type U = object

    iterator pairs(u: U): (float, string) =
      yield (1.2, "1222")

    # ignored by `mapPairs`
    iterator items(u: U): string =
      yield "222"

    let res = U().mapPairs($lhs & " () " & rhs)
    assert res is seq[string]
    assert res == @["1.2 () 1222"]

  test "{mapPairs} custom `items` :template:":
    type U = object

    # No pairs declared - using `items` with index as `lhs`
    iterator items(u: U): string =
      yield "222"
      yield "aaa"

    let res = U().mapPairs($lhs & " () " & rhs)
    assert res is seq[string]
    assert res == @["0 () 222", "1 () aaa"]

  test "{mapPairs} Element index injection :value:template:":
    assertEq [1, 2, 3].mapPairs((val: rhs, id: idx)),
      @[(val: 1, id: 0), (val: 2, id: 1), (val: 3, id: 2)]

  test "{mapPairs} Custom injected name :macro:value:":
    ## Add column and row index to each value in matrix
    assertEq @[
      @[1, 3],
      @[4, 5]
    ].mapPairs(rhs.mapPairs(
      col + row + val,
      (idx: row, rhs: val)
    ), (idx: col)),
      @[
        @[1, 4],
        @[5, 7]
      ]

  test "{mapPairs} Print 2d array :macro:example:":
    ## Absolutely useless example - use for loop instead.
    let mem = @[
      @[1, 3, 4, 5],
      @[4, 5, 6, 7]
    ]

    let res = (
      @["[   " & toSeq(mem[0].mapPairs($lhs)).join(" ")] &
        toSeq(
          mem.mapPairs(
            $i & " | " &
              rhs.mapPairs(mem[i][j], (idx: j)).join(" "), (idx: i)
          )
        )
    ).join("\n")

    assertEq res, """
    [   0 1 2 3
    0 | 1 3 4 5
    1 | 4 5 6 7""".dedent


  test "{mapPairs} Rezip-compare sequence of tuples :template:":
    block: # Two sequence of identical types
      var seq1 = @[(f1: 12, f2: "22"), (f1: 2, f2: "22")]
      var seq2 = @[(f1: 22, f2: "22"), (f1: 2, f2: "20")]
      assertEq zip(seq1, seq2).mapPairs(&"{lhs.f1}+{rhs.f1}={lhs.f2}/{rhs.f2}"),
          @["12+22=22/22", "2+2=22/20"]

    block: # Two sequence of different types
      var seq1 = @[(f1: 12, f2: "22"), (f1: 2, f2: "22")]
      var seq2 = @[(f3: 22, f4: "22"), (f3: 2, f4: "20")]
      assertEq zip(seq1, seq2).mapPairs(&"{lhs.f1}+{rhs.f3}={lhs.f2}/{rhs.f4}"),
          @["12+22=22/22", "2+2=22/20"]

    block: # Get first mismatch for two sequences
      let res = zip(
        @[(1, 2), (3, 4), (3, 4), (3, 9)],
        @[(1, 2), (3, 7), (3, 4), (3, 4)]
      ).mapPairs(
        (id: idx, ok: (lhs[0] == rhs[0]) and (lhs[1] == rhs[1]))
      ).foldl((not a.ok).tern(a, b))
      assertEq res, (id: 1, ok: false)


  block: # Get first mismatch for two sequences
    assert true and zip(
      @[(1, 2)],
      @[(1, 2)]
    ).mapPairs(
      (lhs[0] == rhs[0]) and (lhs[1] == rhs[1])
    ).foldl(a and b)

  test "{subnodesEq} :template:":
    type
      U = object
        s: seq[U]

    assert subnodesEq(U(), U(), s)
    assert subnodesEq(U(s: @[U(), U()]), U(s: @[U(), U()]), s)
    assert not subnodesEq(
      U(s: @[U(), U(), U()]), U(s: @[U(), U()]), s)

  test "{findItFirst} :template:example:":
    assert @["A", "B", "D"].findItFirst(it == "A") == "A"

    expect AssertionError:
      discard @["A", "B"].findItFirst(it == "D")

  test "{findItFirstOpt} :template:example:":
    assert @["A"].findItFirstOpt(it == "A").isSome()
    assert @["A"].findItFirstOpt(it == "D").isNone()

  test "{findIt} :template:":
    assert @[1].findIt(it == 1) == 0
    assert @[2].findIt(it == 1) == -1

  test "{max} :proc:generic:":
    assert @[1, 2, 3].max(90) == 3
    var tmp: seq[int]
    assert tmp.max(80) == 80

  test "{anyOfIt} :template:":
    assert [1, 2, 3].anyOfIt(it > 1)
    assert not [3, 4, 5].anyOfIt(it < 1)
    var tmp: seq[int]
    assert not tmp.anyOfIt(it > 9)

  test "{deduplicateByIt} deduplicate ingeters :template:":
    let s = @[(1, 2), (1, 3), (1, 2)]
    let dedupl = s.deduplicateIt(it[0])
    assertEq dedupl, @[(1, 2)]

  test "{deduplicateByIt} deduplicate without sorting :template:":
    type
      T = object
        f1: int
      U = object
        f1: T
        f2: float

    let s = @[U(), U(f1: T(f1: 1)), U(f2: 3.9)]
    let dedupl = s.deduplicateIt(it.f1)
    assertEq dedupl, @[U(), U(f1: T(f1: 1))]

import math

suite "Misc algorithms":
  test "{longestCommonSubsequence} :generic:value:":
    template tmp(s1, s2, s3: untyped): untyped =
      assertEq longestCommonSubsequence(s1, s2)[0], s3

    assert longestCommonSubsequence(@[1], @[2, 3]).len == 0
    assert longestCommonSubsequence(@["Cond"], @["Int", "Lit"]).len == 0
    assert longestCommonSubsequence(@[1], @[2]).len == 0
    tmp("GAC", "AGCAT", "GA")
    tmp(@[1, 2], @[1, 2], @[1, 2])
    tmp(@[1, 2, 3], @[1, 2], @[1, 2])
    tmp("XMJYAUZ", "MZJAWXU", "MJAU")
    tmp("AABC", "BC", "BC")
    tmp("AC", "ABC", "AC")
    tmp("AB", "A", "A")


  # if true: quit 0
  test "{fuzzyMatch} fuzzy string matching":
    template test(
      patt, input: string, expr: untyped, expected: seq[int]): untyped =
      let res = fuzzyMatch(
        patt, input,
        proc(p, o: string, m: seq[int]): int =
          let p {.inject.} = p
          let o {.inject.} = o
          let m {.inject.} = m
          expr
      )

      assert res.ok
      assertEq res.matches, expected

      if res.matches != expected:
        echo input
        var buf = " ".repeat(input.len)
        for pattIdx, inIdx in res.matches:
          buf[inIdx] = patt[pattIdx]

        echo buf

        buf = " ".repeat(input.len)
        for pattIdx, inIdx in expected:
          buf[inIdx] = patt[pattIdx]

        echo buf

    test("123", "010233", m.sum() * 7, @[1, 3, 5])
    test("123", "01122330", 1, @[1, 3, 5])

  test "Colored string wrapping":
    assertEq "999".toRed(), "\e[31m999\e[39m"
    assertEq "999".toDefault(), "999"

  test "{termLen}":
    assertEq termLen("hhh"), 3
    assertEq termLen("\e[41mhhh\e[49m"), 3
    assertEq termLen("ᛀᛀᛀᛀ"), 4
    assertEq termLen("\e[42mᛀᛀ\e[49m\e[44mᛀᛀ\e[49m"), 4

  test "{splitSGR}":
    func lispRepr(strs: seq[ColoredString]): string =
      strs.mapIt(it.lispRepr()).join(" ").wrap("()")

    # TEST TODO test multiple consecutive control codes
    assertEq "hello \e[31mworld\e[39m".splitSGR(), @[
      "hello ".initColoredString(),
      "world".initColoredString(fg = fgRed)
    ]

    assertEq "--\e[31mAA\e[39m--".splitSGR(), @[
      "--".initColoredString(),
      "AA".initColoredString(fg = fgRed),
      "--".initColoredString()
    ]


    assertEq "\e[92m000\e[39m-\e[94m000\e[39m".splitSGR(), @[
      initColoredString("000", fg = fgGreen, style = {styleBright}),
      initColoredString("-"),
      initColoredString("000", fg = fgBlue, style = {styleBright})
    ]

    assertEq "###---\e[31m\e[44m\e[4m\e[3m###\e[23m\e[24m\e[49m\e[39m".
      splitSGR(), @[
        initColoredString("###---"),
        initColoredString(
          "###", fg = fgRed, bg = bgBlue, style = {
            styleItalic, styleUnderscore})
      ]

  test "{split} ColorString":
    assertEq "Hello-World".toRed().splitSGR()[0].split("-"), @[
      initColoredString("Hello", fg = fgRed),
      initColoredString("World", fg = fgRed)
    ]

    assertEq initColoredString("--").split("-").mapIt(it.str),
         "--".split("-")


    # echo initColoredString("\n\n\n").split("\n")

  test "{splitColor}":
    assertEq "\e[31mHello\nworld\e[39m".splitColor("\n"), @[
      "Hello".toRed(),
      "world".toRed()
    ]

    assertEq "".split("\n")[0], ""
    assertEq "".splitColor("\n")[0], ""

  test "{toString} colored runes":
    assertEq @[
      initColoredRune(uc"¤", initPrintStyling(fg = fgRed)),
      initColoredRune(uc"¤", initPrintStyling(bg = bgGreen, fg = fgRed)),
    ].toString(), "\e[31m¤\e[42m¤\e[39m\e[49m"

  test "{splitSGR_sep}":
    # echo "==".splitSGR_sep("=")
    # echo "==".splitSGR().mapIt(it.split("="))
    assertEq "*=*=*".splitSGR_sep("=").mapIt(it[0]),
      initColoredString("*=*=*").split("=")

    assertEq "hello\n\e[31msfadf\e[39m\nsf".splitSGR_sep("\n"), @[
      @[ initColoredString("hello") ],
      @[ initColoredString("sfadf", fg = fgRed) ],
      @[ initColoredString("sf") ],
    ]

    assertEq "\e[43mhello\e[49m\n-\n\e[41mworld\e[49m".splitSGR_sep(), @[
      @[ initColoredString("hello", bg = bgYellow) ],
      @[ initColoredString("-") ],
      @[ initColoredString("world", bg = bgRed) ]
    ]

    assertEq "\n\nee\n".splitSGR_sep(), @[
      @[ initColoredString("") ],
      @[ initColoredString("") ],
      @[ initColoredString("ee") ],
      @[ initColoredString("") ]
    ]
    # echo "\e[41m*=========\e[49m  eee  \e[41m==========*\e[49m"
    assertEq "-\e[31m--\n--\e[39m-".splitSGR(), @[
      initColoredString("-"),
      initColoredString("--\n--", fg = fgRed),
      initColoredString("-")
    ]

    assertEq "-\e[31m--\e[39m\n\e[31m--\e[39m-".splitSGR(), @[
      initColoredString("-"),
      initColoredString("--", fg = fgRed),
      initColoredString("\n"),
      initColoredString("--", fg = fgRed),
      initColoredString("-")
    ]

    assertEq "-\e[31m--\n--\e[39m-".splitSGR_sep(), @[
      @[ initColoredString("-"), initColoredString("--", fg = fgRed) ],
      @[ initColoredString("--", fg = fgRed), initColoredString("-") ]
    ]

    assertEq "-\e[31m--\e[39m\n\e[31m--\e[39m-".splitSGR_sep(), @[
      @[ initColoredString("-"), initColoredString("--", fg = fgRed) ],
      @[ initColoredString("--", fg = fgRed), initColoredString("-") ]
    ]


  test "{splitSGR_sep} to runes":
    assertEq "-\e[31m$\n$\e[39m-".splitSGR_sep().toRuneGrid(), @[
      @[ initColoredRune(Rune('-')),
         initColoredRune(Rune('$'), initPrintStyling(fg = fgRed))],
      @[ initColoredRune(Rune('$'), initPrintStyling(fg = fgRed)),
         initColoredRune(Rune('-'))]
    ]

  test "{exception printout}":
    let iinfo = instantiationInfo()
    let err = CodeError(
      msg: "Toplevel \e[31mannotation\e[39m ",
      annots: @[
      ErrorAnnotation(
        errpos: LineInfo(
          line: iinfo.line,
          column: iinfo.column,
          filename: currentSourcePath()
        ),
        annotation: "Hell \e[32masdfas\e[39md o",
        linerange: -2,
        expr: "errpos: currLineInf()"
      )
    ])

    echo err.toColorString()

  test "{splitCamel}":
    assertEq "HelloWorld".splitCamel, @["Hello", "World"]
    assertEq "HHeelloWWorld".splitCamel, @["H", "Heello", "W", "World"]
    assertEq "helloWorld".splitCamel, @["hello", "World"]
    assertEq "H".splitCamel, @["H"]
    assertEq "".splitCamel, `@`[string]([])
    assertEq "clang_Hello".splitCamel, @["clang", "Hello"]
    assertEq "clang_HeeH".splitCamel(false), @[
      "clang", "_", "Hee", "H"]

  test "{abbrevCamel}":
    assertEq abbrevCamel(
      "IfList", @["IfStmtList", "IfBlockList", "IfHello"]), @[
        "IfStmtList", "IfBlockList"
    ]

    assertEq do:
      abbrevCamel(
        "Cond", @["ConditionExpr", "ConditionStmt", "Block"])
    do:
      @["ConditionExpr", "ConditionStmt"]

    assertEq do:
      abbrevCamel(@["Cond"],
        @[
          @["Str", "Lit"],
          @["Int", "Lit"],
          @["Ident"],
          @["Call"],
          @["Condition"]
        ])
    do:
      @[ "Condition" ]

    assertEq abbrevCamel("AA", @["ABA", "AZZ", "A)"]), @["ABA"]

  test "{dropPrefix}":
    assertEq "???##".dropPrefix("???"), "##"
    assertEq "".dropPrefix("888"), ""
    assertEq "--".dropPrefix("-"), "-"

  test "{commonPrefix}":
    assertEq @[].commonPrefix(), ""
    assertEq @["00"].commonPrefix(), "00"
    assertEq @["--+", "--="].commonPrefix(), "--"
    assertEq @["+=", "=+"].commonPrefix(), ""

  test "{dropCommonPrefix}":
    assertEq @["--"].dropCommonPrefix(), @[""]
    assertEq @["a@", "a$"].dropCommonPrefix(), @["@", "$"]
    assertEq @["---"].dropCommonPrefix(false), @["---"]

  test "{dropSubseq}":
    assertEq @["C", "X", "X", "E"].dropSubseq(@["C", "X", "X"]), @["E"]
    assertEq @["C", "X"].dropSubseq(@[]), @["C", "X"]
    assertEq emptySeq[string]().dropSubseq(@["E"]), emptySeq[string]()
    assertEq "eCXXE".splitCamel().dropSubseq(@["C", "X", "X"]), @["e", "E"]
    assertEq "Eeeee".dropSubstr("eee"), "Ee"
    assertEq dropSubstr("--+==", "-+="), "-="
    assertEq dropLongestSubseq(
      "CXIdxEntityCXXTemplateKind", @["CX", "CXX"]),
      "CXIdxEntityTemplateKind"

