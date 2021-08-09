# {.define(plainStdout).}

import std/[
  strutils, unicode, macros, sugar, json, sequtils,
  tables, strformat, options, terminal
]

import
  hmisc/core/all,
  hmisc/preludes/unittest,
  hmisc/types/[hvariant, colorstring],
  hmisc/algo/[
    halgorithm, htree_mapping, hseq_distance,
    clformat, namegen, hseq_mapping
  ]

startHax()

configureDefaultTestContext(
  skipAfterException = true
)


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

    doAssert tree.mapItBFStoSeq(
      it.subs,
      it.name & " on level " & $lv) == @[
        "test on level 0",
        "test11 on level 1",
        "test12 on level 1"]

  test "{iterateItBFS}":
    astInval.iterateItBFS(it.subnodes, not it.isToken):
      if it.isToken:
        discard it.tok

  test "{iterateItBFS} int-indexed":
    func len(tr: Ast): int =
      if not tr.isToken:
        result = tr.subnodes.len

    func `[]`(tr: Ast, idx: int): Ast = tr.subnodes[idx]

    astInval.iterateItBFS:
      if it.isToken:
        discard it.tok

  test "{iterateItBFS} using `items` iterator":
    iterator items(tr: Ast): Ast =
      if not tr.isToken:
        for it in tr.subnodes:
          yield it

    astInval.iterateItBFS():
      if it.isToken:
        discard it.tok

  test "{iterateItDFS}":
    astInval.iterateItDFS(it.subnodes, not it.isToken, dfsPreorder):
      if it.isToken:
        discard it.tok

  test "{iterateItDFS} int-indexed":
    func len(tr: Ast): int =
      if not tr.isToken:
        result = tr.subnodes.len

    func `[]`(tr: Ast, idx: int): Ast = tr.subnodes[idx]

    astInval.iterateItDFS(dfsPreorder):
      if it.isToken:
        discard it.tok


  test "{itearteItDFS} using `items` iterator":
    iterator items(tr: Ast): Ast =
      if not tr.isToken:
        for it in tr.subnodes:
          yield it

    astInval.iterateItDFS(dfsPreorder):
      if it.isToken:
        discard it.tok

  test "{iterateItDFS} ordering schemes":
    type
      Tr = object
        sub: seq[Tr]
        ino: int
        pos: int
        pre: int

    let ast = Tr(
               ino: 3, pre: 0, pos: 4,
      sub: @[
        Tr(
               ino: 1, pre: 1, pos: 2,
          sub: @[
            Tr(ino: 0, pre: 2, pos: 0),
            Tr(ino: 2, pre: 3, pos: 1)
          ]
        ),
        Tr(    ino: 4, pre: 4, pos: 3)
      ]
    )

    block:
      var postordCnt = 0
      ast.iterateItDfs(it.sub, true, dfsPostorder):
        check it.pos == postordCnt
        inc postordCnt

    block:
      var preordCnt = 0
      ast.iterateItDFS(it.sub, true, dfsPreorder):
        check it.pre == preordCnt
        inc preordCnt

    block:
      var inordCnt = 0
      ast.iterateItDFS(it.sub, true, dfsInorder):
        check it.ino == inordCnt
        inc inordCnt


  test "{mapDFSpost} value assetions :proc:generic:value:example:":
    doAssert inval.mapDFSpost(
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

    doAssert res is Option[InTest]
    doAssert res.get().mapItBFStoSeq(it.sub, it.val) == @[91, 90, 888]

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

    doAssert values == @["CreateNewDoc()", "OpenDoc()", "CloseDoc()"]

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
          doAssert it is ((int, int), Var2[Cell, Grid])
          it
      ),
      hasSubnodes = (
        it[1].idx == 1 or # Either grid node or cell with nested grid
        (it[1].idx == 0 and not it[1].get(Cell).isVal)
      )
    )

    doAssert res is seq[((int, int), Var2[Cell, Grid])]
    doAssert res[0][1].hasType(Grid) # First element
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

    doAssert res.get().mapItBFStoSeq(
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
    let res = val.mapItDFS(it.sub, seq[string], true) do:
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
        doAssert subt is seq[seq[string]]

      @[
        &"{currId} [label=\"{currName}\"];",
        edgeCode
      ] & subt.concat()

    let inner = res.filterIt(it.len > 0).mapIt("  " & it).join('\n')
    let final = &"digraph G {{\n{inner}\n}}"
    check final == """
      digraph G {
        _0 [label="hello"];
        _0 -> {_0_0,_0_1};
        _0_0 [label="world"];
        _0_1 [label="!"];
      }""".dedent()

  # TODO macro type assertions for option

  test "{mapItDFS} type assertions :macro:type:example:":
    let res = InTest().mapItDFS(it.sub, OutTest, true) do:
      OutTest(val: $it.val & "+")

    doAssert res is OutTest

  test "{mapItDFS} get subnodes using proc call :macro:value:":
    proc getSub(n: InTest): seq[InTest] = n.sub
    discard inval.mapItDFS(it.getSub(), string, true, "")

  test "{mapItDFS} value assertions :macro:value:":
    doAssert inval.mapItDFS(
      it.sub, OutTest, true,
      block:
        OutTest(
          val: $it.val & "--" & $(subt.len()),
          sub: subt
        )) == outval

suite "Simple sequence templates":
  test "{foldlTuple}":
    check @[(0, 2)].foldlTuple(a + b) == (0, @[2])
    check @[(1, 2), (3, 4)].foldlTuple(a + b) == (4, @[2, 4])

  test "{maxIt}":
    check @[1,2,3,4].maxIt(it) == 4
    var empty: seq[int]
    check empty.maxIt(it) == low(int)

  test "{allOfIt} empty sequence :template:":
    var empty: seq[int]
    check empty.allOfIt(false)

  test "{allOfIt} use example :template:example:":
    check @[1, 2, 3].allOfIt(it > 0)

  test "{groupByIt}":
    check @[1, 2, 1].groupByIt(it) == @[@[1, 1], @[2]]
    check @[1].groupByIt(it) == @[@[1]]
    check @[1, 1, 1].groupByIt(it) == @[@[1, 1, 1]]

    check @[(1, 2), (1, 3)].groupByIt(it[0]) == @[@[(1, 2), (1, 3)]]

  test "{mapPairs} type assertion :template:type:":
    check {1: "hello", 2: "222"}.mapPairs(
      $lhs & "--" & rhs
    ) is seq[string]

  test "{mapPairs} return value :template:value:":
    check {1: 2, 3: 4}.mapPairs(
      lhs + rhs
    ) == @[3, 7]

  test "{mapPairs} map table values :template:example:":
    let val = {1: 3, 4: 5}.toTable().mapPairs(max(lhs, rhs))
    check val.sorted() == @[3, 5]

  test "{mapPairs} iterate indexed :template:example:":
    let res = @["a", "b"].mapPairs(
      block:
        check lhs is int
        check rhs is string
        $lhs & ":" & rhs
    )

    check res is seq[string]
    check res == @["0:a", "1:b"]

  test "{mapPairs} custom `pairs` :template:":
    type U = object

    iterator pairs(u: U): (float, string) =
      yield (1.2, "1222")

    # ignored by `mapPairs`
    iterator items(u: U): string =
      yield "222"

    let res = U().mapPairs($lhs & " () " & rhs)
    check res is seq[string]
    check res == @["1.2 () 1222"]

  test "{mapPairs} custom `items` :template:":
    type U = object

    # No pairs declared - using `items` with index as `lhs`
    iterator items(u: U): string =
      yield "222"
      yield "aaa"

    let res = U().mapPairs($lhs & " () " & rhs)
    check res is seq[string]
    check res == @["0 () 222", "1 () aaa"]

  test "{mapPairs} Element index injection :value:template:":
    check [1, 2, 3].mapPairs((val: rhs, id: idx)) ==
      @[(val: 1, id: 0), (val: 2, id: 1), (val: 3, id: 2)]

  test "{mapPairs} Custom injected name :macro:value:":
    ## Add column and row index to each value in matrix
    check mapPairs(
      @[@[1, 3], @[4, 5]], rhs.mapPairs(
        col + row + val, (idx: row, rhs: val)), (idx: col)) ==
    @[@[1, 4], @[5, 7]]

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

    check res == """
    [   0 1 2 3
    0 | 1 3 4 5
    1 | 4 5 6 7""".dedent


  test "{mapPairs} Rezip-compare sequence of tuples :template:":
    block: # Two sequence of identical types
      var seq1 = @[(f1: 12, f2: "22"), (f1: 2, f2: "22")]
      var seq2 = @[(f1: 22, f2: "22"), (f1: 2, f2: "20")]
      check zip(seq1, seq2).mapPairs(&"{lhs.f1}+{rhs.f1}={lhs.f2}/{rhs.f2}") ==
          @["12+22=22/22", "2+2=22/20"]

    block: # Two sequence of different types
      var seq1 = @[(f1: 12, f2: "22"), (f1: 2, f2: "22")]
      var seq2 = @[(f3: 22, f4: "22"), (f3: 2, f4: "20")]
      check zip(seq1, seq2).mapPairs(&"{lhs.f1}+{rhs.f3}={lhs.f2}/{rhs.f4}") ==
          @["12+22=22/22", "2+2=22/20"]

    block: # Get first mismatch for two sequences
      let res = zip(
        @[(1, 2), (3, 4), (3, 4), (3, 9)],
        @[(1, 2), (3, 7), (3, 4), (3, 4)]
      ).mapPairs(
        (id: idx, ok: (lhs[0] == rhs[0]) and (lhs[1] == rhs[1]))
      ).foldl((not a.ok).tern(a, b))
      check res == (id: 1, ok: false)


  block: # Get first mismatch for two sequences
    check true and zip(
      @[(1, 2)],
      @[(1, 2)]
    ).mapPairs(
      (lhs[0] == rhs[0]) and (lhs[1] == rhs[1])
    ).foldl(a and b)

  test "{subnodesEq} :template:":
    type
      U = object
        s: seq[U]

    check subnodesEq(U(), U(), s)
    check subnodesEq(U(s: @[U(), U()]), U(s: @[U(), U()]), s)
    check not subnodesEq(
      U(s: @[U(), U(), U()]), U(s: @[U(), U()]), s)

  test "{findItFirst} :template:example:":
    check @["A", "B", "D"].findItFirst(it == "A") == "A"

    expect ArgumentError:
      discard @["A", "B"].findItFirst(it == "D")

  test "{findItFirstOpt} :template:example:":
    check @["A"].findItFirstOpt(it == "A").isSome()
    check @["A"].findItFirstOpt(it == "D").isNone()

  test "{findIt} :template:":
    check @[1].findIt(it == 1) == 0
    check @[2].findIt(it == 1) == -1

  test "{max} :proc:generic:":
    check @[1, 2, 3].max(90) == 3
    var tmp: seq[int]
    check tmp.max(80) == 80

  test "{anyOfIt} :template:":
    check [1, 2, 3].anyOfIt(it > 1)
    check not [3, 4, 5].anyOfIt(it < 1)
    var tmp: seq[int]
    check not tmp.anyOfIt(it > 9)

  test "{deduplicateByIt} deduplicate ingeters :template:":
    let s = @[(1, 2), (1, 3), (1, 2)]
    let dedupl = s.deduplicateIt(it[0])
    check dedupl == @[(1, 2)]

  test "{deduplicateByIt} deduplicate without sorting :template:":
    type
      T = object
        f1: int
      U = object
        f1: T
        f2: float

    let s = @[U(), U(f1: T(f1: 1)), U(f2: 3.9)]
    let dedupl = s.deduplicateIt(it.f1)
    check dedupl == @[U(), U(f1: T(f1: 1))]

import math


suite "String helper functions":
  test "Namecache":
    var cache: StringNameCache

    check cache.getName("ROFF_bp") == "ROFF_bp"
    check cache.getName("ROFF_bp") == "ROFF_bp"
    check cache.getName("ROFF_BP") == "ROFF_BP1"

  test "toLatin*Char":
    for ch in "[({<>|!@#$%})]":
      echo &"{ch} : {ch.toLatinAbbrChar()}"

    echo "((()))".toNamedMultichar()

  test "Named multichar join":
    check toNamedMulticharJoin(".") == "dot"
    check toNamedMulticharJoin("..") == "doubleDot"

  test "{[^]}":
    check "hello"[^"lo"]
    check "hello"["he"]
    check "hello"[{'h'}, {'o'}]
    check "hello"[{'h'}, "lo"]
    check "hello"['h', "lo"]
    check "hello"['h', ["lo", "le"]]

  test "{msgjoin}":
    check msgjoin("_", "nice", "_") == "_nice_"

    check msgjoin("a", "b") == "a b"
    check msgjoin("[hello,", "nice", "weather]") == "[hello, nice weather]"

    check msgjoin("e", "/e") == "e /e"
    check msgjoin("e", "__e") == "e __e"
    check msgjoin("_a_") == "_a_"
    check msgjoin("_a", "_") == "_a_"
    check msgjoin("_", "a", "_") == "_a_"
    check msgjoin("file", "//ee") == "file //ee"
    check msgjoin("eee", "___eee") == "eee ___eee"
    check msgjoin("22", "22") == "22 22"
    check msgjoin("Found include", "cursor") == "Found include cursor"
    check msgjoin("1", "->", "2") == "1 -> 2"
    check msgjoin("/tt", "->", "/ee") == "/tt -> /ee"
    # check msgjoin("_", "333", "3", "_"), "_3333_"
    # check msgjoin("hello", "_", "a", "_"), "hello _a_"
    # check msgjoin("hello", "_", "a", "_", "world"), "hello _a_ world"
    # check msgjoin("hello", "_", "a", "2", "_", "world"),
    #  "hello _a 2_ world"

  test "{splitCamel}":
    check "FILE_AAName".splitCamel() == @["FILE", "AA", "Name"]
    check "HelloWorld".splitCamel == @["Hello", "World"]
    check "HHeelloWWorld".splitCamel(mergeCapitalized = false) == @["H", "Heello", "W", "World"]
    check "FILE".splitCamel() == @["FILE"]
    check "DBManager". splitCamel() == @["DB", "Manager"]
    check "DBManager". splitCamel(adaptiveMerge = false) == @["DBManager"]
    check "helloWorld".splitCamel == @["hello", "World"]
    check "H".splitCamel == @["H"]
    check "".splitCamel == `@`[string]([])
    check "clang_Hello".splitCamel == @["clang", "Hello"]
    check "clang_HeeH".splitCamel(false) == @["clang", "_", "Hee", "H"]
    check "hello___nice".splitCamel() == @["hello", "nice"]
    check "hello_nice".splitCamel() == @["hello", "nice"]

  test "{abbrevCamel}":
    check abbrevCamel(
      "IfList", @["IfStmtList", "IfBlockList", "IfHello"]) == @[
        "IfStmtList", "IfBlockList"]

    check abbrevCamel("Cond", @["ConditionExpr", "ConditionStmt", "Block"]) ==
      @["ConditionExpr", "ConditionStmt"]

    check abbrevCamel(@["Cond"], @[
        @["Str", "Lit"],
        @["Int", "Lit"],
        @["Ident"],
        @["Call"],
        @["Condition"]
      ]) == @[ "Condition" ]

  test "{dropPrefix}":
    check "???##".dropPrefix("???") == "##"
    check "".dropPrefix("888") == ""
    check "--".dropPrefix("-") == "-"
    check "90".dropPrefix({'1', '9'}) == "0"

  test "{commonPrefix}":
    check @["00"].commonPrefix() == "00"
    check @["--+", "--="].commonPrefix() == "--"
    check @["+=", "=+"].commonPrefix() == ""

  test "{commonPrefix[T]}":
    check commonPrefix(@[@["A", "B"], @["A", "C"]]) == @["A"]

  test "{dropCommonPrefix}":
    check @["--"].dropCommonPrefix() == @[""]
    check @["a@", "a$"].dropCommonPrefix() == @["@", "$"]
    check @["---"].dropCommonPrefix(false) == @["---"]
    check @[].dropCommonPrefix() == emptySeq[string]()


    check @["--", "-="].dropCommonPrefix() == @["-", "="]
    check @["---"].dropCommonPrefix(false) == @["---"]

  test "{isSubseq}":
    check @["usr", "include", "vector"].isSubseq(@["usr", "include", "c++", "vector"])

    check @["A"].isSubseq(@["A"])
    check not @["B"].isSubseq(@["A"])

  test "{dropSubseq}":
    check @["C", "X", "X", "E"].dropSubseq(@["C", "X", "X"]) == @["E"]
    check @["C", "X"].dropSubseq(@[]) == @["C", "X"]
    check emptySeq[string]().dropSubseq(@["E"]) == emptySeq[string]()
    check "eCXXE"
      .splitCamel(mergeCapitalized = false)
      .dropSubseq(@["C", "X", "X"]) == @["e", "E"]

    check "Eeeee".dropSubstr("eee") == "Ee"
    check dropSubstr("--+==", "-+=") == "-="
    check dropLongestSubseq(
      "CXIdxEntityCXXTemplateKind", @["CX", "CXX"]) ==
      "CXIdxEntityTemplateKind"

    # for runnable examples
    check "CXX_CX".dropLongestSubseq(@["CXX", "CX"]) == "_CX"
    check "CX_CX_EEECX".dropSubstr("CX") == "__EEE"


  test "{startsWith}":
    check "        ()".startsWith(Whitespace, "()")
    check "-".startsWith({}, "-")
    check "---".startsWith({'-'})
    check "--".startsWith('-')


  test "{endsWith}":
    check "--, 9".endsWith("9", ", 9")
    check "---".endsWith({'-'})
    check "--".endsWith('-')


  test "{dropSuffix}":
    check "999".dropSuffix("9") == "99"
    check "hello.txt".dropSuffix(".txt").addSuffix(".nim") == "hello.nim"

  test "{splitTokenize}":
    check "std::vector<int>".splitTokenize(@["::", "<", ">"]) == @[
      "std", "::", "vector", "<", "int", ">"]

    check "(())".splitTokenize(@["(", ")"]) == @["(", "(", ")", ")"]
    check "(())".splitTokenize({'(', ')'}) == @["(", "(", ")", ")"]
    check "hello<world>".splitTokenize({'<', '>'}) == @[
      "hello", "<", "world", ">"]

import hmisc/other/strparser

suite "Strparser":
  test "main":
    check("[a,b,c,d]".toStrSeq() == @["a", "b", "c", "d"])
    check("[a,  b]".toStrSeq() == @["a", "b"])
    check("~|[a|b]".toStrSeq() == @["a", "b"])
    check("a,b,c".toStrSeq() == @["a", "b", "c"])

    check(
      toTuple[seq[string]]("(~|[a|b],~![a!b])") ==
      (@["a", "b"], @["a", "b"]))

    check(
      toTuple[seq[string]]("~*([a,b]*[a,b])") ==
      (@["a", "b"], @["a", "b"]))

    check(toTuple[seq[string]]("~*([a]*[a])") == (@["a"], @["a"]))

    check(toTuple[seq[string]]("(a,a)") == (@["a"], @["a"]))

    check("['a', 'b']".toStrSeq() == @["a", "b"])

    check("~|,,a['  a'| 'b']".toStrSeq() == @["a", "'b'"])

    check("['input.tmp.pl']".toStrSeq() == @["input.tmp.pl"])

    check("input.tmp.pl".toStrSeq() == @["input.tmp.pl"])

    check(
      toTuple[string]("~||(test.tmp.pl||test1.sh)") ==
      ("test.tmp.pl", "test1.sh"))
