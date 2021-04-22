# {.define(plainStdout).}

import unittest, strutils, unicode, macros
import sugar, json, sequtils, tables, strformat, options, terminal

import hmisc/types/[hvariant, colorstring]
import hmisc/[helpers, hexceptions]
import hmisc/algo/[halgorithm, htree_mapping, hseq_distance,
                   clformat, namegen]
import hmisc/hdebug_misc
import hmisc/base_errors

startHax()


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

proc show(args: varargs[string, `$`]) =
  echo "    ", join(args, " ")

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
        assertEq it.pos, postordCnt
        inc postordCnt

    block:
      var preordCnt = 0
      ast.iterateItDFS(it.sub, true, dfsPreorder):
        assertEq it.pre, preordCnt
        inc preordCnt

    block:
      var inordCnt = 0
      ast.iterateItDFS(it.sub, true, dfsInorder):
        assertEq it.ino, inordCnt
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
    assertEq final, """
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

import strutils

suite "Simple sequence templates":
  test "{foldlTuple}":
    assertEq @[(0, 2)].foldlTuple(a + b), (0, @[2])
    assertEq @[(1, 2), (3, 4)].foldlTuple(a + b), (4, @[2, 4])

  test "{maxIt}":
    doAssert @[1,2,3,4].maxIt(it) == 4
    var empty: seq[int]
    doAssert empty.maxIt(it) == 0

  test "{allOfIt} empty sequence :template:":
    var empty: seq[int]
    doAssert empty.allOfIt(false)

  test "{allOfIt} use example :template:example:":
    doAssert @[1, 2, 3].allOfIt(it > 0)

  test "{groupByIt}":
    doAssert @[1, 2, 1].groupByIt(it) == @[@[1, 1], @[2]]
    doAssert @[1].groupByIt(it) == @[@[1]]
    doAssert @[1, 1, 1].groupByIt(it) == @[@[1, 1, 1]]

    doAssert @[(1, 2), (1, 3)].groupByIt(it[0]) == @[@[(1, 2), (1, 3)]]

  test "{mapPairs} type assertion :template:type:":
    doAssert {1: "hello", 2: "222"}.mapPairs(
      $lhs & "--" & rhs
    ) is seq[string]

  test "{mapPairs} return value :template:value:":
    doAssert {1: 2, 3: 4}.mapPairs(
      lhs + rhs
    ) == @[3, 7]

  test "{mapPairs} map table values :template:example:":
    let val = {1: 3, 4: 5}.toTable().mapPairs(max(lhs, rhs))
    assertEq(val.sorted(), @[3, 5])

  test "{mapPairs} iterate indexed :template:example:":
    let res = @["a", "b"].mapPairs(
      block:
        doAssert lhs is int
        doAssert rhs is string
        $lhs & ":" & rhs
    )

    doAssert res is seq[string]
    doAssert res == @["0:a", "1:b"]

  test "{mapPairs} custom `pairs` :template:":
    type U = object

    iterator pairs(u: U): (float, string) =
      yield (1.2, "1222")

    # ignored by `mapPairs`
    iterator items(u: U): string =
      yield "222"

    let res = U().mapPairs($lhs & " () " & rhs)
    doAssert res is seq[string]
    doAssert res == @["1.2 () 1222"]

  test "{mapPairs} custom `items` :template:":
    type U = object

    # No pairs declared - using `items` with index as `lhs`
    iterator items(u: U): string =
      yield "222"
      yield "aaa"

    let res = U().mapPairs($lhs & " () " & rhs)
    doAssert res is seq[string]
    doAssert res == @["0 () 222", "1 () aaa"]

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
    doAssert true and zip(
      @[(1, 2)],
      @[(1, 2)]
    ).mapPairs(
      (lhs[0] == rhs[0]) and (lhs[1] == rhs[1])
    ).foldl(a and b)

  test "{subnodesEq} :template:":
    type
      U = object
        s: seq[U]

    doAssert subnodesEq(U(), U(), s)
    doAssert subnodesEq(U(s: @[U(), U()]), U(s: @[U(), U()]), s)
    doAssert not subnodesEq(
      U(s: @[U(), U(), U()]), U(s: @[U(), U()]), s)

  test "{findItFirst} :template:example:":
    doAssert @["A", "B", "D"].findItFirst(it == "A") == "A"

    expect ArgumentError:
      discard @["A", "B"].findItFirst(it == "D")

  test "{findItFirstOpt} :template:example:":
    doAssert @["A"].findItFirstOpt(it == "A").isSome()
    doAssert @["A"].findItFirstOpt(it == "D").isNone()

  test "{findIt} :template:":
    doAssert @[1].findIt(it == 1) == 0
    doAssert @[2].findIt(it == 1) == -1

  test "{max} :proc:generic:":
    doAssert @[1, 2, 3].max(90) == 3
    var tmp: seq[int]
    doAssert tmp.max(80) == 80

  test "{anyOfIt} :template:":
    doAssert [1, 2, 3].anyOfIt(it > 1)
    doAssert not [3, 4, 5].anyOfIt(it < 1)
    var tmp: seq[int]
    doAssert not tmp.anyOfIt(it > 9)

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

suite "String distance algorithms":
  test "{longestCommonSubsequence} :generic:value:":
    template tmp(s1, s2, s3: untyped): untyped =
      assertEq longestCommonSubsequence(s1, s2)[0].matches, s3

    doAssert longestCommonSubsequence(@[1], @[2, 3])[0].matches.len == 0
    doAssert longestCommonSubsequence(@["Cond"], @["Int", "Lit"])[0].matches.len == 0
    doAssert longestCommonSubsequence(@[1], @[2])[0].matches.len == 0
    tmp("GAC", "AGCAT", "GA")
    tmp(@[1, 2], @[1, 2], @[1, 2])
    tmp(@[1, 2, 3], @[1, 2], @[1, 2])
    tmp("XMJYAUZ", "MZJAWXU", "MJAU")
    tmp("AABC", "BC", "BC")
    tmp("AC", "ABC", "AC")
    tmp("AB", "A", "A")


  test "{longestCommonSubsequence} :generic:value:":
    template lcs(s1, s2, matches, xIdx, yIdx): untyped =
      let (lcsMatch, lcsXIdx, lcsYIdx) = longestCommonSubsequence(
        s1, s2)[0]

      try:
        assertEq lcsMatch, matches
        assertEq lcsXIdx, xIdx
        assertEq lcsYIdx, yIdx
      except:
        echo s1, ", ", s2
        raise

    lcs("GAC", "AGCAT", "GA", @[0, 1], @[1, 3])
    lcs(@[1, 2], @[1, 2], @[1, 2], @[0, 1], @[0, 1])
    lcs("AABC", "BC", "BC", @[2, 3], @[0, 1])
    lcs("AC", "ABC", "AC", @[0, 1], @[0, 2])
    lcs("-AB", "A", "A", @[1], @[0])
    lcs("AB", "A", "A", @[0], @[0])


  proc showMatches(patt, input: string, matches: seq[int]) =
    show "input:", input
    var buf = " ".repeat(input.len)
    for pattIdx, inIdx in matches:
      buf[inIdx] = patt[pattIdx]

    show "match:", buf

    buf = " ".repeat(input.len)


  test "{fuzzyMatch} fuzzy string matching":
    doAssert not fuzzyMatch("/tmp", "zzz", @{{'/'} : 10}).ok

    template test(
      patt, input: string, expr: untyped, expected: seq[int]): untyped =
      let res = fuzzyMatch(
        patt, input,
        proc(p, o: string, m: seq[int]): int =
          let p {.inject.} = p
          let o {.inject.} = o
          let m {.inject.} = m
          # echo m
          expr
      )

      doAssert res.ok

      if res.matches != expected:
        showMatches(patt, input, res.matches)

        var buf = " ".repeat(input.len)
        for pattIdx, inIdx in expected:
          buf[inIdx] = patt[pattIdx]

        show "expec:", buf

      assertEq res.matches, expected

    test("123", "1123", (
      block:
        var res: int
        for idx in m:
          res += o.len - idx
        res
    ) , @[0, 2, 3])

    test("123", "010233", m.sum() * 7, @[1, 3, 5])
    test("123", "01122330", 1, @[1, 3, 5])
    test("===", "===", 1, @[0, 1, 2])
    test("123", "0123", 1, @[1, 2, 3])
    test("123", "0123", m.sum(), @[1, 2, 3])

    test("123", "1123", m.sum(), @[1, 2, 3])


  test "{fuzzyMatch} scored characters":
    proc test(
      patt, input: string,
      scores: openarray[(set[char], int)],
    ) =

      let res = fuzzyMatch(patt, input, toSeq(scores))
      showMatches(patt, input, res.matches)

    test("/tmp", "/tmp/zz", {{'/'} : 10})
    test("///", "/tmp///zz", {{'/'} : 10})
    test("/q//", "/tmp/q//zz", {{'/'} : 10})

  test "{fuzzyMatch} sorted":
    var dataset = @["tMap", "tMap.nim", "tMatching",]

    let patt = "tMap.nim"
    let dataset2 = dataset.
      mapIt((patt, it, fuzzyMatch(patt, it, @[]))).
      sortedByIt(-it[2].score)

    for entry in dataset2:
      show "---"
      showMatches(entry[0], entry[1], entry[2].matches)
      show entry[2]



  test "{levenshteinDistance}":
    stopHax()
    let tests = {
      "a" : "b",
      "a" : "aa",
      "aa" : "a",
      "aaaa" : "a",
      "123" : "345",
      "horse" : "ros",
      # "interest" : "industry",
      "" : "",
      "A_A" : "_",
      "" : "1",
      "aa" : "bb",
      "ros" : "rose",
      "a" : "bc",
      "hros" : "ros",
      "A__A" : "A_B_A",
    }

    # startHax()
    for (src, target) in tests:
      let (src, target) = (toseq src, toseq target)
      var ins = src
      var ok = false
      try:
        let (dist, ops) = levenshteinDistance(src, target)
        for op in ops:
          # echo op
          ins.apply(op)

        ok = true
      except:
        discard

      if ins != target or (not ok):
        startHax()
        echov &"{src} -> {target}"
        echov &"{src} -> {ins}"

        let (dist, ops) = levenshteinDistance(src, target)
        ins = src
        for op in ops:
          echov ins
          ins.apply(op)

        echo ins

        stopHax()
        echo "fail"

  test "Levenstein edit colored":
    template impl(inSrc, inTarget: string) =
      block:
        var src = toSeq(inSrc)
        var target = toSeq(inTarget)
        let (dist, ops) = levenshteinDistance(src, target)
        let output = getEditVisual(src, target, ops)
        show output

    impl("nme", "name")
    impl("name", "nme")
    impl("hello", "hello")
    impl("one", "two")

  test "Identifier mismatch":
    template mis(a, b): untyped = stringMismatchMessage(a, b)

    show mis("nme", ["name"])
    show mis("hello world", ["hllo world"])
    show mis("person", ["table"])
    show mis("person", ["table", "distance"])
    show mis("person", ["table", "distance", "distance"])
    show mis("person", newSeq[string]())


suite "Colored string":
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

    block:
      let tt = splitSGR_sep("test" & toRed("colored") & "12312")[0]
      assertEq tt.len, 3
      assertEq tt, @[
        initColoredString("test"),
        initColoredString("colored", fg = fgRed),
        initColoredString("12312")
      ]

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
        fromString: false,
        annotation: "Hell \e[32masdfas\e[39md o",
        linerange: -2,
        expr: "errpos: currLineInf()"
      )
    ])

    # echo err.toColorString()

suite "String helper functions":
  test "Namecache":
    var cache: StringNameCache

    assertEq cache.getName("ROFF_bp"), "ROFF_bp"
    assertEq cache.getName("ROFF_bp"), "ROFF_bp"
    assertEq cache.getName("ROFF_BP"), "ROFF_BP1"

  test "toLatin*Char":
    for ch in "[({<>|!@#$%})]":
      echo &"{ch} : {ch.toLatinAbbrChar()}"

    echo "((()))".toNamedMultichar()

  test "Named multichar join":
    assertEq toNamedMulticharJoin("."), "dot"
    assertEq toNamedMulticharJoin(".."), "doubleDot"

  test "{[^]}":
    doAssert "hello"[^"lo"]
    doAssert "hello"["he"]
    doAssert "hello"[{'h'}, {'o'}]
    doAssert "hello"[{'h'}, "lo"]
    doAssert "hello"['h', "lo"]
    doAssert "hello"['h', ["lo", "le"]]

  test "{msgjoin}":
    assertEq msgjoin("_", "nice", "_"), "_nice_"
    # echo msgjoin("[a", "b]")
    # echo "-", msgjoin("a", "b"), "-"

    doAssert msgjoin("a", "b") == "a b"
    assertEq msgjoin("[hello,", "nice", "weather]"),
            "[hello, nice weather]"

    assertEq msgjoin("e", "/e"), "e /e"
    assertEq msgjoin("e", "__e"), "e __e"
    assertEq msgjoin("_a_"), "_a_"
    assertEq msgjoin("_a", "_"), "_a_"
    assertEq msgjoin("_", "a", "_"), "_a_"
    assertEq msgjoin("file", "//ee"), "file //ee"
    assertEq msgjoin("eee", "___eee"), "eee ___eee"
    assertEq msgjoin("22", "22"), "22 22"
    assertEq msgjoin("Found include", "cursor"), "Found include cursor"
    assertEq msgjoin("1", "->", "2"), "1 -> 2"
    assertEq msgjoin("/tt", "->", "/ee"), "/tt -> /ee"
    # assertEq msgjoin("_", "333", "3", "_"), "_3333_"
    # assertEq msgjoin("hello", "_", "a", "_"), "hello _a_"
    # assertEq msgjoin("hello", "_", "a", "_", "world"), "hello _a_ world"
    # assertEq msgjoin("hello", "_", "a", "2", "_", "world"),
    #  "hello _a 2_ world"

  test "{splitCamel}":
    assertEq "FILE_AAName".splitCamel(), @["FILE", "AA", "Name"]
    assertEq "HelloWorld".splitCamel, @["Hello", "World"]
    assertEq "HHeelloWWorld".splitCamel(mergeCapitalized = false),
            @["H", "Heello", "W", "World"]

    assertEq "FILE".splitCamel(), @["FILE"]
    assertEq "DBManager". splitCamel(), @["DB", "Manager"]
    assertEq "DBManager". splitCamel(adaptiveMerge = false), @["DBManager"]
    assertEq "helloWorld".splitCamel, @["hello", "World"]
    assertEq "H".splitCamel, @["H"]
    assertEq "".splitCamel, `@`[string]([])
    assertEq "clang_Hello".splitCamel, @["clang", "Hello"]
    assertEq "clang_HeeH".splitCamel(false), @[
      "clang", "_", "Hee", "H"]

    assertEq "hello___nice".splitCamel(), @["hello", "nice"]
    assertEq "hello_nice".splitCamel(), @["hello", "nice"]

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

    # assertEq abbrevCamel("AA", @["ABA", "AZZ", "A)"]), @["ABA"]

  test "{dropPrefix}":
    assertEq "???##".dropPrefix("???"), "##"
    assertEq "".dropPrefix("888"), ""
    assertEq "--".dropPrefix("-"), "-"
    assertEq "90".dropPrefix({'1', '9'}), "0"

  test "{commonPrefix}":
    # assertEq @[].commonPrefix(), ""
    assertEq @["00"].commonPrefix(), "00"
    assertEq @["--+", "--="].commonPrefix(), "--"
    assertEq @["+=", "=+"].commonPrefix(), ""

  test "{commonPrefix[T]}":
    assertEq commonPrefix(@[@["A", "B"], @["A", "C"]]), @["A"]

  test "{dropCommonPrefix}":
    assertEq @["--"].dropCommonPrefix(), @[""]
    assertEq @["a@", "a$"].dropCommonPrefix(), @["@", "$"]
    assertEq @["---"].dropCommonPrefix(false), @["---"]
    assertEq @[].dropCommonPrefix(), emptySeq[string]()


    doAssert @["--", "-="].dropCommonPrefix() == @["-", "="]
    doAssert @["---"].dropCommonPrefix(false) == @["---"]

  test "{isSubseq}":
    doAssert @["usr", "include", "vector"].isSubseq(@[
      "usr", "include", "c++", "vector"])

    doAssert @["A"].isSubseq(@["A"])
    doAssert not @["B"].isSubseq(@["A"])

  test "{dropSubseq}":
    assertEq @["C", "X", "X", "E"].dropSubseq(@["C", "X", "X"]), @["E"]
    assertEq @["C", "X"].dropSubseq(@[]), @["C", "X"]
    assertEq emptySeq[string]().dropSubseq(@["E"]), emptySeq[string]()
    assertEq "eCXXE"
      .splitCamel(mergeCapitalized = false)
      .dropSubseq(@["C", "X", "X"]), @["e", "E"]

    assertEq "Eeeee".dropSubstr("eee"), "Ee"
    assertEq dropSubstr("--+==", "-+="), "-="
    assertEq dropLongestSubseq(
      "CXIdxEntityCXXTemplateKind", @["CX", "CXX"]),
      "CXIdxEntityTemplateKind"

    # for runnable examples
    doAssert "CXX_CX".dropLongestSubseq(@["CXX", "CX"]) == "_CX"
    doAssert "CX_CX_EEECX".dropSubstr("CX") == "__EEE"


  test "{startsWith}":
    doAssert "        ()".startsWith(Whitespace, "()")
    doAssert "-".startsWith({}, "-")
    doAssert "---".startsWith({'-'})
    doAssert "--".startsWith('-')


  test "{endsWith}":
    doAssert "--, 9".endsWith("9", ", 9")
    doAssert "---".endsWith({'-'})
    doAssert "--".endsWith('-')


  test "{dropSuffix}":
    assertEq "999".dropSuffix("9"), "99"
    assertEq "hello.txt".dropSuffix(".txt").addSuffix(".nim"), "hello.nim"

  test "{splitTokenize}":
    assertEq "std::vector<int>".splitTokenize(@["::", "<", ">"]), @[
      "std", "::", "vector", "<", "int", ">"
    ]

    assertEq "(())".splitTokenize(@["(", ")"]), @["(", "(", ")", ")"]
    assertEq "(())".splitTokenize({'(', ')'}), @["(", "(", ")", ")"]
    assertEq "hello<world>".splitTokenize({'<', '>'}), @[
      "hello", "<", "world", ">"]

import hmisc/other/strparser

suite "Strparser":
  test "main":
    assertEq("[a,b,c,d]".toStrSeq(), @["a", "b", "c", "d"])
    assertEq("[a,  b]".toStrSeq(), @["a", "b"])
    assertEq("~|[a|b]".toStrSeq(), @["a", "b"])
    assertEq("a,b,c".toStrSeq(), @["a", "b", "c"])

    assertEq(
      toTuple[seq[string]]("(~|[a|b],~![a!b])"),
      (@["a", "b"], @["a", "b"]))

    assertEq(
      toTuple[seq[string]]("~*([a,b]*[a,b])"),
      (@["a", "b"], @["a", "b"]))

    assertEq(toTuple[seq[string]]("~*([a]*[a])"), (@["a"], @["a"]))

    assertEq(toTuple[seq[string]]("(a,a)"), (@["a"], @["a"]))

    assertEq("['a', 'b']".toStrSeq(), @["a", "b"])

    assertEq("~|,,a['  a'| 'b']".toStrSeq(), @["a", "'b'"])

    assertEq("['input.tmp.pl']".toStrSeq(), @["input.tmp.pl"])

    assertEq("input.tmp.pl".toStrSeq(), @["input.tmp.pl"])

    assertEq(
      toTuple[string]("~||(test.tmp.pl||test1.sh)"),
      ("test.tmp.pl", "test1.sh"))
