import
  std/[unittest, strformat, sequtils, strutils, math],
  hmisc/algo/hseq_distance,
  hmisc/types/colorstring,
  hmisc/[hdebug_misc, hexceptions, helpers]


proc show(args: varargs[string, `$`]) = echo "    ", join(args, " ")

suite "String matches":
  test "Gitignore glob matches":
    for (text, patt) in {
      "a", "b":                             ("/*", true),
      "x/b", "a/a/b":                       ("/*", false),
      "--":                                 ("--", true),
      "123", "1234", "12345":               ("???*", true),
      "axb", "ayb":                         ("a[xy]b", true),
      "aab", "abb", "acb", "azb":           ("a[a-z]b", true),
      "aab", "abb", "acb", "azb":           ("a[^xy]b", true),
      "a3b", "aAb", "a3b", "aAb", "aZb":    ("a[^a-z]b", true),
      "a", "b", "x/a", "x/y/b":             ("*", true),
      "a", "x/a", "x/y/a":                  ("a", true),
      "a", "b", "x/a", "x/y/b":             ("*", true),
      "a", "x/a", "x/y/a":                  ("a", true),
      "a":                                  ("/a", true),
      "axb", "ayb":                         ("a?b", true),
      "a/x/b", "a/y/b":                     ("a/*/b", true),
      "a", "x/a", "x/y/a":                  ("**/a", true),
      "a/b", "a/x/b", "a/x/y/b":            ("a/**/b", true),
      "a/x", "a/y", "a/x/y":                ("a/**", true),
      "a?b":                                ("a\\?b", true),

      "x/a", "x/b", "x/y/a":                ("/a", false),
      "x/a", "x/y/a":                       ("a?b", false),
      "a", "b", "ab", "a/b":                ("a[xy]b", false),
      "a", "b":                             ("a[a-z]b", false),
      "a", "b":                             ("a[^xy]b", false),
      "a", "b", "axb", "ayb":               ("a[^a-z]b", false),
      "a", "b", "aab", "abb", "acb", "azb": ("a/*/b", false),
      "a/b", "a/x/y/b":                     ("a/*/b", false),
      "b", "x/b":                           ("**/a", false),
      "a", "b/x":                           ("a/**", false),
      "a", "b", "ab", "axb", "a/b":         ("a\\?b", false),
      "x/a/b", "a/b/x":                     ("a/**/b", false),
    }:
      let res = gitignoreGlobMatch(text, patt[0])
      if res != patt[1]:
        echo &"{text:<8} ~ {patt[0]:<8} {res == patt[1]:<5} (got {res:<5}, expected {patt[1]})"

  test "Gitignore glob accept tests":
    let tests = {
      ("a", false), ("b", true): @[**"a"],
      ("a.b", true), ("b", false): @[**"*", *!"*.*"]
    }

    for (test, filter) in tests:
      doAssert test[1] == accept(test[0], filter),
        &"{test[0]} failed validation expected accept '{test[1]}', but " &
        &"got '{not test[1]}'"



  test "Sequence alignment":
    const
      gapPenalty = -1
      match_award = 1
      mismatchPenalty = -1

    let (a, b, _) = needlemanWunschAlign(
      "ATGTAGTGTATAGTACATGCA".toSeq(),
      "ATGTAGTACATGCA".toSeq(),
      gapPenalty
    ) do(alpha, beta: char) -> int:
      if alpha == beta:
        match_award
      elif alpha == '-' or beta == '-':
        gapPenalty
      else:
        mismatchPenalty

    echo a.mapIt(if it.isGap: '-' else: it.item).join("")
    echo b.mapIt(if it.isGap: '-' else: it.item).join("")


  test "Weighted sequence alignment":
    let
      seq1 = "ATGTAGTGTATAGTACATGCA".toSeq()
      seq2 = "ATGTAGTACATGCA".toSeq()
    let paths = affineGapAlign(
      seq1, seq2,
      matchScore = proc(a, b: char): int =
                       if a == b:
                         if a in {'(', ')', ':', '='}:
                           4
                         else:
                           0
                       else:
                         -1

    )

    let aligns = sortAlignments(
      seq1, seq2, paths,
      scoreFunc = proc(align1: AlignSeq[char]): int =
                    1
    )

    for (a, b, _) in aligns:
      echo a.mapIt(if it.isGap: ' ' else: it.item).join("")
      echo b.mapIt(if it.isGap: ' ' else: it.item).join("")
      echo "---"


  test "Token alignment":
    let gapCost = proc(a: char): int =
      if a == '=':
        -2
      else:
        -1

    let cmp = proc(a, b: char): int =
      if a == b:
        if a in {'(', '=', ':', ')'}:
          10
        elif a in {'0' .. '9'}:
          8
        else:
          0
      else:
        if a == '=' or b == '=':
          -6
        else:
          -2

    block:
      let (al1, al2, _) = align(
        "let a = 12", "let nice = 90", matchScore = cmp)

      echo al1.toString()
      echo al2.toString()
      echo "---"

    block:
      var group: AlignGroup[char] = AlignGroup[char](@[
          (idx: 2'i8, align: "let nice = 12".toAlignSeq()),
          (idx: 0'i8, align: "let   a = 12".toAlignSeq()),
          (idx: 1'i8, align: "let qwe = 12".toAlignSeq()),
        ])

      for _ in 0 ..< 1:
        for i in 0 .. group.high:
          let align = alignToGroup(
            group[0 ..< i] & group[i + 1 .. ^1],
            group[i].align,
            gapToItemPenalty = gapCost,
            matchScore = cmp
          )

          group[i] = (idx: group[i].idx, align: align)



    block:
      let seqs = @["let a = 12", "let nice = 90", "let qwe = 2"]
      discard bartonSternbergAlign(
        seqs.mapIt(it.toSeq()), cmp,
        gapToItemPenalty = gapCost,
        realignIterations = 1
      )

suite "Edit distance":
  test "{levenshteinDistance}":
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
    template mis(a, b: untyped, exp: bool = false): untyped =
      stringMismatchMessage(a, b, showAll = exp)

    show mis("nme", ["name"])
    show mis("hello world", ["hllo world"])
    show mis("person", ["table"])
    show mis("person", ["table", "distance"])
    show mis("person", ["table", "distance", "distance"])
    show mis("person", newSeq[string]())

    show mis("Error", [
      "prog", "EOFError", "IOError", "OSError", "array",
      "or", "repr", "xor"], exp = true)


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
