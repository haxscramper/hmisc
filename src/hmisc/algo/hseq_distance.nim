import sequtils, tables, strformat, strutils, math, algorithm
import ../hdebug_misc
import ../macros/traceif
import ../types/hprimitives

## Sequence distance metrics

# TODO performance benchmars for fuzzy string matching - no copying
#      should occur

# TODO Implement all dynamic programming algorithms using two separate
#      procs - for building matrix and backtracking.

# TODO add custom `cmp` proc for fuzzy matching instead of using `==`

func zeroEqualCmp*[T](x, y: T): int = (if x == y: 0 else: -1)

func longestCommonSubsequence*[T](
  x, y: seq[T],
  itemCmp: EqCmpProc[T] = (proc(x, y: T): bool = x == y)
    ): seq[tuple[matches: seq[T], xIndex, yIndex: seq[int]]] =
  ## Find longest common subsequence for `x` and `y`. Use `itemCmp` to
  ## compare for item equality. In case if there is more than one
  ## common subsequence of equal lenght return all. In most cases you
  ## can just call `longetstCommonSubsequece(@[...], @[...])[0].matches` -
  ## unless you need all subsequences.
  # TODO make all returned elements optional - e.g. if you only need
  # matches in first array, or in second, there is no need to copy all
  # elements over.
  if x.len == 0 or y.len == 0:
    return @[(matches: newSeq[T](),
              xIndex: newSeq[int](),
              yIndex: newSeq[int]())]

  var mem: CountTable[(int, int)]
  proc lcs(i, j: int): int =
    if (i, j) notin mem:
      mem[(i, j)] =
        traceIf false:
          if i == -1 or j == -1:
            0
          elif itemCmp(x[i], y[j]):
            lcs(i - 1, j - 1) + 1
          else:
            max(lcs(i, j - 1), lcs(i - 1, j))

    mem[(i, j)]

  let
    m = x.len - 1
    n = y.len - 1

  # echov "_-----_"

  proc backtrack(i, j: int): seq[
    tuple[matches: seq[T], xIndex, yIndex: seq[int]]
  ] =
    traceIf false:
      if lcs(i, j) == 0:
        result = @[]
      elif i == 0:
        var jRes = j
        while true:
          if (i, jRes - 1) in mem and
             mem[(i, jRes - 1)] == mem[(i, j)]:
            dec jRes
          else:
            break

        result = @[ (
          matches: @[x[i]],
          xIndex: @[i],
          yIndex: @[jRes]
        ) ]
        # echov result
      elif j == 0:
        var iRes = i
        while true:
          if (iRes, j) in mem and
             mem[(iRes - 1, j)] == mem[(i, j)]:
            dec iRes
          else:
            break

        result = @[ (
          matches: @[y[j]],
          xIndex: @[iRes],
          yIndex: @[j]
        ) ]
        # echov result
      elif itemCmp(x[i], y[j]):
        for (match, xIdx, yIdx) in backtrack(i - 1, j - 1):
          result.add((
            matches: match & @[x[i]],
            xIndex: xIdx & @[i],
            yIndex: yIdx & @[j]
          ))

        # echov result
      elif lcs(i, j - 1) > lcs(i - 1, j):
        result = backtrack(i, j - 1)
        # echov result
      elif lcs(i, j - 1) < lcs(i - 1, j):
        result = backtrack(i - 1, j)
        # echov result
      else: # both paths has valid subsequences. Can return all of them
        result = backtrack(i - 1, j) & backtrack(i - 1, j)

  result = backtrack(m, n)

  if result.len == 0:
    result = @[(matches: newSeq[T](),
                xIndex: newSeq[int](),
                yIndex: newSeq[int]())]

  when false: # Print grid
    var grid = newSeqWith(x.len, "  ".repeat(y.len))
    for xId in 0 ..< x.len:
      for yId in 0 ..< y.len:
        if (xId, yId) in mem:
          grid[xId][yId] = ($mem[(xId, yId)])[0]

    debugecho "  | ", toSeq(0 .. y.len()).join("")
    for idx, line in grid:
      debugecho &"{idx:>2}| {line}"




func longestCommonSubsequence*[T](
  x, y: openarray[T],
  itemCmp: EqCmpProc[T] = (proc(x, y: T): bool = x == y)
    ): seq[tuple[matches: seq[T], xIndex, yIndex: seq[int]]] =
  longestCommonSubsequence(toSeq(x), toSeq(y), itemCmp)

func byCharSimilarityScore*(
  x, y: string, emptyScore: range[0.0 .. 100.0] = 100.0): range[0.0 .. 100.0] =
  if x.len == 0 and y.len == 0:
    emptyScore
  else:
    100 * (
      longestCommonSubsequence(x, y)[0].matches.len /
      max(x.len, y.len)
    )

func byWordSimilarityScore*(
  x, y: string, emptyScore: range[0.0 .. 100.0] = 100.0): range[0.0 .. 100.0] =
  let split1 = x.split(" ")
  let split2 = y.split(" ")
  if x.len == 0 and y.len == 0:
    emptyScore
  else:
    100 * (
      longestCommonSubsequence(split1, split2)[0].matches.len /
      max(split1.len, split2.len)
    )



proc fuzzyMatchRecursive[Seq, Item](
  patt, other: Seq,
  pattIdx, otherIdx: int8,
  recLevel, maxRec: int,
  succStart: var int, matches: var seq[int8],
  eqCmp: EqCmpProc[Item],
  scoreFunc: proc(patt, other: Seq, matches: seq[int]): int
    ): tuple[ok: bool, score: int] =


  var otherIdx = otherIdx
  var pattIdx = pattIdx

  if (recLevel > maxRec) or
     (pattIdx == patt.len) or
     (otherIdx == other.len):
    result.ok = false
    return result

  var hadRecursiveMatch: bool = false
  var bestRecursiveScore: int = 0
  var bestRecursiveMatches: seq[int8]

  var succMatchBuf: seq[int8] = matches
  while (pattIdx < patt.len) and (otherIdx < other.len):
    if eqCmp(patt[pattIdx], other[otherIdx]):
      let recRes = fuzzyMatchRecursive(
        patt,
        other,
        pattIdx,
        otherIdx + 1,
        recLevel + 1,
        maxRec,
        succStart,
        succMatchBuf,
        eqCmp,
        scoreFunc
      )

      # echo &"Recursive test score: {recRes.score}, curr: {bestRecursiveScore}"
      if (not hadRecursiveMatch) or (recRes.score > bestRecursiveScore):
        # echo &"Updated best recursive score, sub buf: {succMatchBuf}"
        bestRecursiveScore = recRes.score
        bestRecursiveMatches = succMatchBuf

      matches[pattIdx] = otherIdx
      succMatchBuf[pattIdx] = otherIdx

      hadRecursiveMatch = true

      # echo &"Has match on idx: {otherIdx}, patt: {pattIdx}, matches: {matches}"
      inc pattIdx

    inc otherIdx


  let fullMatch: bool = (pattIdx == patt.len)
  let currScore = scoreFunc(patt, other, matches.mapIt(it.int))
  # echo &"Score: {currScore}, matches: {matches}, best rec: {bestRecursiveScore} {bestRecursiveMatches}"

  # echo &"Full match: {fullMatch}, {pattIdx} == {patt.len}"
  if fullMatch:
    result.score = currScore

  if hadRecursiveMatch and (not fullMatch or (bestRecursiveScore > currScore)):
    # echo &"Recursive had better results: {bestRecursiveScore} > {currScore}"
    result.score = bestRecursiveScore
    result.ok = true
    matches = bestRecursiveMatches
    # echo &"Assign to matches: {matches}"
    # echo &"Recursive match has better results: {bestRecursiveMatches}"
  elif fullMatch:
    # echo "Full match completed"
    # echo &"Full match results: {matches}"
    result.ok = true
  else:
    # echo &"Else"
    result.ok = false

type FuzzyMatchRes* = tuple[ok: bool, score: int, matches: seq[int]] 

proc fuzzyMatchImpl[Seq, Item](
    patt, other: Seq,
    matchScore: proc(patt, other: Seq, matches: seq[int]): int,
    eqCmp: EqCmpProc[Item]
  ): FuzzyMatchRes =

  ## Perform fuzzy matching of `other` agains `patt`. Return `score` -
  ## how similar two sequences are and `matches` - indices for which
  ## other matches pattern.
  var matchBuf: seq[int8] = newSeqWith(patt.len, 0.int8)
  var succStart = 0
  # echo &"Calling recursive implementation: input buffer {matchBuf}"
  let recMatch = fuzzyMatchRecursive[Seq, Item](
    patt = patt,
    other = other,
    pattIdx = 0,
    otherIdx = 0,
    recLevel = 0,
    maxRec = 10,
    succStart = succStart,
    matches = matchBuf,
    eqCmp = eqCmp,
    scoreFunc = matchScore
  )

  # echo &"Finished recursive implementation, buffer: {matchBuf}"

  return (
    ok: recMatch.ok,
    score: recMatch.score,
    matches: matchBuf.mapIt(it.int)
  )

proc fuzzyMatch*[T](
    patt, other: openarray[T],
    matchScore: proc(patt, other: openarray[T], matches: seq[int]): int,
    eqCmp: EqCmpProc[T]
  ): FuzzyMatchRes =
  ## Generic fuzzy match algorithm. Returns similarity between two
  ## sequences using `matchScore` function to determine weight of each
  ## possible match. Highest possible match score for two sequences is
  ## returned (in case of )
  ##
  ## `matchScore` callback number means better match. Value of this
  ##  function determines behaviour of the fuzzy matching. First
  ##  argument is an input pattern, second is sequence being matched
  ##  agains. `matches` is a sequence of indices in `other`, showing
  ##  potential match.
  ##
  ##
  ## ## Parameters
  ##
  ## :patt: Pattern sequence
  ## :other: Sequence to match against.
  ## :matchScore: Callback function for getting score match
  ## :eqCmp: Comparison proc for two elements in the sequence
  ##
  ##
  fuzzyMatchImpl[openarray[T], T](patt, other, matchScore, eqCmp)


proc fuzzyMatch*[T](
    patt, other: openarray[T],
    matchScore: proc(patt, other: openarray[T], matches: seq[int]): int
  ): FuzzyMatchRes =

  fuzzyMatchImpl[openarray[T], T](
    patt, other, matchScore,
    (proc(a, b: T): bool = a == b)
  )

proc fuzzyMatch*(
    patt, other: string,
    matchScore: proc(patt, other: string, matches: seq[int]): int
  ): FuzzyMatchRes =

  ## Fuzzy match overload for strings
  fuzzyMatchImpl[string, char](
    patt, other, matchScore, (proc(a, b: char): bool = a == b))


proc fuzzyMatch*(
    patt, other: string,
    scores: seq[(set[char], int)],
  ): FuzzyMatchRes =

  var scoreArr: array[char, int]
  for item in mitems(scoreArr):
    item = 1

  for (chars, value) in items(scores):
    for ch in items(chars):
      scoreArr[ch] = value

  fuzzymatchImpl[string, char](
    patt,
    other,
    (
      proc(patt, other: string, matches: seq[int]): int =
        var prevIdx = matches[0]
        for idx in items(matches):
          if prevIdx <= idx:
            break

          let score = scoreArr[other[idx]]

          result += score

    ),
    (
      proc(a, b: char): bool =
        a == b
    )
  )

type
  LevEditKind* = enum
    lekInsert
    lekReplace
    lekDelete

  LevEdit* = object
    case kind*: LevEditKind
      of lekInsert:
        insertPos*: int
        insertItem*: char

      of lekDelete:
        deletePos*: int

      of lekReplace:
        replacePos*: int
        replaceItem*: char

func getPos*(edit: LevEdit): int =
  case edit.kind:
    of lekInsert: edit.insertPos
    of lekDelete: edit.deletePos
    of lekReplace: edit.replacePos


func apply*(str: var seq[char], op: LevEdit) =
  case op.kind:
    of lekDelete:
      let start = min(str.high, op.deletePos)
      str.delete(start, start)
    of lekReplace:
      str[op.replacePos] = op.replaceItem
    of lekInsert:
      str.insert($op.insertItem, op.insertPos)


proc levenshteinDistance*(str1, str2: seq[char]): tuple[
  distance: int, operations: seq[LevEdit]] =
  # Adapted from https://phiresky.github.io/levenshtein-demo/
  var
    l1 = str1.len
    l2 = str2.len

    m: seq[seq[int]] = newSeqWith(l1 + 1, newSeqWith(l2 + 1, 0))
    paths: seq[seq[(int, int)]] = newSeqWith(l1 + 1,
                                             newSeqWith(l2 + 1, (0, 0)))

  for i in 0 .. l1:
    m[i][0] = i
    paths[i][0] = (i - 1, 0)

  for j in 0 .. l2:
    m[0][j] = j
    paths[0][j] = (0, j - 1)

  for i in 1 .. l1:
    for j in 1 .. l2:
      if (str1[i - 1] == str2[j - 1]):
        m[i][j] = m[i - 1][j - 1]
        paths[i][j] = (i - 1, j - 1)
      else:
        let min = min([m[i - 1][j], m[i][j - 1], m[i - 1][j - 1]])
        m[i][j] = min + 1;
        if (m[i - 1][j] == min):
          paths[i][j] = (i - 1, j)

        elif (m[i][j - 1] == min):
          paths[i][j] = (i, j - 1)

        elif (m[i - 1][j - 1] == min):
          paths[i][j] = (i - 1, j - 1)

  var levenpath: seq[tuple[i, j: int, t: string]]

  var j = l2
  var i = l1
  while i >= 0 and j >= 0:
    j = l2
    while i >= 0 and j >= 0:
      levenpath.add((i, j, ""))
      let t = i
      i = paths[i][j][0]
      j = paths[t][j][1]


  reverse(levenpath)
  result.distance = m[levenpath[^1][0]][levenpath[^1][1]]

  for i in 1 ..< levenpath.len:
    var
      last = levenpath[i - 1]
      cur = levenpath[i]

    if i != 0:
      if (
        cur.i == last.i + 1 and
        cur.j == last.j + 1 and
        m[cur.i][cur.j] != m[last.i][last.j]
      ):
        result.operations.add LevEdit(
          kind: lekReplace,
          replacePos: cur.j - 1,
          replaceItem: str2[cur.j - 1]
        )

        cur.t = "replace"

      elif (cur.i == last.i and cur.j == last.j + 1):
        result.operations.add LevEdit(
          kind: lekInsert,
          insertPos: cur.i,
          insertItem: str2[cur.j - 1]
        )

        cur.t = "insert"

      elif (cur.i == last.i + 1 and cur.j == last.j):
        result.operations.add LevEdit(
          kind: lekDelete,
          deletePos: cur.i - 1,
        )

        cur.t = "delete"




type
  Align[T] = seq[AlignElem[T]]
  AlignSeq[T] = tuple[align1, align2: Align[T], score: int]
  AlignGroup[T] = seq[tuple[idx: int8, align: Align[T]]]
  AlignElem[T] = object
    case isGap: bool
      of true:
        discard
      of false:
        idx: int
        item: T

func initAlignElem[T](idx: int, item: T): AlignElem[T] =
  AlignElem[T](idx: idx, isGap: false, item: item)

proc toString*[T](
    align: Align[T],
    gapSize: int = 1,
    gapString: string = "∅"
  ): string =

  for elem in align:
    if elem.isGap:
      result &= strutils.repeat(gapString, gapSize)
    else:
      result &= alignLeft($elem.item, gapSize)

proc toString*[T](
    align: Align[AlignElem[T]],
    gapSize: int = 1,
    gapString: string = "∅"
  ): string =

  for elem in align:
    if elem.isGap or elem.item.isGap:
      result &= strutils.repeat(gapString, gapSize)
    else:
      result &= alignLeft($elem.item.item, gapSize)


# TODO make separate penalties on start/end of the sequence
proc needlemanWunschAlign*[T](
  seq1, seq2: seq[T],
  gapPenalty: int,
  matchScore: proc(a, b: T): int,): AlignSeq[T] =
  var score = newSeqWith(seq2.len + 1, newSeqWith(seq1.len + 1, 0))

  for i in 0 .. seq2.len:
    score[i][0] = gapPenalty * i

  for j in 0 .. seq1.len:
    score[0][j] = gapPenalty * j

  # Fill out all other values in the score matrix
  for i in 1 .. seq2.len:
    for j in 1 .. seq1.len:
      # Record the maximum score from the three possible scores calculated above
      score[i][j] = max([
        score[i - 1][j - 1] + match_score(seq1[j - 1], seq2[i - 1]),
        score[i - 1][j] + gapPenalty,
        score[i][j - 1] + gapPenalty
      ])

  # var
  #   align1: seq[AlignElem[T]]
  #   align2: seq[AlignElem[T]]

  var (i, j) = (seq2.len, seq1.len)
  while i > 0 and j > 0:
    let
      curr = score[i][j]
      diag = score[i - 1][j - 1]
      vert = score[i][j - 1]
      horiz = score[i - 1][j]

    if curr == diag + match_score(seq1[j - 1], seq2[i - 1]):
      result.align1 &= initAlignElem(j - 1, seq1[j - 1])
      result.align2 &= initAlignElem(i - 1, seq2[i - 1])

      dec i
      dec j

    elif curr == vert + gapPenalty:
      result.align1 &= initAlignElem(j - 1, seq1[j - 1])
      result.align2 &= AlignElem[T](isGap: true)

      dec j

    elif curr == horiz + gapPenalty:
      result.align1 &= AlignElem[T](isGap: true)
      result.align2 &= initAlignElem(i - 1, seq2[i - 1])

      dec i

  while j > 0:
    result.align1 &= initAlignElem(j - 1, seq1[j - 1])
    result.align2 &= AlignElem[T](isGap: true)

    dec j

  while i > 0:
    result.align1 &= AlignElem[T](isGap: true)
    result.align2 &= initAlignElem(i - 1, seq2[i - 1])

    dec i

  reverse(result.align1)
  reverse(result.align2)
  result.score = score[^1][^1]




type
  Node = object
    i: int
    j: int

func empty[T](s: seq[T]): bool = s.len == 0

template newGridWith(rows, cols: int, elem: untyped): untyped =
  newSeqWith(rows, newSeqWith(cols, elem))

proc makeAlign[T](
  seq1, seq2: seq[T], path: seq[Node]): AlignSeq[T] =
  for ii in 0 ..< path.len - 1:
    let
      cur = path[ii]
      next = path[ii + 1]

    if (cur.i == next.i + 1 and cur.j == next.j + 1):
      result.align1 &= AlignElem[T](
        isGap: false, item: seq1[next.i], idx: next.i)

      result.align2 &= AlignElem[T](
        isGap: false, item: seq2[next.j], idx: next.j)


    elif (cur.i == next.i and cur.j == next.j + 1):
      result.align1 &= AlignElem[T](isGap: true)
      result.align2 &= AlignElem[T](
        isGap: false, item: seq2[next.j], idx: next.j)

    elif (cur.i == next.i + 1 and cur.j == next.j):
      result.align2 &= AlignElem[T](isGap: true)
      result.align1 &= AlignElem[T](
        isGap: false, item: seq1[next.i], idx: next.j)

  reverse(result.align2)
  reverse(result.align1)
  # FIXME set score for resulting alignment (`result.score`)



# func `!`(b: bool): bool = not b
# func `&&`(a, b: bool): bool = a and b
# func `||`(a, b: bool): bool = a or b
func `$`[T](grid: seq[seq[T]]): string =
  for row in grid:
    for cell in row:
      when cell is bool:
        if cell:
          result &= "\e[32mtrue\e[39m"
        else:
          result &= "\e[31mfalse\e[39m"

      else:
        result &= $cell

      result &= "\t"
    result &= "\n"


proc affineGapAlign*[T](
    seq1, seq2: seq[T],
    gapOpenPenalty: int = -2,
    gapExtPenalty: int = -1,
    matchScore: proc(a, b: T): int
  ): seq[seq[Node]] =
  # Implementation adapted from https://github.com/ruolin/affine-gap-gotoh/blob/master/main.cpp

  let
    nseq1: int = seq1.len + 1
    nseq2: int = seq2.len + 1

  var
    alignPath: seq[Node]
    paths: seq[seq[Node]]
    rMatrix: seq[seq[int]]    = newGridWith(nseq1, nseq2, 0)
    pMatrix                   = newGridWith(nseq1, nseq2, 0)
    qMatrix                   = newGridWith(nseq1, nseq2, 0)
    vertWhole: seq[seq[bool]] = newGridWith(nseq1 + 1, nseq2 + 1, false)
    horiWhole                 = newGridWith(nseq1 + 1, nseq2 + 1, false)
    diagWhole                 = newGridWith(nseq1 + 1, nseq2 + 1, false)
    vertTopHalf               = newGridWith(nseq1 + 1, nseq2 + 1, false)
    vertBottomHalf            = newGridWith(nseq1 + 1, nseq2 + 1, false)
    horiLeftHalf              = newGridWith(nseq1 + 1, nseq2 + 1, false)
    horiRightHalf             = newGridWith(nseq1 + 1, nseq2 + 1, false)

  proc DFS(cn: Node, must_go_dir: int) =
    let prev: Node = if align_path.empty(): Node(i: 0, j: 0) else: align_path[^1]
    align_path.add(cn);
    if (cn.i == 0 and cn.j == 0):
      paths.add(align_path);

    else:
      if (must_go_dir == 1):
        let next_must_go = if (
          hori_whole[cn.i][cn.j] and hori_left_half[cn.i][cn.j]): 1 else: 0
        DFS(Node(i: cn.i, j: cn.j - 1), next_must_go);

      elif (must_go_dir == 2):
        let next_must_go = if (
          vert_whole[cn.i][cn.j] and vert_top_half[cn.i][cn.j]): 2 else: 0
        DFS(Node(i: cn.i - 1, j: cn.j), next_must_go);

      else:
        if (diag_whole[cn.i][cn.j]):
          DFS(Node(i: cn.i - 1, j: cn.j - 1), 0);

        if (vert_whole[cn.i][cn.j]):
          if (vert_bottom_half[cn.i][cn.j]):
            if (cn.i + 1 != prev.i or cn.j != prev.j):
              return

          let next_must_go = if(vert_top_half[cn.i][cn.j]):  2 else: 0
          DFS(Node(i: cn.i - 1, j: cn.j), next_must_go)

        if (hori_whole[cn.i][cn.j]):
          if (hori_right_half[cn.i][cn.j]):
            if (cn.i != prev.i or cn.j + 1 != prev.j):
              return

          let next_must_go = if(hori_left_half[cn.i][cn.j]): 1 else: 0
          DFS(Node(i: cn.i, j: cn.j - 1), next_must_go)

    discard align_path.pop()



  proc impl() =
    for j in 0 ..< nseq2:
      pMatrix[0][j] = 2 * gapOpenPenalty + max(nseq2, nseq1) * gapExtPenalty - 1;
      rMatrix[0][j] = gapOpenPenalty + j * gapExtPenalty;


    for i in 0 ..< nseq1:
      qMatrix[i][0] = 2 * gapOpenPenalty + max(nseq2, nseq1) * gapExtPenalty - 1;
      rMatrix[i][0] = gapOpenPenalty + i * gapExtPenalty;

    rMatrix[0][0] = 0;
    diag_whole[nseq1][nseq2] = true;

    for i in 0 ..< nseq1:
      for j in 0 ..< nseq2:
        if i != 0:
          pMatrix[i][j] = gapExtPenalty + max(
            pMatrix[i - 1][j], rMatrix[i - 1][j] + gapOpenPenalty);
          if (pMatrix[i][j] == gapExtPenalty + pMatrix[i - 1][j]):
            vert_top_half[i - 1][j] = true

          if (pMatrix[i][j] == gapExtPenalty + gapOpenPenalty + rMatrix[i - 1][j]):
            vert_bottom_half[i - 1][j] = true

        if j != 0:
          qMatrix[i][j] = gapExtPenalty + max(
            qMatrix[i][j - 1], rMatrix[i][j - 1] + gapOpenPenalty);
          if qMatrix[i][j] == gapExtPenalty + qMatrix[i][j - 1]:
            hori_left_half[i][j - 1] = true

          if qMatrix[i][j] == gapExtPenalty + gapOpenPenalty + rMatrix[i][j - 1]:
            hori_right_half[i][j - 1] = true

        if i != 0 and j != 0:
          rMatrix[i][j] = max(
            rMatrix[i - 1][j - 1] + matchScore(
              seq2[j - 1], seq1[i - 1]
            ),
            max(qMatrix[i][j], pMatrix[i][j]))

          if rMatrix[i][j] == rMatrix[i - 1][j - 1] + matchScore(
            seq2[j - 1], seq1[i - 1]):
            diag_whole[i][j] = true

        if rMatrix[i][j]  == pMatrix[i][j]:
          vert_whole[i][j] = true

        if rMatrix[i][j]  == qMatrix[i][j]:
          hori_whole[i][j] = true


    for i in countdown(nseq1 - 1,  0):
      for j in countdown(nseq2 - 1, 0):
        if ((not vert_whole[i+1][j] or not vert_bottom_half[i][j]) and
            (not hori_whole[i][j+1] or not hori_right_half[i][j]) and
            not diag_whole[i+1][j+1]):
          vert_whole[i][j] = false
          hori_whole[i][j] = false
          diag_whole[i][j] = false

        if (not vert_whole[i+1][j] and
            not hori_whole[i][j+1] and
            not diag_whole[i+1][j+1]):
          continue

        else:
          if (vert_whole[i+1][j] and vert_top_half[i][j]):
            vert_top_half[i + 1][j] = not vert_bottom_half[i][j]
            vert_bottom_half[i][j] = not vert_whole[i][j]
            vert_whole[i][j] = true
          else:
            vert_top_half[i + 1][j] = false
            vert_bottom_half[i][j] = false

          if (hori_whole[i][j + 1] and hori_left_half[i][j]):
            hori_left_half[i][j + 1] = not hori_right_half[i][j]
            hori_right_half[i][j] = not hori_whole[i][j]
            hori_whole[i][j] = true
          else:
            hori_left_half[i][j + 1] = false
            hori_right_half[i][j] = false

    DFS(Node(i: nseq1 - 1, j: nseq2 - 1), 0)

  impl()

  return paths

proc sortAlignments*[T](
  seq1, seq2: seq[T],
  paths: seq[seq[Node]],
  scoreFunc: proc(align: AlignSeq[T]): int): seq[AlignSeq[T]] =
  for path in paths:
    result.add makeAlign(seq1, seq2, path)

  result.sort(proc(sa1, sa2: AlignSeq[T]): int =
      if scoreFunc(sa1) > scoreFunc(sa2): 1 else: -1
  )

iterator allAlign*[T](
  seq1, seq2: seq[T],
  gapOpenPenalty: int = -2,
  gapExtPenalty: int = -1,
  matchScore: ScoreCmpProc[T]): AlignSeq[T] =

  let paths = affineGapAlign(
    seq1, seq2,
    gapOpenPenalty = gapOpenPenalty,
    gapExtPenalty = gapExtPenalty,
    matchScore = matchScore
  )

  for path in paths:
    yield makeAlign(seq1, seq2, path)

proc bestAlign*[T](
    seq1, seq2: seq[T],
    alignQualityScore: ScoreProc[AlignSeq[T]],
    gapOpenPenalty: int = -2,
    gapExtPenalty: int = -1,
    matchScore: ScoreCmpProc[T] = (
      proc(a, b: T): int = (if a == b: 0 else: -1)
    )
  ): AlignSeq[T] =
  var
    bestScore = 0
    idx = 0

  for align in allAlign(
    seq1, seq1,
    gapOpenPenalty = gapOpenPenalty,
    gapExtPenalty = gapExtPenalty,
    matchScore = matchScore
  ):
    if idx == 0:
      bestScore = alignQualityScore(align)
      result = align
    else:
      if alignQualityScore(align) > bestScore:
        result = align

    inc idx



proc firstAlign*[T](
    seq1, seq2: seq[T],
    gapOpenPenalty: int = -2,
    gapExtPenalty: int = -1,
    matchScore: ScoreCmpProc[T]
  ): AlignSeq[T] =
  for align in allAlign(
    seq1, seq2,
    gapOpenPenalty = gapOpenPenalty,
    gapExtPenalty = gapExtPenalty,
    matchScore = matchScore
  ):
    return align

proc toAlignSeq*[T](inSeq: openarray[T]): Align[T] =
  for idx, item in inSeq:
    result.add initAlignElem(idx, item)

proc flatten[T](align: Align[AlignElem[T]]): Align[T] =
  for al in align:
    if al.isGap or al.item.isGap:
      result.add AlignElem[T](isGap: true)
    else:
      result.add al.item

proc alignToGroup*[T](
  group: AlignGroup[T], seqN: seq[T] | Align[T],
  matchScore: ScoreCmpProc[T],
  gapToGapPenalty: int  = -1,
  gapOpenPenalty: int   = -2,
  gapExtPenalty: int    = -1,
  gapToItemPenalty: ScoreProc[T]): Align[T] =
  when seqN is seq[T]:
    let seqN = toAlignSeq(seqN)

  defer:
    echo &"\n<- {seqN.toString()}"
    for (_, elem) in group:
      echo &"-- {elem.toString()}"

    echo &"-> {result.toString()}"

  var bestScore: int = 0
  for idx, align in group:
    let tmp = firstAlign(
      align.align, seqN,
      matchScore = proc(el1, el2: AlignElem[T]): int =
                     if el1.isGap and el2.isGap:
                       gapToGapPenalty
                     elif not el1.isGap:
                       gapToItemPenalty(el1.item)
                     elif not el2.isGap:
                       gapToItemPenalty(el2.item)
                     else:
                       matchScore(el1.item, el2.item)

    )
    if idx == 0:
      # Get alignment from second sequence - `align` from `group` is
      # assumed to be fixed
      result = tmp.align2.flatten
      bestScore = tmp.score
    else:
      if tmp.score > bestScore:
        result = tmp.align2.flatten
        bestScore = tmp.score



proc bartonSternbergAlign*(
  seqs: seq[seq[char]],
  matchScore: ScoreCmpProc[char],
  gapToItemPenalty: ScoreProc[char],
  gapOpenPenalty: int = -2,
  gapToGapPenalty: int = -2,
  realignIterations: int = 2): seq[Align[char]] =

  let
    maxLen = seqs.mapIt(it.len).max()
    allIndices: set[int8] = { 0.int8 .. seqs.high.int8 }

  var
    group: AlignGroup[char]
    maxPos: tuple[i, j, score: int]
    bestPair: tuple[al1, al2: Align[char]]

  block:
    var isFirst: bool = true
    for i in 0 .. seqs.high:
      for j in 0 .. seqs.high:
        let (al1, al2, score) = needlemanWunschAlign(
          seqs[i], seqs[j], gapOpenPenalty, matchScore)

        if (score > maxPos.score or isFirst) and i != j:
          isFirst = false
          bestPair = (al1, al2)
          maxPos = (i, j, score)

    var addedSeqs: set[int8] = { maxPos.i.int8, maxPos.j.int8 }

    group.add([
      (maxPos.i.int8, bestPair.al1),
      (maxPos.j.int8, bestPair.al2)
    ])

    while (allIndices - addedSeqs).len > 0:
      let index = min(toSeq(allIndices - addedSeqs)) # NOTE sampling scheme
      # might use more advances heuristics, for now I just do what works
      let align = alignToGroup(
        group, seqs[index],
        gapToItemPenalty = gapToItemPenalty,
        gapToGapPenalty = gapToGapPenalty,
        matchScore = matchScore,
      )

      group.add (index, align)
      addedSeqs.incl index

  for _ in 0 ..< realignIterations:
    for i in 0 .. group.high:
      let align = alignToGroup(
        group[0 ..< i] & group[i + 1 .. ^1],
        group[i].align,
        gapToItemPenalty = gapToItemPenalty,
        matchScore = matchScore
      )

      group[i] = (idx: group[i].idx, align: align)

  for entry in group:
    result.add entry.align




proc align*[T](
  seq1, seq2: openarray[T],
  gapOpenPenalty: int = -2,
  gapExtPenalty: int = -1,
  matchScore: ScoreCmpProc[T] = zeroEqualCmp[T]): AlignSeq[T] =
  firstAlign(
    toSeq(seq1), toSeq(seq2),
    gapOpenPenalty = gapOpenPenalty,
    gapExtPenalty = gapExtPenalty,
    matchScore = matchScore
  )

when isMainModule:
  if true:
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


  if true:
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

  block:
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

    if true:
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



    echo "\e[31m-------------------------------------\e[39m"
    if true:
      let seqs = @["let a = 12", "let nice = 90", "let qwe = 2"]
      discard bartonSternbergAlign(
        seqs.mapIt(it.toSeq()), cmp,
        gapToItemPenalty = gapCost,
        realignIterations = 1
      )
