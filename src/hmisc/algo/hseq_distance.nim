{.experimental: "strictEffects".}

import std/[sequtils, tables, strformat, strutils, math, algorithm]
import ../core/all
import ../macros/traceif
import ../types/hprimitives


## Sequence distance metrics

# TODO performance benchmars for fuzzy string matching - no copying
#      should occur

# TODO Implement all dynamic programming algorithms using two separate
#      procs - for building matrix and backtracking.

# TODO add custom `cmp` proc for fuzzy matching instead of using `==`

func zeroEqualCmp*[T](x, y: T): int = (if x == y: 0 else: -1)

proc longestCommonSubsequence*[T](
    x, y: seq[T],
    itemCmp: proc(a, b: T): bool {.closure.} =
      (
        proc(x, y: T): bool =
          x == y
      )
  ): seq[tuple[matches: seq[T], xIndex, yIndex: seq[int]]]
    {.effectsOf: itemCmp}
  =

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

  var mem: Table[(int, int), int]
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
    # echov (i, j), lcs(i, j)
    traceIf false:
      if lcs(i, j) == 0:
        result = @[]
      elif i == 0:
        var jRes = j
        while true:
          if (i, jRes - 1) in mem and
             lcs(i, jRes - 1) == lcs(i, j):
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
             lcs(iRes - 1, j) == lcs(i, j):
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

  # ploc "Backtrack"
  result = backtrack(m, n)
  # ploc "backgrack done"

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

proc longestCommonSubsequenceForStringStartsWith*(
    a, b: seq[string]
  ): seq[tuple[matches: seq[string], xIndex, yIndex: seq[int]]] =

  longestCommonSubsequence[string](
    a, b, proc(lhs, rhs: string): bool {.noSideEffect.} =
            rhs.startsWith(lhs))


proc byCharSimilarityScore*(
    x, y: string,
    emptyScore: range[0.0 .. 100.0] = 100.0
  ): range[0.0 .. 100.0] =

  if x.len == 0 and y.len == 0:
    emptyScore
  else:
    100 * (
      longestCommonSubsequence(toSeq(x), toSeq(y))[0].matches.len /
      max(x.len, y.len)
    )

proc byWordSimilarityScore*(
    x, y: string,
    emptyScore: range[0.0 .. 100.0] = 100.0
  ): range[0.0 .. 100.0] =

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

  for item in items(scores):
    for ch in items(item[0]):
      scoreArr[ch] = item[1]

  fuzzymatchImpl[string, char](
    patt,
    other,
    (
      proc(patt, other: string, matches: seq[int]): int =
        var prevIdx = matches[0]
        for idx in items(matches):
          if prevIdx > idx:
            break

          result += scoreArr[other[idx]]

    ),
    (
      proc(a, b: char): bool =
        a == b
    )
  )

type
  LevEditKind* = enum
    lekNone
    lekUnchanged
    lekInsert
    lekReplace
    lekDelete

  LevEdit*[T] = object
    sourcePos*: int
    targetPos*: int
    case kind*: LevEditKind
      of lekNone, lekUnchanged:
        discard

      of lekInsert:
        insertPos*: int
        insertItem*: T

      of lekDelete:
        deletePos*: int

      of lekReplace:
        replacePos*: int
        replaceItem*: T

func getPos*[T](edit: LevEdit[T]): int =
  case edit.kind:
    of lekUnchanged: edit.sourcePos
    of lekInsert: edit.insertPos
    of lekDelete: edit.deletePos
    of lekReplace: edit.replacePos
    of lekNone: raise newUnexpectedKindError(edit)


func apply*[T](str: var seq[T], op: LevEdit[T]) =
  case op.kind:
    of lekUnchanged:
      discard

    of lekNone:
      raise newUnexpectedKindError(op)

    of lekDelete:
      let start = min(str.high, op.deletePos)
      str.delete(start .. start)

    of lekReplace:
      str[op.replacePos] = op.replaceItem

    of lekInsert:
      # echov op.insertPos, str
      # op.insertPos
      str.insert(op.insertItem, op.insertPos)
      # echov str


proc levenshteinDistance*[T](str1, str2: seq[T]): tuple[
  distance: int, operations: seq[LevEdit[T]]] =
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

  var levenpath: seq[tuple[i, j: int, t: LevEditKind]]

  var j = l2
  var i = l1
  while i >= 0 and j >= 0:
    j = l2
    while i >= 0 and j >= 0:
      levenpath.add((i, j, lekNone))
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
        result.operations.add LevEdit[T](
          kind: lekReplace,
          sourcePos: cur.i,
          targetPos: cur.j,
          replacePos: cur.j - 1,
          replaceItem: str2[cur.j - 1]
        )

      elif (cur.i == last.i and cur.j == last.j + 1):
        result.operations.add LevEdit[T](
          kind: lekInsert,
          insertPos: cur.j - 1,
          insertItem: str2[cur.j - 1]
        )

      elif (cur.i == last.i + 1 and cur.j == last.j):
        result.operations.add LevEdit[T](
          kind: lekDelete,
          deletePos: cur.i - 1,
        )

      else:
        result.operations.add LevEdit[T](kind: lekUnchanged)

      result.operations[^1].sourcePos = cur.i - 1
      result.operations[^1].targetPos = cur.j - 1



type
  DiffEditKind* = enum
    dekDelete
    dekInsert
    dekKeep

  DiffShiftKind* = enum
    dskDelete
    dskInsert
    dskKeep
    dskEmpty


  DiffEdit* = object
    kind*: DiffEditKind
    oldpos*: int
    newPos*: int

  ShiftedDiff* = object
    oldShifted*: seq[tuple[kind: DiffShiftKind, item: int]]
    newShifted*: seq[tuple[kind: DiffShiftKind, item: int]]


proc myersDiff*[T](
    aSeq, bSeq: openarray[T],
    itemCmp: EqCmpProc[T] = (
      when compiles(proc(x, y: T): bool = x == y):
        proc cmpProc(x, y: T): bool = x == y
        cmpProc

      else:
        nil
    )
  ): seq[DiffEdit] =
  # https://gist.github.com/adamnew123456/37923cf53f51d6b9af32a539cdfa7cc4
  var front: Table[int, tuple[x: int, history: seq[DiffEdit]]]
  front[1] = (0, @[])

  template one(idx: int): int = idx - 1

  let
    aMax = len(aSeq)
    bMax = len(bSeq)

  for d in countup(0, aMax + bMax + 1):
    for k in countup(-d, d + 1, 2):
      let goDown =
        (k == -d or (k != d and front[k - 1].x < front[k + 1].x))


      var (x, history) =
        if goDown:
          (front[k + 1].x, front[k + 1].history)

        else:
          (front[k - 1].x + 1, front[k - 1].history)

      var y = x - k

      if 1 <= y and y <= bMax and goDown:
        history.add DiffEdit(kind: dekInsert, newPos: one(y))

      elif 1 <= x and x <= aMax:
        history.add DiffEdit(kind: dekDelete, oldPos: one(x))

      while x < aMax and
            y < bMax and
            itemCmp(aSeq[x], bSeq[y]):

        x += 1
        y += 1
        history.add DiffEdit(kind: dekKeep, oldPos: one(x), newPos: one(y))

      if x >= aMax and y >= bMax:
        return history

      else:
        front[k] = (x, history)

proc shiftDiffed*[T](
    diff: seq[DiffEdit], oldSeq, newSeq: openarray[T]): ShiftedDiff =

  for line in items(diff):
    case line.kind:
      of dekDelete:
        result.oldShifted.add(dskDelete, line.oldPos)

      of dekInsert:
        result.newShifted.add(dskInsert, line.newPos)

      of dekKeep:
        var
          oldLen = result.oldShifted.len
          newLen = result.newShifted.len

        if oldLen < newLen:
          while oldLen < newLen:
            result.oldShifted.add(dskEmpty, 0)
            inc oldLen

        elif newLen < oldLen:
          while newLen < oldLen:
            result.newShifted.add(dskEmpty, 0)
            inc newLen

        result.oldShifted.add(dskKeep, line.oldPos)
        result.newShifted.add(dskKeep, line.newPos)

type
  Align*[T] = seq[AlignElem[T]]
  AlignSeq*[T] = tuple[align1, align2: Align[T], score: int]
  AlignGroup*[T] = seq[tuple[idx: int8, align: Align[T]]]
  AlignElem*[T] = object
    case isGap*: bool
      of true:
        discard
      of false:
        idx*: int
        item*: T

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

template rangeMatchImpl(): untyped {.dirty.} =
  var matched = false;
  var reverse = globPos + 1 < m and (glob[globPos + 1] == '^' or glob[globPos + 1] == '!')

  if (reverse):
    inc globPos;

  inc globPos
  while globPos < m and glob[globPos] != ']':
    if glob[globPos + 1] == '-' and
       CASE(glob[globPos]) <= CASE(text[txtPos]) and
       CASE(text[txtPos]) <= CASE(glob[globPos + 2])
      :
      matched = true
      inc globPos, 2

    else:
      if CASE(glob[globPos]) == CASE(text[txtPos]):
        matched = true
      inc globPos

  if (matched == reverse): break

  inc txtPos

  if (globPos < m): inc globPos

  continue


proc globmatch*(
    text, glob: string,
    useDotglob: bool = false,
    caseInsensetive: bool = false
  ): bool =
  var
    txtPos = 0
    globPos = 0
    n = text.len()
    m = glob.len()
    textBackup = -1
    globBackup = -1

  var nodot = not useDotGlob
  const pathSep = '/'
  template CASE(ch: char): char =
    if caseInsensetive: toLowerAscii(ch) else: ch

  while (txtPos < n):
    if (globPos < m):
      case (glob[globPos]):
        of '*':
          block caseBlock:
            if (nodot and text[txtPos] == '.'): break caseBlock
            textBackup = txtPos
            globBackup = (inc globPos; globPos)
            continue

        of '?':
          block caseBlock:
            if (nodot and text[txtPos] == '.'): break caseBlock
            if (text[txtPos] == pathSep): break caseBlock

            inc txtPos
            inc globPos
            continue

        of '[':
          block caseBlock:
            if (nodot and text[txtPos] == '.'): break caseBlock
            if (text[txtPos] == pathSep): break caseBlock

            rangeMatchImpl()

        else:
          block caseBlock:
            if glob[globPos] == '\\':
              if (globPos + 1 < m):
                inc globPos

            if (CASE(glob[globPos]) != CASE(text[txtPos]) and
                not(glob[globPos] == '/' and text[txtPos] == pathSep)):
              break caseBlock

            nodot = (not useDotGlob) and glob[globPos] == '/'
            inc txtPos
            inc globPos
            continue

    if (globBackup == -1 or text[textBackup] == pathSep):
      return false

    txtPos = (inc textBackup; textBackup)
    globPos = globBackup

  while (globPos < m and glob[globPos] == '*'):
    inc globPos

  return globPos >= m

proc gitignoreGlobMatch*(
    text, glob: string,
    useDotglob: bool = false,
    caseInsensetive: bool = false
  ): bool =

  const pathSep = '/'
  template CASE(ch: char): char =
    if caseInsensetive: toLowerAscii(ch) else: ch

  var
    txtPos = 0
    globPos = 0
    n = text.len()
    m = glob.len()
    text1Backup = -1
    glob1Backup = -1
    text2Backup = -1
    glob2Backup = -1


  var nodot = useDotglob

  if (globPos + 1 < m and glob[globPos] == '/'):
    while (txtPos + 1 < n and text[txtPos] == '.' and
           text[txtPos + 1] == pathSep):
      txtPos += 2;

    if (txtPos < n and text[txtPos] == pathSep):
      inc txtPos

    inc globPos

  elif (glob.find('/') == -1):
    let sep = text.rfind(pathSep);
    if (sep != -1):
      txtPos = sep + 1

  while (txtPos < n):
    if (globPos < m):
      case (glob[globPos]):
        of '*':
          block caseBlock:
            if (nodot and text[txtPos] == '.'): break caseBlock

            if ((inc globPos; globPos) < m and glob[globPos] == '*'):
              if ((inc globPos; globPos) >= m):
                return true;

              if (glob[globPos] != '/'):
                return false;

              text1Backup = -1
              glob1Backup = -1
              text2Backup = txtPos
              glob2Backup = (inc globPos; globPos)
              continue;

            text1Backup = txtPos
            glob1Backup = globPos
            continue;

        of '?':
          block caseBlock:
            if (nodot and text[txtPos] == '.'): break caseBlock
            if (text[txtPos] == pathSep): break caseBlock

            inc txtPos
            inc globPos
            continue

        of '[':
          block caseBlock:
            if (nodot and text[txtPos] == '.'): break caseBlock
            if (text[txtPos] == pathSep): break caseBlock

          rangeMatchImpl()
        else:
          block caseBlock:
            if glob[globPos] == '\\':
              if (globPos + 1 < m):
                inc globPos;

            if (CASE(glob[globPos]) != CASE(text[txtPos]) and
                not(glob[globPos] == '/' and text[txtPos] == pathSep)):
              break caseBlock

            nodot = (not useDotglob) and glob[globPos] == '/';
            inc txtPos
            inc globPos
            continue

    if (glob1Backup != -1 and text[text1Backup] != pathSep):
      txtPos = (inc text1Backup; text1Backup)
      globPos = glob1Backup
      continue

    if (glob2Backup != -1):
      txtPos = (inc text2Backup; text2Backup)
      globPos = glob2Backup
      continue

    return false

  while (globPos < m and glob[globPos] == '*'):
    inc globPos

  return globPos >= m;


type
  GitGlob* = object
    patt: string
    ign: bool

proc `**`*(str: string): GitGlob = GitGlob(patt: str, ign: true)
proc `*!`*(str: string): GitGlob = GitGlob(patt: str, ign: false)

func `$`*(glob: GitGlob): string =
  if not glob.ign:
    result &= "!"

  result &= glob.patt

func toGitGlob*(str: string): GitGlob =
  assert str.len > 0
  if str[0] == '!':
    GitGlob(patt: str[1..^1], ign: false)

  else:
    GitGlob(patt: str, ign: true)

proc match*(glob: GitGlob, str: string): bool =
  gitignoreGlobMatch(str, glob.patt)

proc accept*(glob: GitGlob, str: string, invert: bool = false): bool =
  if gitignoreGlobMatch(str, glob.patt):
    if invert:
      result = glob.ign

    else:
      result = not glob.ign

  else:
    result = not invert

proc accept*(
    globs: seq[GitGlob], str: string, invert: bool = false): bool =
  result = not invert
  for glob in globs:
    if gitignoreGlobMatch(str, glob.patt):
      if invert:
        result = glob.ign

      else:
        result = not glob.ign




type
  GenGlobPartKind* = enum
    ggkWord
    ggkDotAnchor
    ggkSeparator
    ggkAnyStar
    ggkAnyOne
    ggkTest

proc gitignoreGlobMatch*[B, G](
    text: seq[B],
    glob: seq[G],
    eqCmp: proc(text: B, glob: G): bool,
    useDotglob: bool = false,
  ): bool =

  var
    txtPos = 0
    globPos = 0
    n = text.len()
    m = glob.len()
    text1Backup = -1
    glob1Backup = -1
    text2Backup = -1
    glob2Backup = -1


  var nodot = useDotglob

  while (txtPos < n):
    if (globPos < m):
      case glob[globPos].globKind:
        of ggkAnyStar:
          block caseBlock:
            if (nodot and text[txtPos].globKind == ggkDotAnchor):
              break caseBlock

            if ((inc globPos; globPos) < m and
                glob[globPos].globKind == ggkAnyStar):

              if ((inc globPos; globPos) >= m):
                return true;

              if (glob[globPos].globKind == ggkSeparator):
                return false;

              text1Backup = -1
              glob1Backup = -1
              text2Backup = txtPos
              glob2Backup = (inc globPos; globPos)
              continue;

            text1Backup = txtPos
            glob1Backup = globPos
            continue;

        of ggkAnyOne:
          block caseBlock:
            if (nodot and text[txtPos].globKind == ggkDotAnchor): break caseBlock
            if (text[txtPos].globKind == ggkSeparator): break caseBlock

            inc txtPos
            inc globPos
            continue

        of ggkTest:
          block caseBlock:
            if (nodot and text[txtPos].globKind == ggkDotAnchor): break caseBlock
            if (text[txtPos].globKind == ggkSeparator): break caseBlock

          raise newImplementError()

        else:
          block caseBlock:
            if (
              not eqCmp(text[txtPos], glob[globPos]) and
              not(glob[globPos].globKind == ggkSeparator and
                  text[txtPos].globKind == ggkSeparator)):

              break caseBlock

            nodot = (not useDotglob) and glob[globPos].globKind == ggkSeparator;
            inc txtPos
            inc globPos
            continue

    if (glob1Backup != -1 and text[text1Backup].globKind != ggkSeparator):
      txtPos = (inc text1Backup; text1Backup)
      globPos = glob1Backup
      continue

    if (glob2Backup != -1):
      txtPos = (inc text2Backup; text2Backup)
      globPos = glob2Backup
      continue

    return false

  while (globPos < m and glob[globPos].globKind == ggkAnyStar):
    inc globPos

  return globPos >= m;
