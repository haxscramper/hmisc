import sequtils, tables, strformat, strutils, math, algorithm
import ../hdebug_misc

## Sequence distance metrics

# TODO performance benchmars for fuzzy string matching - no copying
#      should occur

# TODO add custom `cmp` proc for fuzzy matching instead of using `==`

type EqCmpProc[T] = proc(x, y: T): bool

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
  var mem: CountTable[(int, int)]
  proc lcs(i, j: int): int =
    if (i, j) notin mem:
      mem[(i, j)] =
        if i == -1 or j == -1:
          0
        elif itemCmp(x[i], y[j]):
          lcs(i - 1, j - 1) + 1
        else:
          max(
            lcs(i, j - 1),
            lcs(i - 1, j)
          )

    mem[(i, j)]

  let
    m = x.len - 1
    n = y.len - 1

  # echov "_-----_"

  proc backtrack(i, j: int): seq[
    tuple[matches: seq[T], xIndex, yIndex: seq[int]]
  ] =
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



proc fuzzyMatchImpl[Seq, Item](
  patt, other: Seq,
  matchScore: proc(patt, other: Seq, matches: seq[int]): int,
  eqCmp: EqCmpProc[Item]): tuple[ok: bool, score: int, matches: seq[int]] =
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
  eqCmp: EqCmpProc[T]): tuple[ok: bool, score: int, matches: seq[int]] =
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
                  ): tuple[ok: bool, score: int, matches: seq[int]] =

  fuzzyMatchImpl[openarray[T], T](
    patt, other, matchScore,
    (proc(a, b: T): bool = a == b)
  )

proc fuzzyMatch*(
  patt, other: string,
  matchScore: proc(patt, other: string, matches: seq[int]): int
                 ): tuple[ok: bool, score: int, matches: seq[int]] =
  ## Fuzzy match overload for strings
  fuzzyMatchImpl[string, char](
    patt, other, matchScore, (proc(a, b: char): bool = a == b))

type
  LevEditKind* = enum
    lekInsert
    lekReplace
    lekDelete

  LevEdit* = object
    case kind: LevEditKind
      of lekInsert:
        insertPos: int
        insertItem: char
      of lekDelete:
        deletePos: int
      of lekReplace:
        replacePos: int
        replaceItem: char

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

  # m = @[toSeq(0 .. l1)]
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

  echov levenpath
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

    # echov m.join("\n")
    echov last
    echov cur
    # levenpath[i] = cur


  echov m.join("\n")
  echov levenpath
  # return { matrix: m, levenpath: levenpath };

type
  AlignElem[T] = object
    case isGap: bool
      of true:
        discard
      of false:
        idx: int
        item: T


proc needlemanWunschAlign*[T](
  seq1, seq2: seq[T],
  gapPenalty: int,
  matchScore: proc(a, b: T): int,
     ): tuple[seq1Align, seq2Align: seq[AlignElem[T]]] =
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

  var
    align1: seq[AlignElem[T]]
    align2: seq[AlignElem[T]]

  var (i, j) = (seq2.len, seq1.len)
  while i > 0 and j > 0:
    let
      curr = score[i][j]
      diag = score[i - 1][j - 1]
      vert = score[i][j - 1]
      horiz = score[i - 1][j]

    if curr == diag + match_score(seq1[j - 1], seq2[i - 1]):
      align1 &= AlignElem[T](isGap: false, item: seq1[j - 1], idx: j - 1)
      align2 &= AlignElem[T](isGap: false, item: seq2[i - 1], idx: i - 1)

      dec i
      dec j

    elif curr == vert + gapPenalty:
      align1 &= AlignElem[T](isGap: false, item: seq1[j - 1], idx: j - 1)
      align2 &= AlignElem[T](isGap: true)

      dec j

    elif curr == horiz + gapPenalty:
      align1 &= AlignElem[T](isGap: true)
      align2 &= AlignElem[T](
        isGap: false, item: seq2[i - 1], idx: i - 1)

      dec i

  while j > 0:
    align1 &= AlignElem[T](isGap: false, item: seq1[j - 1], idx: j - 1)
    align2 &= AlignElem[T](isGap: true)

    dec j

  while i > 0:
    align1 &= AlignElem[T](isGap: true)
    align2 &= AlignElem[T](isGap: false, item: seq2[i - 1], idx: i - 1)

    dec i

  reverse(align1)
  reverse(align2)

  result.seq1Align = align1
  result.seq2Align = align2


when isMainModule:
  const
    gapPenalty = -1
    match_award = 1
    mismatchPenalty = -1

  let (a, b) = needlemanWunschAlign(
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
