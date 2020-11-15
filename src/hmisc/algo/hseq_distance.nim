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

      # echo &"Recursive test score: {recRes.score}, current: {bestRecursiveScore}"
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
  let currentScore = scoreFunc(patt, other, matches.mapIt(it.int))
  # echo &"Score: {currentScore}, matches: {matches}, best rec: {bestRecursiveScore} {bestRecursiveMatches}"

  # echo &"Full match: {fullMatch}, {pattIdx} == {patt.len}"
  if fullMatch:
    result.score = currentScore

  if hadRecursiveMatch and (not fullMatch or (bestRecursiveScore > currentScore)):
    # echo &"Recursive had better results: {bestRecursiveScore} > {currentScore}"
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
  LevEditKind = enum
    lekInsert
    lekReplace
    lekDelete

  LevEdit = object
    case kind: LevEditKind
      of lekInsert:
        insertPos: int
        insertItem: char
      of lekDelete:
        deletePos: int
      of lekReplace:
        replacePos: int
        replaceItem: char

func apply(str: var string, op: LevEdit) =
  case op.kind:
    of lekDelete:
      str.delete(op.deletePos, op.deletePos + 1)
    of lekReplace:
      str[op.replacePos] = op.replaceItem
    of lekInsert:
      str.insert($op.insertItem, op.insertPos)




proc levenshteinDistance(src, target: string): tuple[
  distance: int, operations: seq[LevEdit]] =
  var
    src = src
    target = target

  var matrix: seq[seq[int]] = newSeqWith(
    target.len + 1, newSeqWith(src.len + 1, 0))

  for col in 0 .. src.len:
    matrix[0][col] = col

  for row in 0 .. target.len:
    matrix[row][0] = row

  for row in 1 .. target.len:
    for col in 1 .. src.len:
      matrix[row][col] = min([
        matrix[row - 1][col] + 1,
        matrix[row][col - 1] + 1,
        matrix[row - 1][col - 1] + (
          if src[col - 1] == target[row - 1]: 0 else: 1)
      ])

  if haxRunning():
    echo "@[   ", src.toSeq().join(", ")
    echo matrix.join("\n")

  result.distance = matrix[^1][^1]

  var row = 0
  var col = 0
  while (row < matrix.len) and (col < matrix.len):
    let
      diagonal = matrix[row + 1][col + 1]
      vertical = matrix[row + 1][col]
      horizontal = matrix[row][col + 1]
      current = matrix[row][col]

    var res = LevEdit(kind: lekDelete, deletePos: -420)
    if diagonal >= vertical and
       diagonal >= horizontal and
       diagonal >= current
      :
      inc row
      inc col
      if diagonal == current + 1:
        res = LevEdit(
          kind: lekReplace, replacePos: col, replaceItem: target[row]
        )
        echov "rep", &"Replace '{src[col]}'({col}) with '{target[row]}'({row})"
      elif horizontal >= vertical and
           horizontal >= current
        :
        inc col
        res = LevEdit(
          kind: lekInsert, insertPos: col, insertItem: src[col])
        echov "ins", &"Insert '{src[col]}' at {col}"
      elif vertical >= horizontal and
           vertical >= current
        :
        inc row
        res = LevEdit(kind: lekDelete, deletePos: row)
        echov "del", &"Delete at pos {res.deletePos} ({target[row]})"
    elif horizontal >= vertical and
         horizontal >= current
      :
      inc col
      res = LevEdit(kind: lekInsert, insertPos: col,
                    insertItem: src[col])
      echov "ins", &"Insert '{src[col]}' at {col}"
    elif vertical >= horizontal and
         vertical >= current
      :
      inc row
      res = LevEdit(kind: lekDelete, deletePos: row)
      echov "del", &"Delete at pos {res.deletePos} ({target[row]})"
    else:
      raiseAssert("#[ IMPLEMENT ]#")

    echov (row, col), &" - ({target}), ({src})"
    if not (res.kind == lekDelete and res.deletePos == -420):
      result.operations.add res
    else:
      echov "skip"

  reverse(result.operations)

when isMainModule:
  let tests = {
    "interest" : "industry",
    "ros" : "horse",
    "horse" : "ros"
  }

  for (src, target) in tests:
    var ins = src
    let (dist, ops) = levenshteinDistance(src, target)

    var ok = false
    try:
      for op in ops:
        ins.apply(op)

      ok = true
    except:
      echo getCurrentExceptionMsg()


    if ins != target or (not ok):
      startHax()
      let (dist, ops) = levenshteinDistance(src, target)
      ins = src
      for op in ops:
        echo ins
        ins.apply(op)

      echo ins

      stopHax()
      quit 1
