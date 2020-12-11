import unicode, strutils, sequtils, strformat
import ../algo/hmath

import colors
const colNoColor* = Color(-1)


#=========================================================================#
#===========================  String helpers  ============================#
#=========================================================================#

#============================  string block  =============================#

type
  StrBlock* = seq[string]

func toBlock*(s: string): StrBlock = s.split('\n')
func toBlock*(s: seq[string]): StrBlock = StrBlock(s)
func height*(s: StrBlock): int = s.len
func width*(s: StrBlock): int =
  if s.len == 0: 0
  else: s.mapIt(it.runeLen).max()


#============================  rune sequence  ============================#

type
  RuneSeq* = seq[Rune]
  RuneBlock* = seq[RuneSeq]

func toRunes*(s: StrBlock): RuneBlock =
  for line in s:
    result.add toRunes(line)

converter toRune*(c: char): Rune = toRunes($c)[0]

const whitespaceRune*: Rune = toRunes(" ")[0]
var emptyRune*: Rune


#=========================================================================#
#==========================  Sequence-related  ===========================#
#=========================================================================#

#================================  Size  =================================#

type
  ArrSize* = object
    width: int
    height: int

const size1x1* = ArrSize(width: 1, height: 1)
func width*(size: ArrSize): int = size.width
func height*(size: ArrSize): int = size.height
func makeArrSize*(w, h: int): ArrSize = ArrSize(width: w, height: h)
func makeArrSize*(widthHeight: (int, int)): ArrSize =
  ArrSize(width: widthHeight[0], height: widthHeight[1])


#================================  Range  ================================#

type
  ArrRange* = object
    a*: int
    b*: int

import hashes
func makeArrRange*(a, b: int): ArrRange = ArrRange(a: a, b: b)

func toRange*(a, b: int): ArrRange {.deprecated.} =
  ArrRange(a: a, b: b)

func toArrRange*(a, b: int): ArrRange = ArrRange(a: a, b: b)
func hash*(r: ArrRange): Hash = hash(r.a) !& hash(r.b)
func decRight*(r: ArrRange, diff: int = 1): ArrRange =
  ## Shift right side of range by one.
  assert r.b - diff >= r.a,
     &"Cannot shift right range side past left side: [{r.a}, {r.b}]" &
       &" -> [{r.a}, {r.b - diff}]"
  makeArrRange(r.a, r.b - diff)


func incLeft*(r: ArrRange, diff: int = 1): ArrRange =
  ## Shift left side of range by one.
  assert r.a + diff <= r.b,
     &"Cannot shift right range side past left side: [{r.a}, {r.b}]" &
       &" -> [{r.a + diff}, {r.b}]"
  makeArrRange(r.a + diff, r.b)

func contains*(r: ArrRange, item: int): bool =
  r.a <= item and item <= r.b

func middles*(r: ArrRange): int =
  ## Number of gaps in between range points
  (r.b - r.a - 1)

func isPoint*(r: ArrRange): bool =
  ## If range starts is equal to end
  (r.a == r.b)

func unpack*(r: ArrRange): (int, int) = (r.a, r.b)

func point*(r: ArrRange): int =
  assert r.isPoint()
  r.a

func sumjoin*(a: openarray[int], r: ArrRange, sep: int): int =
  sumjoin(a[r.a .. r.b], sep)

func isValid*(r: ArrRange): bool = r.b >= r.a
func overlap*(r1, r2: ArrRange): ArrRange =
  result = makeArrRange(max(r1.a, r2.a), min(r1.b, r2.b))

func `$`*(r: ArrRange): string = &"[{r.a}, {r.b}]"
func len*(r: ArrRange): int = r.b - r.a + 1

func contains*(sl: Slice[int], arr: ArrRange): bool =
  sl.a <= arr.a and arr.b <= sl.b

func hasOverlap*(a, b: ArrRange): bool =
  overlap(a, b).len > 0

func geCmpPositions*(lhs, rhs: ArrRange): bool {.inline.} =
  assert not hasOverlap(lhs, rhs),
     &"Cannot compare overlapping ranges: {lhs} > {rhs}"
  return lhs.b > rhs.a

func geqCmpPositions*(lhs, rhs: ArrRange): bool {.inline.} =
  assert not hasOverlap(lhs, rhs),
     &"Cannot compare overlapping ranges: {lhs} >= {rhs}"
  return lhs.b >= rhs.a

func leCmpPositions*(lhs, rhs: ArrRange): bool {.inline.} =
  assert not hasOverlap(lhs, rhs),
     &"Cannot compare overlapping ranges: {lhs} < {rhs}"
  return lhs.b < rhs.a

func leqCmpPositions*(lhs, rhs: ArrRange): bool {.inline.} =
  assert not hasOverlap(lhs, rhs),
     &"Cannot compare overlapping ranges: {lhs} <= {rhs}"
  return lhs.b <= rhs.a


iterator items*(r: ArrRange): int =
  for it in r.a .. r.b:
    yield it

iterator items*(r: (ArrRange, ArrRange)): (int, int) =
  for x in r[0].a .. r[0].b:
    for y in r[1].a .. r[1].b:
      yield (x, y)

iterator `[]`*[T](s: seq[T], r: ArrRange): T =
  for it in s[r.a .. r.b]:
    yield it

iterator inrange*(s: seq[int], r: ArrRange, lDiff, rDiff: int = 0): int =
  ## Iterate over all values between `s[r.a]` to `s[r.b]`. Shift
  ## left/right edge of the range by `l/rDiff` respectively.
  for v in (s[r.a] + lDiff) .. (s[r.b] + rDiff):
    yield v


#==============================  Position  ===============================#
# TODO is it possible to define custom unpacker for object? to write
#      `let (row, col) = Pos()`

type
  ArrPos* = object
    row*: int
    col*: int

func makeArrPos*(row, col: int): ArrPos = ArrPos(row: row, col: col)
func makeArrPos*(pos: (int, int)): ArrPos = ArrPos(row: pos[0], col: pos[1])
func toPos*(row, col: int): ArrPos = ArrPos(row: row, col: col)
func isValid*(pos: ArrPos): bool = (pos.row >= 0) and (pos.col >= 0)
func expandSize*(pos: ArrPos, size: ArrSize): ArrSize =
  makeArrSize(
    h = pos.row + size.height,
    w = pos.col + size.width
  )


func shiftRC*(pos: ArrPos, dRow: int = 1, dCol: int = 1): ArrPos =
  makeArrPos(pos.row + dRow, pos.col + dCol)

func shiftRc*(pos: ArrPos, dRC: (int, int) = (1, 1)): ArrPos =
  makeArrPos(pos.row + dRC[0], pos.col + dRC[1])

func shiftC*(pos: ArrPos, dCol: int = 1): ArrPos = shiftRC(pos, 0, dCol)
func shiftR*(pos: ArrPos, dRow: int = 1): ArrPos = shiftRC(pos, dRow, 0)
converter toArrPos*(pos: (int, int)): ArrPos = makeArrPos(pos)
func toArrSize*(pos: ArrPos): ArrSize =
  makeArrSize(w = pos.col + 1, h = pos.row + 1)
func unpack*(pos: ArrPos): tuple[row, col: int] = (pos.row, pos.col)
func `==`(a, b: ArrPos): bool = (a.row == b.row) and (a.col == b.col)

type
  RelPos* = enum
    rpLeft
    rpRight
    rpBottom
    rpTop

func toDiffRC*(rp: RelPos): (int, int) =
  case rp:
    of rpLeft: (0, -1)
    of rpRight: (0, 1)
    of rpBottom: (1, 0)
    of rpTop: (-1, 0)


#===============================  ArrRect  ===============================#

type
  ArrRect* = object
    pos*: ArrPos
    size*: ArrSize

func makeArrRect*(pos: (int, int), w, h: int): ArrRect =
  ArrRect(
    pos: ArrPos(row: pos[0], col: pos[1]),
    size: ArrSize(width: w, height: h)
  )

func makeArrRect*(pos: (int, int), size: ArrSize): ArrRect =
  ArrRect(pos: ArrPos(row: pos[0], col: pos[1]), size: size)

func makeArrRect*(pos: ArrPos, size: ArrSize): ArrRect =
  ArrRect(pos: pos, size: size)

func rowRange*(rect: ArrRect): ArrRange =
  makeArrRange(rect.pos.row, rect.pos.row + rect.size.height - 1)

func colRange*(rect: ArrRect): ArrRange =
  makeArrRange(rect.pos.col, rect.pos.col + rect.size.width - 1)

iterator itercells*(rect: ArrRect): (int, int) =
  for (row, col) in (rect.rowRange(), rect.colRange):
    yield (row, col)

#==============================  compound  ===============================#

func rowRange*(pos: ArrPos, size: ArrSize): ArrRange =
  ## Get *indices* of rows that multicell of `size` at `pos` would
  ## occupy
  makeArrRange(pos.row, pos.row + size.height - 1)

func colRange*(pos: ArrPos, size: ArrSize): ArrRange =
  ## Get *indices* of columns that multicell of `size` at `pos` would
  ## occupy
  makeArrRange(pos.col, pos.col + size.width - 1)



#*************************************************************************#
#****************************  Tree-related  *****************************#
#*************************************************************************#

type
  TreePath* = seq[int] ## Path into heterogenous, int-indexed tree. First
  ## element if the path is index of *current node*, everything else refers
  ## to position. So `[0]` points is a root node, `[0, 1]` is a second
  ## subnode and so on.

const rootTreePath*: TreePath = @[0]

func pathTail*(path: TreePath): TreePath = path[1..^1]

func `&`*(path: TreePath, newIdx: int): TreePath =
  path & @[newIdx]

func followPath*[T](node: T, path: TreePath): T =
  var res: T = node
  for step in path.pathTail():
    res = node[step]

  res

func followPathPtr*[T](node: T, path: TreePath): ptr T =
  result = node.unsafeAddr
  for step in path.pathTail():
    result = unsafeAddr result[][step]

func followPathPtr*[T](node: var T, path: TreePath): ptr T =
  result = addr node
  for step in path.pathTail():
    result = addr result[][step]


#*************************************************************************#
#*************************  Callback typedefs  ***************************#
#*************************************************************************#

type
  EqCmpProc*[T] = proc(x, y: T): bool
  ScoreProc*[T] = proc(x: T): int
  ScoreCmpProc*[T] = proc(x, y: T): int
