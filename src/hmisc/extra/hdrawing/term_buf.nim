import
  hmisc/types/[seq2d, hprimitives, colorstring, geometry_primitives],
  hmisc/algo/[hmath, htemplates]

import std/[unicode, sequtils, strutils, enumerate]

#*************************************************************************#
#***************************  terminal buffer  ***************************#
#*************************************************************************#

type
  TermTextConf* = object
    nil

  TermBuf* = object
    buf: Seq2d[ColoredRune]
    xDiff: int
    yDiff: int

const emptyTermBuf*: TermBuf = TermBuf()


func width*(buf: TermBuf): int = buf.buf.colNum()
func height*(buf: TermBuf): int = buf.buf.rowNum()


#============================  constructors  =============================#

func toColored*(grid: Seq2D[Rune]): Seq2D[ColoredRune] =
  grid.mapIt2D(initColoredRune(it))

func toTermBufFast*(str: string): TermBuf =
  ## Create new term buffer without checking for newlines.
  ##
  ## WARN: use only when there is no newlines or escape sequences in
  ## input string, otherwise it will look like garbage in terminal.
  TermBuf(buf: makeSeq2D(str.toRunes()).toColored())

func makeTermBuf*(w = 2, h = 1): TermBuf =
  result.buf.fillToSize(rows = h, cols = 2, coloredWhitespaceRune)

func toTermBuf*(str: string): TermBuf =
  TermBuf(buf: str.toColoredRuneGrid().makeSeq2D(coloredWhitespaceRune))


# func toTermBuf*(rows: seq[string]): TermBuf =
#   TermBuf(buf: rows.mapIt(it.toRunes()).makeSeq2D(whitespaceRune))

func toTermBuf*(strs: seq[seq[string]]): TermBuf =
  TermBuf(buf: strs.mapIt(it.toRunes().concat()
  ).makeSeq2D(whitespaceRune).toColored())

func toTermBufGrid*(strs: seq[seq[string]]): Seq2D[TermBuf] =
  strs.makeSeq2D("").mapIt2D(it.toTermBuf())
  # TermBuf(buf: strs.mapIt(it.toRunes().concat()).makeSeq2D(whitespaceRune))

func toTermBufGrid*(strs: Seq2D[string]): Seq2D[TermBuf] =
  strs.mapIt2D(it.toTermBuf())

func toTermBuf*(bufs: Seq2D[TermBuf]): TermBuf

func toTermBuf*(bufs: seq[seq[TermBuf]]): TermBuf =
  ## Merge multiple string buffers together
  toTermBuf(makeSeq2D(bufs, emptyTermBuf))

func toTermBuf*(bufs: seq[TermBuf]): TermBuf =
  ## Merge multiple string buffers together
  toTermBuf(makeSeq2D(bufs))

func concatBufsLeft*(bufs: seq[TermBuf]): TermBuf =
  toTermBuf(@[bufs])

func concatBufsTop*(bufs: seq[TermBuf]): TermBuf =
  toTermBuf(bufs.mapIt(@[it]))



func toTermBuf*(strs: StrBlock): TermBuf =
  TermBuf(buf: strs.mapIt(it.toRunes()).makeSeq2D(
    whitespaceRune).toColored())

func toTermBuf*(strs: RuneBlock): TermBuf =
  TermBuf(buf: strs.makeSeq2D(whitespaceRune).toColored())

func newBuf*(offset: (int, int) = (0, 0)): TermBuf =
  TermBuf(xDiff: offset[0], yDiff: offset[1])

#==============================  accessors  ==============================#

func reserve*(buf: var TermBuf, rows, cols: int): void =
  buf.buf.fillToSize(
    makeArrSize(w = cols + 1, h = rows + 1),
    coloredWhitespaceRune)

func `[]=`*(buf: var Seq2D[ColoredRune], row, col: int, rune: Rune): void =
    buf[row, col] = initColoredRune(rune)

func setAtPoint(buf: var TermBuf, row, col: int, rune: ColoredRune): void =
  reserve(buf, row, col)
  buf.buf[row, col] = rune

func `[]=`*(buf: var TermBuf, row, col: int, text: string) =
  let text = text.split("\n")
  buf.reserve(
    rows = row + buf.yDiff + text.len() - 1,
    cols = col + buf.xDiff + maxIt(text, it.len()) - 1)

  for rowIdx, line in text:
    for colIdx, ch in enumerate(runes(line)):
      buf.buf[rowIdx + row, colIdx + col] = initColoredRune(ch)

func `[]=`*(buf: var TermBuf, x, y: int, rune: ColoredRune): void =
  let y = y + buf.yDiff
  let x = x + buf.xDiff
  buf.setAtPoint(y, x, rune)

func `[]=`*(buf: var TermBuf, x, y: int, rune: Rune): void {.inline.} =
  buf[x, y] = initColoredRune(rune)

func `[]=`*(buf: var TermBuf, pos: Point[int], c: ColoredRune) {.inline.} =
  buf[pos.x, pos.y] = c

func `[]=`*(buf: var TermBuf, pos: Point[int], c: Rune) {.inline.} =
  buf[pos.x, pos.y] = c

type TermGridAssign* = string | char | Rune | ColoredRune | ColoredString

func `[]=`*(
    buf: var TermBuf, row: int, col: BackwardsIndex, text: TermGridAssign) =
  buf[row, buf.width(row) - col.int] = text

func `[]=`*(
    buf: var TermBuf, row: BackwardsIndex, col: int, text: TermGridAssign) =
  buf[buf.height(row) - row.int, col] = text

#==============================  modifiers  ==============================#

func renderOnto*(buf: TermBuf, other: var TermBuf, pos: Point[int]): void =
  let (x, y) = pos.unpack()
  for row in 0 ..< buf.height:
    for col in 0 ..< buf.width:
      other[x + col, y + row] = buf.buf[row, col]


#=============================  converters  ==============================#

func toString*(buf: TermBuf): string =
  for idx, row in buf.buf:
    if idx != 0: result &= "\n"
    result &= row.toString(color = not defined(plainStdout))


func sideBySide*(str1, str2: string): string =
  concatBufsLeft(@[
    str1.toTermBuf(),
    makeTermBuf(w = 3, h = 1),
    str2.toTermBuf()
  ]).toString()

func sideBySide*(bufs: seq[string]): string =
  toTermBuf(bufs.mapIt(it.toTermBuf())).toString()

template withBufEcho*(body: untyped): untyped =
  var buf {.inject.} = newBuf()

  body

  echo buf.toString()


template withBufStr*(body: untyped): untyped =
  var buf {.inject.} = newBuf()

  body

  buf.toString()

func `$`*(buf: TermBuf): string =
     "cols: " & $buf.buf.usafeColNum() & ", elems: @[" &
       buf.buf.mapIt("\"" & $it & "\"").join(", ") & "]"

func toStringBlock*(buf: TermBuf): StrBlock = buf.buf.mapIt($it)

#==============================  compound  ===============================#

func toTermBuf*(bufs: Seq2D[TermBuf]): TermBuf =
  ## Merge multiple string buffers together
  if (bufs.colNum() == 0 or bufs.rowNum() == 0):
    return emptyTermBuf

  else:
    discard
    let cellws = bufs.maximizeColIt: it.width()
    let cellhs = bufs.maximizeRowIt: it.height()

    let absCellX = cellws.cumsumjoin(0, true, true)
    let absCellY = cellhs.cumsumjoin(0, true, true)

    result.reserve(rows = absCellY[^1], cols = absCellY[^1])
    for colIdx, cellX in absCellX:
      for rowIdx, cellY in absCellY:
        bufs[rowIdx, colIdx].renderOnto(result, makePoint(cellX, cellY))
