import seq2d, hprimitives, hgeometry_primitives
import unicode, sequtils, strutils
import ../algo/hmath

#*************************************************************************#
#***************************  terminal buffer  ***************************#
#*************************************************************************#

type
  TermTextConf* = object
    nil

  TermBuf* = object
    buf: Seq2d[Rune]
    xDiff: int
    yDiff: int

const emptyTermBuf*: TermBuf = TermBuf()


func width*(buf: TermBuf): int = buf.buf.colNum()
func height*(buf: TermBuf): int = buf.buf.rowNum()


#============================  constructors  =============================#

func toTermBufFast*(str: string): TermBuf =
  ## Create new term buffer without checking for newlines.
  ##
  ## WARN: use only when there is no newlines in input string,
  ## otherwise it will look like garbage in terminal.
  TermBuf(buf: makeSeq2D(str.toRunes()))

func toTermBuf*(str: string): TermBuf =
  TermBuf(buf: str.split("\n").mapIt(
    it.toRunes()).makeSeq2D(whitespaceRune))


# func toTermBuf*(rows: seq[string]): TermBuf =
#   TermBuf(buf: rows.mapIt(it.toRunes()).makeSeq2D(whitespaceRune))

func toTermBuf*(strs: seq[seq[string]]): TermBuf =
  TermBuf(buf: strs.mapIt(it.toRunes().concat()).makeSeq2D(whitespaceRune))

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

func toTermBuf*(strs: StrBlock): TermBuf =
  TermBuf(buf: strs.mapIt(it.toRunes()).makeSeq2D(whitespaceRune))

func toTermBuf*(strs: RuneBlock): TermBuf =
  TermBuf(buf: strs.makeSeq2D(whitespaceRune))

func newBuf*(offset: (int, int) = (0, 0)): TermBuf =
  TermBuf(xDiff: offset[0], yDiff: offset[1])

#==============================  accessors  ==============================#

func reserve*(buf: var TermBuf, rows, cols: int): void =
  buf.buf.fillToSize(
    makeArrSize(w = cols + 1, h = rows + 1),
    whitespaceRune)

func setAtPoint(buf: var TermBuf, row, col: int, rune: Rune): void =
  reserve(buf, row, col)
  buf.buf[row, col] = rune

func `[]=`*(buf: var TermBuf, x, y: int, rune: Rune): void =
  let y = y + buf.yDiff
  let x = x + buf.xDiff
  buf.setAtPoint(y, x, rune)

func `[]=`*(buf: var TermBuf, pos: Point[int], c: Rune): void =
  buf[pos.x, pos.y] = c


#==============================  modifiers  ==============================#

func renderOnto*(buf: TermBuf, other: var TermBuf, pos: Point[int]): void =
  let (x, y) = pos.unpack()
  for row in 0 ..< buf.height:
    for col in 0 ..< buf.width:
      other[x + col, y + row] = buf.buf[row, col]


#=============================  converters  ==============================#

func toString*(buf: TermBuf): string = buf.buf.join("\n")
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
    let cellws = bufs.maximizeColIt: it.width()
    let cellhs = bufs.maximizeRowIt: it.height()

    let absCellX = cellws.cumsumjoin(0, true, true)
    let absCellY = cellhs.cumsumjoin(0, true, true)

    result.reserve(rows = absCellY[^1], cols = absCellY[^1])
    for colIdx, cellX in absCellX:
      for rowIdx, cellY in absCellY:
        bufs[rowIdx, colIdx].renderOnto(result, makePoint(cellX, cellY))
