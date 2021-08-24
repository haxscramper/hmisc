import std/[
  sequtils, strutils, math, lenientops, strformat,
  tables, sugar, options, unicode
]

import
  ./term_buf

import
  ../../algo/[halgorithm, hseq_mapping],
  ../../types/[seq2d, hprimitives, colorstring, geometry_primitives],
  ../../core/all

export term_buf, hprimitives

# import ../hcommon_converters
# export hcommon_converters
# import hterm_buf

#*************************************************************************#
#*************************  primitive rendering  *************************#
#*************************************************************************#

func renderLine(x0, y0, x1, y1: int, buf: var TermBuf,
                rune: ColoredRune): void =
  # de x0, y0, x1, y1
  let
    dx = x1 - x0
    dy = y1 - y0
    steps = if abs(dx) > abs(dy): abs(dx) else: abs(dy)
    xInc = dx / float(steps)
    yInc = dy / float(steps)

  var
    x = x0
    y = y0

  for i in 0 .. steps:
    when not defined(nimdoc):
      buf[x, y] = rune
      x += xInc.int
      y += yInc.int

func renderLine*(p: Point[int], w, h: int, buf: var TermBuf,
                 c: ColoredRune): void =
  renderLine(
    p.x,
    p.y,
    p.x + w,
    p.y + h,
    buf,
    c
  )

#*************************************************************************#
#********************************  Shape  ********************************#
#*************************************************************************#

type
  Shape* = ref object of RootObj

method render*(s: Shape, buf: var TermBuf): void {.base.} =
  raiseAssert("Cannot draw base shape implementation")

#=============================  Multishape  ==============================#

type
  Multishape* = ref object of Shape
    shapes: seq[Shape]


func newMultishape(shapes: seq[Shape]): Multishape =
  Multishape(shapes: shapes)

func nthShape*(mshape: Multishape, idx: int = 0): Shape =
  mshape.shapes[idx]

method render*(multi: Multishape, buf: var TermBuf): void =
  for shape in multi.shapes:
    render(shape, buf)

#==============================  Term line  ==============================#

type
  SLine*[T, Num] = ref object of Shape
    start: Point[Num]
    length: Num
    angle: Radian
    config: T

method render*(line: SLine[char, int], buf: var TermBuf): void =
  renderLine(
    x0 = line.start.x,
    y0 = line.start.y,
    x1 = int(line.start.x + cos(line.angle) * line.length),
    y1 = int(line.start.y + sin(line.angle) * line.length),
    buf = buf,
    rune = initColoredRune(line.config)
  )

func newTermVline*(
  start: (int, int), length: int, c: char = '|', isDown: bool = true): auto =
  SLine[char, int](
    angle: Radian(if isDown: (PI/2) else: 3 * (PI/2)),
    config: c,
    start: start.makePoint(),
    length: length
  )

func newTermHline*(
  start: (int, int), length: int, c: char = '-', isRight: bool = true): auto =
  SLine[char, int](
    angle: Radian(if isRight: PI*0 else: PI),
    config: c,
    start: start.makePoint(),
    length: length)


#=============================  Term point  ==============================#

type
  SPoint*[T, Num] = ref object of Shape
    point: Point[Num]
    config: T

func x*[T, Num](p: SPoint[T, Num]): Num = p.point.x
func y*[T, Num](p: SPoint[T, Num]): Num = p.point.y

func newTermPoint*(start: (int, int), c: char = '+'): SPoint[char, int] =
  SPoint[char, int](point: start.makePoint(), config: c)

method render*(point: SPoint[char, int], buf: var TermBuf): void =
  when not defined(nimdoc):
    buf[point.point] = toColoredRune(point.config)

#==============================  Term rect  ==============================#

type
  RectPoint* = enum
    rpoLeftEdge
    rpoRightEdge
    rpoBottomEdge
    rpoTopEdge
    rpoTopLeft
    rpoTopRight
    rpoBottomLeft
    rpoBottomRight

  SRect*[T, Num] = ref object of Shape
    start: Point[Num]
    height: Num
    width: Num
    config: T

  TermRectConf* = array[RectPoint, ColoredRune]
  TermRect* = SRect[TermRectConf, int]

func makeTwoLineRectBorder*(): TermRectConf =
  toMapArray(procIt[Rune](initColoredRune), {
    rpoLeftEdge:    uc"║",
    rpoRightEdge:   uc"║",
    rpoBottomEdge:  uc"═",
    rpoTopEdge:     uc"═",
    rpoTopLeft:     uc"╔",
    rpoTopRight:    uc"╗",
    rpoBottomLeft:  uc"╚",
    rpoBottomRight: uc"╝",
  })


func makeAsciiRectBorder*(): TermRectConf =
  toMapArray(procIt[Rune](initColoredRune), {
    rpoLeftEdge:    uc"|",
    rpoRightEdge:   uc"|",
    rpoBottomEdge:  uc"-",
    rpoTopEdge:     uc"-",
    rpoTopLeft:     uc"+",
    rpoTopRight:    uc"+",
    rpoBottomLeft:  uc"+",
    rpoBottomRight: uc"+",
  })

func newTermRect*(
  start: (int, int),
  width, height: int, border: char = '+'): SRect[char, int] =
  SRect[char, int](
    start: makePoint(start[0], start[1]),
    config: border,
    width: width,
    height: height
  )

func newTermRect*(
  start: (int, int), width, height: int, conf: TermRectConf): TermRect =
    TermRect(
      start: start.makePoint(),
      height: height,
      width: width,
      config: conf
    )

method render*(rect: SRect[char, int], buf: var TermBuf): void =
  let maxp = rect.start.shiftXY(rect.width, rect.height)
  buf.reserve(maxp.y, maxp.x)
  renderLine(
    rect.start, rect.width, 0, buf, rect.config.toColored())
  renderLine(
    rect.start, 0, rect.height - 1, buf, rect.config.toColored())

  renderLine(
    rect.start.shiftY(rect.height - 1), rect.width, 0, buf,
    rect.config.toColored())
  renderLine(
    rect.start.shiftX(rect.width), 0, rect.height - 1, buf,
    rect.config.toColored())

func rectRenderAux(
  start: Point[int], height, width: int, config: TermRectConf,
  buf: var TermBuf): void =

  let h = height
  let w = width

  if config[rpoLeftEdge].isValid():
    renderLine(
      start, 0, h - 1, buf, config[rpoLeftEdge])

  if config[rpoRightEdge].isValid():
    renderLine(
      start.shiftX(w - 1), 0, h - 1, buf, config[rpoRightEdge])

  if config[rpoTopEdge].isValid():
    renderLine(start, w - 1, 0, buf, config[rpoTopEdge])

  if config[rpoBottomEdge].isValid():
    renderLine(start.shiftY(h - 1), w - 1, h, buf, config[rpoBottomEdge])

  if config[rpoTopLeft].isValid():
    when not defined(nimdoc):
      buf[start.shiftXY(0, 0)] = config[rpoTopLeft]

  if config[rpoTopRight].isValid():
    when not defined(nimdoc):
      buf[start.shiftXY(w - 1, 0)] = config[rpoTopRight]

  if config[rpoBottomLeft].isValid():
    when not defined(nimdoc):
      buf[start.shiftXY(0, h - 1)] = config[rpoBottomLeft]

  if config[rpoBottomRight].isValid():
    when not defined(nimdoc):
      buf[start.shiftXY(w - 1, h - 1)] = config[rpoBottomRight]


method render*(rect: TermRect, buf: var TermBuf): void =
  rectRenderAux(
    rect.start,
    rect.height,
    rect.width,
    rect.config,
    buf
  )


#==============================  Term text  ==============================#

type
  SText*[Num] = ref object of Shape
    start: Point[Num]
    case reflow: bool
      of true:
        text: string
        maxWidth: Num
        maxHeight: Num
      of false:
        lines: TermBuf

func width*(text: SText[int]): int =
  if text.reflow:
    text.maxWidth
  else:
    text.lines.width()

func height*(text: SText[int]): int =
  if text.reflow:
    text.maxHeight
  else:
    text.lines.height()

func newTermText*(start: (int, int), text: RuneBlock): SText[int] =
  # debugecho start
  SText[int](
    start: makePoint(start[0], start[1]),
    lines: text.toTermBuf(),
    reflow: false
  )

func newTermText*(start: (int, int), text: TermBuf): SText[int] =
  SText[int](
    start: makePoint(start[0], start[1]),
    lines: text,
    reflow: false
  )

func newBoxedTermText*(
  start: (int, int), text: seq[RuneSeq], boxc: char = '#'): Multishape =
  let inner = newTermText(start.shiftXY(1, 1), text)
  Multishape(shapes: @[
    cast[Shape](inner),
    newTermRect(
      start, width = inner.width + 2, height = inner.height + 2, border = boxc)
  ])

func newBoxedTermText*(
  start: (int, int),
  text: seq[RuneSeq],
  conf: TermRectConf,
  size: (int, int) = (-1, -1)): Multishape =
  let inner = newTermText(start.shiftXY(1, 1), text)
  Multishape(shapes: @[
    cast[Shape](inner),
    newTermRect(
      start,
      width = (size[0] == -1).tern(inner.width + 2, size[0]),
      height = (size[1] == -1).tern(inner.height + 2, size[1]),
      conf
    )
  ])

method render*(text: SText[int], buf: var TermBuf): void =
  case text.reflow:
    of true:
      raiseAssert("reflow text is not implemented")
    of false:
      text.lines.renderOnto(buf, text.start)
      # for rId, row in text.lines:
      #   for cId in 0 ..< min(row.len, text.width):
      #     # echo &"{text.start}, ({cId}, {rId}), {text.start.shiftXY(cId, rId)}"
      #     buf[text.start.shiftXY(cId, rId)] = row[cId]


#==============================  Term grid  ==============================#

type
  GridPoint* = enum
    gpoIntersection
    gpoTopLeft
    gpoTopRight
    gpoBottomLeft
    gpoBottomRight
    gpoLeftBorder
    gpoLeftIntersection
    gpoRightBorder
    gpoRightIntersection
    gpoTopBorder
    gpoTopIntersection
    gpoBottomBorder
    gpoBottomIntersection
    gpoHorizontalGap
    gpoVerticalGap

  SGrid*[T, Num] = ref object of Shape
    start: Point[Num]
    cellWidths: seq[Num]
    cellHeights: seq[Num]
    config: T

  TermGridConf* = array[GridPoint, ColoredRune]
  TermGrid* = SGrid[TermGridConf, int]



func makeThinLineGridBorders*(
  styling: PrintStyling = initPrintStyling()): TermGridConf =
  toMapArray(procIt[Rune](initColoredRune, styling), {
    gpoIntersection : uc"┼",
    gpoTopLeft : uc"┌",
    gpoTopRight : uc"┐",
    gpoBottomLeft : uc"└",
    gpoBottomRight : uc"┘",
    gpoLeftBorder : uc"│",
    gpoLeftIntersection : uc"├",
    gpoRightBorder : uc"│",
    gpoRightIntersection : uc"┤",
    gpoTopBorder : uc"─",
    gpoTopIntersection : uc"┬",
    gpoBottomBorder : uc"─",
    gpoBottomIntersection : uc"┴",
    gpoHorizontalGap : uc"─",
    gpoVerticalGap : uc"│",
  })


func makeAsciiGridBorders*(
  styling: PrintStyling = initPrintStyling()): TermGridConf =
  toMapArray(procIt[Rune](initColoredRune, styling), {
    gpoIntersection : uc"+",
    gpoTopLeft: uc"+",
    gpoTopRight: uc"+",
    gpoBottomLeft: uc"+",
    gpoBottomRight: uc"+",
    gpoLeftBorder: uc"|",
    gpoLeftIntersection: uc"+",
    gpoRightBorder: uc"|",
    gpoRightIntersection: uc"+",
    gpoTopBorder: uc"-",
    gpoTopIntersection: uc"+",
    gpoBottomBorder: uc"-",
    gpoBottomIntersection: uc"+",
    gpoHorizontalGap: uc"-",
    gpoVerticalGap: uc"|",
  })

func spacingDimensions*(rc: TermGridConf): tuple[
  vSpacing, hSpacing, left, right, top, bottom: int] =
  result.vSpacing = (
    (rc[gpoHorizontalGap].isValid()) or
    (rc[gpoLeftIntersection].isValid()) or
    (rc[gpoRightIntersection].isValid)
  ).tern(1, 0)

  result.hSpacing = (
    (rc[gpoVerticalGap].isValid()) or
    (rc[gpoTopIntersection].isValid()) or
    (rc[gpoBottomIntersection].isValid())
  ).tern(1, 0)


func gridDimensions*(grid: TermGrid): tuple[
  vSpacing, hSpacing, totalW, totalH: int] =
  let rc = grid.config
  let (vSpacing, hSpacing, _, _, _, _) = rc.spacingDimensions()

  result.vSpacing = vSpacing
  result.hSpacing = hSpacing
  result.totalW = grid.cellWidths.sumjoin(result.vSpacing)
  result.totalH = grid.cellHeights.sumjoin(result.hSpacing)



func makeEmptyGridBorders*(): TermGridConf = discard

func newTermGrid*(
  start: (int, int),
  cellws: seq[int],
  cellhs: seq[int],
  conf: TermGridConf): TermGrid =
  TermGrid(
    start: start.makePoint(),
    cellWidths: cellws,
    cellHeights: cellhs,
    config: conf
  )


func newTermGrid*(
  start: (int, int), cells: Seq2d[TermBuf],
  conf: TermGridConf): Multishape =
  let cellws: seq[int] = cells.maximizeColIt: it.width()
  let cellhs: seq[int] = cells.maximizeRowIt: it.height()

  let grid = newTermGrid(start, cellws, cellhs, conf)
  let (_, _, left, right, top, bottom) = spacingDimensions(conf)
  let (vSpacing, hSpacing, totalW, totalH) = gridDimensions(grid)
  let absColPos: seq[int] = grid.cellWidths.cumsumjoin(hSpacing, true)
  let absRowPos: seq[int] = grid.cellHeights.cumsumjoin(vSpacing, true)
  # echov hSpacing
  # echov left
  let cellShapes: seq[Shape] = collect(newSeq):
    for (pos, cell) in cells.itercells():
      Shape(newTermText(
        start = (
          start[0] + absColPos[pos[1]] + left + hSpacing,
          start[1] + absRowPos[pos[0]] + top + vSpacing
        ), cell))

  newMultishape(@[Shape(grid)] & cellShapes)

func newTermGridVert*(
  cells: seq[string] | seq[StrBlock], sep: char = '-'): Multishape =
  newTermGrid((0, 0), cells.mapIt(@[it.toTermBuf()]).makeSeq2D(), toMapArray {
    gpoHorizontalGap: toColored(sep)
  })

func newTermGridHoriz*(
  cells: seq[string] | seq[StrBlock], sep: char = '|'): Multishape =
  newTermGrid((0, 0), @[cells.mapIt(it.toTermBuf())].makeSeq2D(), toMapArray {
    gpoVerticalGap: toColored(sep)
  })

func gridRenderAux(rect: TermGrid, buf: var TermBuf): void =
  let gridX = rect.start.x
  let gridY = rect.start.y
  let rc = rect.config
  let (vSpacing, hSpacing, totalW, totalH) = gridDimensions(rect)

  block outerBorder:
    if rc[gpoTopBorder].isValid():
      renderLine(
        x0 = gridX, x1 = gridX + totalW,
        y0 = gridY, y1 = gridY,
        buf, rc[gpoTopBorder])

    if rc[gpoBottomBorder].isValid():
      renderLine(
        x0 = gridX, x1 = gridX + totalW,
        y0 = gridY + totalH + 1, y1 = gridY + totalH + 1,
        buf, rc[gpoBottomBorder])

    if rc[gpoLeftBorder].isValid():
      renderLine(
        x0 = gridX, x1 = gridX,
        y0 = gridY, y1 = gridY + totalH,
        buf, rc[gpoLeftBorder])

    if rc[gpoRightBorder].isValid():
      renderLine(
        x0 = gridX + totalW + 1, x1 = gridX + totalW + 1,
        y0 = gridY, y1 = gridY + totalH,
        buf, rc[gpoRightBorder])

    if rc[gpoTopLeft].isValid():
      buf[gridX, gridY] = rc[gpoTopLeft]

    if rc[gpoBottomLeft].isValid():
      buf[gridX, gridY + totalH + 1] = rc[gpoBottomLeft]

    if rc[gpoTopRight].isValid():
      buf[gridX + totalW + 1, gridY] = rc[gpoTopRight]

    if rc[gpoBottomRight].isValid():
      buf[gridX + totalW + 1, gridY + totalH + 1] = rc[gpoBottomRight]


  block inerGrid:
    if vSpacing == 1:
      for row in rect.cellHeights.cumsumjoin(vSpacing)[0..^2]:
        if rc[gpoHorizontalGap].isValid():
          renderLine(
            y0 = row + gridY, y1 = row + gridY,
            x0 = 0 + gridX, x1 = totalW + gridX,
            buf,
            rc[gpoHorizontalGap])

        if rc[gpoLeftIntersection].isValid():
          buf[gridX, gridY + row] = rc[gpoLeftIntersection]

        if rc[gpoRightIntersection].isValid():
          buf[gridX + totalW + 1, gridY + row] = rc[gpoRightIntersection]


    if hSpacing == 1:
      for col in rect.cellWidths.cumsumjoin(vSpacing)[0..^2]:
        if rc[gpoVerticalGap].isValid():
          renderLine(
            y0 = gridY, y1 = gridY + totalH,
            x0 = gridX + col, x1 = gridX + col,
            buf,
            rc[gpoVerticalGap]
          )

        if rc[gpoTopIntersection].isValid():
          buf[gridX + col, gridY] = rc[gpoTopIntersection]

        if rc[gpoBottomIntersection].isValid():
          buf[gridX + col, gridY + totalH + 1] = rc[gpoBottomIntersection]

    if rc[gpoIntersection].isValid():
      for row in rect.cellHeights.cumsumjoin(vSpacing)[0..^2]:
        for col in rect.cellWidths.cumsumjoin(hSpacing)[0..^2]:
          buf[gridX + col, gridY + row] = rc[gpoIntersection]


method render*(rect: TermGrid, buf: var TermBuf): void =
  gridRenderAux(rect, buf)

#===========================  multicell grid  ============================#

type
  SMulticellGrid[T, Num] = ref object of SGrid[T, Num]
    # start: Point[Num]
    cells: Seq2D[Option[ArrSize]]
    # config: T
    # cellWidths: seq[Num]
    # cellHeights: seq[Num]

  TermMultiGrid* = SMultiCellGrid[TermGridConf, int]

func newTermMultiGrid*(
  start: (int, int),
  cells: Seq2D[Option[ArrSize]],
  widths: seq[int],
  heights: seq[int],
  config: TermGridConf): TermMultiGrid =
  assert cells.len == heights.len, &"Mismatch in number of rows: cell sizes has {cells.len} " &
    &"rows, but heights has {heights.len}"
  for idx, row in cells:
    assert row.len == widths.len, &"Mismatch in number of columns: row {idx} has {row.len} " &
      &"elements, buf widths has {widths.len}"

  TermMultiGrid(
    start: start.makePoint(),
    cells: cells,
    cellWidths: widths,
    cellHeights: heights,
    config: config)

func getSizes(
  grid: Seq2D[Option[(ArrSize, TermBuf)]],
  vertSpacing, horSpacing: int = 0): tuple[widths, heights: seq[int]] =
  var rowHs: seq[int] = newSeqWith(grid.rowNum, 0)
  var colWs: seq[int] = newSeqWith(grid.colNum, 0)

  for (pos, cell) in grid.iterSomeCells():
    if cell[0] == size1x1:
      let text = cell[1]
      let (row, col) = pos
      colWs[col].setMax(text.width)
      rowHs[row].setMax(text.height)

  for (pos, cell) in grid.iterSomeCells():
    if cell[0] != size1x1:
      let
        (size, text) = cell
        rowRange: ArrRange = pos.makeArrPos().rowRange(size)
        colRange: ArrRange = pos.makeArrPos().colRange(size)
        rowHsum: int = sumjoin(rowHs, rowRange, vertSpacing)
        colWsum: int = sumjoin(colWs, colRange, horSpacing)
        textW: int = text.width
        textH: int = text.height

      if colWsum < textW:
        let diff = textW - colWsum
        let (val, rem) = diff.modiv(colRange.len)
        for col in colRange:
          colWs[col] += val # NOTE for now widths is just increased on
          # left side, but it is possible to provide more sophisticaed
          # layout - adding left padding for shifted cells (for
          # centering origincal content in the cell), determining how
          # cell width should be modified (just increase width for
          # each cell or make last/first one larger etc).

        colWs[colRange.b] += rem

      if rowHsum < textH:
        let diff = textH - rowHsum
        let (val, rem) = diff.modiv(rowRange.len)
        for row in rowRange:
          rowHs[row] += val

        rowHs[rowRange.b] += rem

  result.widths = colWs
  result.heights = rowHs

func newTermMultiGrid*(
  start: (int, int),
  blocks: Seq2D[Option[(ArrSize, TermBuf)]], config: TermGridConf): Multishape =
  let (cellws, cellhs) = getSizes(blocks, 1, 1 #[ IMPLEMENT pass vert spacing ]#)
  let grid = newTermMultiGrid(
    start = start,
    cells = blocks.mapIt2d(it.isSome().tern(some(it.get[0]), none(ArrSize))),
    widths = cellws,
    heights = cellhs,
    config = config
  )

  let (vertSpacing, horSpacing, totalW, totalH) = gridDimensions(grid)
  var res: seq[Shape] = @[Shape(grid)]

  let absCellX: seq[int] = cellws.cumsumjoin(horSpacing, true)
  let absCellY: seq[int] = cellhs.cumsumjoin(vertSpacing, true)

  for (pos, cell) in blocks.iterSomeCells():
    let (size, buf) = cell
    let (row, col) = pos
    res.add newTermText((absCellX[col] + 1, absCellY[row] + 1), buf)

  return newMultishape(res)

method render*(grid: TermMultiGrid, buf: var TermBuf): void =
  gridRenderAux(TermGrid(grid), buf)

  let
    x0 = grid.start.x
    y0 = grid.start.y
    rc = grid.config

  let (vertSpacing, horSpacing, totalW, totalH) = gridDimensions(grid)
  let absCellX: seq[int] = grid.cellWidths.cumsumjoin(horSpacing, true)
  let absCellY: seq[int] = grid.cellHeights.cumsumjoin(vertSpacing, true)

  for (pos, size) in grid.cells.iterSomeCells:
    # de size, pos
    if size != size1x1:
      let (row, col) = pos
      let
        x = absCellX[col]
        y = absCellY[row]
        width = absCellX[col + size.width] - absCellX[col]
        height = absCellY[row + size.height] - absCellY[row]
        wRange = makeArrRange(x + 1, x + width - 1)
        hRange = makeArrRange(y + 1, y + height - 1)

      block: # Remove things in grid
        for (x, y) in (wRange, hRange):
          buf[x, y] = toColoredRune(' ')

      block: # Fix intersections
        for x in absCellX:
          for y in absCellY:
            if (y + 1 == hRange.a) and (x in wRange):
              if (y == y0) and rc[gpoTopBorder].isValid():
                buf[x, y] = rc[gpoTopBorder]
              elif rc[gpoBottomIntersection].isValid():
                buf[x, y] = rc[gpoBottomIntersection]

            if (x + 1 == wRange.a) and (y in hRange):
              if x == x0 and rc[gpoLeftBorder].isValid():
                buf[x, y] = rc[gpoLeftBorder]
              elif rc[gpoRightIntersection].isValid():
                buf[x, y] = rc[gpoRightIntersection]

            if (x - 1 == wRange.b) and (y in hRange):
              if (x - 1) == totalW and rc[gpoRightBorder].isValid():
                buf[x, y] = rc[gpoRightBorder]
              elif rc[gpoLeftIntersection].isValid():
                buf[x, y] = rc[gpoLeftIntersection]

            if (y - 1 == hRange.b) and (x in wRange):
              if (y - 1) == totalH and rc[gpoBottomBorder].isValid():
                buf[x, y] = rc[gpoBottomBorder]
              elif rc[gpoTopIntersection].isValid():
                buf[x, y] = rc[gpoTopIntersection]

      block:
        let lookup = makeLookup(grid.cells)
        for (pos, size) in grid.cells.iterSomeCells():
          for (cellPos, cellSize, relPos) in lookup.cellsAround(pos):
            let rowOverlap = cellPos.rowRange(cellSize).
              overlap(pos.rowRange(size))

            let colOverlap = cellPos.colRange(cellSize).
              overlap(pos.colRange(size))

            let rowRange = cellPos.rowRange(cellSize)
            let colRange = cellPos.colRange(cellSize)

            if rowOverlap.len > 1:
              let (minColX, maxColX) = colRange.unpack()
              # NOTE this is under-implmenented: need to cover all
              # cases (cell on top/bottom and left/right).
              for rowY in absCellY.inrange(rowOverlap, 1):
                case relPos:
                  of rpRight:
                    if rc[gpoVerticalGap].isValid():
                      buf[absCellX[maxColX - 1], rowY] = rc[gpoVerticalGap]
                  else:
                    discard

            if colRange.len > 1:
              let (minRow, maxRow) = rowRange.unpack()
              for colX in absCellX.inrange(colOverlap, 1):
                case relPos:
                  of rpBottom:
                    if rc[gpoHorizontalGap].isValid():
                      buf[colX, absCellY[maxRow]] = rc[gpoHorizontalGap]
                  else:
                    discard



#==============================  ---------  ==============================#

func toStringBlock*(shape: Shape): seq[string] =
  var buf = newBuf()

  {.cast(noSideEffect).}:
    shape.render(buf)

  return buf.toStringBlock()

func toStringBlock*(grid: Seq2D[string]): seq[string] =
  discard

func toTermBuf*(shape: Shape): TermBuf =
  var buf = newBuf()

  {.cast(noSideEffect).}:
    shape.render(buf)

  return buf

# func toRunes*(s: seq[string]): seq[RuneSeq] =
#   s.mapIt(unicode.toRunes(it))

# func toRunes*(s: seq[seq[string]]): seq[seq[RuneSeq]] =
#   s.mapIt(it.mapIt(unicode.toRunes(it)))

# func toRunes*(s: seq[seq[seq[string]]]): seq[seq[seq[RuneSeq]]] =
#   s.mapIt(it.mapIt(it.mapIt(unicode.toRunes(it))))
