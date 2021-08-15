import tables, sequtils, sugar, options
import hmisc/types/[seq2d, hprimitives, geometry_primitives]

import ./term_buf, ./hdrawing
import hmisc/hdebug_misc
import hmisc/algo/[hseq_mapping, halgorithm]

## Nested table with multicol support

#===========================  type definition  ===========================#

type
  SizePolicy* = enum
    spExpanding
    spFixed

  GridCell* = object
    # TODO add support for cell configuration: additional annotations
    # etc.
    size: ArrSize
    vertPolicy: SizePolicy
    horizPolicy: SizePolicy
    borders*: Table[RectPoint, string]

    case isItem*: bool
      of true:
        item*: ColoredText

      of false:
        grid*: BlockGrid

  BlockGrid* = object
    borders*: TermGridConf # REVIEW remove - this is an implementation
                           # detail form particular rendering backend.
    grid*: MulticellGrid[GridCell]

  BlockGridRow* = seq[GridCell]


#=========================  accessor functions  ==========================#

func size*(cell: GridCell): ArrSize = cell.size
func width*(cell: GridCell): int = cell.size.width
func height*(cell: GridCell): int = cell.size.height

converter toStrBlock*(s: seq[string]): StrBlock =
  StrBlock(s)


func totalWidth*(grid: BlockGrid, colRange: ArrRange): int =
  ## Get total width of columns in `colRange`, including horisontal
  ## grid gap
  toSeq(grid.maxW.valuesBetween(colRange.a, colRange.b)).sumjoin(
    grid[gpoHorizontalGap].len()
  )

func totalHeight*(grid: BlockGrid, rowRange: ArrRange): int =
  ## Get total height of the rows in `rowRange`, including vertical
  ## grid gap.
  toSeq(grid.maxH.valuesBetween(rowRange.a, rowRange.b)).sumjoin(
    grid[gpoVerticalGap].len()
  )

func columns*(grid: BlockGrid): seq[int] =
  grid.maxH.mapPairs(lhs)

func colSizes*(grid: BlockGrid): seq[int] = grid.maxW
func rowSizes*(grid: BlockGrid): seq[int] = grid.maxH

func colSizes*(grid: BlockGrid, a, b: int): seq[int] =
  toSeq(grid.maxW.valuesBetween(a, b))

func colAfter*(grid: BlockGrid, b: int): seq[int] =
  toSeq(grid.maxW.valuesFrom(b))

func lastCol*(grid: BlockGrid, row: int): int =
  ## Index of last column for row
  # toSeq(grid.maxH.keys()).max()
  toSeq(grid.grid.columns(row)).mapIt(it.idx).max()

func lastRow*(grid: BlockGrid): int =
  ## Index of last row for grid
  toSeq(grid.maxW.keys()).max()
  # toSeq(grid.grid.rows(row)).mapIt(it.idx).max()

func rows*(grid: BlockGrid): seq[int] =
  grid.maxW.mapPairs(rhs).sorted()

func width*(grid: BlockGrid): int =
  let (_, hSpacing, left, right, _, _) = spacingDimensions(grid.borders)
  grid.maxW.mapPairs(rhs).sumjoin(hSpacing) + left + right

func height*(grid: BlockGrid): int =
  let (vSpacing, _, _, _, top, bottom) = spacingDimensions(grid.borders)
  grid.maxH.mapPairs(rhs).sumjoin(vSpacing) + top + bottom

func rowHeight*(grid: BlockGrid, row: int): int = grid.maxH[row]
func occupied*(cell: GridCell): ArrSize =
  makeArrSize(w = cell.cols, h = cell.rows)

func internal*(cell: GridCell): ArrSize = cell.size
func colNum*(grid: BlockGrid): int = grid.grid.elems.colNum()
func rowNum*(grid: BlockGrid): int = grid.grid.elems.rowNum()
func size*(grid: BlockGrid): ArrSize = grid.grid.elems.size()

func colRange*(grid: BlockGrid,
                  pos: ArrPos | tuple[row, col: int]): ArrRange =
  let start = pos.col
  var finish = pos.col

  return toRange((start, finish))

func rowRange*(grid: BlockGrid,
                  pos: ArrPos | tuple[row, col: int]): ArrRange =
  let start = pos.row
  var finish = pos.row


  return toRange((start, finish))

func `[]=`*(grid: var BlockGrid,
               row, col: int, cell: GridCell): void =
  grid.grid[makeArrPos(row, col)] = (cell.size, cell)

func `[]=`*(grid: var BlockGrid,
               pos: ArrPos, cell: GridCell): void =
  grid.grid[makeArrRect(pos, cell.size)] = cell

iterator itercells*(grid: BlockGrid): (ArrPos, Option[GridCell]) =
  for (pos, cell) in grid.grid.elems.itercells():
    yield (makeArrPos(pos), cell)


iterator iterSomeCells*(grid: BlockGrid): (ArrPos, GridCell) =
  for (pos, cell) in grid.grid.elems.iterSomeCells():
    yield (makeArrPos(pos), cell)

#============================  constructors  =============================#

func toMulticell*(
  grid: Seq2D[Option[GridCell]]): MulticellGrid[GridCell] =

  for (pos, cell) in grid.iterSomeCells():
    result.fillToSize(pos.makeArrPos().expandSize(cell.size))
    result[makeArrRect(pos, cell.size)] = cell

func makeCell*(
    arg: T,
    cellSize: (int, int) = (1, 1), # TODO replace with `ArrSize`
    policies: (SizePolicy, SizePolicy) = (spExpanding, spExpanding)
  ): GridCell =

  GridCell(
    isItem: true,
    item: arg,
    size: makeArrSize(cellSize),
    vertPolicy: policies[0],
    horizPolicy: policies[1]
  )

func toCell*(
  grid: BlockGrid, size: ArrSize = size1x1): GridCell =
  GridCell(isItem: false, grid: grid, size: size)

func makeUnicodeCell*(
  arg: T, w, h: int,
  sizes: (int, int) = (1, 1)): GridCell =
  let borderTable = {
    rpoLeftEdge : "║",
    rpoRightEdge : "║",
    rpoBottomEdge : "═",
    rpoTopEdge : "═",
    rpoTopLeft : "╔",
    rpoTopRight : "╗",
    rpoBottomLeft : "╚",
    rpoBottomRight : "╝",
  }.toTable()

  result = makeCell(arg, w + 2, h + 2, sizes)
  result.borders = borderTable

func makeCell*(text: StrBlock): GridCell[StrBlock] =
  makeCell(text, (1, 1))

func makeGrid*(
  arg: MulticellGrid[GridCell], conf: TermGridConf): BlockGrid =
  result = BlockGrid(grid: arg, borders: conf)

func makeGrid*(arg: Seq2D, conf: TermGridConf): BlockGrid =
  let cells = arg.mapIt2d(makeCell(it))
  BlockGrid(grid: cells.toMulticell(), borders: conf)

func makeGrid*(strs: seq[seq[string]]): BlockGrid[StrBlock] =
  BlockGrid[StrBlock](grid:
    strs.toStrGrid(@[""].toStrBlock()).mapIt2D(makeCell(it)).toMulticell()
  )

func makeGrid*(arg: Seq2D[GridCell], conf: TermGridConf): BlockGrid =
  BlockGrid(grid: arg.toMulticell(), borders: conf)


func makeGrid*(header: GridCell, conf: TermGridConf): BlockGrid =
  BlockGrid(
    grid: toMulticell(header, header.size),
    borders: conf
  )

func makeGrid*(rows, cols: int, borders: TermGridConf): BlockGrid =
  result.borders = borders
  result.grid = makeMulticell[GridCell](rows, cols)


func addHeader*(grid: var BlockGrid, cell: GridCell): void =
  assert cell.size.height() == 1
  grid.grid.addHeader(
    colIdx = 0,
    width = cell.size.width(),
    val = cell
  )

func appendRow*(grid: var BlockGrid, row: seq[GridCell]): void =
  for cell in row:
    assert cell.size.height() == 1

  grid.grid.insertRow(rowIdx = grid.rowNum(), row)

func setOrAdd*(grid: var BlockGrid,
                  pos: ArrPos, subgrid: BlockGrid) =
  grid.grid.setOrAdd(pos, toCell(subgrid))


#==========================  string conversion  ==========================#

func toTermBuf*(grid: BlockGrid): TermBuf

func toTermBuf*(cell: GridCell): TermBuf =
  case cell.isItem:
    of true:
      result = toTermBuf(cell.item)
    of false:
      result = cell.grid.toTermBuf()

func toTermBuf*(grid: BlockGrid): TermBuf =
  let cells: Seq2D[Option[(ArrSize, TermBuf)]] = grid.grid.elems.mapIt2D(
    # REVIEW grid is seq2d internally. there is no need for `default`
    # on mapping.
    block:
      expectType(it, Option[GridCell])
      if it.isSome():
        some((it.get().size, it.get().toTermBuf()))
      else:
        none((ArrSize, TermBuf))
    ,
    none((ArrSize, TermBuf))
  )

  newTermMultiGrid((0, 0), cells, grid.borders).toTermBuf()
