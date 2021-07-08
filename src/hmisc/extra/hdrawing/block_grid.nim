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

  GridCell*[T] = object
    # TODO add support for cell configuration: additional annotations
    # etc.
    size: ArrSize
    vertPolicy: SizePolicy
    horizPolicy: SizePolicy
    borders*: Table[RectPoint, string]

    case isItem*: bool
      of true:
        item*: T
      of false:
        grid*: BlockGrid[T]

  BlockGrid*[T] = object
    borders*: TermGridConf # REVIEW remove - this is an implementation
                           # detail form particular rendering backend.
    grid*: MulticellGrid[GridCell[T]]

  BlockGridRow*[T] = seq[GridCell[T]]


#=========================  accessor functions  ==========================#

func size*[T](cell: GridCell[T]): ArrSize = cell.size
func width*[T](cell: GridCell[T]): int = cell.size.width
func height*[T](cell: GridCell[T]): int = cell.size.height

converter toStrBlock*(s: seq[string]): StrBlock =
  StrBlock(s)


func totalWidth*[T](grid: BlockGrid[T], colRange: ArrRange): int =
  ## Get total width of columns in `colRange`, including horisontal
  ## grid gap
  toSeq(grid.maxW.valuesBetween(colRange.a, colRange.b)).sumjoin(
    grid[gpoHorizontalGap].len()
  )

func totalHeight*[T](grid: BlockGrid[T], rowRange: ArrRange): int =
  ## Get total height of the rows in `rowRange`, including vertical
  ## grid gap.
  toSeq(grid.maxH.valuesBetween(rowRange.a, rowRange.b)).sumjoin(
    grid[gpoVerticalGap].len()
  )

func columns*[T](grid: BlockGrid[T]): seq[int] =
  grid.maxH.mapPairs(lhs)

func colSizes*[T](grid: BlockGrid[T]): seq[int] = grid.maxW
func rowSizes*[T](grid: BlockGrid[T]): seq[int] = grid.maxH

func colSizes*[T](grid: BlockGrid[T], a, b: int): seq[int] =
  toSeq(grid.maxW.valuesBetween(a, b))

func colAfter*[T](grid: BlockGrid[T], b: int): seq[int] =
  toSeq(grid.maxW.valuesFrom(b))

func lastCol*[T](grid: BlockGrid[T], row: int): int =
  ## Index of last column for row
  # toSeq(grid.maxH.keys()).max()
  toSeq(grid.grid.columns(row)).mapIt(it.idx).max()

func lastRow*[T](grid: BlockGrid[T]): int =
  ## Index of last row for grid
  toSeq(grid.maxW.keys()).max()
  # toSeq(grid.grid.rows(row)).mapIt(it.idx).max()

func rows*[T](grid: BlockGrid[T]): seq[int] =
  grid.maxW.mapPairs(rhs).sorted()

func width*[T](grid: BlockGrid[T]): int =
  let (_, hSpacing, left, right, _, _) = spacingDimensions(grid.borders)
  grid.maxW.mapPairs(rhs).sumjoin(hSpacing) + left + right

func height*[T](grid: BlockGrid[T]): int =
  let (vSpacing, _, _, _, top, bottom) = spacingDimensions(grid.borders)
  grid.maxH.mapPairs(rhs).sumjoin(vSpacing) + top + bottom

func rowHeight*[T](grid: BlockGrid[T], row: int): int = grid.maxH[row]
func occupied*[T](cell: GridCell[T]): ArrSize =
  makeArrSize(w = cell.cols, h = cell.rows)

func internal*[T](cell: GridCell[T]): ArrSize = cell.size
func colNum*[T](grid: BlockGrid[T]): int = grid.grid.elems.colNum()
func rowNum*[T](grid: BlockGrid[T]): int = grid.grid.elems.rowNum()
func size*[T](grid: BlockGrid[T]): ArrSize = grid.grid.elems.size()

func colRange*[T](grid: BlockGrid[T],
                  pos: ArrPos | tuple[row, col: int]): ArrRange =
  let start = pos.col
  var finish = pos.col

  return toRange((start, finish))

func rowRange*[T](grid: BlockGrid[T],
                  pos: ArrPos | tuple[row, col: int]): ArrRange =
  let start = pos.row
  var finish = pos.row


  return toRange((start, finish))

func `[]=`*[T](grid: var BlockGrid[T],
               row, col: int, cell: GridCell[T]): void =
  grid.grid[makeArrPos(row, col)] = (cell.size, cell)

func `[]=`*[T](grid: var BlockGrid[T],
               pos: ArrPos, cell: GridCell[T]): void =
  grid.grid[makeArrRect(pos, cell.size)] = cell

iterator itercells*[T](grid: BlockGrid[T]): (ArrPos, Option[GridCell[T]]) =
  for (pos, cell) in grid.grid.elems.itercells():
    yield (makeArrPos(pos), cell)


iterator iterSomeCells*[T](grid: BlockGrid[T]): (ArrPos, GridCell[T]) =
  for (pos, cell) in grid.grid.elems.iterSomeCells():
    yield (makeArrPos(pos), cell)

#============================  constructors  =============================#

func toMulticell*[T](
  grid: Seq2D[Option[GridCell[T]]]): MulticellGrid[GridCell[T]] =

  for (pos, cell) in grid.iterSomeCells():
    result.fillToSize(pos.makeArrPos().expandSize(cell.size))
    result[makeArrRect(pos, cell.size)] = cell

func makeCell*[T](
    arg: T,
    cellSize: (int, int) = (1, 1), # TODO replace with `ArrSize`
    policies: (SizePolicy, SizePolicy) = (spExpanding, spExpanding)
  ): GridCell[T] =

  GridCell[T](
    isItem: true,
    item: arg,
    size: makeArrSize(cellSize),
    vertPolicy: policies[0],
    horizPolicy: policies[1]
  )

func toCell*[T](
  grid: BlockGrid[T], size: ArrSize = size1x1): GridCell[T] =
  GridCell[T](isItem: false, grid: grid, size: size)

func makeUnicodeCell*[T](
  arg: T, w, h: int,
  sizes: (int, int) = (1, 1)): GridCell[T] =
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

func makeGrid*[T](
  arg: MulticellGrid[GridCell[T]], conf: TermGridConf): BlockGrid[T] =
  result = BlockGrid[T](grid: arg, borders: conf)

func makeGrid*[T](arg: Seq2D[T], conf: TermGridConf): BlockGrid[T] =
  let cells = arg.mapIt2d(makeCell[T](it))
  BlockGrid[T](grid: cells.toMulticell(), borders: conf)

func makeGrid*(strs: seq[seq[string]]): BlockGrid[StrBlock] =
  BlockGrid[StrBlock](grid:
    strs.toStrGrid(@[""].toStrBlock()).mapIt2D(makeCell(it)).toMulticell()
  )

func makeGrid*[T](arg: Seq2D[GridCell[T]], conf: TermGridConf): BlockGrid[T] =
  BlockGrid[T](grid: arg.toMulticell(), borders: conf)


func makeGrid*[T](header: GridCell[T], conf: TermGridConf): BlockGrid[T] =
  BlockGrid[T](
    grid: toMulticell(header, header.size),
    borders: conf
  )

func makeGrid*[T](rows, cols: int, borders: TermGridConf): BlockGrid[T] =
  result.borders = borders
  result.grid = makeMulticell[GridCell[T]](rows, cols)


func addHeader*[T](grid: var BlockGrid[T], cell: GridCell[T]): void =
  assert cell.size.height() == 1
  grid.grid.addHeader(
    colIdx = 0,
    width = cell.size.width(),
    val = cell
  )

func appendRow*[T](grid: var BlockGrid[T], row: seq[GridCell[T]]): void =
  for cell in row:
    assert cell.size.height() == 1

  grid.grid.insertRow(rowIdx = grid.rowNum(), row)

func setOrAdd*[T](grid: var BlockGrid[T],
                  pos: ArrPos, subgrid: BlockGrid[T]) =
  grid.grid.setOrAdd(pos, toCell(subgrid))


#==========================  string conversion  ==========================#

func toTermBuf*[T](grid: BlockGrid[T]): TermBuf

func toTermBuf*[T](cell: GridCell[T]): TermBuf =
  case cell.isItem:
    of true:
      result = toTermBuf(cell.item)
    of false:
      result = cell.grid.toTermBuf()

func toTermBuf*[T](grid: BlockGrid[T]): TermBuf =
  let cells: Seq2D[Option[(ArrSize, TermBuf)]] = grid.grid.elems.mapIt2D(
    # REVIEW grid is seq2d internally. there is no need for `default`
    # on mapping.
    block:
      expectType(it, Option[GridCell[T]])
      if it.isSome():
        some((it.get().size, it.get().toTermBuf()))
      else:
        none((ArrSize, TermBuf))
    ,
    none((ArrSize, TermBuf))
  )

  newTermMultiGrid((0, 0), cells, grid.borders).toTermBuf()
