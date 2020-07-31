import sugar, sequtils
import hmap

type
  SparseGrid*[T] = object
    elems: Map[int, Map[int, T]]

func maxRow*[T](s: SparseGrid[T]): int =
  toSeq(s.elems.keys()).max()

func minRow*[T](s: SparseGrid[T]): int =
  toSeq(s.elems.keys()).min()

func add*[T](s: var SparseGrid[T], row: seq[T]): void =
  s.elems[s.elems.keys().max() + 1] row

func prepend*[T](s: var SparseGrid[T], row: seq[T]): void =
  var newRow = initMap[int, T]()
  for idx, item in row:
    newRow[idx] = item

  s.elems[s.minRow() - 1] = newRow

  s.elems =
    block:
      var tmp = initMap[int, Map[int, T]]()
      for idx, v in s.elems:
        tmp[idx + 1] = v

      tmp

func rowAppend*[T](s: var SparseGrid[T], elem: T, idx: int): void =
  ## Add element to `idx` row
  if s.minRow() <= idx and idx <= s.maxRow():
    let row = s.elems.mgetOrPut(idx, newTable[int, T]())
    row[s.maxRow() + 1] = elem
  else:
    raiseAssert("Sdfasdfasdf")

converter toSparseGrid*[T](s: seq[seq[T]]): SparseGrid[T] =
  SparseGrid[T](
    elems:
      block:
        var elems = initMap[int, Map[int, T]]()
        for rowIdx, row in s:
          elems[rowIdx] =
            block:
              var cells = initMap[int, T]()
              for colIdx, cell in row:
                cells[colIdx] = cell
              cells

        elems
  )

iterator rows*[T](s: SparseGrid[T]): tuple[
  idx: int, row: Map[int, T]] =
  for key in toSeq(s.elems.keys()).sorted():
    yield (idx: key, row: s.elems[key])

iterator columns*[T](s: SparseGrid[T], row: int
                    ): tuple[idx: int, cell: T] =
  for idx in toSeq(s.elems[row].keys()).sorted():
    yield (idx: idx, cell: s.elems[row][idx])

func firstColumn*[T](grid: SparseGrid[T], row: int): int =
  toSeq(grid.elems[row].keys()).sorted()[0]

func `[]`*[T](grid: SparseGrid[T], row, col: int): T =
  grid.elems[row][col]

func `[]`*[T](grid: SparseGrid[T], cell: (int, int)): T =
  grid.elems[cell[0]][cell[1]]

func `[]=`*[T](grid: var SparseGrid[T], pos: (int, int), val: T): void =
  # static:
  #   echo typeof grid
  #   echo typeof pos
  #   echo typeof val
  #   echo typeof grid.elems
  #   echo typeof grid.elems[0]
  #   quit 1
  if pos[0] notin grid.elems:
    grid.elems[pos[0]] = initMap[int, T]()

  var gr = grid.elems[pos[0]]
  gr[pos[1]] = val
  discard

func `[]=`*[T](grid: var SparseGrid[T], row, col: int, val: T): void =
  grid[(row, col)] = val

template mapIt2d*[T](inseq: SparseGrid[T], op: untyped): untyped =
  type ResT = typeof((
    block:
      var it {.inject.}: T
      var rowIdx {.inject.}: int
      var colIdx {.inject.}: int
      op))

  var result: SparseGrid[ResT]
  for rowId, row in inseq.elems:
    for colId, cell in row:
      let it {.inject.} = cell
      let rowIdx {.inject.}: int = rowId
      let colIdx {.inject.}: int = colId
      result[(rowIdx, colIdx)] = op

  result

template mapItRows*[T](inseq: SparseGrid[T], op: untyped): untyped =
  type ResT = typeof((
    block:
      var it {.inject.}: Table[int, T]
      var rowIdx {.inject.}: int
      op))

  var result: seq[tuple[idx: int, val: ResT]]
  for rowId, row in inseq.elems:
    let it {.inject.} = row
    let rowIdx {.inject.}: int = rowId
    result.add (idx: rowIdx, val: op)

  result

import seq2d

func toSparse*[T](inseq: Seq2D[T]): SparseGrid[T] =
  for (pos, cell) in inseq.itercells():
    let (row, col) = pos
    result[(row, col)] = cell
