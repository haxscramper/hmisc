import std/[sequtils, with, tables, hashes, sets, strformat]
export sequtils, with, tables, hashes, sets, strformat

import
  hmisc/algo/halgorithm,
  hmisc/other/hpprint,
  hmisc/[base_errors, hdebug_misc, helpers]

export
  halgorithm,
  base_errors,
  hdebug_misc,
  helpers,
  hpprint

startHax()

type
  Iterable*[T] = iterator(): T

func preInc*(arg: var int): int {.discardable.} = inc arg; result = arg
func preDec*(arg: var int): int {.discardable.} = dec arg; result = arg
func postInc*(arg: var int): int {.discardable.} = result = arg; inc arg
func postDec*(arg: var int): int {.discardable.} = result = arg; dec arg


func findAny*[T](s: HashSet[T]): T = 
  for val in s:
    return val

  assert false

func nseq*[T](dim1: int, def: T): seq[T] = 
  newSeqWith(dim1, def)

func nseq*[T](dim1, dim2: int, def: T): seq[seq[T]] = 
  newSeqWith(dim1, newSeqWith(dim2, def))

func nseq*[T](dim1, dim2, dim3: int, def: T): seq[seq[seq[T]]] = 
  newSeqWith(dim1, newSeqWith(dim2,  newSeqWith(dim3, def)))

func narr*[R, T](arrRange: typedesc[R], key: T): array[R, T] = 
  for val in mitems(result):
    val = key

func fill*[R, T](arr: var array[R, T], key: T) =  
  for val in mitems(arr):
    val = key

func fill*[T](arr: var seq[T], key: T) =  
  for val in mitems(arr):
    val = key


func `=*=`*[T1, T2](o1: ref T1, o2: ref T2): bool =
  cast[int](o1) == cast[int](o2)

func isEmpty*[T](s: seq[T]): bool = s.len == 0

func remove*[T](s: var seq[T], item: T): bool {.discardable.} =
  let idx = s.find(item)
  if idx != -1:
    s.del idx
    result = true

func remove*[T](s: var seq[T], idx: int): T {.discardable.} =
  if 0 <= idx and idx < s.len:
    result = s[idx]
    s.del idx
