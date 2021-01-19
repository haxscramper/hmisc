## Small helper math utilities

import math, sequtils

func sumjoin*(a: openarray[int], sep: int): int =
  a.sum() + (if a.len > 0: ((a.len - 1) * sep) else: 0)


func cumsumjoin*(
  a: openarray[int],
  sep: int,
  addFirst: bool = false,
  dropLast: bool = false
     ): seq[int] =
  var curr: int = 0
  if addFirst:
    result.add curr

  for val in a:
    result.add(val + sep + curr)
    curr += val + sep

  if dropLast:
    result = result[0..^2]


proc `..+`*(start: int, offset: int): Slice[int] =
  Slice[int](a: start, b: start + offset - 1)
  # for i in start ..< start + offset:
  #   yield i

func order*[Num](a, b: Num): (Num, Num) =
  if a > b: (b, a)
  else: (a, b)

func clampLow*[T](x, minval: T): T =
  if x < minval: minval
  else: x

func modiv*(a, b: int): tuple[val, rem: int] =
  result.val = a div b
  result.rem = a mod b

proc max*[T](x: openArray[T], default: T): T =
  ## The maximum value of `x`. ``T`` needs to have a ``<`` operator.
  ## use `default` if array is empty
  if x.len == 0:
    result = default
  else:
    for i in x:
      if result < i: result = i


func setMax*[T](a: var T, b: T): void =
  if a < b:
    a = b
