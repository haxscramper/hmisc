type
  SliceTypes* = Slice[int] | Slice[BackwardsIndex] | HSlice[int, BackwardsIndex]
  IndexTypes* = int | BackwardsIndex

proc normalizeIndex*(idx: IndexTypes, base: int): int =
  result = when idx is int: idx else: base - idx.int

  
proc normalizeSlice*(slice: SliceTypes, base: int): Slice[int] =
  result.a = when slice.a is int: slice.a else: base - slice.a.int
  result.b = when slice.b is int: slice.b else: base - slice.b.int

proc startFor*(slice: SliceTypes, base: int): int =
  when slice.a is int:
    result = slice.a

  else:
    result = base - slice.a.int + 1

proc endFor*(slice: SliceTypes, base: int): int =
  when slice.b is int:
    result = slice.b

  else:
    result = base - slice.b.int + 1

func intersect*(slice1, slice2: Slice[int]): Slice[int] =
  max(slice1.a, slice2.a) .. min(slice1.b, slice2.b)

proc clamp*(slice: SliceTypes, base: int): Slice[int] =
  # if base == 0:
  #   result = 0 .. -1

  # else:
  result =
    clamp(startFor(slice, base), 0, base) ..
    clamp(endFor(slice, base), 0, base)

  # if base < result.a and base < result.b:
  #   result = 0 .. -1

proc clamp*(slice: SliceTypes, base: Slice[int]): Slice[int] =
  let (s, e) = (startFor(slice, base.b), endFor(slice, base.b))

  if intersect(s .. e, base).len == 0:
    result = 0 .. -1

  else:
    result = clamp(s, base.a, base.b) .. clamp(e, base.a, base.b)

func getClamped*[T](s: seq[T], idx: IndexTypes): T =
  assert s.len > 0
  when idx is BackwardsIndex:
    let pos = clamp(s.len - idx.int + 1, 0, s.high)

  else:
    let pos = clamp(idx, 0, s.high)

  return s[pos]

func `@`*(slice: Slice[int]): seq[int] =
  for idx in slice:
    result.add idx

func clampIdx*(val: int): int = clamp(val, 0, high(int))

type
  It*[T] = object
    it: seq[T]

func getIt*[T](it: It[T]): T = it.it[0]
func setIt*[T](it: var It[T], val: T): void = (it.it[0] = val)
func getIt*[T](it: var It[T]): var T = it.it[0]
func newIt*[T](it: T): It[T] = It[T](it: @[it])
converter toT*[T](it: It[T]): T = it.it[0]
