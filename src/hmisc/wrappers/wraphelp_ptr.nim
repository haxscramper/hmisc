

type
  UArray*[T] = UncheckedArray[T]
  PUarray*[T] = ptr UncheckedArray[T]

template `+`*[T](p: ptr T, offset: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[ByteAddress](p) +% int(offset) * sizeof(p[]))

template `+=`*[T](p: ptr T, offset: SomeInteger) =
  p = p + offset

template `-`*[T](p: ptr T, offset: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[ByteAddress](p) -% int(offset) * sizeof(p[]))

template `-=`*[T](p: ptr T, offset: SomeInteger) =
  p = p - offset

template `[]`*[T](p: ptr T, offset: SomeInteger): T =
  (p + offset)[]

template `[]=`*[T](p: ptr T, offset: SomeInteger, val: T) =
  (p + offset)[] = val

proc allocPUarray*[T](size: Natural): PUarray[T] =
  cast[PUarray[T]](alloc(size * sizeof(T)))

proc deallocPUarray*[T](arr: PUarray[T]) =
  dealloc(cast[pointer](arr))

template toPUarray*[T](p: ptr T): PUarray[T] = cast[PUarray[T]](p)
template toPtr*[T](p: PUArray[T]): ptr T = cast[ptr T](p)

template toPtr*[T](r: ref T): ptr T = cast[ptr T](r)
template toPUarray*[T](r: ref T): PUarray[T] = cast[PUarray[T]](r)

iterator items*[T](arr: PUarray[T], size: int): T =
  var idx = 0
  while idx < size:
    yield arr[][idx]
    inc idx

iterator pairs*[T](arr: PUarray[T], size: int): (int, T) =
  var idx = 0
  while idx < size:
    yield (idx, arr[][idx])
    inc idx

template subArrayPtr*[T](arr: PUArray[T], idx: SomeInteger): PUarray[T] =
  toPUarray(toPtr(arr) + idx)
