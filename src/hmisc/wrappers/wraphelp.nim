func closureToCdecl*[T0, T1](
    cb: proc(a: var T0, b: T1) {.closure.}
  ): proc(a: var T0, b: T1, env: pointer) {.cdecl.} =

  discard

type
  StdInitializerList*[T] {.
    importcpp: "std::initializer_list",
    header: "<initializer_list>"
  .} = object

  CxxTemplateUndefined* = object

  cchar16* = uint16
  cchar32* = uint32
  cwchar* = uint32
  nullptr_t* = typeof(nil)

  StdNullptrT* = nullptr_t
  StdSizeT* = culong
  StdPtrdiffT* = clong

proc `as`*[T](
  undef: CxxTemplateUndefined, asTarget: typedesc[T]): T {.importcpp: "(#)"}

proc cxxInitList*[T](args: T) {.importcpp: "{@}", varargs.}


proc newImportAux*() {.importc: "//", header: "<new>".} =
  discard

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

template toPUarray*[T](p: ptr T): PUarray[T] = cast[PUarray[T]](p)
template toPtr*[T](p: PUArray[T]): ptr T = cast[ptr T](p)

template toPtr*[T](r: ref T): ptr T = cast[ptr T](r)
template toPUarray*[T](r: ref T): PUarray[T] = cast[PUarray[T]](r)

template subArrayPtr*[T](arr: PUArray[T], idx: SomeInteger): PUarray[T] =
  toPUarray(toPtr(arr) + idx)
