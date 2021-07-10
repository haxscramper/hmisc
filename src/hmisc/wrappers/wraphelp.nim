func closureToCdecl*[T0, T1](
    cb: proc(a: var T0, b: T1) {.closure.}
  ): proc(a: var T0, b: T1, env: pointer) {.cdecl.} =

  discard

type
  StdInitializerList*[T] {.
    importcpp: "std::initializer_list",
    header: "<initializer_list>"
  .} = object

  CxxTemplateUndefined = object

  cchar16* = uint16
  cchar32* = uint32
  cwchar* = uint32

proc `as`*[T](undef: CxxTemplateUndefined, asTarget: typedesc[T]): T {.importcpp: "(#)"} =

proc cxxInitList*[T](args: T) {.importcpp: "{@}", varargs.}

type nullptr_t* = typeof(nil)

proc newImportAux*() {.importc: "//", header: "<new>".} =
  discard
