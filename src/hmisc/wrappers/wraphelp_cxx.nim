{.push warning[InheritFromException]:off.}

import ./wraphelp
import std/macros

##[

[[code:CxxTemplateUndefined]] is made mostly for automatic wrapper
generator, specifically for cases like these, where it might be /obvious,
that most use cases/ for `==` return `bool`, but this intuition cannot be
transferred to the wrapper generator. In that specific case you would need
to do `(str1 == str2) as bool`.

```c++
template<typename _CharT>
  inline
  typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value, bool>::__type
  operator==(const basic_string<_CharT>& __lhs,
       const basic_string<_CharT>& __rhs) _GLIBCXX_NOEXCEPT
  { return (__lhs.size() == __rhs.size()
      && !std::char_traits<_CharT>::compare(__lhs.data(), __rhs.data(),
              __lhs.size())); }
```

]##


type
  StdInitializerList*[T] {.
    importcpp: "std::initializer_list",
    header: "<initializer_list>"
  .} = object

  CxxTemplateUndefined* = object
  CxxTemplateApproximate*[T] = object


  StdNullptrT* = nullptr_t
  StdSizeT* = culong
  StdPtrdiffT* = clong

  StdException* {.
    importcpp: "std::exception",
    header: "<stdexcept>",
    byref,
    inheritable
  .} =
    object of Exception

{.pop.}

proc what*(ex: StdException): cstring {.importcpp: "#.what()".}


proc `as`*[T1, T2](
  approx: CxxTemplateApproximate[T1],
  asTarget: typedesc[T2]): T2 {.importcpp: "(#)".}

proc `as`*[T1, T2](asSource: T1, target: typedesc[CxxTemplateApproximate[T2]]):
  CxxTemplateUndefined {.importcpp: "(#)".}

converter toT*[T](approx: CxxTemplateApproximate[T]): T
  {.importcpp: "(#) /*implicit conversion from approximate template*/".}

converter toCxxTemplateApproximate*[T](base: T): CxxTemplateApproximate[T]
  {.importcpp: "(#)".}

proc `as`*[T](
  undef: CxxTemplateUndefined, asTarget: typedesc[T]): T {.importcpp: "(#)"}

proc `as`*[T](asSource: T, target: typedesc[CxxTemplateUndefined]):
  CxxTemplateUndefined {.importcpp: "(#)".}


type StdInitializerListBuilder = object
const lc* = StdInitializerListBuilder()

proc cxxInitList*[T](args: T) {.importcpp: "{@}", varargs.}
macro `[]`*(lc: StdInitializerListBuilder, a: varargs[untyped]): untyped =
  result = newCall("cxxInitList")
  for arg in a:
    result.add arg

proc newImportAux*() {.importc: "//", header: "<new>".} =
  discard
