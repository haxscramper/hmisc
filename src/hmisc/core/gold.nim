import std/[options, strutils, strformat]

## Most important templates and additional overloads that I use in
## absoltely all parts of the code.

template tern*(predicate: bool, tBranch: untyped, fBranch: untyped): untyped =
  ## Shorthand for inline if/else. Allows use of conditions in strformat,
  ## simplifies use in expressions. Less picky with formatting
  runnableExamples:
    let a = tern(1 == 2, "99", "0-")
    doAssert a == "0-"

  block:
    if predicate: tBranch else: fBranch

template withIt*(val, body: untyped): untyped =
  ## Copy `val` to `it`, execute body and then return `it`.
  ##
  ## - TIP :: Can be used for field modification syntax -
  ##   nim`newT().withIt: ... it.name = "something"`.
  ## - NOTE :: Copying is done using simple assignment, which means
  ##   changing `ref` object would mutate original value as well. In order
  ##   to correctly work with ref objets use [[code:withDeepIt]], which
  ##   performs `deepCopy`.
  block:
    var it {.inject.} = val
    block:
      body
    it

template withDeepIt*(expr, body: untyped): untyped =
  ## Similar to [[code:withIt]], but perform [[code:system.deepCopy()]] of
  ## the expression
  block:
    var it {.inject.} = deepCopy(expr)
    block:
      body

    it

template withResIt*(val, body: untyped): untyped =
  ## identical to [[code:withIt]], but does not return injected `it`
  ## variable
  block:
    var it {.inject.} = val
    body

template withMutIt*[T](val: var T, body: untyped): untyped =
  ## Inject mutable `it` in the scope - can be used as a slightly more
  ## verbose version of [[code:std/with.with()]], which does not perform
  ## any magical body rewrites.
  block:
    var tmp: ptr T = addr(val)
    template it(): untyped {.inject.} = tmp[]
    body

template notNil*(arg: untyped): bool =
  ## `not isNil(x)` shortcut
  not isNil(arg)

func `-`*[I](s: set[I], i: I): set[I] = s - {i}

func nor*(args: varargs[bool]): bool =
  for arg in args:
    result = arg or result

  result = not result

func nand*(args: varargs[bool]): bool =
  result = true
  for arg in args:
    result = arg and result

  result = not result

func `or`*(args: varargs[bool]): bool =
  for arg in args:
    result = arg and result

func `and`*(args: varargs[bool]): bool =
  result = true
  for arg in args:
    result = arg and result

proc add*[T](s: var seq[T], opt: Option[T]) =
  if opt.isSome():
    s.add opt.get()

proc `&`*[T](elements: openarray[seq[T]]): seq[T] =
  for element in elements:
    result &= element

proc `&`*(strings: openarray[string]): string =
  for str in strings:
    result &= str

proc `&=`*(target: var string, args: openarray[string]) =
  for arg in args:
    target &= arg


proc toRef*[T](t: T): ref T =
  new(result)
  result[] = t

template currIInfo*(): untyped =
  instantiationInfo(fullpaths = true)

proc `of`*[A: object or ref object; K: enum](item: A, kind: K | set[K]): bool =
  ## Check if @arg{item} has @arg{kind}
  when kind is set:
    item.kind in kind

  else:
    item.kind == kind

template last*(s: typed): untyped = s[^1]

func dollar*[T](arg: T): string =
  mixin `$`
  return $arg

func pop*[E](s: var set[E]): E =
  assert len(s) > 0, "Cannot pop from empty set"

  for val in s:
    result = val
    s.excl result
    return


func width*(s: string): int =
  var cnt = 0
  for ch in s:
    if ch == '\n':
      result = max(cnt, result)
      cnt = 0

    else:
      inc cnt

  if result == 0:
    result = cnt
