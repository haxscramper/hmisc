import std/[options, strutils, strformat, macros]

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

template asVar*[T](t: T): untyped =
  var tmp = t
  tmp

template asPtr*[T](t: T): untyped =
  var tmp = t
  addr tmp

template asConst*[T](t: T): untyped =
  const tmp = t
  tmp

template currIInfo*(): untyped =
  instantiationInfo(fullpaths = true)

proc `of`*[A: object or ref object; K: enum](item: A, kind: K | set[K]): bool =
  ## Check if @arg{item} has @arg{kind}
  when kind is set:
    item.kind in kind

  else:
    item.kind == kind

template getSomeIt*[T](opt: Option[T], value, default: untyped): untyped =
  if opt.isSome():
    let it {.inject.} = opt.get()
    value
  else:
    default


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

macro lit3*(str: static[string]): untyped =
  ## Dedent static string literals
  newLit(dedent(str))

func add*[A, B](s: var seq[(A, B)], a: A, b: B) = s.add((a, b))
func add*[A, B, C](s: var seq[(A, B, C)], a: A, b: B, c: C) = s.add((a, b, c))

proc add*[T](s: var seq[T], opt: Option[T]) =
  if opt.isSome():
    s.add opt.get()

func add*(s: var string, s1, s2: string, other: varargs[string]) =
  s.add s1
  s.add s2
  for arg in other:
    s.add arg


template procIt*[T](procname: untyped): untyped =
  proc cb(arg: T): auto = procname(arg)
  cb

template procIt*[T](procname: untyped, arg1: untyped): untyped =
  proc cb(arg: T): auto = procname(arg, arg1)
  cb
