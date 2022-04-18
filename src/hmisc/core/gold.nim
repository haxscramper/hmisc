import std/[options, strutils, macros, os]

## Most important templates and additional overloads that I use in
## absoltely all parts of the code.

type NoValue* = distinct char

template Attr*() {.pragma.}

template tern*(predicate: bool, tBranch: untyped, fBranch: untyped): untyped =
  ## Shorthand for inline if/else. Allows use of conditions in strformat,
  ## simplifies use in expressions. Less picky with formatting
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      if predicate: tBranch else: fBranch

template ternIt*(expr, predicate, tBranch, fBranch: untyped): untyped =
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      let it {.inject.} = expr
      if predicate: tBranch else: fBranch

template inWhile*(expr, body: untyped): untyped =
  while expr:
    body

template inFor*(ident, expr, body: untyped): untyped =
  for ident in expr:
    body

template inFor*(ident1, ident2, expr, body: untyped): untyped =
  for ident1, ident2 in expr:
    body

template dowhile*(expr, body: untyped): untyped =
  block:
    body
    while expr:
      body

template withNewIt*(T: typedesc, body: untyped): untyped =
  block:
    var it {.inject.}: ref T
    new(it)
    block:
      body
    it


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
  {.line: instantiationInfo(fullPaths = true).}:
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

proc asRef*[T: not ref and not ptr](t: sink T): ref T =
  new(result)
  result[] = t

proc asRef*[T: not ref and not ptr](t: ptr T): ref T =
  new(result)
  result[] = t[]

proc asRef*[T: not ref and not ptr](t: ref T): ref T = t


template asVar*[T](t: T): untyped =
  var tmp = t
  tmp

template asPtr*[T](t: T): untyped =
  var tmp = t
  addr tmp

template asConst*[T](t: T): untyped =
  const tmp = t
  tmp

template asExpr*(arg: untyped): untyped = arg

proc asSet*[E: enum](en: E): set[E] = {en}
proc asSet*[E: enum](en: set[E]): set[E] = en

template currIInfo*(): untyped =
  instantiationInfo(fullpaths = true)

template curIDir*(): untyped =
  bind splitFile
  splitFile(instantiationInfo(fullPaths = true).filename).dir

template currentSourceDir*(): untyped =
  bind parentDir
  instantiationInfo(fullPaths = true).filename.parenTdir()

template relToSource*(path: string): untyped =
  bind splitFile, joinPath
  joinPath(splitFile(instantiationInfo(fullPaths = true).filename).dir, path)


proc `of`*[A: object or ref object or distinct; K: enum](item: A, kind: K | set[K]): bool =
  ## Check if @arg{item} has @arg{kind}
  when kind is set:
    item.kind in kind

  else:
    item.kind == kind

proc `of`*[En: enum](item: En, kind: En | set[En]): bool =
  when kind is set:
    item in kind

  else:
    item == kind

template getSomeIt*[T](opt: Option[T], value, default: untyped): untyped =
  if opt.isSome():
    let it {.inject.} = opt.get()
    value
  else:
    default

template canGet*[T](opt: Option[T], injected: untyped): untyped =
  let expr = opt
  expr.isSome() and (let injected {.inject.} = expr.get(); true)

func mget*[T](opt: var Option[T], value: T = default(T)): var T =
  if opt.isNone(): opt = some value
  return opt.get()

template top*[T](s: seq[T]): untyped = s[^1]
template last*[T](s: seq[T]): untyped = s[^1]
template last*[T](s: seq[T], item: T): untyped =
  if len(s) == 0:
    s.add item

  s[^1]

template last2*(s: seq): untyped = s[^1][^1]
template first*(s: seq): untyped = s[0]
template clear*(s: seq): untyped = s.setlen(0)
template empty*(s: seq): bool = len(s) == 0

template `?`*[T](s: seq[T]): bool = len(s) > 0
template `?`*(s: string): bool = len(s) > 0
template `?`*[T](o: Option[T]): bool = isSome(o)

func getOr*[T](s: seq[T], idx: int, value: T = default(T)): T =
  if idx < s.len: s[idx] else: value

func dollar*[T](arg: T): string =
  mixin `$`
  return $arg

proc `not`*[K](s: set[K]): set[K] = ({ low(K) .. high(K) } - s)

func first*[E](s: set[E]): E =
  assert len(s) > 0, "Cannot get value from empty set"
  for val in s:
    result = val
    return

func pop*[E](s: var set[E]): E =
  assert len(s) > 0, "Cannot pop from empty set"

  for val in s:
    result = val
    s.excl result
    return


iterator pairs*[I](s: set[I]): (int, I) =
  var idx = 0
  for val in items(s):
    yield (idx, val)
    inc idx

func quoteLiteral*(str: string): string =
  result.add "\""
  for ch in str:
    result.add case ch:
      of '"': "\\\""
      of '\n': "\\n"
      of '\t': "\\t"
      of '\r': "\\r"
      of '\\': "\\\\"
      else: $ch

  result.add "\""


macro lit3*(str: static[string]): untyped =
  ## Dedent static string literals
  newLit(dedent(str))

macro lit3*(ind: static[int], str: static[string]): untyped =
  ## Dedent static string literals
  let text = dedent(str)
  if text.len == 0:
    newLit("")

  elif text[^1] == '\n':
    newLit(text[0 .. ^2].indent(ind) & "\n")

  else:
    newLit(text.indent(ind))

func add*[A, B](s: var seq[(A, B)], a: A, b: B) = s.add((a, b))
func add*[A, B, C](s: var seq[(A, B, C)], a: A, b: B, c: C) = s.add((a, b, c))

func clear*[T](o: var Option[T]) = o = none(T)

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

macro `//`*(arg: string): untyped =
  ## Emit C comment in generated source code
  ##
  ## `// "C comment"` will yield `/* C comment */` emited.
  let lit = newLit("/* " & arg.strVal() & " */")

  quote do:
    {.emit: `lit`.}

macro `///`*(name: static[string], body: untyped): untyped =
  let
    lit1 = newLit("/* " & name & " */ /* BEGIN */")
    lit2 = newLit("/* " & name & " */ /* END */")

  quote do:
    {.emit: "\n\n\n".}
    {.emit: `lit1`.}
    `body`
    {.emit: `lit2`.}
    {.emit: "\n\n\n".}

template cblock*(name: static[string], body: untyped): untyped =
  block:
    /// name:
      body

template cexpr*(name: static[string], body: untyped): untyped =
  // name
  body

macro importx*(imports: untyped): untyped =
  proc aux(tree: NimNode, prefix: seq[NimNode]): seq[seq[NimNode]] =
    case tree.kind:
      of nnkIdent, nnkStrLit:
        result.add prefix & tree

      of nnkPrefix:
        let str = tree[0].strVal()
        assert str.startsWith("./") or str.startsWith("../"), str
        result = aux(tree[1], prefix & ident(tree[0].strVal()[0..^2]))

      of nnkInfix:
        result = aux(tree[2], prefix & tree[1])

      of nnkStmtList, nnkBracket:
        for imp in tree:
          result.add aux(imp, prefix)

      else:
        error("Unexpected input node for `importx` - " & tree.treeRepr(), tree)

  result = nnkImportStmt.newTree()
  for imp in aux(imports, @[]):
    var infix = imp[0]
    var idx = 1
    while idx < imp.len:
      infix = nnkInfix.newTree(ident"/", infix, imp[idx])
      inc idx

    result.add infix


proc postDec*[T](value: var T): T {.discardable.} =
  result = value
  dec value

proc preDec*[T](value: var T): var T {.discardable.} =
  dec value
  value

proc postInc*[T](value: var T): T {.discardable.} =
  result = value
  inc value

proc preInc*[T](value: var T): var T {.discardable.} =
  inc value
  value

macro dumpTyped*(a: typed): untyped =
  case a.kind:
    of nnkSym:
      echo a.getTypeInst().treeRepr()

    else:
      echo a.treeRepr()

  result = a

func asgnAux*[T](target: var T, source: sink T) =
  ## Helper assignment procedure, performs `target = source` operation
  ## without any additional checks. This can be used in order to
  ##
  ## - Assing discriminant field under `cast(uncheckedAssign)` without
  ##   having to temporarily disable bound checking
  ## - Assign to the embedded supertype of the object. In that
  ##   case assignment is done over a section of an object -
  ##   `asgnAux[Base](<derived>, <base>)` makes it possible to
  ##   "promote" a base object by copying it's fields to derived one.
  ##
  ## .. note:: When using for embedded supertype assignments, generic
  ##      parameter **must** be supplied, otherwise object slicing
  ##      won't kick in. So `asgnAux[Base](Derived, Base)`
  target = source

template setKind*[V](target, source: V) =
  {.cast(uncheckedAssign).}:
    asgnAux[V](target, source)

template byaddr*(lhs, typ, ex) =
  when typ is typeof(nil):
    when compiles(addr(ex)):
      let tmp = addr(ex)

    else:
      let tmp = unsafeAddr(ex)

  else:
    when compiles(addr(ex)):
      let tmp: ptr typ = addr(ex)

    else:
      let tmp: ptr typ = unsafeaddr(ex)

  template lhs: untyped = tmp[]

template pcast*[T](arg: untyped): untyped = cast[ptr T](arg)
