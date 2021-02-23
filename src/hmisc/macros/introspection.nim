import std/[macros, tables]
import ../algo/hstring_algo
export hstring_algo

macro directEnumName*(arg: typed): untyped =
  ## #DOC
  let argImpl = getTypeImpl(arg)
  result = nnkCaseStmt.newTree(arg)
  for sym in argImpl[1..^1]:
    result.add nnkOfBranch.newTree(sym, sym.toStrLit())

func enumNamesTable*[En: enum](
    arg: En,
    withDirectName: bool = true,
    withDroppedPrefix: bool = true
  ): seq[tuple[names: seq[string], value: En]] =
  ##[

Return list of all enum names.

- NOTE :: for best performance assign results to `const` as this
  function computes stringified value for each enum.

]##

  var prefix: string
  if withDroppedPrefix:
    var names: seq[string]
    for enval in En:
      names.add directEnumName(enval)

    prefix = commonPrefix(names)

  mixin dropPrefix
  for enval in En:
    var names: seq[string]
    if withDirectName:
      let name = directEnumName(enval)
      names.add name

      if withDroppedPrefix:
        names.add name.dropPrefix(prefix)


    names.add $enval
    result.add (names, enval)

func enumNamesTable*(
    En: typedesc,
    withDirectName: bool = true,
    withDroppedPrefix: bool = true
  ): seq[tuple[names: seq[string], value: En]] =

  var tmp: En
  enumNamesTable(tmp, withDirectName, withDroppedPrefix)

func namedSubnode*[T: enum](
    kind: T,
    idx: int,
    map: static[seq[tuple[key: T, subnames: seq[string]]]],
    fallback: string = "<<fail>>"
  ): string =

  const remap: Table[T, seq[string]] = toTable(map)

  if kind notin remap or remap[kind].len < idx:
    result = fallback

  else:
    result = remap[kind][idx]
