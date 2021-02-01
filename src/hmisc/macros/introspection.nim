import std/[macros, tables]

macro directEnumName*(arg: typed): untyped =
  ## #DOC
  let argImpl = getTypeImpl(arg)
  result = nnkCaseStmt.newTree(arg)
  for sym in argImpl[1..^1]:
    result.add nnkOfBranch.newTree(sym, sym.toStrLit())

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
