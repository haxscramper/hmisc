import std/macros
import std/enumutils {.all.}

macro namesImpl(a: typed, rankLookup: static[bool]): untyped =
  if rankLookup:
    result = newNimNode(nnkBracket)
    for ai in a.getType[1][1..^1]:
      assert ai.kind == nnkSym
      result.add newLit ai.strVal

  else:
    result = newNimNode(nnkBracket)
    for ai in a.getType[1][1..^1]:
      assert ai.kind == nnkSym
      result.add nnkPar.newTree(newCall("ord", ai), newLit ai.strVal)

    # echo result.repr()

func symbolName*[T: enum](a: T): string =
  const ranked = ord(high(T)) <= 0xFFFF
  const names = namesImpl(T, rankLookup = ranked)
  when ranked:
    return names[a.symbolRank]

  else:
    for (value, name) in names:
      if value == ord(a):
        return name
