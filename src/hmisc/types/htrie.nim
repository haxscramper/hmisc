import tables, options
import algorithm
export tables

import ../algo/halgorithm

type
  Trie*[Key, Val] = object
    # Ordered table used for ease of testing - compare `paths()` with
    # sequence literal
    subn: OrderedTable[Key, Trie[Key, Val]]
    value: Option[Val]

proc `[]`*[Key, Val](trie: Trie[Key, Val], path: openarray[Key]): Val =
  ## Get value at path
  var curr = trie
  for key in path:
    if key in curr.subn:
      curr = curr.subn[key]
    else:
      raise newException(
        KeyError, "Trie key not found: " & $path)

  # echo curr
  if curr.value.isNone():
    raise newException(
      KeyError, "Trie key not found: " & $path)
  else:
    return curr.value.get()


proc `[]`*[Key, Val](
  trie: var Trie[Key, Val], path: openarray[Key]): var Val =
  ## Get value at path
  var curr: ptr Trie[Key, Val] = addr trie
  for key in path:
    if key in curr.subn:
      curr = addr curr.subn[key]
    else:
      raise newException(
        KeyError, "Trie key not found: " & $path)

  if curr.value.isNone():
    raise newException(
      KeyError, "Trie key not found: " & $path)
  else:
    return curr.value.get()

proc `[]=`*[Key, Val](
  trie: var Trie[Key, Val], path: openarray[Key], val: Val) =
  ## Set value at path
  # echo "Setting value ", val, " at path ", path
  var curr: ptr Trie[Key, Val] = addr trie
  for idx, key in path:
    if key notin curr.subn:
      curr.subn[key] = Trie[Key, Val]()

    curr = addr curr.subn[key]

  curr.value = some(val)


proc prefixHasValue*[Key, Val](
  trie: Trie[Key, Val], path: openarray[Key]): bool =
  ## Return true of trie contains object on path that is prefix of
  ## `path` parameter. I.e in situations like `trie: [0, 0, 1] ->
  ## some(T)` and `path = [0, 0, 1, 2]` it will return `true` since
  ## `[0, 0, 1]` is a prefix for path and it also has some value
  ## associated with it. `trie: [0, 0, 2] -> some(T)` will return
  ## `false` for the same path as there is not value on `[0, 0]`
  var curr: Trie[Key, Val] = trie
  for key in path:
    if curr.value.isSome():
      return true

    if key in curr.subn:
      curr = curr.subn[key]
      if curr.value.isSome():
        return true
    else:
      return false

iterator prefixedValues*[Key, Val](
  trie: Trie[Key, Val], path: openarray[Key], topDown: bool = true): Val =

  # for key in path:
  #   if path[0 .. pathEnd] in trie:
  #     yield trie[path[0 .. pathEnd]]

  #   pathEnd += direction

  var buf: seq[Val]
  var curr = trie
  for key in path:
    if curr.value.isSome():
      buf.add curr.value.get()

    if key in curr.subn:
      curr = curr.subn[key]

  if curr.value.isSome():
    buf.add curr.value.get()

  if topDown:
    for val in buf:
      yield val
  else:
    for val in buf.reversed():
      yield val


proc paths*[Key, Val](trie: Trie[Key, Val]): seq[seq[Key]] =
  # echo trie
  # echo "searching for paths"
  if trie.subn.len == 0:
    # echo "Subnode is empty"
    return @[]
  else:
    # echo "Subnode has ", trie.subn.len(), " nodes"
    for key, subtrie in trie.subn:
      let sub = subtrie.paths()
      if sub.len == 0:
        # echo "Subtrie has no paths"
        if subtrie.subn.len == 0:
          result.add @[key]
        else:
          for subKey, _ in subtrie.subn:
            result.add @[key, subKey]
      else:
        # echo "sub has paths:"
        # echo sub
        for path in subtrie.paths():
          result.add @[key] & path

proc contains*[Key, Val](trie: Trie[Key, Val], path: openarray[Key]): bool =
  try:
    discard trie[path]
    return true
  except KeyError:
    return false

proc merge*[Key, Val](trie: var Trie[Key, Val], other: Trie[Key, Val]): void =
  for path in other.paths():
    trie[path] = other[path]


proc merge*[Key, Val](
  trie: Trie[Key, Val], other: Trie[Key, Val]): Trie[Key, Val] =
  result = trie
  result.merge(other)
