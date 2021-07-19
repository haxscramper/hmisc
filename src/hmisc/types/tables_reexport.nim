include fusion/btreetables

iterator mvaluesFrom*[A, B](b: Table[A, B], fromKey: A): var B =
  ## Iterates over the values in the table from the given key to the end.
  for e in entriesFrom(b, fromKey):
    yield e.mval

iterator mvaluesBetween*[A, B](
    b: Table[A, B], fromKey: A, toKey: A): B =
  ## Iterates over the values in the table from `fromKey` to `toKey`
  ## inclusive.
  for e in entriesBetween(b, fromKey, toKey):
    yield e.mval

iterator mpairsFrom*[A, B](
    b: Table[A, B], fromKey: A): tuple[key: A, val: var B] =
  ## Iterates over `(key, value)` pairs in the table from the given key to
  ## the end.
  for e in entriesFrom(b, fromKey):
    yield (e.key, e.mval)

iterator mpairsBetween*[A, B](
    b: Table[A, B], fromKey: A, toKey: A): tuple[key: A, val: var B] =
  ## Iterates over `(key, value)` pairs in the table from `fromKey` to
  ## `toKey` inclusive.
  for e in entriesBetween(b, fromKey, toKey):
    yield (e.key, e.mval)

proc prev[A, B](cursor: var Cursor[A, B]): bool =
  ## Moves the cursor forward returning true if cursor.current is now valid.
  ## Never call current after next returns false.
  if cursor.len == 0:
    return false

  var (node, oldEntry) = cursor.pop()
  if not node.isNil:
    var newEntry = oldEntry - 1
    if newEntry >= 0:
      cursor.add((node, newEntry))

    var child = node.e[oldEntry].p
    if not child.isNil:
      while not child.isNil:
        cursor.add((child, 0))
        child = child.p0

  return cursor.len > 0 and cursor.current.node.m > 0

# import hmisc/other/hpprint

proc firstPair*[A, B](table: Table[A, B]): tuple[key: A, val: B] =
  var node {.cursor.} = table.root
  assert node.m > 0
  while true:
    if isNil(node.e[0].p):
      return (node.e[0].key, node.e[0].val)

    else:
      node = node.e[0].p


proc lastPair*[A, B](table: Table[A, B]): tuple[key: A, val: B] =
  var node {.cursor.} = table.root
  assert node.m > 0
  # pprint node
  while true:
    let idx = node.m - 1
    if isNil(node.e[idx].p):
      return (node.e[idx].key, node.e[idx].val)

    else:
      node = node.e[idx].p

  # while node.m > 0:
  #   node = node.e[node.m - 1].p

  # return (node.e[0].key, node.e[0].val)


proc searchOrBefore[A, B](b: Table[A, B], key: A): Cursor[A, B] =
  ## Calculates the cursor pointing to the given key.
  var a = b.root
  var exact = false
  while not a.isNil:
    var r = binarySearch(key, a)
    if r <= a.m:
      result.add((a, r))
      if eq(key, a.e[r].key):
        exact = true
        break
    a = (if r == 0: a.p0 else: a.e[r-1].p)

  if exact:
    result.add((nil, 0))
  # pprint result

iterator entriesBefore[A, B](
    b: Table[A, B], fromKey: A, withKey: bool = false): CursorPosition[A, B] =
  # Iterates the sorted table from the given key to the end.
  var cursor = b.searchOrBefore(fromKey)
  while cursor.prev():
    if withKey or cursor.current.key < fromKey:
      yield cursor.current

iterator valuesBefore*[A, B](
    b: Table[A, B], fromKey: A, withKey: bool = false): B =
  ## Iterates over the values in the table from the given key to the start.
  for e in entriesBefore(b, fromKey, withKey):
    yield e.val

iterator pairsBefore*[A, B](
    b: Table[A, B], fromKey: A, withKey: bool = false): tuple[key: B, val: B] =
  ## Iterates over the values in the table from the given key to the end.
  for e in entriesBefore(b, fromKey, withKey):
    yield (e.key, e.val)

# TODO `valuesUntil`, `smallestPair` (first pair), `largestPair` (last
# pair)
