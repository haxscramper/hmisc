import std/[tables]

type
  RevSetTable*[K, V] = object
    maxId: int
    keySets: Table[K, int]
    revKeys: Table[int, seq[K]]
    valueTable: Table[int, V]
    revValue: Table[V, int]

proc addPair*[K, V](table: var RevSetTable[K, V], key: K, val: V) =
  var id: int
  if val in table.revValue:
    id = table.revValue[val]

  else:
    id = table.maxId
    inc table.maxId

  table.keySets[key] = id
  table.valueTable[id] = val
  table.revValue[val] = id
  table.revKeys.mgetOrPut(id, @[]).add key

proc getKeys*[K, V](table: RevSetTable[K, V], val: V): seq[K] =
  table.revKeys[table.revValue[val]]

proc getValue*[K, V](table: RevSetTable[K, V], key: K): V =
  table.valueTable[table.keySets[key]]

proc otherKeys*[K, V](table: RevSetTable[K, V], key: K): seq[K] =
  table.revKeys[table.keySets[key]]

proc keyValueGroup*[K, V](table: RevSetTable[K, V], key: K):
  tuple[keys: seq[K], value: V] =

  result.keys = table.otherKeys(key)
  result.value = table.getValue(key)

proc `[]`*[K, V](table: RevSetTable[K, V], key: K): V =
  table.getValue(key)

proc `[]=`*[K, V](table: var RevSetTable[K, V], key: K, val: V) =
  table.addPair(key, val)

proc contains*[K, V](table: RevSetTable[K, V], key: K): bool =
  key in table.keySets
