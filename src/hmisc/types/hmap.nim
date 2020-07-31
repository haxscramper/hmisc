import sorta
export sorta

type
  Map*[K, V] = SortedTable[K, V]

func initMap*[K, V](): Map[K, V] = initSortedTable[K, V]()
func `[]=`*[K, V](map: var Map[K, V], key: K, val: V): void =
  sorta.`[]=`(map, key, val)

func toMap*[K, V](its: seq[(K, V)]): Map[K, V] =
  result = initMap[K, V]()
  for (key, val) in its:
    result[key] = val

func toMap*[T](val: seq[T]): Map[int, T] =
  for idx, it in val:
    result[idx] = it


func maxOrSet*[K, V](tbl: var Map[K, V], key: K, val: V): void =
  if not tbl.hasKey(key):
    tbl[key] = val
  else:
    if tbl[key] < val:
      tbl[key] = val
