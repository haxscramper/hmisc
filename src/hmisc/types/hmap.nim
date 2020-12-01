import fusion/btreetables

export btreetables except
  Table, TableRef, initTable, toTable, newTable, newTableFrom,
  CountTable, CountTableRef, initCountTable, toCountTable, newCountTable,
  OrderedTable, OrderedTableRef, initOrderedTable, toOrderedTable,
  newOrderedTable

type
  Map*[K, V] = Table[K, V]
  MapRef*[K, V] = TableRef[K, V]
  CountMap*[A] = CountTable[A]
  CountMapRef*[A] = ref CountTableRef[A]
  OrderedMap*[A, B] = OrderedTable[A, B]
  OrderedMapRef*[A, B] = ref OrderedTableRef[A, B]


func initMap*[K, V](): Map[K, V] = initTable[K, V]()

proc toMap*[A, B](pairs: openArray[(A, B)]): Map[A, B] = toTable(pairs)

proc newMap*[A, B](): MapRef[A, B] =
  btreetables.newTable[A, B]()

proc newMap*[A, B](pairs: openArray[(A, B)]): MapRef[A, B] =
  btreetables.newTable(pairs)

proc newMapFrom*[A, B, C](collection: A, index: proc(x: B): C): MapRef[C, B] =
  btreetables.newTableFrom(collection, index)

proc initOrderedMap*[A, B](initialSize = 64): OrderedMap[A, B] =
  btreetables.initOrderedTable[A, B](initialSize)

proc toOrderedMap*[A, B](pairs: openArray[(A, B)]): OrderedMap[A, B] =
  btreetables.toOrderedTable(pairs)

proc newOrderedMap*[A, B](initialSize = 64): OrderedMapRef[A, B] =
  btreetables.newOrderedTable[A, B](initialSize)

proc newOrderedMap*[A, B](pairs: openArray[(A, B)]): OrderedMapRef[A, B] =
  btreetables.newOrderedTable(pairs)

proc initCountMap*[A](initialSize = 64): CountMap[A] =
  btreetables.initCountTable(initialSize)

proc toCountMap*[A](keys: openArray[A]): CountMap[A] =
  btreetables.toCountTable(keys)

proc newCountMap*[A](initialSize = 64): CountMapRef[A] =
  btreetables.newCountTable[A](initialSize)

proc newCountMap*[A](keys: openArray[A]): CountMapRef[A] =
  btreetables.newCountTable(keys)
