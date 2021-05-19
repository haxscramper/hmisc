import ../hdebug_misc, ../base_errors
import std/[tables, options, sequtils]

type
  PTree*[K, V] = ref object
    case isValue: bool
      of true:
        value: V

      of false:
        case isAttrProvider: bool
          of true:
            getAttr: proc(key: K): PTree[K, V]

          of false:
            attrs: Table[K, PTree[K, V]]

        case isSubProvider: bool
          of true:
            getSub: proc(idx: int): PTree[K, V]
            subLen: Option[int]

          of false:
            subtree: seq[PTree[K, V]]


func newPTree*[K, V](kvPairs: openarray[(K, PTree[K, V])]): PTree[K, V] =
  result = PTree[K, V](
    isValue: false, isAttrProvider: false, isSubProvider: false)

  for (key, val) in kvPairs:
    result.attrs[key] = val


func newPTree*[K; V: not seq and not array](value: V): PTree[K, V] =
  PTree[K, V](isValue: true, value: value)

func newPTree*[K, V](subItems: openarray[PTree[K, V]]): PTree[K, V] =
  result = PTree[K, V](
    isValue: false, isAttrProvider: false, isSubProvider: false)

  for item in items(subItems):
    result.subTree.add item

func newPTree*[K, V](kvPairs: openarray[(K, V)]): PTree[K, V] =
  result = PTree[K, V](
    isValue: false, isAttrProvider: false, isSubProvider: false)

  for (key, val) in kvPairs:
    result.attrs[key] = PTree[K, V](isValue: true, value: val)

proc getVal*[K, V](tree: PTree[K, V]): V = tree.value

proc getKey*[K, V](tree: PTree[K, V], key: K): PTree[K, V] =
  if tree.isAttrProvider:
    result = tree.getAttr(key)

  else:
    when not defined(nimdoc):
      result = tree.attrs[key]


proc getIdx*[K, V](tree: PTree[K, V], idx: int): PTree[K, V] =
  if tree.isSubProvider:
    tree.getSub(idx)

  else:
    tree.subtree[idx]

proc setKey*[K, V](tree: var PTree[K, V], key: K, value: PTree[K, V]) =
  if tree.isAttrProvider:
    raiseArgumentError("Cannot set value to attr provider")

  else:
    tree.attrs[key] = value


proc setIdx*[K, V](
  tree: var PTree[K, V], idx: int, value: PTree[K, V]): PTree[K, V] =
  if tree.isSubProvider:
    raiseArgumentError("Cannot set index to subtree provider")

  else:
    tree.subtree[idx] = value

proc add*[K, V](tree: var PTree[K, V], sub: PTree[K, V]): PTree[K, V] =
  tree.subtree.add sub

iterator items*[K, V](tree: PTree[K, V]): PTree[K, V] =
  if tree.isSubProvider:
    for i in 0 ..< tree.subLen.get():
      yield tree.getSub(i)

  else:
    for sub in tree.subtree:
      yield sub


iterator pairs*[K, V](tree: PTree[K, V]): (K, PTree[K, V]) =
  for key, val in pairs(tree.attrs):
    yield (key, val)
