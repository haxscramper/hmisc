import ../hdebug_misc, ../base_errors
import std/[tables, options]

type
  PTree[K, V] = ref object
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


proc getKey*[K, V](tree: PTree[K, V], key: K): PTree[K, V] =
  if tree.isAttrProvider:
    tree.getAttr(key)

  else:
    tree.attrs[key]


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


proc setIdx*[K, V](tree: var PTree[K, V], idx: int, value: PTree[K, V]): PTree[K, V] =
  if tree.isSubProvider:
    raiseArgumentError("Cannot set index to subtree provider")

  else:
    tree.subtree[idx] = value

proc add*[K, V](tree: var PTree[K, V], sub: PTree[K, V]): PTree[K, V] =
  tree.subtree.add sub

iterator items*[K, V](tree: PTree[K, V]): PTree[K, V] =
  if tree.isSubProvider:
    for sub in tree.subtree:
      yield sub

  else:
    for i in 0 ..< tree.subLen:
      yield tree.getSub(i)

iterator pairs*[K, V](tree: PTree[K, V]): (K, PTree[K, V]) =
  for key, val in pairs(tree.attrs):
    yield (key, val)
