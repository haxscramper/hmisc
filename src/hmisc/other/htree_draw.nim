import sugar, strutils, sequtils, strformat, sugar, lenientops, math

## https://llimllib.github.io/pymag-trees/

import ../types/colorstring
import ../algo/[htree_mapping, halgorithm]
import ../hdebug_misc

type
  DrawTree[T] = ref object
    x, y: float
    tree: T
    children: seq[DrawTree[T]]
    parent: DrawTree[T]
    thread: DrawTree[T]
    offset: float
    ancestor: DrawTree[T]
    change, shift: float
    lmost_sibling: DrawTree[T]
    number: int
    tmod: float


func `$`[T](dt: DrawTree[T]): string =
  if dt == nil:
    "nil"
  else:
    (@[
      &"{dt.tree} @ ({dt.x} {dt.y})"
    ] & dt.children.mapIt($it)).join(" ").wrap("{}")


# converter toBool*[T](tr: DrawTree[T]): bool = (tr != nil)

func initDrawTree[T](
  tree: T, getSubn: T -> seq[T],
  parent: DrawTree[T] = nil, depth: int = 0, number: int = 1): DrawTree[T] =
  new(result)
  result.x = -1
  result.y = depth.float
  result.tree = tree
  result.children = collect(newSeq):
    for idx, ch in tree.getSubn():
      initDrawTree(ch, getSubn, result, depth + 1, number + 1)

  result.offset = 0
  result.ancestor = result
  result.number = number
  result.parent = parent

func left_brother[T](self: DrawTree[T]): DrawTree[T] =
  if self.parent != nil:
    for node in self.parent.children:
      if node == self:
        return result
      else:
        result = node

func get_lmost_sibling[T](self: var DrawTree[T]): DrawTree[T] =
  if (self.lmost_sibling == nil) and (self.parent != nil) and
     self != self.parent.children[0]:
    self.lmost_sibling = self.parent.children[0]

  return self.lmost_sibling

func right[T](dt: DrawTree[T]): DrawTree[T] =
  if dt.thread != nil:
    dt.thread
  else:
    if dt.children.len > 0:
      dt.children[^1]
    else:
      nil


func left[T](dt: DrawTree[T]): DrawTree[T] =
  if dt.thread != nil:
    dt.thread
  else:
    if dt.children.len > 0:
      dt.children[0]
    else:
      nil

func ancestor[T](vil, v, default_ancestor: DrawTree[T]): DrawTree[T] =
  if vil.ancestor in v.parent.children:
    vil.ancestor
  else:
    default_ancestor


func move_subtree[T](wl, wr: var DrawTree[T], shift: float): void =
    let subtrees = wr.number - wl.number
    wr.change -= shift / subtrees
    wr.shift += shift
    wl.change += shift / subtrees
    wr.x += shift
    wr.tmod += shift

iterator reversed*[T](s: seq[T]): T =
  for i in countdown(s.len - 1, 0):
    yield s[i]

func execute_shifts[T](v: var DrawTree[T]): void =
  var
    shift = 0.0
    change = 0.0

  for w in v.children.reversed():
    w.x += shift
    w.tmod += shift
    change += w.change
    shift += w.shift + change

func apportion[T](
  v: var DrawTree[T],
  default_ancestor: var DrawTree[T], distance: float): DrawTree[T] =
  var w = v.left_brother()
  if w != nil:
    #in buchheim notation:
    #i == inner; o == outer; r == right; l == left;
    var
      vir = v
      vor = v
      vil = w
      vol = v.get_lmost_sibling
      sir = v.tmod
      sor = v.tmod
      sil = vil.tmod
      sol = vol.tmod

    while vil.right() != nil and vir.left() != nil:
        vil = vil.right()
        vir = vir.left()
        vol = vol.left()
        vor = vor.right()
        vor.ancestor = v
        let shift = (vil.x + sil) - (vir.x + sir) + distance
        if shift > 0:
            var a = ancestor(vil, v, default_ancestor)
            move_subtree(a, v, shift)
            sir = sir + shift
            sor = sor + shift
        sil += vil.tmod
        sir += vir.tmod
        sol += vol.tmod
        sor += vor.tmod
    if vil.right() != nil and vor.right() == nil:
        vor.thread = vil.right()
        vor.tmod += sil - sor
    else:
        if vir.left() != nil and vol.left() == nil:
            vol.thread = vir.left()
            vol.tmod += sir - sol
        default_ancestor = v
  return default_ancestor


func firstwalk[T](v: var DrawTree[T], distance: float = 1.0): DrawTree[T] =
  if len(v.children) == 0:
    if v.get_lmost_sibling != nil:
      v.x = v.left_brother().x + distance
    else:
      v.x = 0.0
  else:
    var default_ancestor = v.children[0]
    for w in mitems(v.children):
      discard firstwalk(w)
      default_ancestor = apportion(w, default_ancestor,
                                   distance)
    execute_shifts(v)

    let
      midpoint: float = (v.children[0].x + v.children[^1].x) / 2

      ell = v.children[0]
      arr = v.children[^1]
      w = v.left_brother()
    if w != nil:
      v.x = w.x + distance
      v.tmod = v.x - midpoint
    else:
      v.x = midpoint
  return v


func second_walk[T](
  v: var DrawTree[T], m: float = 0.0, depth: int = 0): void =
  v.x += m
  v.y = depth.float

  for w in mitems(v.children):
    second_walk(w, m + v.tmod, depth+1)

func buchheim[T](tree: var DrawTree[T]): DrawTree[T] =
  result = firstwalk(tree)
  second_walk(result)

type
  Tree = ref object
    a: string
    subn: seq[Tree]

func initTree(a: string, b: varargs[Tree]): Tree =
  new(result)
  result.a = a
  result.subn = toSeq(b)

let tree = initTree(
  "1",
  initTree("2"),
  initTree("3"),
  initTree(
    "0",
    initTree("-"),
    initTree("+")
  )
)

func `$`(tr: Tree): string = tr.a

var dt = tree.initDrawTree(tr => tr.subn)
let res = buchheim(dt)

func toString*[T](dt: DrawTree[T]): string =
  var buf: seq[seq[ColoredRune]]
  dt.iterateItBFS(it.children, true):
    let str = $it.tree
    for i, ch in str:
      let
        row = int(it.x * 3) + i
        col = int(it.y * 1)

      buf[col, row] = toColored(ch)

  return buf.mapIt($it).join("\n")

echo res.toString()



when isMainModule:
  echo "hello"
