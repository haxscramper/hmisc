import types/[htrie, hnim_ast]
import hpprint

#===========================  type definition  ===========================#

type
  ObjDiffKind* = enum
    odkLen
    odkKind
    odkValue

  ObjDiff* = object
    case kind*: ObjDiffKind:
      of odkLen:
        lhsLen*, rhsLen*: int
      else:
        discard

  ObjAccessor = object
    case kind: ObjKind
      of okConstant:
        nil
      of okSequence:
        idx: int
      of okComposed:
        name: string
      of okTable:
        key: string

  TreePath = seq[int]
  ObjDiffPaths = Trie[int, ObjDiff]

#=========================  diff implementation  =========================#

proc diff*[T](lhsIn, rhsIn: T, path: TreePath = @[0]): ObjDiffPaths = #[

TODO sort fields in unordered associative containers. Otherwise it
     will be impossible to adequately check what is missing.

IDEA provide LCS-based difference reports for sequences (if possible)
     instead of simply comparing lengths

]#

  when (T is seq) or (T is array):
    if lhsIn.len() != rhsIn.len():
      result[path] = ObjDiff(kind: odkLen, lhsLen: lhsIn.len(), rhsLen: rhsIn.len())

    for idx, (lval, rval) in zip(lhsIn, rhsIn):
      result.merge diff(lval, rval, path & @[idx])
  elif (T is object) or (T is tuple):
    mixin parallelFieldPairs
    parallelFieldPairs(lhsIn, rhsIn):
      when isKind:
        if lhs != rhs:
          result[path] = ObjDiff(kind: odkKind)
      else:
        result.merge diff(lhs, rhs, path & @[valIdx])
  # elif T is tuple:
  #   var idx = 0
  #   for lhs, rhs in fieldPairs(lhsIn, rhsIn):

  #     inc idx
  else:
    static: echo typeof(T)
    if lhsIn != rhsIn:
      result[path] = ObjDiff(kind: odkValue)

#========================  diff pretty-printing  =========================#

func getAtPath*[Node](tree: ObjTree[Node], path: TreePath): (ObjTree[Node], seq[ObjAccessor]) =
  case tree.kind:
    of okComposed:
      if path.len <= 1:
        return (tree, @[ObjAccessor(kind: okConstant)])
      else:
        let (subtree, accs) = getAtPath(tree.fldPairs[path[1]].value, path[1..^1])
        return (subtree, @[
          ObjAccessor(kind: okComposed, name: tree.fldPairs[path[1]].name)
        ] & accs)
    of okConstant:
      return (tree, @[ObjAccessor(kind: okConstant)])
    of okSequence:
      let (subtree, accs) = getAtPath(tree.valItems[path[1]], path[1..^1])
      return (subtree, @[
        ObjAccessor(kind: okSequence, idx: path[1])
      ] & accs)
    of okTable:
      let (subtree, accs) = getAtPath(tree.valPairs[path[1]].val, path[1..^1])
      return (subtree, @[
        ObjAccessor(kind: okTable, key: tree.valPairs[path[1]].key)
      ])

func toStr*(accs: seq[ObjAccessor]): string =
  for acc in accs:
    case acc.kind:
      of okComposed:
        result &= "." & acc.name
      of okTable:
        result &= "[" & acc.key & "]"
      of okSequence:
        result &= "[" & $acc.idx & "]"
      of okConstant:
        discard

proc ppDiff*[T](lhs, rhs: T): void =
  ## Pretty-print difference between two objects
  let diffpaths = diff(lhs, rhs)
  if diffpaths.paths().len == 0:
    return

  let
    lhsObjTree = toValObjTree(lhs)
    rhsObjTree = toValObjTree(rhs)

  for path in diffpaths.paths():
    let (lhsTree, lhsNamePath) = getAtPath(lhsObjTree, path)
    let (rhsTree, rhsNamePath) = getAtPath(rhsObjTree, path)

    echo "Difference at path ", lhsNamePath.toStr(), " kind: ", diffpaths[path]
    echo "lhs val:"
    pprint lhsTree
    echo "rhs val:"
    pprint rhsTree

proc assertNoDiff*[T](lhs, rhs: T): void =
  if lhs != rhs:
    ppDiff(lhs, rhs)
    raiseAssert("Difference")
