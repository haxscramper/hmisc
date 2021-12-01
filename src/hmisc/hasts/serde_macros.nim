import std/[typetraits]
import hmisc/core/all

import std/macros {.all.}

proc getCustomPragmaValues(
    n, cp: NimNode,
    wrap: NimNodeKind,
    default: NimNode = nil
  ): NimNode =

  result = default
  let pragmaNode = customPragmaNode(n)
  for param in pragmaNode:
    if param.kind in nnkPragmaCallKinds and
       param.len > 0 and
       param[0].kind == nnkSym and
       param[0] == cp:

      if param.len == 2:
        result = param[1]

      else:
        let def = param[0].getImpl[3]
        result = newTree(wrap)
        for i in 1 ..< def.len:
          let key = def[i][0]
          let val = param[i]
          result.add newTree(nnkExprColonExpr, key, val)

      break

  if result.kind in {nnkEmpty, nnkNilLit}:
    error(n.repr & " doesn't have a pragma named " & cp.repr(), n)

macro getCustomPragmaValuesTuple*(
    sym: typed,
    pragma: typed{nkSym},
    default: untyped = nil
  ): untyped =

  getCustomPragmaValues(sym, pragma, nnkPar, default)

macro getCustomPragmaValuesSet*(
    sym: typed,
    pragma: typed{nkSym},
    default: untyped = nil
  ): untyped =

  getCustomPragmaValues(sym, pragma, nnkCurly, default)

macro getCustomPragmaValuesArray*(
    sym: typed,
    pragma: typed{nkSym},
    default: untyped = nil
  ): untyped =

  getCustomPragmaValues(sym, pragma, nnkBracket, nil)

macro isDiscriminantField*(obj: typed, name: static[string]): untyped =
  proc auxImpl(f: NimNode): NimNode =
    case f.kind:
      of nnkSym: f.getTypeImpl()
      of nnkTupleConstr, nnkObjectTy, nnkTupleTy: f
      of nnkBracketExpr: auxImpl(f[0])
      else: raise newUnexpectedKindError(f, treeRepr(f))

  proc fieldList(impl: NimNode): seq[string] =
    case impl.kind:
      of nnkBracketExpr: result = impl[1].auxImpl().fieldList()
      of nnkTupleConstr, nnkIdentDefs, nnkTupleTy: discard
      of nnkObjectTy: result = fieldList(impl[2])
      of nnkOfBranch: result = fieldList(impl[^1])
      of nnkElse: result = fieldList(impl[0])
      of nnkRecList:
        for node in items(impl):
          result.add fieldList(node)

      of nnkRecCase:
        result.add impl[0][0].strVal()
        for sub in items(impl[1..^1]):
          result.add fieldList(sub)

      else: raise newUnexpectedKindError(impl, impl.treeRepr())

  return newLit(name in obj.auxImpl().fieldList())
