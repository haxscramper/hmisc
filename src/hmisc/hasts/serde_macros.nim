import std/[macros, typetraits]
import hmisc/core/all

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
