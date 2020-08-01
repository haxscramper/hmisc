import macroutils
import strutils, strformat, macros, sequtils

import ../algo/halgorithm
import ../helpers
import ../types/hnim_ast

proc getFields*(node: NimNode): seq[Field[NimNode]]

proc getBranches(node: NimNode): seq[FieldBranch[NimNode]] =
  assert node.kind == nnkRecCase, &"Cannot get branches from node kind {node.kind}"
  let caseType = $node[0][1]
  for branch in node[1..^1]:
    case branch.kind:
      of nnkOfBranch:
        result.add FieldBranch[NimNode](
          ofValue: (newTree(nnkCurly, branch[0..^2])).normalizeSet(),
          flds: branch[^1].getFields(),
          isElse: false
        )
      of nnkElse:
        result.add FieldBranch[NimNode](
          flds: branch[0].getFields(), isElse: true
        )
      else:
        raiseAssert(&"Unexpected branch kind {branch.kind}")


proc getFieldDescription(node: NimNode): tuple[name, fldType: string] =
  # echo node.treeRepr()
  case node.kind:
    of nnkIdentDefs:
      let name: string =
        case node[0].kind:
          of nnkPostfix:
            $node[0][1]
          else:
            $node[0]

      return (
        name: name,
        fldType: $(node[1].toStrLit)
      )
    of nnkRecCase:
      return getFieldDescription(node[0])

    else:
      raiseAssert(
        &"Cannot get field description from node of kind {node.kind}")

proc getFields*(node: NimNode): seq[Field[NimNode]] =
  # TODO ignore `void` fields
  case node.kind:
    of nnkObjConstr:
      # echo node.treeRepr()
      return getFields(node[0])
    of nnkSym, nnkCall, nnkDotExpr:
      let kind = node.getTypeImpl().kind
      case kind:
        of nnkBracketExpr:
          let typeSym = node.getTypeImpl()[1]
          # echo "Type symbol: ", typeSym.treeRepr()
          # echo "Impl: ", typeSym.getTypeImpl().treeRepr()
          result = getFields(typeSym.getTypeImpl())
        of nnkObjectTy, nnkRefTy, nnkTupleTy, nnkTupleConstr:
          result = getFields(node.getTypeImpl())
        else:
          raiseAssert("Unknown parameter kind: " & $kind)
    of nnkObjectTy:
      # echo "\e[41m*==========\e[49m  nnkObjectTy  \e[41m===========*\e[49m"
      # echo node[2].treeRepr()
      return node[2].getFields()
    of nnkRefTy:
      # echo "\e[41m*============\e[49m  nnkRefTy  \e[41m============*\e[49m"
      # echo node.treeRepr()
      return node.getTypeImpl()[0].getImpl()[2][0].getFields()
    of nnkRecList:
      for elem in node:
        let descr = getFieldDescription(elem)
        case elem.kind:
          of nnkRecCase: # Case field
            result.add Field[NimNode](
              isTuple: false,
              isKind: true,
              branches: getBranches(elem),
              name: descr.name,
              fldType: descr.fldType
            )

          of nnkIdentDefs: # Regular field definition
            result.add getFields(elem)[0]

          else:
            discard

    of nnkTupleTy:
      for fld in node:
        result.add fld.getFields()
    of nnkTupleConstr:
      for idx, sym in node:
        result.add Field[NimNode](isTuple: true, tupleIdx: idx)

    of nnkIdentDefs:
      let descr = getFieldDescription(node)
      result.add Field[NimNode](
        isTuple: false,
        isKind: false,
        name: descr.name,
        fldType: descr.fldType
      )
    # of nnkIntLit:
    #   let descr = getFieldDescription(node)
    #   result.add Field[NimNode](
    #     isKind: false,
    #     name: descr.name,
    #     fldType: descr.fldType
    #   )
    else:
      raiseAssert(
        &"Unexpected node kind in `getFields`: {node.kind}."
      )

proc getKindFields*[Node](flds: seq[Field[Node]]): seq[Field[Node]] =
  for fld in flds:
    if fld.isKind:
      result.add Field[Node](
        isTuple: false,
        isKind: true,
        name: fld.name,
        value: fld.value,
        fldType: fld.fldType,
        branches: fld.branches.mapIt(
          FieldBranch[Node](
            value: it.value,
            isElse: it.isElse,
            flds: it.flds.getKindFields()
          )
        ).filterIt(it.flds.len > 0)
      )

proc discardNimNode(input: seq[Field[NimNode]]): seq[ValField] =
  for fld in input:
    case fld.isKind:
      of true:
        result.add ValField(
          isTuple: false,
          isKind: true,
          name: fld.name,
          fldType: fld.fldType,
          selected: fld.selected,
          branches: fld.branches.mapIt(
            ValFieldBranch(
              value: ValObjTree(
                kind: okConstant,
                constType: (it.isElse).tern("", fld.fldType),
                strLit: (it.isElse).tern("", $it.ofValue.toStrLit())
              ),
              isElse: it.isElse,
              flds: it.flds.discardNimNode()
            )
          )
        )

      of false:
        result.add ValField(
          isTuple: false,
          isKind: false,
          name: fld.name,
          fldType: fld.fldType
        )

macro makeFieldsLiteral*(node: typed): seq[ValField] =
  result = newLit(node.getFields().discardNimNode)

proc unrollFieldLoop(
  flds: seq[Field[NimNode]],
  body: NimNode,
  fldIdx: int,
  genParam: tuple[
    lhsObj, rhsObj, lhsName, rhsName, idxName, valIdxName, isKindName, fldName: string]
     ): tuple[node: NimNode, fldIdx: int] =

  result.node = newStmtList()

  var fldIdx: int = fldIdx
  for fld in flds:
    var tmpRes = newStmtList()
    # echo &"Fld idx: {fldIdx} for {fld.name}"
    let lhsId = ident(genParam.lhsName)
    let rhsId = ident(genParam.rhsName)

    let valdefs =
      if fld.isTuple:
        let
          tupleIdx = newLit(fld.tupleIdx)
          fldName = newLit("Field" & $fldIdx)

        superquote do:
          let `lhsId` = `ident(genParam.lhsObj)`[`tupleIdx`]
          let `rhsId` = `ident(genParam.rhsObj)`[`tupleIdx`]
          const `ident(genParam.fldName)`: string = `fldName`
      else:
        let fldId = ident(fld.name)
        superquote do:
          let `lhsId` = `ident(genParam.lhsObj)`.`fldId`
          let `rhsId` = `ident(genParam.rhsObj)`.`fldId`
          const `ident(genParam.fldName)`: string = `newLit(fld.name)`

    tmpRes.add superquote do:
      const `ident(genParam.isKindName)`: bool = `newLit(fld.isKind)`
      let `ident(genParam.idxName)`: int = `newLit(fldIdx)`
      `valDefs`
      block:
        `body`
        inc `ident(genParam.valIdxName)`

    inc fldIdx
    if fld.isKind:
      # TODO check if two field kinds are identical
      var caseBlock = nnkCaseStmt.newTree(
        newDotExpr(ident genParam.lhsObj, ident fld.name)
      )

      for branch in fld.branches:
        let (branchBody, lastIdx) =
          branch.flds.unrollFieldLoop(body, fldIdx, genParam)

        fldIdx = lastIdx
        caseBlock.add nnkOfBranch.newTree(
          branch.ofValue,
          newStmtList(
            branchBody
          )
        )

      tmpRes.add do:
        if fld.isTuple:
          let fldIdx = newLit(fld.tupleIdx)
          superquote do:
            if `ident(genParam.lhsObj)`[`fldIdx`] == `ident(genParam.rhsObj)`[`fldIdx`]:
              `caseBlock`
        else:
          let fldId = ident(fld.name)
          superquote do:
            if `ident(genParam.lhsObj)`.`fldId` == `ident(genParam.rhsObj)`.`fldId`:
              `caseBlock`

    let comment =
      if fld.isTuple:
        newCommentStmtNode("Tuple field idx: " & $fld.tupleIdx)
      else:
        newCommentStmtNode("Field: " & $fld.name)

    result.node.add quote do:
      block:
        `comment`
        `tmpRes`



  result.node = newBlockStmt(result.node)
  result.fldIdx = fldIdx


macro parallelFieldPairs*(lhsObj, rhsObj: typed, body: untyped): untyped =
  ##[

Iterate two objects in parallel. Works for case objects.

Similar to parallel `fieldPairs` but also works for case objects.
Allows to iterate two objects at once, while keeping track of `kind`
fields for each type. The body is unrolled and variables are injected
for each field.

## Injected variables

:name: name of the current field
:lhs, rhs: value of current fields
:fldIdx: int. Index of current field in the object.
:valIdx: int. Index of current *value field* in the object
:lhsObj, rhsObj: Original objects being iterated. [1]_
:isKind:
  bool. Whether or not current field is used as case parameter for object

[1] Useful when iterating over results of expression

## Notes

Difference between `fldIdx` and `valIdx`. First one describes order of
fields **as defined** in object while second one shows order of fields
**as accessed** in object. For example, in object like this:

.. code-block:: nim
    type
      Case = object
        f1: int                # fldIdx - `0`, valIdx - `0`
        case kind: bool        # fldIdx - `1`, valIdx - `1`
          of true: f2: float   # fldIdx - `2`, valIdx - `2`
          of false: f3: string # fldIdx - `3`, valIdx - `2`

          # Fields `f2` and `f3` have the same `valIdx` because they
          # are located in different branches and cannot be accessed
          # at the same time.

  ]##

  let genParams = (
    lhsObj: "lhsObj",
    rhsObj: "rhsObj",
    lhsName: "lhs",
    rhsName: "rhs",
    idxName: "fldIdx",
    valIdxName: "valIdx",
    isKindName: "isKind",
    fldName: "name"
  )

  let (unrolled, _) = getFields(lhsObj).unrollFieldLoop(body, 0, genParams)
  result = superquote do:

    block: ## Toplevel
      var `ident(genParams.valIdxName)`: int = 0
      let `ident(genParams.lhsObj)` = `lhsObj`
      let `ident(genParams.rhsObj)` = `rhsObj`
      `unrolled`

  # echo "\e[41m==============\e[49m"
  # echo result.toStrLit()
  # echo "\e[41m==============\e[49m"
