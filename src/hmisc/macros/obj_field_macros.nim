import macroutils, sugar, options
import strutils, strformat, macros, sequtils

import ../algo/halgorithm
import ../helpers
import ../types/[hnim_ast, colorstring]
import ../hdebug_misc

func idxTreeRepr*(inputNode: NimNode): string =
  ## `treeRepr` with indices for subnodes
  ## .. code-block::
  ##                 TypeDef
  ##     [0]            PragmaExpr
  ##     [0][0]            Postfix
  ##     [0][0][0]            Ident *
  ##     [0][0][1]            Ident Hello
  ##     [0][1]            Pragma

  func aux(node: NimNode, parent: seq[int]): seq[string] =
    result.add parent.mapIt(&"[{it}]").join("") &
      "  ".repeat(6) &
      ($node.kind)[3..^1] &
      (node.len == 0).tern(" " & node.toStrLit().strVal(), "")

    for idx, subn in node:
      result &= aux(subn, parent & @[idx])

  return aux(inputNode, @[]).join("\n")

proc getFields*[A](
  node: NimNode, cb: ParseCb[A]): seq[ObjectField[NimNode, A]]

proc getBranches*[A](
  node: NimNode, cb: ParseCb[A]): seq[ObjectBranch[NimNode, A]] =
  assert node.kind == nnkRecCase, &"Cannot get branches from node kind {node.kind}"
  let caseType = $node[0][1]
  for branch in node[1..^1]:
    case branch.kind:
      of nnkOfBranch:
        result.add ObjectBranch[NimNode, A](
          ofValue: (newTree(nnkCurly, branch[0..^2])).normalizeSet(),
          flds: branch[^1].getFields(cb),
          isElse: false
        )
      of nnkElse:
        result.add ObjectBranch[NimNode, A](
          flds: branch[0].getFields(cb), isElse: true)
      else:
        raiseAssert(&"Unexpected branch kind {branch.kind}")


proc getFieldDescription(
  node: NimNode): tuple[name: string, fldType: NType] =
  case node.kind:
    of nnkIdentDefs:
      let name: string =
        case node[0].kind:
          of nnkPostfix:
            $node[0][1]
          of nnkPragmaExpr:
            $node[0][0]
          else:
            $node[0]

      return (name: name, fldType: node[1].mkNType())
    of nnkRecCase:
      return getFieldDescription(node[0])
    else:
      raiseAssert(
        &"Cannot get field description from node of kind {node.kind}")

proc getFields*[A](
  node: NimNode, cb: ParseCb[A]): seq[ObjectField[NimNode, A]] =
  # TODO ignore `void` fields
  case node.kind:
    of nnkObjConstr:
      # echo node.treeRepr()
      return getFields(node[0], cb)
    of nnkSym, nnkCall, nnkDotExpr:
      let kind = node.getTypeImpl().kind
      case kind:
        of nnkBracketExpr:
          let typeSym = node.getTypeImpl()[1]
          result = getFields(typeSym.getTypeImpl(), cb)
        of nnkObjectTy, nnkRefTy, nnkTupleTy, nnkTupleConstr:
          result = getFields(node.getTypeImpl(), cb)
        else:
          raiseAssert("Unknown parameter kind: " & $kind)
    of nnkObjectTy:
      return node[2].getFields(cb)
    of nnkRefTy:
      return node.getTypeImpl()[0].getImpl()[2][0].getFields(cb)
    of nnkRecList:
      for elem in node:
        if elem.kind != nnkRecList:
          let descr = getFieldDescription(elem)
          case elem.kind:
            of nnkRecCase: # Case field
              # NOTE possible place for `cb` callbac
              result.add ObjectField[NimNode, A](
                isTuple: false,
                isKind: true,
                branches: getBranches(elem, cb),
                name: descr.name,
                fldType: descr.fldType,
              )

            of nnkIdentDefs: # Regular field definition
              result.add getFields(elem, cb)[0]

            else:
              discard

    of nnkTupleTy:
      for fld in node:
        result.add fld.getFields(cb)
    of nnkTupleConstr:
      for idx, sym in node:
        result.add ObjectField[NimNode, A](
          isTuple: true, tupleIdx: idx# , value: initObjTree[NimNode]()
        )

    of nnkIdentDefs:
      let descr = getFieldDescription(node)
      result.add ObjectField[NimNode, A](
        isTuple: false,
        isKind: false,
        name: descr.name,
        fldType: descr.fldType,
        value: (node[2].kind != nnkEmpty).tern(
          some(node[2]), none(NimNode))
      )

      when not (A is void):
        if node[0].kind == nnkPragmaExpr and cb != nil:
          result[^1].annotation = some cb(node, oakObjectField)

    else:
      raiseAssert(
        &"Unexpected node kind in `getFields`: {node.kind}."
      )

template filterIt2(op, body: untyped): untyped = filterIt(body, op)

proc getKindFields*[Node, A](
  flds: seq[ObjectField[Node, A]]): seq[ObjectField[Node, A]] =
  for fld in flds:
    if fld.isKind:
      result.add ObjectField[Node, A](
        isTuple: false,
        isKind: true,
        name: fld.name,
        fldType: fld.fldType,
        branches:
          block:
            filterIt2(it.flds.len > 0):
              collect(newSeq):
                for it in fld.branches:
                  ObjectBranch[Node, A](
                    ofValue: it.ofValue,
                    isElse: it.isElse,
                    flds: it.flds.getKindFields()))

proc discardNimNode(
  input: seq[ObjectField[NimNode, void]]): seq[ValField] =
  for fld in input:
    case fld.isKind:
      of true:
        result.add ValField(
          # value: initObjTree[void](),
          isTuple: false,
          isKind: true,
          name: fld.name,
          fldType: fld.fldType,
          selected: fld.selected,
          branches: fld.branches.mapIt(
            ValFieldBranch(
              ofValue: ObjTree(
                styling: initPrintStyling(),
                kind: okConstant,
                constType: (it.isElse).tern("", $fld.fldType),
                strLit: (it.isElse).tern("", $it.ofValue.toStrLit())
              ),
              isElse: it.isElse,
              flds: it.flds.discardNimNode())))

      of false:
        result.add ValField(
          # value: initObjTree[void](),
          isTuple: false,
          isKind: false,
          name: fld.name,
          fldType: fld.fldType
        )

func parseNimPragma*(node: NimNode, position: ObjectAnnotKind): NPragma =
  result.kind = position
  case position:
    of oakObjectField:
      # debugecho node.idxTreeRepr
      node.expectKind nnkIdentDefs
      result.elements = toSeq(node[0][1].children)
    else:
      node.expectKind nnkPragma
      result.elements = toSeq(node.children)



proc parseObject*[A](node: NimNode, cb: ParseCb[A]): Object[NimNode, A] =
  node.expectKind nnkTypeDef
  result = Object[NimNode, A](
    flds: node[2].getFields(cb)
  )

  case node[0].kind:
    of nnkPragmaExpr:
      case node[0][0].kind:
        of nnkPostfix:
          result.name = mkNType(node[0][0][1].strVal())
          result.exported = true
        else:
          result.name = mkNType(node[0][0].strVal())
    else:
      result.name = mkNType(node[0].strVal())

  if node[0].kind == nnkPragmaExpr and cb != nil:
    result.annotation = some cb(node[0][1], oakObjectToplevel)


macro makeFieldsLiteral*(node: typed): untyped =
  return node.getFields(noParseCb).discardNimNode.makeConstructAllFields()

type
  GenParams = object
    lhsObj, rhsObj, lhsName, rhsName, idxName, valIdxName, isKindName, fldName: string
    hackFields: bool

proc unrollFieldLoop[A](
  flds: seq[ObjectField[NimNode, A]],
  body: NimNode,
  fldIdx: int,
  genParam: GenParams): tuple[node: NimNode, fldIdx: int] =

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
        let
          fldId = ident(fld.name)
          fldName = newLit(fld.name)

        if genParam.hackFields:
          let fldType = fld.fldType.toNimNode()
          let tmp = superquote do:
            const `ident(genParam.fldName)`: string = `fldName`
            let `lhsId`: `fldType` = block:
              var tmp: `fldType`
              for name, val in fieldPairs(`ident(genParam.lhsObj)`):
                when val is `fldType`:
                  if name == `fldName`:
                    tmp = val
                    break

              tmp

            let `rhsId`: `fldType` = block:
              var tmp: `fldType`
              for name, val in fieldPairs(`ident(genParam.rhsObj)`):
                when val is `fldType`:
                  if name == `fldName`:
                    tmp = val
                    break

              tmp
          # echo tmp.toStrLit()
          tmp
        else:
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
        ident(genParam.lhsName)
        # newDotExpr(ident genParam.lhsObj, ident fld.name)
      )


      for branch in fld.branches:
        let (branchBody, lastIdx) =
          branch.flds.unrollFieldLoop(body, fldIdx, genParam)

        fldIdx = lastIdx
        if branch.isElse:
          caseBlock.add nnkElse.newTree(
            newStmtList(
              newCommentStmtNode(
                "Fallback for value `" & $branch.ofValue.toStrLit() &
                  "` of field " & fld.name),
              branchBody
            )
          )
        else:
          caseBlock.add nnkOfBranch.newTree(
            branch.ofValue,
            newStmtList(
              newCommentStmtNode(
                "Branch for value `" & $branch.ofValue.toStrLit() &
                  "` of field " & fld.name),
              branchBody
            )
          )

      caseBlock = newStmtList(
        newCommentStmtNode("Selector "),
        caseBlock)

      tmpRes.add do:
        if fld.isTuple:
          let fldIdx = newLit(fld.tupleIdx)
          superquote do:
            if `ident(genParam.lhsObj)`[`fldIdx`] == `ident(genParam.rhsObj)`[`fldIdx`]:
              `caseBlock`
        else:
          let fldId = ident(fld.name)
          superquote do:
            ## Case field comparison
            if `lhsId` == `rhsId`:
              `caseBlock`
            # if `ident(genParam.lhsObj)`.`fldId` == `ident(genParam.rhsObj)`.`fldId`:
            #   `caseBlock`

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

### Limitations

- Private fields cannot be accessed directly - compilation will fail.
  Can be fixed by defining getter with the same name as a field (e.g
  for field `kind` define `func kind(t: T): KindType`). Dirty workaround
  is to use `hackPrivateParallelFieldPairs` - it can access private
  fields (but does not work if your private private field has a
  non-exported type)

### `fldIdx` and `valIdx`

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
  # TODO optionally ignore private fields

  let genParams = GenParams(
    lhsObj: "lhsObj",
    rhsObj: "rhsObj",
    lhsName: "lhs",
    rhsName: "rhs",
    idxName: "fldIdx",
    valIdxName: "valIdx",
    isKindName: "isKind",
    fldName: "name"
  )

  let (unrolled, _) = getFields(
    lhsObj, noParseCb).unrollFieldLoop(body, 0, genParams)
  result = superquote do:

    block: ## Toplevel
      var `ident(genParams.valIdxName)`: int = 0
      let `ident(genParams.lhsObj)` = `lhsObj`
      let `ident(genParams.rhsObj)` = `rhsObj`
      `unrolled`


macro hackPrivateParallelFieldPairs*(lhsObj, rhsObj: typed, body: untyped): untyped =
  ## Same API as `parallelFieldPairs` but uses HACK to access private
  ## fields. NOT: due to HACK being used compilation is even slower.
  let genParams = GenParams(
    lhsObj: "lhsObj",
    rhsObj: "rhsObj",
    lhsName: "lhs",
    rhsName: "rhs",
    idxName: "fldIdx",
    valIdxName: "valIdx",
    isKindName: "isKind",
    fldName: "name",
    hackFields: true
  )

  let (unrolled, _) = getFields(
    lhsObj, noParseCb).unrollFieldLoop(body, 0, genParams)
  let section = newCommentStmtNode(
    "Type: " & $lhsObj.getTypeInst().toStrLit())
  result = superquote do:
    block: ## Toplevel
      `section`
      var `ident(genParams.valIdxName)`: int = 0
      let `ident(genParams.lhsObj)` = `lhsObj`
      let `ident(genParams.rhsObj)` = `rhsObj`
      `unrolled`

  # result.colorPrint()
