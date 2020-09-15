import sequtils, macros, tables, options, strformat, sugar
import ../helpers
import ../hexceptions

type
  EStructKind = enum
    kItem
    kList
    kTuple
    kPairs
    kObject

  EMatchKind = enum
    mkEq
    mkIn
    mkItExpr

  EStruct = ref object
    case kind: EStructKind
      of kItem:
        discard
      of kList:
        item: EStruct
      of kPairs:
        key: EStruct
        value: EStruct
      of kTuple:
        elements: seq[EStruct]
      of kObject:
        flds: seq[tuple[
          name: string,
          struct: EStruct
        ]]

  AccsElem = object
    case inStruct: EStructKind
      of kList, kTuple:
        idx: int
      of kObject:
        fld: string
      of kPairs:
        parentKey: bool
        key: NimNode
      of kItem:
        discard

  Path = seq[AccsElem]

  VarSpec = object
    name: string
    isList: bool
    firstDecl: NimNode

  VarTable = Table[string, VarSpec]

  Case = object
    expr: NimNode
    heads: seq[NimNode]
    default: Option[NimNode]

func makeExpected(node: NimNode): EStruct =
  case node.kind:
    of nnkCurly:
      return EStruct(kind: kPairs)
    of nnkPar:
      if node.allOfIt(it.kind in {nnkExprColonExpr, nnkIdent}):
        return EStruct(kind: kObject)
      elif node.noneOfIt(it.kind in {nnkExprColonExpr, nnkIdent}):
        return EStruct(kind: kTuple)
      else:
        node.raiseCodeError(
          "Mix of named and unnamed fields is not allowed")

    of nnkIdent, nnkIntLit, nnkInfix:
      return EStruct(kind: kItem)
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {node.kind} ]#")

func updateExpected(
  parent: var EStruct, node: NimNode, path: Path): void =
  case node.kind:
    of nnkCurly:
      assert parent.kind == kPairs
      for idx, subn in node:
        assertNodeKind(subn, {nnkExprColonExpr})
        parent.key.updateExpected(subn[0], path & @[
          AccsElem(inStruct: kPairs, parentKey: true)])
        parent.value.updateExpected(subn[1], path & @[
          AccsElem(inStruct: kPairs, parentKey: true)])
    of nnkPar:
      case parent.kind:
        of kObject:
          for idx, subn in node:
            if subn.kind notin {nnkIdent}:
              assertNodeKind(subn[0], {nnkIdent}) # TODO nnkAccQuoted
              let fld = subn[0].strVal()
              var idx = parent.flds.findIt(it.name == fld)
              if idx == -1:
                parent.flds.add (name: fld, struct: makeExpected(subn[1]))
                idx = parent.flds.len - 1

              parent.flds[idx].struct.updateExpected(subn[1], path & @[
                AccsElem(inStruct: kObject, fld: fld)
              ])

        of kTuple:
          for idx, subn in node:
            if parent.elements.len >= idx:
              assertNodeKindNot(subn, {nnkExprColonExpr})
              parent.elements.add makeExpected(subn)

            parent.elements[idx].updateExpected(subn, path & @[
              AccsElem(inStruct: kTuple, idx: idx)])
        else:
          raiseAssert("#[ IMPLEMENT ]#")


    of nnkIdent, nnkIntLit, nnkInfix:
      discard

    else:
      raiseAssert(&"#[ IMPLEMENT for kind {node.kind} ]#")

func toAccs(path: Path): NimNode =
  func aux(prefix: NimNode, top: Path): NimNode =
    let head = top[0]
    result = case head.inStruct:
      of kList, kTuple:
        nnkBracketExpr.newTree(prefix, newLit(top[0].idx))
      of kObject:
        nnkDotExpr.newTree(prefix, ident head.fld)
      of kPairs:
        nnkBracketExpr.newTree(prefix, head.key)
      of kItem:
        prefix

    result = result.aux(top[1 ..^ 1])

  return (ident "expr").aux(path)

func makeInput(top: EStruct, path: Path): NimNode =
  case top.kind:
    of kItem:
      discard
    of kTuple:
      return newPar: collect(newSeq):
        for subn in top.elements:
          subn.makeInput(path)
    of kObject:
      return newPar: collect(newSeq):
        for (name, struct) in top.flds:
          newColonExpr(ident name, struct.makeInput(path))

    else:
      raiseAssert(&"#[ IMPLEMENT for kind {top.kind} ]#")


macro match*(n: tuple | object | ref object | openarray): untyped =
  var cs: Case
  cs.expr = n[0]
  for elem in n[1 .. ^1]:
    case elem.kind:
      of nnkOfBranch, nnkElifBranch:
        cs.heads.add elem[0]
      of nnkElse:
        cs.default = some(elem)
      else:
        raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")

  # Determine kind of expected toplevel structure
  var toplevel: EStruct
  for head in n[1 ..^  1]:
    if head.kind == nnkOfBranch:
      let head = head[0]
      let next = makeExpected(head)
      if toplevel.isNil:
        toplevel = next
      else:
        if next.kind != toplevel.kind:
          raiseCodeError(head, "#[ IMPLEMENT:ERRMSG ]#")

  # For each branch, update toplevel structure
  let path = @[AccsElem(inStruct: toplevel.kind)]
  for head in cs.heads:
    toplevel.updateExpected(head, path)

  # Generate input tuple for expression
  result = toplevel.makeInput(path)

  # Generate matcher expressions

  echo result.toStrLit()


