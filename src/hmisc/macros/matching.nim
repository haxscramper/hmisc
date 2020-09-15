import sequtils, macros, tables, options, strformat, sugar, strutils,
       parseutils

import ../helpers
import ../hexceptions

template `->`(a, b: bool): bool = (if a: b else: true)

import hpprint

var doLog {.compiletime.}: bool = false

template startLog*() =
  static:
    doLog = true

template stopLog*() =
  static:
    doLog = false

template logThis(a: untyped): untyped =
  if doLog:
    echov a


func parseEnumField*(fld: NimNode): string =
  case fld.kind:
    of nnkEnumFieldDef:
      fld[0].strVal
    of nnkSym:
      fld.strVal
    else:
      raiseAssert(&"#[ IMPLEMENT {fld.kind} ]#")

func parseEnumImpl*(en: NimNode): seq[string] =
  case en.kind:
    of nnkSym:
      let impl = en.getTypeImpl()
      case impl.kind:
        of nnkBracketExpr:
          return parseEnumImpl(impl.getTypeInst()[1].getImpl())
        of nnkEnumTy:
          result = parseEnumImpl(impl)
        else:
          raiseAssert(&"#[ IMPLEMENT {impl.kind} ]#")
    of nnkTypeDef:
      result = parseEnumImpl(en[2])
    of nnkEnumTy:
      for fld in en[1..^1]:
        result.add parseEnumField(fld)
    of nnkTypeSection:
      result = parseEnumImpl(en[0])
    else:
      raiseAssert(&"#[ IMPLEMENT {en.kind} ]#")


func pref*(name: string): string =
  discard name.parseUntil(result, {'A' .. 'Z', '0' .. '9'})

macro hasKindImpl*(head: typed, kind: untyped): untyped =
  let
    impl = head.getTypeImpl().parseEnumImpl()
    pref = impl.commonPrefix().pref()
    names = impl.dropPrefix(pref)
    kind = ident(kind.toStrLit().strVal().addPrefix(pref))

  return nnkInfix.newTree(ident "==", head, kind)

template hasKind*(head, kindExpr: untyped): untyped =
  hasKindImpl(head.kind, kindExpr)

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

func `$`(es: EStruct): string =
  if es.isNil:
    "nil"
  else:
    case es.kind:
      of kList: &"[{es.item}]"
      of kItem: "*"
      of kTuple: es.elements.mapIt($it).join(", ").wrap("()")
      of kPairs: &"{{{es.key}: {es.value}}}"
      of kObject: es.flds.
        mapPairs(&"{lhs}: {rhs}").
        join(", ").wrap(("#(", ")"))


func isNamedTuple(node: NimNode): bool =
  node.allOfIt(it.kind in {nnkExprColonExpr, nnkIdent}) and
  node.allOfIt((it.kind == nnkIdent) -> it.strVal == "_")

func isKindCall(node: NimNode): bool =
  node[0].kind == nnkIdent and node[0].strVal()[0].isUpperAscii()

func makeExpected(node: NimNode): EStruct =
  # echov node
  case node.kind:
    of nnkCurly:
      return EStruct(kind: kPairs)
    of nnkPar:
      if node.isNamedTuple():
        return EStruct(kind: kObject)
      elif node.noneOfIt(it.kind in {nnkExprColonExpr}):
        return EStruct(kind: kTuple)
      else:
        node.raiseCodeError(
          "Mix of named and unnamed fields is not allowed")

    of nnkIdent, nnkIntLit, nnkInfix, nnkStrLit:
      return EStruct(kind: kItem)
    of nnkCall:
      if node.isKindCall():
        return EStruct(kind: kObject)
      else:
        return EStruct(kind: kItem)
    of nnkTableConstr:
      return EStruct(kind: kPairs)
    of nnkPrefix:
      assertNodeIt(node, node[0] == ident("@"), "Unexpected prefix")
      return EStruct(kind: kList)
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {node.kind} ]#")

func updateExpected(
  parent: var EStruct, node: NimNode, path: Path): void =
  assert not parent.isNil
  case node.kind:
    of nnkTableConstr:
      assert parent.kind == kPairs
      for kv in node:
        assertNodeKind(kv, {nnkExprColonExpr})
        if parent.key.isNil:
          parent.key = makeExpected(kv[0])

        if parent.value.isNil:
          parent.value = makeExpected(kv[1])

        parent.key.updateExpected(kv[0], path & @[
          AccsElem(inStruct: kPairs, parentKey: true, key: kv[0])
        ])

        parent.value.updateExpected(kv[1], path & @[
          AccsElem(inStruct: kPairs, parentKey: false, key: kv[1])
        ])


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

    of nnkPrefix:
      assert parent.kind == kList

      for idx, subn in node[1]:
        if parent.item.isNil:
          parent.item = makeExpected(subn)

        parent.item.updateExpected(subn, path & @[
          AccsElem(inStruct: kList)
        ])

    of nnkIdent, nnkIntLit, nnkInfix, nnkStrLit, nnkCall:
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

    if top.len > 1:
      result = result.aux(top[1 ..^ 1])

  if path.len > 0:
    return (ident "expr").aux(path)
  else:
    return ident "expr"

func makeInput(top: EStruct, path: Path): NimNode =
  case top.kind:
    of kItem:
      return path.toAccs()
    of kTuple:
      return newPar: collect(newSeq):
        for idx, subn in top.elements:
          subn.makeInput(path & @[
            AccsElem(inStruct: kTuple, idx: idx)
          ])
    of kObject:
      return newPar: collect(newSeq):
        for (name, struct) in top.flds:
          newColonExpr(ident name, struct.makeInput(path & @[
            AccsElem(inStruct: kObject, fld: name)
          ]))

    of kPairs:
      discard
    of kList:
      return newCall("toSeq", path.toAccs())

func makeMatchExpr(n: NimNode, path: Path, struct: EStruct): NimNode =
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit:
      if n == ident "_":
        result = ident("true")
      else:
        result = nnkInfix.newTree(ident "==", path.toAccs(), n)
    of nnkPar:
        let conds =
          if n.isNamedTuple():
            collect(newSeq):
              for idx, kv in n:
                if kv.kind == nnkIdent:
                  makeMatchExpr(kv, path & @[
                    AccsElem(inStruct: kTuple, idx: idx)
                  ], struct.flds[idx].struct)
                else:
                  makeMatchExpr(kv[1], path & @[
                    AccsElem(inStruct: kObject, fld: kv[0].strVal()
                    )], struct.flds.findItFirst(
                      it.name == kv[0].strVal()).struct
                  )
          else:
            collect(newSeq):
              for idx, val in n:
                makeMatchExpr(val, path & @[
                  AccsElem(inStruct: kTuple, idx: idx)
                ], struct.elements[idx])

        result = conds.foldl(nnkInfix.newTree(ident "and", a, b))

    of nnkPrefix:
      let conds = collect(newSeq):
        for idx, elem in n[1]:
          let
            parent = path.toAccs()
            key = newLit(idx)
            subexp = elem.makeMatchExpr(@[
              AccsElem(inStruct: kList, idx: idx)
            ], struct.item)

            input = struct.item.makeInput(@[])

          quote do:
            ((
              block:
                if `key` < `parent`.len:
                  # let it {.inject.} = expr[`key`]
                  # let expr {.inject.} = `parent`[`key`]
                  `subexp`
                else:
                  false
            ))

      result = conds.foldl(nnkInfix.newTree(ident "and", a, b))

    of nnkTableConstr:
      let conds = collect(newSeq):
        for kv in n:
          let
            parent = path.toAccs()
            key = kv[0]
            subexp = kv[1].makeMatchExpr(@[
                AccsElem(inStruct: kPairs, key: kv[0])
            ], struct.value)
            input = struct.value.makeInput(@[])


          quote do:
            ((
              block:
                if `key` in `parent`:
                  let expr {.inject.} = `parent`[`key`]
                  `subexp`
                else:
                  false
            ))

      result = conds.foldl(nnkInfix.newTree(ident "and", a, b))

    elif n.isKindCall():
      result = newCall(ident "hasKind", path.toAccs(), n[0])

    elif n.kind in {nnkInfix, nnkCall}:
      let accs = path.toAccs()

      result = quote do:
        let it {.inject.} = `accs`
        `n`
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")



func makeMatch(n: NimNode, path: Path, top: EStruct): NimNode =
  result = nnkIfStmt.newTree()
  for elem in n[1 ..^ 1]:
    case elem.kind:
      of nnkOfBranch:
        result.add nnkElifBranch.newTree(
          makeMatchExpr(elem[0], path, top),
          elem[1]
        )
      of nnkElifBranch, nnkElse:
        result.add elem
      else:
        block:
          raiseAssert(&"#[ IMPLEMENT for kind {elem.kind} ]#")


macro match*(
  n: tuple | object | ref object | seq): untyped =
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
  let path: Path = @[] #  @ =  case toplevel.kind:
    # of kObject:
    #   @[AccsElem(inStruct: kObject, fld: "expr")]
    # else:
    #   @[]

  for head in cs.heads:
    toplevel.updateExpected(head, path)

  # echo toplevel

  # Generate input tuple for expression
  let inputExpr = toplevel.makeInput(path)

  # Generate matcher expressions
  let matchcase = n.makeMatch(path, toplevel)


  let head = cs.expr
  result = quote do:
    block:
      let expr {.inject.} = `head`
      let input {.inject.} = `inputExpr`
      `matchcase`

  logThis result.toStrLit()


