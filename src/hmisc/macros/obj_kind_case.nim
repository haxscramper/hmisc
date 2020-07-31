import sugar, strutils, sequtils, strformat, sets
import hmisc/[hpprint]
import hmisc/types/[htrie, hnim_ast]
import unittest

#====================  type match inforcement macro  =====================#

import typeinfo

template objKind(o: untyped): AnyKind =
  when o is bool:
    akBool                  #  a ``bool``
  elif o is char:
    akChar                  #  a ``char``
  elif o is enum:
      akEnum                  #  an enum
  elif o is array:
    akArray                 #  an array
  elif o is object:
    akObject                #  an object
  elif o is tuple:
    akTuple                 #  a tuple
  elif o is set:
    akSet                   #  a set
  elif o is range:
    akRange                 #  a range
  elif o is ptr: # Nim pointer type
    akPtr                   #  a ptr
  elif o is ref:
    akRef                   #  a ref
  elif o is seq:
    akSequence              #  a sequence
  elif (o is proc):
    akProc                  #  a proc
  elif o is pointer: # Opaque pointer to data
    akPointer               #  a pointer
  elif o is string:
    akString                #  a string
  elif o is string:
    akCString               #  a cstring
  elif o is int:
    akInt                   #  an int
  elif o is int8:
    akInt8                  #  an int8
  elif o is int16:
    akInt16                 #  an int16
  elif o is int32:
    akInt32                 #  an int32
  elif o is int64:
    akInt64                 #  an int64
  elif o is float:
    akFloat                 #  a float
  elif o is float32:
    akFloat32               #  a float32
  elif o is float64:
    akFloat64               #  a float64
  elif o is uint:
    akUInt                  #  an unsigned int
  elif o is uint8:
    akUInt8                 #  an unsigned int8
  elif o is uint16:
    akUInt16                #  an unsigned in16
  elif o is uint32:
    akUInt32                #  an unsigned int32
  elif o is uint64:
    akUInt64                #  an unsigned int64
  else:
    akNone                   ## invalid any

# const
#   akIntegerKinds = {akInt .. akInt64}
#   akFloatKinds = {akFloat .. akFloat64}
#   akUIntKinds = {akUint .. akUInt64}
#   akNumericKinds = {akInt .. akUInt64}

import macros


import hmisc/nimast_trs


import hmisc/helpers

proc makeSetLiteral[Enum](s: set[Enum]): NimNode =
  result = nnkCurly.newTree()
  for item in s:
    result.add ident($item)

proc expandSetLiteral[Enum](
  source: NimNode, namedSets: Table[string, set[Enum]]): NimNode =
  parseEnumSet[Enum](source, namedSets).makeSetLiteral()


proc makeLiteralBranch(setExpr, caseBody: NimNode): NimNode =
  let setLiteral =
    case setExpr.kind:
      of nnkIdent: nnkCurly.newTree(setExpr)
      else: setExpr

  result = nnkElifExpr.newTree(
    nnkInfix.newTree(ident "in", ident "objType", setLiteral),
    caseBody
  )



macro switchType(expr, body: untyped): untyped =
  # echo body.treeRepr()
  var branchSets {.global, compiletime.}: set[AnyKind] = {akFloat128}
  var kindSet: set[AnyKind]
  for val in disjointIter(AnyKind):
    kindSet.incl val


  # IDEA generate named sets from consts (convert `const` to key-value
  # literal using) template and `astToStr`
  let namedSets = {
    "akIntKinds": {akInt .. akInt64},
    "akFloatKinds": {akFloat .. akFloat64},
    "akUIntKinds": {akUint .. akUInt64},
    "akNumericKinds": {akInt .. akUInt64}
  }.toTable()

  proc registerSet(node: NimNode, anchor: NimNode): void =
    let parsed = parseEnumSet[AnyKind](node, namedSets)
    let diff = (branchSets * parsed) - {akFloat128}
    if diff.len > 0:
      raiseAssert(
        "Wrong type match: expression " & posString(anchor) &
          " is not disjoint from previous branches. Overlaps: " &
          $(diff)
      )
    else:
      branchSets.incl parsed

  var hasElse {.global, compiletime.} = false
  var branches {.global, compiletime.}: seq[NimNode]
  branches = @[]
  let rewrite = makeNodeRewriteSystem:
    rule:
      patt: Call([[setExpr]], [[caseBody]])
      outp:
        registerSet(setExpr, setExpr)
        branches.add makeLiteralBranch(
          setExpr.expandSetLiteral(namedSets), caseBody)

        nnkStmtList.newTree()


    rule:
      patt: Infix(Ident(".."), [[start]], [[final]], [[caseBody]])
      outp:
        let setLiteral = nnkCurly.newTree(
            nnkInfix.newTree(ident "..", start, final)
        )

        registerSet(setLiteral, start)
        branches.add makeLiteralBranch(setLiteral, caseBody)

        nnkStmtList.newTree()

    rule:
      patt: Call([[setExpr]], [[caseBody]], Else([[elseBody]]))
      outp:
        block:
          registerSet(setExpr, setExpr)
          branches.add makeLiteralBranch(
            setExpr.expandSetLiteral(namedSets), caseBody)

        block:
          hasElse = true
          let diff = kindSet - branchSets
          let setLiteral = nnkCurly.newTree(toSeq(diff).mapIt(ident $it))
          registerSet(setLiteral, elseBody)
          branches.add makeLiteralBranch(setLiteral, caseBody)

        nnkStmtList.newTree()

    rule:
      patt: Infix(
        Ident(".."), [[start]], [[final]], [[caseBody]], [[elseBody]])
      outp:
        block:
          let setLiteral = nnkCurly.newTree(
              nnkInfix.newTree(ident "..", start, final)
          )

          registerSet(setLiteral, start)
          branches.add makeLiteralBranch(setLiteral, caseBody)

        block:
          hasElse = true
          let diff = kindSet - branchSets
          let setLiteral = nnkCurly.newTree(toSeq(diff).mapIt(ident $it))
          registerSet(setLiteral, elseBody)
          branches.add makeLiteralBranch(setLiteral, caseBody)

        nnkStmtList.newTree()

  let term = body.toTerm()
  let reduced = reduce(
    term, rewrite,
    reduceConstraints = rcRewriteOnce
  )

  if reduced.ok:
    let diff = (kindSet - branchSets)
    if diff.len > 0:
      raiseAssert("Not all cases are covered in type match " &
        posString(expr) & ". Missing " & $diff)

    result = nnkWhenStmt.newTree(branches)

    let kindId = ident "objType"
    result =
      quote do:
        const `kindId`: AnyKind = objKind(`expr`)
        `result`

    # echo "----vvvv----"
    # echo result.toStrLit()
    # echo "----^^^^----"

block:
  macro varPragma(a, b, c: untyped): untyped =
    discard

  let v {.varPragma.} = 111
  var v {.varPragma.} = 111

when false:
  block:
    macro getConst(sym: typed) =
      echo sym.treeRepr()
      echo sym.toStrLit()
      echo sym.getImpl().treeRepr()

    block:
      const c = "hello"
      getConst(c)

    block:
      const c = akInt
      getConst(c)

  macro getConst(sym: untyped) =
    echo sym.toStrLit()
    echo sym.treeRepr()

  block:
    const c = "hello"
    getConst(c)

  type
    En = enum
      en1
      en2

  const c = en1
  getConst(c)

suite "Field switch macro":
  test "{switchType} :macro:example:":
    block:
      var res = 1
      switchType(2):
        akNone:
          res = 90
        akInt:
          res = 0
        else:
          res = 8

      assert res == 0

  test "{switchType} Named set :macro:example:":
    block:
      var res = 1
      switchType(2):
        akInt:
          res = 90
        else:
          res = 8

      assertEq res, 90


#=======================  objdiff implementation  ========================#
