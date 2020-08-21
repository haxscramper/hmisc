## Statically typed nim ast representation

import hmisc/helpers
import hmisc/types/colorstring
import sequtils, colors, macros, tables, strutils,
       terminal, options, parseutils, sets, strformat

type
  NType* = object
    head*: string
    genParams*: seq[NType]

  NVarDeclKind* = enum
    nvdVar
    nvdConst
    nvdLet

  NIdentDefs* = object
    varname: string
    kind: NVarDeclKind
    vtype: NType


type
  ObjectAnnotKind* = enum
    oakCaseOfBranch
    oakObjectToplevel
    oakFieldValue

  ParseCb*[Annot] = proc(pragma: NimNode, kind: ObjectAnnotKind): Annot

  ObjectBranch*[Node, Annot] = object
    annot*: Annot
    ## Single branch of case object
    # IDEA three possible parameters: `NimNode` (for compile-time
    # operations), `PNode` (for analysing code at runtime) and.
    # when Node is NimNode:
    # ofValue*: Node ## Exact AST used in field branch
    # else:
    # TODO move `ofValue` under `isElse` case
    ofValue*: Node ## Match value for case branch

    flds*: seq[ObjectField[Node, Annot]] ## Fields in the case branch
    isElse*: bool ## Whether this branch is placed under `else` in
                  ## case object.

  ObjectField*[Node, Annot] = object
    ## More complex representation of object's field - supports
    ## recursive fields with case objects.
    annotation*: Annot
    value*: Option[Node]
    case isTuple*: bool
      of true:
        tupleIdx*: int
      of false:
        name*: string

    fldType*: string ## Type of field value
    # TODO use `NType` instead
    case isKind*: bool
      of true:
        selected*: int ## Index of selected branch
        branches*: seq[ObjectBranch[Node, Annot]] ## List of all
        ## branches as `value-branch` pairs.
      of false:
        discard

  Object*[Node, Annot] = object
    annotation*: Annot
    namedObject*: bool ## This object's type has a name? (tuples
    ## does not have name for a tyep)
    namedFields*: bool ## Fields have dedicated names? (anonymous
    ## tuple does not have a name for fields)
    name*: NType ## Name for an object
    flds*: seq[ObjectField[Node, Annot]]

  Enum = object
    name*: string
    values*: seq[tuple[
      name: string,
      value: Option[NimNode]
    ]]

  FieldBranch*[Node] = ObjectBranch[Node, void]
  Field*[Node] = ObjectField[Node, void]
  NObject*[Node] = Object[Node, void]

const noParseCb*: ParseCb[void] = nil

# proc runAnnot*(pragma: NimNode, kind: ObjectAnnotKind, cb)

type
  ObjKind* = enum
    okConstant ## Literal value
    okSequence ## Sequence of items
    okTable ## List of key-value pairs with single types for keys and
    ## values
    okComposed ## Named list of field-value pairs with possilby
    ## different types for fields (and values). List name is optional
    ## (unnamed object), field name is optional (unnamed fields)

  ObjRelationKind = enum
    orkComposition
    orkReference
    orkPointer

  ObjAccs = object
    case isIdx*: bool
      of true:
        idx*: int
      of false:
        name*: string

  ObjPath = seq[ObjAccs]

  ObjTree* = object
    ##[

## Fields

:isPrimitive: Value is primitve or not?

  Primitive value will be added to graphviz node export as part of the
  table (in regular export) as oppposed to non-primitive value (it
  will be rendered as separate node). By default primitive values are
  `int`, `string`, `float` etc. types, tuples/objects that are (1)
  composed only from primitive types (`(int, int)`), (2) have four
  fields or less. Also tables/sequences with four elements or less are
  considered primitive if (1) both keys and values are primitive (2)
  container has four elements or less.

    ]##
    path*: seq[int] ## Path of object in original tree
    objId*: int ## Unique object id
    isPrimitive*: bool ## Whether or not value can be considered primitive
    annotation*: string ## String annotation for object
    styling* {.requiresinit.}: PrintStyling ## Print styling for object
    # NOTE styling is currently unused
    case kind*: ObjKind
      of okConstant:
        constType*: string ## Type of the value
        strlit*: string ## Value representation in string form
      of okSequence:
        itemType*: string ## Type of the sequence item
        valItems*: seq[ObjTree] ## List of values
      of okTable:
        keyType*: string ## Type of table key
        valType*: string ## TYpe of value key
        valPairs*: seq[tuple[key: string, val: ObjTree]] ## List of
        ## key-value pairs for table
        # XXXX TODO TEST used `ObjTree` for key too. Non-trivial types
        # can be used. Write unit tests for this functionality.

        # NOTE REFACTOR use `value` for enum field.
      of okComposed:
        namedObject*: bool ## This object's type has a name? (tuples
        ## does not have name for a tyep)
        namedFields*: bool ## Fields have dedicated names? (anonymous
        ## tuple does not have a name for fields)
        name*: string ## Name for an object
        # TODO Add field type
        fldPairs*: seq[tuple[name: string, value: ObjTree]] ## Sequence
        ## of field-value pairs for object representation



  ObjElem*[Conf] = object
    case isValue: bool
      of true:
        text*: string
        config*: Conf
      of false:
        relType*: ObjRelationKind
        targetId*: int


type
  ValField* = Field[ObjTree]
  ValFieldBranch* = FieldBranch[ObjTree]


func makeObjElem*[Conf](text: string, conf: Conf): ObjElem[Conf] =
  ObjElem[Conf](isValue: true, text: text, config: conf)

func initObjTree*(): ObjTree =
  ObjTree(styling: initPrintStyling())

#==============================  operators  ==============================#

func `==`*[Node](lhs, rhs: Field[Node]): bool

func `==`*(lhs, rhs: ObjTree): bool =
  lhs.kind == rhs.kind and
    (
      case lhs.kind:
        of okConstant:
          lhs.constType == rhs.constType and
          lhs.strLit == rhs.strLit
        of okSequence:
          lhs.itemType == rhs.itemType and
          subnodesEq(lhs, rhs, valItems)
        of okTable:
          lhs.keyType == rhs.keyType and
          lhs.valType == rhs.valType and
          zip(lhs.valPairs, rhs.valPairs).allOfIt(
            (it[0].key == it[1].key) and (it[0].val == it[1].val)
          )
        of okComposed:
          lhs.namedObject == rhs.namedObject and
          lhs.namedFields == rhs.namedFields and
          lhs.name == rhs.name and
          # lhs.sectioned == rhs.sectioned and
          subnodesEq(lhs, rhs, fldPairs)
          # (
          #   case lhs.sectioned:
          #     of true:
          #       subnodesEq(lhs, rhs, kindBlocks)
          #     of false:
          #       zip(lhs.fldPairs, rhs.fldPairs).mapPairs(
          #         (lhs.name == rhs.name) and (lhs.value == rhs.value)
          #       ).foldl(a and b)
          # )
    )

func `==`*[Node](lhs, rhs: Field[Node]): bool =
  lhs.isKind == rhs.isKind and
    (
      case lhs.isKind:
        of true:
          lhs.name == rhs.name and
          lhs.fldType == rhs.fldType and
          # lhs.value == rhs.value and
          subnodesEq(lhs, rhs, branches)
        of false:
          true
    )

#*************************************************************************#
#***********************  Annotation and styling  ************************#
#*************************************************************************#

func annotate*(tree: var ObjTree, annotation: string): void =
  tree.annotation = annotation

func stylize*(tree: var ObjTree, conf: PrintStyling): void =
  tree.styling = conf

func styleTerm*(str: string, conf: PrintStyling): string =
  $ColoredString(str: str, styling: conf)

#*************************************************************************#
#*****************************  Path access  *****************************#
#*************************************************************************#

func objAccs*(idx: int): ObjAccs = ObjAccs(isIdx: true, idx: idx)
func objAccs*(name: string): ObjAccs = ObjAccs(isIdx: false, name: name)
func objPath*(path: varargs[ObjAccs, `objAccs`]): ObjPath = toSeq(path)
# func `@/`*(idx: int, name: string): ObjPath = @[objAccs(idx), objAccs(name)]
# func `@/`*(name: string, idx: int): ObjPath = @[objAccs(name), objAccs(idx)]
# func `@/`*(path: ObjPath, idx: int): ObjPath = path & @[objAccs(idx)]
# func `@/`*(path: ObjPath, name: string): ObjPath = path & @[objAccs(name)]

func getAtPath*(obj: var ObjTree, path: ObjPath): var ObjTree =
  # debugecho path
  case obj.kind:
    of okComposed:
      if path.len < 1:
        return obj
      else:
        if path[0].isIdx:
          return obj.fldPairs[path[0].idx].value.getAtPath(path[1..^1])
        else:
          if obj.namedFields:
            for fld in mitems(obj.fldPairs):
              if fld.name == path[0].name:
                 return fld.value.getAtPath(path[1..^1])

            raisejoin(@["Cannot get field name '", path[0].name,
              "' from object - no such field found"])
          else:
            raisejoin(@["Cannot get field name '", path[0].name,
              "' from object with unnamed fields"])
    of okConstant:
      if path.len > 1:
        raiseAssert(msgjoin(
          "Attempt to access subelements of constant value at path ",
          path))
      else:
        return obj
    of okSequence:
      if path.len == 0:
        return obj

      if not path[0].isIdx:
        raiseAssert(msgjoin(
          "Cannot access sequence elements by name, path", path,
          "starts with non-index"))
      elif path.len == 1:
        return obj.valItems[path[0].idx]
      else:
        return obj.valItems[path[0].idx].getAtPath(path[1..^1])

    else:
      raiseAssert("#[ IMPLEMENT ]#")


#*************************************************************************#
#****************************  Ast reparsing  ****************************#
#*************************************************************************#

#=======================  Enum set normalization  ========================#

proc normalizeSetImpl(node: NimNode): seq[NimNode] =
   case node.kind:
    of nnkIdent, nnkIntLit, nnkCharLit:
      return @[ node ]
    of nnkCurly:
      for subnode in node:
        result &= normalizeSetImpl(subnode)
    of nnkInfix:
      assert node[0] == ident("..")
      result = @[ node ]
    else:
      raiseAssert("Cannot normalize set: " & $node.lispRepr())


proc normalizeSet*(node: NimNode, forcebrace: bool = false): NimNode =
  ## Convert any possible set representation (e.g. `{1}`, `{1, 2}`,
  ## `{2 .. 6}` as well as `2, 3` (in case branches). Return
  ## `nnkCurly` node with all values listed one-by-one (if identifiers
  ## were used) or in ranges (if original node contained `..`)
  let vals = normalizeSetImpl(node)
  if vals.len == 1 and not forcebrace:
    return vals[0]
  else:
    return nnkCurly.newTree(vals)

proc parseEnumSet*[Enum](
  node: NimNode,
  namedSets: Table[string, set[Enum]] =
      initTable[string, set[Enum]]()): set[Enum] =
  ## Parse `NimNode` into set of `Enum` values. `namedSets` is an
  ## ident-set mapping for additional identifiers that might be used
  ## as set values.
  case node.kind:
    of nnkIdent:
      try:
        return {parseEnum[Enum]($node)}
      except ValueError:
        if $node in namedSets:
          namedSets[$node]
        else:
          raise newException(
            ValueError,
            "Invalid enum value '" & $node & "' for expression " &
              posString(node) &
              " and no such named set exists (available ones: " &
              namedSets.mapPairs(lhs).joinq() & ")"
          )
    of nnkInfix:
      assert node[0] == ident("..")
      return {parseEnum[Enum]($node[1]) ..
              parseEnum[Enum]($node[2])}
    of nnkCurly:
      for subnode in node.children:
        result.incl parseEnumSet[Enum](subnode, namedSets)

    else:
      # QUESTION there was something useful or what? Do I need it
      # here?
      discard

#==================  Helper procs for ast construction  ==================#

func toBracket*(elems: seq[NimNode]): NimNode =
  ## Create `nnkBracket` with elements
  nnkBracket.newTree(elems)

func toBracketSeq*(elems: seq[NimNode]): NimNode =
  ## Create `nnkBracket` with `@` prefix - sequence literat
  ## l
  nnkPrefix.newTree(ident "@", nnkBracket.newTree(elems))

func isEnum*(en: NimNode): bool =
  ## Check if `typeImpl` for `en` is `enum`
  en.getTypeImpl().kind == nnkEnumTy

func `$!`(n: NimNode): string = n.toStrLit().strVal()

func parseEnumImpl*(en: NimNode): Enum =
  # echov en.kind
  # debugecho en.treeRepr()
  case en.kind:
    of nnkSym:
      let impl = en.getTypeImpl()
      # echov impl.kind
      case impl.kind:
        of nnkBracketExpr:
          # let impl = impl.getTypeInst()[1].getImpl()
          return parseEnumImpl(impl.getTypeInst()[1].getImpl())
        of nnkEnumTy:
          result = parseEnumImpl(impl)
        else:
          raiseAssert(&"#[ IMPLEMENT {impl.kind} ]#")
    of nnkTypeDef:
      # result = Enum(name: )
      result = parseEnumImpl(en[2])
      result.name = en[0].strVal()
    of nnkEnumTy:
      for fld in en[1..^1]:
        case fld.kind:
          of nnkEnumFieldDef:
            result.values.add (name: fld[0].strVal(), value: some(fld[1]))
          of nnkSym:
            result.values.add (name: fld.strVal(), value: none(NimNode))
          else:
            raiseAssert(&"#[ IMPLEMENT {fld.kind} ]#")
    else:
      raiseAssert(&"#[ IMPLEMENT {en.kind} ]#")

func getEnumPref*(en: NimNode): string =
  ## Get enum prefix. As per `Nep style guide<https://nim-lang.org/docs/nep1.html#introduction-naming-conventions>`_
  ## it is recommended for members of enums should have an identifying
  ## prefix, such as an abbreviation of the enum's name. This functions
  ## returns this prefix.
  let impl = en.parseEnumImpl()
  # echov impl
  let
    name = impl.values[0].name
    pref = name.parseUntil(result, {'A' .. 'Z', '0' .. '9'})

macro enumPref*(a: typed): string = newLit(getEnumPref(a))

func getEnumNames*(en: NimNode): seq[string] =
  en.parseEnumImpl().values.mapIt(it.name)

macro enumNames*(en: typed): seq[string] = newLit en.getEnumNames()

func `$`*(nt: NType): string =
  ## Convert `NType` to textul representation
  if nt.genParams.len > 0:
    nt.head & "[" & nt.genParams.mapIt($it).join(", ") & "]"
  else:
    nt.head


func mkNType*(name: string, gparams: seq[string] = @[]): NType =
  ## Make `NType`
  NType(head: name, genParams: gparams.mapIt(mkNType(it, @[])))

func mkNType*(name: string, gparams: openarray[NType]): NType =
  ## Make `NType`
  NType(head: name, genParams: toSeq(gparams))

func mkNType*(impl: NimNode): NType =
  ## Convert type described in `NimNode` into `NType`
  case impl.kind:
    of nnkBracketExpr:
      mkNType(impl[0].strVal(), impl[1..^1].mapIt(it.mkNType()))
    of nnkIdent, nnkSym:
      mkNType(impl.strVal)
    else:
      raiseAssert("#[ IMPLEMENT ]#")

func toNimNode*(ntype: NType): NimNode =
  ## Convert `NType` to nim node
  if ntype.genParams.len == 0:
    return ident(ntype.head)
  else:
    result = nnkBracketExpr.newTree(newIdentNode(ntype.head))
    for param in ntype.genParams:
      result.add param.toNimNode()

func mkVarDecl*(name: string, vtype: NType,
                kind: NVarDeclKind = nvdLet): NIdentDefs =
  ## Declare varaible `name` of type `vtype`
  # TODO initalization value, pragma annotations and `isGensym`
  # parameter
  NIdentDefs(varname: name, kind: kind, vtype: vtype)

func toFormalParam*(nident: NIdentDefs): NimNode =
  # TODO:DOC
  let typespec =
    case nident.kind:
      of nvdVar: newTree(nnkVarTy, nident.vtype.toNimNode())
      of nvdLet: nident.vtype.toNimNode()
      of nvdConst: newTree(nnkConstTy, nident.vtype.toNimNode())

  nnkIdentDefs.newTree(
    newIdentNode(nident.varname),
    typespec,
    newEmptyNode()
  )

func mkVarDeclNode*(name: string, vtype: NType,
                    kind: NVarDeclKind = nvdLet): NimNode =
  # TODO:DOC
  mkVarDecl(name, vtype, kind).toFormalParam()


func mkNTypeNode*(name: string, gparams: seq[string]): NimNode =
  ## Create `NimNode` for type `name[@gparams]`
  mkNType(name, gparams).toNimNode()

func mkNTypeNode*(name: string, gparams: varargs[NType]): NimNode =
  ## Create `NimNode` for type `name[@gparams]`
  mkNType(name, gparams).toNimNode()

func mkCallNode*(
  dotHead: NimNode, name: string,
  args: seq[NimNode], genParams: seq[NType] = @[]): NimNode =
  ## Create node `dotHead.name[@genParams](genParams)`
  let dotexpr = nnkDotExpr.newTree(dotHead, ident(name))
  if genParams.len > 0:
    result = nnkCall.newTree()
    result.add nnkBracketExpr.newTree(
      @[ dotexpr ] & genParams.mapIt(it.toNimNode))
  else:
    result = nnkCall.newTree(dotexpr)

  for arg in args:
    result.add arg

  # debugecho "\e[31m32333\e[39m"
  # debugecho result.toStrLit().strVal()
  # debugecho result.treeRepr()
  # debugecho "\e[31m32333\e[39m"

func mkCallNode*(
  name: string,
  args: seq[NimNode],
  genParams: seq[NType] = @[]): NimNode =
  ## Create node `name[@genParams](@args)`
  if genParams.len > 0:
    result = nnkCall.newTree()
    result.add nnkBracketExpr.newTree(
      @[ newIdentNode(name) ] & genParams.mapIt(it.toNimNode()))

  else:
    result = nnkCall.newTree(ident name)

  for node in args:
    result.add node


func mkCallNode*(name: string,
                 gentypes: openarray[NType],
                 args: varargs[NimNode]): NimNode =
  ## Create node `name[@gentypes](@args)`. Overload with more
  ## convinient syntax if you have predefined number of genric
  ## parameters - `mkCallNode("name", [<param>](arg1, arg2))` looks
  ## almost like regular `quote do` interpolation.
  mkCallNode(name, toSeq(args), toSeq(genTypes))

func mkCallNode*(
  arg: NimNode, name: string,
  gentypes: openarray[NType] = @[]): NimNode =
  mkCallNode(name, @[arg], toSeq(genTypes))

func mkCallNode*(
  dotHead: NimNode, name: string,
  gentypes: openarray[NType],
  args: seq[NimNode]): NimNode =
  mkCallNode(dotHead, name, toSeq(args), toSeq(genTypes))


func toNTypeAst*[T](): NType =
  let str = $typeof(T)
  let expr = parseExpr(str)

func makeInitCalls*[T](val: T): NimNode =
  when T is enum:
    ident($val)
  else:
    newLit(val)

func makeInitAllFields*[T](val: T): NimNode =
  result = newCall("init" & $typeof(T))
  for name, val in fieldPairs(val):
    result.add nnkExprEqExpr.newTree(
      ident(name), makeInitCalls(val))

func makeConstructAllFields*[T](val: T): NimNode =
  when val is seq:
    result = val.mapPairs(
      rhs.makeConstructAllFields()).toBracketSeq()
  elif val is int | float | string | bool | enum | set:
    result = newLit(val)
  else:
    result = nnkObjConstr.newTree(ident $typeof(T))
    for name, val in fieldPairs(val):
      # debugecho name
      when val is Option:
        if val.isSome():
          result.add nnkExprColonExpr.newTree(
            ident(name),
            newCall("some", makeConstructAllFields(val.get())))
      else:
        result.add nnkExprColonExpr.newTree(
          ident(name), makeConstructAllFields(val))

  # debugecho result.toStrLit().strVal()

func makeInitCalls*[A, B](table: Table[A, B]): NimNode =
  mixin makeInitCalls
  result = nnkTableConstr.newTree()
  for key, val in table:
    result.add newColonExpr(key.makeInitCalls, val.makeInitCalls)

  result = newCall(
    nnkBracketExpr.newTree(
      ident("toTable"),
      parseExpr($typeof(A)),
      parseExpr($typeof(B))
    ),
    result
  )

func makeInitCalls*[A](hset: HashSet[A]): NimNode =
  mixin makeInitCalls
  result = nnkBracket.newTree()
  for val in hset:
    result.add val.makeInitCalls()

  result = newCall("toHashSet", result)

proc pprintCalls*(node: NimNode, level: int): void =
  let pref = "  ".repeat(level)
  let pprintKinds = {nnkCall, nnkPrefix, nnkBracket}
  case node.kind:
    of nnkCall:
      if ($node[0].toStrLit()).startsWith("make"):
        echo pref, "make", (($node[0].toStrLit())[4..^1]).toGreen()
      else:
        echo pref, $node[0].toStrLit()

      if node[1..^1].noneOfIt(it.kind in pprintKinds):
        echo pref, "  ",
          node[1..^1].mapIt($it.toStrLit()).join(", ").toYellow()
      else:
        for arg in node[1..^1]:
          pprintCalls(arg, level + 1)
    of nnkPrefix:
      echo pref, node[0]
      pprintCalls(node[1], level)
    of nnkBracket:
      for subn in node:
        pprintCalls(subn, level + 1)
    of nnkIdent:
      echo pref, ($node).toGreen()
    else:
      echo ($node.toStrLit()).indent(level * 2)
