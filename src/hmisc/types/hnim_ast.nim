## Statically typed nim ast representation

import hmisc/helpers
import hmisc/types/colorstring
import sequtils, colors, macros, tables, strutils,
       terminal, options, parseutils, sets, strformat

func `$!`*(n: NimNode): string =
  ## NimNode stringification that does not blow up in your face on
  ## 'invalid node kind'
  n.toStrLit().strVal()

type
  ObjectAnnotKind* = enum
    oakCaseOfBranch
    oakObjectToplevel
    oakObjectField



#*************************************************************************#
#**********************  NType - nim type wrapper  ***********************#
#*************************************************************************#
#===========================  Type definition  ===========================#

# TODO support `range[a..b]`, generic constraints: `A: B | C` and `A:
# B or C`
type
  NType* = object
    head*: string
    genParams*: seq[NType]

  NVarDeclKind* = enum
    nvdLet
    nvdVar
    nvdConst

  NIdentDefs* = object
    varname: string
    kind: NVarDeclKind
    vtype: NType
    value: Option[NimNode]

#=============================  Predicates  ==============================#

#============================  Constructors  =============================#
func toNIdentDefs*(
  args: openarray[tuple[name: string, atype: NType]]): seq[NIdentDefs] =
  ## Convert array of name-type pairs into sequence of `NIdentDefs`.
  ## Each identifier will be immutable (e.g. no `var` annotation).
  for (name, atype) in args:
    result.add NIdentDefs(varname: name, vtype: atype)

func toNIdentDefs*(
  args: openarray[tuple[
    name: string,
    atype: NType,
    nvd: NVarDeclKind
     ]]): seq[NIdentDefs] =
  ## Convert array of name-type pairs into sequence of `NIdentDefs`.
  ## Each identifier must supply mutability parameter (e.g `nvdLet` or
  ## `vndVar`)
  for (name, atype, nvd) in args:
    result.add NIdentDefs(varname: name, vtype: atype, kind: nvd)

func toNimNode*(ntype: NType): NimNode =
  ## Convert `NType` to nim node
  if ntype.genParams.len == 0:
    return ident(ntype.head)
  else:
    result = nnkBracketExpr.newTree(newIdentNode(ntype.head))
    for param in ntype.genParams:
      result.add param.toNimNode()

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
      debugecho impl.treeRepr
      raiseAssert("#[ IMPLEMENT ]#")

func mkVarDecl*(name: string, vtype: NType,
                kind: NVarDeclKind = nvdLet): NIdentDefs =
  ## Declare varaible `name` of type `vtype`
  # TODO initalization value, pragma annotations and `isGensym`
  # parameter
  NIdentDefs(varname: name, kind: kind, vtype: vtype)


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


#========================  Other implementation  =========================#



func toNTypeAst*[T](): NType =
  let str = $typeof(T)
  let expr = parseExpr(str)

#===========================  Pretty-printing  ===========================#
func `$`*(nt: NType): string =
  ## Convert `NType` to textul representation
  if nt.genParams.len > 0:
    nt.head & "[" & nt.genParams.mapIt($it).join(", ") & "]"
  else:
    nt.head


#*************************************************************************#
#*******************  Enum - enum declaration wrapper  *******************#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  Enum*[Node] = object
    ## Enum declaration wrapper
    name*: string
    values*: seq[tuple[name: string, value: Option[Node]]]

  NEnum* = Enum[NimNode]

#=============================  Predicates  ==============================#
func isEnum*(en: NimNode): bool =
  ## Check if `typeImpl` for `en` is `enum`
  en.getTypeImpl().kind == nnkEnumTy

#============================  Constructors  =============================#
func parseEnumImpl*(en: NimNode): NEnum =
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


#========================  Other implementation  =========================#
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



#===========================  Pretty-printing  ===========================#






#*************************************************************************#
#*****************  Pragma - pragma annotation wrapper  ******************#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  Pragma*[Node] = object
    kind*: ObjectAnnotKind
    elements*: seq[Node]

  NPragma* = Pragma[NimNode]

#===============================  Getters  ===============================#
func getElem*(pragma: NPragma, name: string): Option[NimNode] =
  for elem in pragma.elements:
    case elem.kind:
      of nnkIdent:
        if elem.eqIdent(name):
          return some(elem)
      of nnkCall:
        if elem[0].eqIdent(name):
          return some(elem)
      else:
        raiseAssert("#[ IMPLEMENT ]#")

func getElem*(optPragma: Option[NPragma], name: string): Option[NimNode] =
  if optPragma.isSome():
    return optPragma.get().getElem(name)

#============================  constructors  =============================#
func mkNPragma*(names: varargs[string]): NPragma =
  NPragma(elements: names.mapIt(ident it))

#========================  Other implementation  =========================#

func toNimNode*(pragma: NPragma): NimNode =
  if pragma.elements.len == 0:
    newEmptyNode()
  else:
    nnkPragma.newTree(pragma.elements)

# ~~~~ proc declaration ~~~~ #

func createProcType*(p, b: NimNode, annots: NPragma): NimNode =
  ## Copy-past of `sugar.createProcType` with support for annotations
  result = newNimNode(nnkProcTy)
  var formalParams = newNimNode(nnkFormalParams)

  formalParams.add b

  case p.kind
  of nnkPar, nnkTupleConstr:
    for i in 0 ..< p.len:
      let ident = p[i]
      var identDefs = newNimNode(nnkIdentDefs)
      case ident.kind
      of nnkExprColonExpr:
        identDefs.add ident[0]
        identDefs.add ident[1]
      else:
        identDefs.add newIdentNode("i" & $i)
        identDefs.add(ident)
      identDefs.add newEmptyNode()
      formalParams.add identDefs
  else:
    var identDefs = newNimNode(nnkIdentDefs)
    identDefs.add newIdentNode("i0")
    identDefs.add(p)
    identDefs.add newEmptyNode()
    formalParams.add identDefs

  result.add formalParams
  result.add annots.toNimNode()

macro `~>`*(a, b: untyped): untyped =
  ## Construct proc type with `noSideEffect` annotation.
  result = createProcType(a, b, mkNPragma("noSideEffect"))
  # echo $!result


func mkProcDeclNode*(
  procHead: NimNode,
  rtype: Option[NType],
  args: seq[NIdentDefs],
  impl: NimNode,
  pragma: NPragma = NPragma()): NimNode =

  nnkProcDef.newTree(
    procHead,
    newEmptyNode(),
    newEmptyNode(),  # XXXX generic type parameters,
    nnkFormalParams.newTree( # arguments
      @[
        rtype.isSome().tern(
          rtype.get().toNimNode(),
          newEmptyNode()
        )
      ] &
      args.mapIt(it.toFormalParam())
    ),
    pragma.toNimNode(),
    newEmptyNode(), # XXXX reserved slot,
    impl,
  )

func mkProcDeclNode*(
  head: NimNode,
  args: openarray[tuple[name: string, atype: NType]],
  impl: NimNode,
  pragma: NPragma = NPragma()): NimNode=
  mkProcDeclNode(head, none(NType), args.toNIdentDefs(), impl, pragma)


func mkProcDeclNode*(
  accq: openarray[NimNode],
  rtype: NType,
  args: openarray[tuple[name: string, atype: NType]],
  impl: NimNode,
  pragma: NPragma = NPragma()): NimNode=
  mkProcDeclNode(
    nnkAccQuoted.newTree(accq),
    some(rtype), args.toNIdentDefs(), impl, pragma)


func mkProcDeclNode*(
  accq: openarray[NimNode],
  args: openarray[tuple[name: string, atype: NType]],
  impl: NimNode,
  pragma: NPragma = NPragma()): NimNode=
  mkProcDeclNode(
    nnkAccQuoted.newTree(accq),
    none(NType), args.toNIdentDefs(), impl, pragma)


func mkProcDeclNode*(
  head: NimNode,
  rtype: NType,
  args: openarray[tuple[name: string, atype: NType]],
  impl: NimNode,
  pragma: NPragma = NPragma()): NimNode =
  mkProcDeclNode(head, some(rtype), args.toNIdentDefs(), impl, pragma)

func mkProcDeclNode*(
  head: NimNode,
  args: openarray[tuple[
    name: string,
    atype: NType,
    nvd: NVarDeclKind]
  ],
  impl: NimNode,
  pragma: NPragma = NPragma()): NimNode =
  mkProcDeclNode(head, none(NType), args.toNIdentDefs(), impl, pragma)


func newAccQuoted*(args: varargs[NimNode]): NimNode =
  nnkAccQuoted.newTree(args)

func newAccQuoted*(args: varargs[string]): NimNode =
  nnkAccQuoted.newTree(args.mapIt(ident it))


#===========================  Pretty-printing  ===========================#

#*************************************************************************#
#*****************  Object - object declaration wrapper  *****************#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  ParseCb*[Annot] = proc(pragma: NimNode, kind: ObjectAnnotKind): Annot

  ObjectBranch*[Node, Annot] = object
    ## Single branch of case object
    annotation*: Option[Annot]
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
    # TODO:DOC
    ## More complex representation of object's field - supports
    ## recursive fields with case objects.
    annotation*: Option[Annot]
    value*: Option[Node]
    case isTuple*: bool # REVIEW REFACTOR move tuples into separate
                        # object instead of mixing them into `object`
                        # wrapper.
      of true:
        tupleIdx*: int
      of false:
        name*: string

    fldType*: NType ## Type of field value
    case isKind*: bool
      of true:
        selected*: int ## Index of selected branch
        branches*: seq[ObjectBranch[Node, Annot]] ## List of all
        ## branches as `value-branch` pairs.
      of false:
        discard

  Object*[Node, Annot] = object
    # TODO:DOC
    # TODO `flatFields` iterator to get all values with corresponding
    # parent `ofValue` branches. `for fld, ofValues in obj.flatFields()`
    exported*: bool
    annotation*: Option[Annot]
    # namedObject*: bool ## This object's type has a name? (tuples
    # ## does not have name for a tyep)
    # namedFields*: bool ## Fields have dedicated names? (anonymous
    # ## tuple does not have a name for fields)
    name*: NType ## Name for an object
    # TODO rename to objType
    flds*: seq[ObjectField[Node, Annot]]

  # FieldBranch*[Node] = ObjectBranch[Node, void]
  # Field*[Node] = ObjectField[Node, void]

  NBranch*[A] = ObjectBranch[NimNode, A]
  NField*[A] = ObjectField[NimNode, A]
  NObject*[A] = Object[NimNode, A]

  # PragmaField*[Node] = ObjectField[Node, Pragma[Node]]
  # NPragmaField* = PragmaField[NimNode]

const noParseCb*: ParseCb[void] = nil



#=============================  Predicates  ==============================#

#===============================  Getters  ===============================#

# ~~~~ each field mutable ~~~~ #

func eachFieldMut*[Node, A](
  obj: var Object[Node, A],
  cb: (var ObjectField[Node, A] ~> void)): void

func eachFieldMut*[Node, A](
  branch: var ObjectBranch[Node, A],
  cb: (var ObjectField[Node, A] ~> void)): void =
  for fld in mitems(branch.flds):
    cb(fld)
    if fld.isKind:
      for branch in mitems(fld.branches):
        eachFieldMut(branch, cb)


func eachFieldMut*[Node, A](
  obj: var Object[Node, A],
  cb: (var ObjectField[Node, A] ~> void)): void =

  for fld in mitems(obj.flds):
    cb(fld)
    if fld.isKind:
      for branch in mitems(fld.branches):
        eachFieldMut(branch, cb)

# ~~~~ each annotation mutable ~~~~ #

func eachAnnotMut*[Node, A](
  branch: var ObjectBranch[Node, A], cb: (var Option[A] ~> void)): void =
  for fld in mitems(branch.flds):
    cb(fld.annotation)
    if fld.isKind:
      for branch in mitems(fld.branches):
        eachAnnotMut(branch, cb)

func eachAnnotMut*[Node, A](
  obj: var Object[Node, A], cb: (var Option[A] ~> void)): void =

  cb(obj.annotation)

  for fld in mitems(obj.flds):
    cb(fld.annotation)
    if fld.isKind:
      for branch in mitems(fld.branches):
        branch.eachAnnotMut(cb)


# ~~~~ each field immutable ~~~~ #

func eachField*[Node, A](
  obj: Object[Node, A],
  cb: (ObjectField[Node, A] ~> void)): void

func eachField*[Node, A](
  branch: ObjectBranch[Node, A],
  cb: (ObjectField[Node, A] ~> void)): void =
  for fld in items(branch.flds):
    cb(fld)
    if fld.isKind:
      for branch in items(fld.branches):
        eachField(branch, cb)


func eachField*[Node, A](
  obj: Object[Node, A],
  cb: (ObjectField[Node, A] ~> void)): void =

  for fld in items(obj.flds):
    cb(fld)
    if fld.isKind:
      for branch in items(fld.branches):
        eachField(branch, cb)

# ~~~~ each alternative in case object ~~~~ #
# TODO generate `eachPath` - one huge if statment with all possible
#      combinations of case objects matched. In this case callback
#      should accept
#      - path - sequence of branch `ofValue` nodes

func eachCase*[A](
  fld: NField[A], objId: NimNode, cb: (NField[A] ~> NimNode)): NimNode =
  # assert fld.isKind

  if fld.isKind:
    result = nnkCaseStmt.newTree(newDotExpr(objId, ident fld.name))
    for branch in fld.branches:
      if branch.isElse:
        result.add nnkElse.newTree(
          branch.flds.mapIt(it.eachCase(objId, cb))
        )
      else:
        result.add nnkOfBranch.newTree(
          branch.ofValue,
          branch.flds.mapIt(
            it.eachCase(objId, cb)).newStmtList()
        )

    result = newStmtList(cb(fld), result)
  else:
    result = newStmtList(cb(fld))

func eachCase*[A](
  objId: NimNode, obj: NObject[A], cb: (NField[A] ~> NimNode)): NimNode =
  result = newStmtList()
  for fld in obj.flds:
    result.add fld.eachCase(objid, cb)

func eachParallelCase*[A](
  fld: NField[A], objId: (NimNode, NimNode),
  cb: (NField[A] ~> NimNode)): NimNode =

  if fld.isKind:
    result = nnkCaseStmt.newTree(newDotExpr(objId[0], ident fld.name))
    for branch in fld.branches:
      if branch.isElse:
        result.add nnkElse.newTree(
          branch.flds.mapIt(it.eachParallelCase(objId, cb))
        )
      else:
        result.add nnkOfBranch.newTree(
          branch.ofValue,
          branch.flds.mapIt(
            it.eachParallelCase(objId, cb)).newStmtList()
        )

    let
      fldId = ident fld.name
      lhsId = objId[0]
      rhsId = objId[1]

    let cbRes = cb(fld)
    result = quote do:
      `cbRes`
      if `lhsId`.`fldId` == `rhsId`.`fldId`:
        `result`

  else:
    result = newStmtList(cb(fld))

func eachParallelCase*[A](
  objid: (NimNode, NimNode), obj: NObject[A], cb: (NField[A] ~> NimNode)): NimNode =
  result = newStmtList()
  for fld in obj.flds:
    result.add fld.eachParallelCase(objid, cb)



# ~~~~ each annotation immutable ~~~~ #

func eachAnnot*[Node, A](
  branch: ObjectBranch[Node, A], cb: (Option[A] ~> void)): void =
  for fld in items(branch.flds):
    cb(fld.annotation)
    if fld.isKind:
      for branch in items(fld.branches):
        eachAnnot(branch, cb)

func eachAnnot*[Node, A](
  obj: Object[Node, A], cb: (Option[A] ~> void)): void =

  cb(obj.annotation)

  for fld in items(obj.flds):
    cb(fld.annotation.get())
    if fld.isKind:
      for branch in items(fld.branches):
        branch.eachAnnot(cb)


#===============================  Setters  ===============================#

#============================  Constructors  =============================#

#========================  Other implementation  =========================#
func toNimNode*[A](fld: NField[A], annotConv: A ~> NimNode): NimNode
func toNimNode*[A](branch: NBranch[A], annotConv: A ~> NimNode): NimNode =
  nnkOfBranch.newTree(
    branch.ofValue,
    nnkRecList.newTree(branch.flds.mapIt(it.toNimNode(annotConv))))

func toNimNode*[A](fld: NField[A], annotConv: A ~> NimNode): NimNode =
  let selector = nnkIdentDefs.newTree(
    ident fld.name,
    fld.fldType.toNimNode(),
    fld.annotation.isSome().tern(
      annotConv(fld.annotation.get()), newEmptyNode()))

  if fld.isKind:
    return nnkRecCase.newTree(
      @[selector] & fld.branches.mapIt(it.toNimNode(annotConv)))
  else:
    return selector

func toNimNode*[A](obj: NObject[A], annotConv: A ~> NimNode): NimNode =
  let header =
    if obj.annotation.isSome():
      let node = annotConv obj.annotation.get()
      if node.kind != nnkEmpty:
        nnkPragmaExpr.newTree(ident obj.name.head, node)
      else:
        ident(obj.name.head)
    else:
      ident(obj.name.head)

  let genparams: NimNode =
    block:
      let maps = obj.name.genParams.mapIt(it.toNimNode())
      if maps.len == 0:
        newEmptyNode()
      else:
        nnkGenericParams.newTree(maps)

  result = nnkTypeDef.newTree(
    header,
    genparams,
    nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode(),
      nnkRecList.newTree(
        obj.flds.mapIt(it.toNimNode(annotConv))))) # loud LISP sounds

  # echov result.treeRepr()


func toNimNode*(obj: NObject[NPragma]): NimNode =
  obj.toNimNode do(pr: NPragma) -> NimNode:
    pr.toNimNode()

#===========================  Pretty-printing  ===========================#







#*************************************************************************#
#*******  ObjTree - 'stringly typed' object value representation  ********#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  ObjKind* = enum
    # TODO:DOC
    okConstant ## Literal value
    okSequence ## Sequence of items
    okTable ## List of key-value pairs with single types for keys and
    ## values
    okComposed ## Named list of field-value pairs with possilby
    ## different types for fields (and values). List name is optional
    ## (unnamed object), field name is optional (unnamed fields)

  ObjRelationKind = enum
    # TODO:DOC
    orkComposition
    orkReference
    orkPointer

  ObjAccs = object
    # TODO:DOC
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
    # TODO:DOC
    case isValue: bool
      of true:
        text*: string
        config*: Conf
      of false:
        relType*: ObjRelationKind
        targetId*: int


type
  ValField* = ObjectField[ObjTree, void]
  ValFieldBranch* = ObjectBranch[ObjTree, void]



#=============================  Predicates  ==============================#
func `==`*[Node, A](lhs, rhs: ObjectField[Node, A]): bool

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
          subnodesEq(lhs, rhs, fldPairs)
    )

func `==`*[Node, A](lhs, rhs: ObjectField[Node, A]): bool =
  lhs.isKind == rhs.isKind and
    (
      case lhs.isKind:
        of true:
          lhs.name == rhs.name and
          lhs.fldType == rhs.fldType and
          (when A is void: true else: subnodesEq(lhs, rhs, branches))
        of false:
          true
    )

#===============================  Getters  ===============================#

#===============================  Setters  ===============================#

#============================  Constructors  =============================#
func makeObjElem*[Conf](text: string, conf: Conf): ObjElem[Conf] =
  # TODO:DOC
  ObjElem[Conf](isValue: true, text: text, config: conf)

func initObjTree*(): ObjTree =
  # TODO:DOC
  ObjTree(styling: initPrintStyling())

#========================  Other implementation  =========================#

#===========================  Pretty-printing  ===========================#





#==============================  operators  ==============================#


#*************************************************************************#
#***********************  Annotation and styling  ************************#
#*************************************************************************#

func annotate*(tree: var ObjTree, annotation: string): void =
  # TODO:DOC
  tree.annotation = annotation

func stylize*(tree: var ObjTree, conf: PrintStyling): void =
  # TODO:DOC
  tree.styling = conf

func styleTerm*(str: string, conf: PrintStyling): string =
  # TODO:DOC
  $ColoredString(str: str, styling: conf)

#*************************************************************************#
#*****************************  Path access  *****************************#
#*************************************************************************#

func objAccs*(idx: int): ObjAccs =
  # TODO:DOC
  ObjAccs(isIdx: true, idx: idx)
func objAccs*(name: string): ObjAccs =
  # TODO:DOC
  ObjAccs(isIdx: false, name: name)

func objPath*(path: varargs[ObjAccs, `objAccs`]): ObjPath =
  # TODO:DOC
  toSeq(path)

func getAtPath*(obj: var ObjTree, path: ObjPath): var ObjTree =
  # TODO:DOC
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
  ## Create `nnkBracket` with `@` prefix - sequence literal
  nnkPrefix.newTree(ident "@", nnkBracket.newTree(elems))



#*************************************************************************#
#***********************  Init call construction  ************************#
#*************************************************************************#
func makeInitCalls*[T](val: T): NimNode =
  # TODO:DOC
  when T is enum:
    ident($val)
  else:
    newLit(val)

func makeInitAllFields*[T](val: T): NimNode =
  # TODO:DOC
  result = newCall("init" & $typeof(T))
  for name, val in fieldPairs(val):
    result.add nnkExprEqExpr.newTree(
      ident(name), makeInitCalls(val))

func makeConstructAllFields*[T](val: T): NimNode =
  # TODO:DOC
  when val is seq:
    result = val.mapPairs(
      rhs.makeConstructAllFields()).toBracketSeq()
  elif val is int | float | string | bool | enum | set:
    result = newLit(val)
  else:
    when val is Option:
      when val is Option[void]:
        result = newCall(ident "none", ident "void")
      else:
        if val.isSome():
          result = newCall(ident "none", parseExpr $typeof(T))
        else:
          result = newCall(ident "some", makeConstructAllFields(val.get()))
    else:
      result = nnkObjConstr.newTree(parseExpr $typeof(T))
      for name, fld in fieldPairs(val):
        when (fld is Option) and not (fld is Option[void]):
          # debugecho name, " ", typeof(fld)
          if fld.isSome():
            result.add nnkExprColonExpr.newTree(
              ident(name),
              newCall("some", makeConstructAllFields(fld.get())))
        else:
          result.add nnkExprColonExpr.newTree(
            ident(name), makeConstructAllFields(fld))

func makeInitCalls*[A, B](table: Table[A, B]): NimNode =
  # TODO:DOC
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
  # TODO:DOC
  mixin makeInitCalls
  result = nnkBracket.newTree()
  for val in hset:
    result.add val.makeInitCalls()

  result = newCall("toHashSet", result)

proc pprintCalls*(node: NimNode, level: int): void =
  # TODO:DOC
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

# func toNimType*[Node, A](obj: )


#*************************************************************************#
#***********************  Helper proc procedures  ************************#
#*************************************************************************#
