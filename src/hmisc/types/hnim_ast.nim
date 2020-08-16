## Statically typed nim ast representation

import hmisc/helpers
import sequtils, colors, macros, tables, strutils, terminal
import hmisc/types/colorstring

type
  FieldBranch*[Node] = object
    # IDEA three possible parameters: `NimNode` (for compile-time
    # operations), `PNode` (for analysing code at runtime) and.
    when Node is NimNode:
      ofValue*: Node ## Exact AST used in field branch
    else:
      value*: ObjTree[Node] ## Match value for case branch

    flds*: seq[Field[Node]] ## Fields in the case branch
    isElse*: bool

  Field*[Node] = object
    ## More complex representation of object's field - supports
    ## recursive fields with case objects. IMPLEMENT - not currently
    ## supported.
    case isTuple*: bool
      of true:
        tupleIdx*: int
      of false:
        name*: string

    fldType*: string ## Type of field value
    # value*: ObjTree[Node]
    case isKind*: bool
      of true:
        selected*: int ## Index of selected branch
        branches*: seq[FieldBranch[Node]] ## List of all branches as
                                    ## `value-branch` pairs.
      of false:
        discard

  NObject*[Node] = object
    namedObject*: bool ## This object's type has a name? (tuples
    ## does not have name for a tyep)
    namedFields*: bool ## Fields have dedicated names? (anonymous
    ## tuple does not have a name for fields)
    name*: string ## Name for an object
    flds*: seq[Field[Node]]

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

  ObjTree*[Node] = object
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
    objId*: int
    isPrimitive*: bool ## Whether or not value can be considered primitive
    annotation*: string
    styling* {.requiresinit.}: PrintStyling
    case kind*: ObjKind
      of okConstant:
        constType*: string ## Type of the value
        strlit*: string ## Value representation in string form
      of okSequence:
        itemType*: string ## Type of the sequence item
        valItems*: seq[ObjTree[Node]] ## List of values
      of okTable:
        keyType*: string ## Type of table key
        valType*: string ## TYpe of value key
        valPairs*: seq[tuple[key: string, val: ObjTree[Node]]] ## List of
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
        case sectioned*: bool
          of false:
            # Simpler representation for object tree without
            # sectioning on different blocks depending on `kind`
            # fields: everything is put into single key-value
            # sequence.

            # XXX TODO Add field type
            fldPairs*: seq[tuple[name: string, value: ObjTree[Node]]] ## Sequence
            ## of field-value pairs for object representation
          of true:
            # Most of the case objects have one `kind` field named
            # 'kind' but this should account for cases with multiple
            # case fields as well as nested ones
            kindBlocks*: seq[Field[Node]] ## Object field tree. TODO -
            ## currently not implemented




  ObjElem*[Conf] = object
    case isValue: bool
      of true:
        text*: string
        config*: Conf
      of false:
        relType*: ObjRelationKind
        targetId*: int




  ValObjTree* = ObjTree[void] ## Object tree used at runtime.
  ValField* = Field[void] ## Field used at runtime
  ValFieldBranch* = FieldBranch[void] ## Field branch used at runtime

func makeObjElem*[Conf](text: string, conf: Conf): ObjElem[Conf] =
  ObjElem[Conf](isValue: true, text: text, config: conf)

func initObjTree*[Node](): ObjTree[Node] =
  ObjTree[Node](styling: initPrintStyling())

#==============================  operators  ==============================#

func `==`*[Node](lhs, rhs: Field[Node]): bool

func `==`*[Node](lhs, rhs: ObjTree[Node]): bool =
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
          lhs.sectioned == rhs.sectioned and
          (
            case lhs.sectioned:
              of true:
                subnodesEq(lhs, rhs, kindBlocks)
              of false:
                zip(lhs.fldPairs, rhs.fldPairs).mapPairs(
                  (lhs.name == rhs.name) and (lhs.value == rhs.value)
                ).foldl(a and b)
          )

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

func annotate*(tree: var ValObjTree, annotation: string): void =
  tree.annotation = annotation

func stylize*(tree: var ValObjTree, conf: PrintStyling): void =
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

func getAtPath*(obj: var ValObjTree, path: ObjPath): var ValObjTree =
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
