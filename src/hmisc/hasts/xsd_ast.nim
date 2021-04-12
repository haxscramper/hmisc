import std/[options, strutils, tables]
export tables

import ./xml_ast

import ../algo/[hstring_algo, halgorithm, clformat]
import ../other/oswrap
import ../helpers

import ../types/colorstring



type
  XsdEntryKind* = enum
    xekComplexComplexType ## Complex type with complex content
    xekComplexSimpleType ## Complex type with simple content
    xekComplexType
    xekSimpleType ## Simple type
    xekElement ## `xsd:element`
    xekAttribute ## `xsd:attribute`
    xekGroupDeclare ## `xsd:group`
    xekChoice ## `xsd:choice`
    xekGroupRef ## `xsd:group ref=`
    xekSequence ## `xsd:sequence`. Ordered list of elements
    xekExtension ## `xsd:extension`
    xekRestriction ## `xsd:restriction`
    xekSimpleContent ## `xsd:simpleContent`
    xekTop ## Toplevel element in schema document
    xekEnumeration ## `xsd:enumeration`
    xekPattern

  XsdOccursCount = object
    case isFinite*: bool
      of true:
        count*: int

      of false:
        discard
        # unbounded*: bool

  XsdEntry* = ref object
    xsdName*: Option[string] ## `name=""` attribute
    xsdType: Option[string] ## `type=""` attribute
    xsdMinOccurs*: Option[XsdOccursCount] ## `minOccurs`
    xsdMaxOccurs*: Option[XsdOccursCount] ## `maxOccurs`
    subnodes: seq[XsdEntry]
    attrs*: StringTableRef
    xsdTypeImpl*: Option[XsdEntry] ## Reference to the type implementation

    case kind*: XsdEntryKind
      of xekComplexSimpleType:
        cstContent: string

      of xekGroupRef:
        groupName*: string
        groupRef*: XsdEntry ## Reference to group implementation

      of xekTop:
        startElement: XsdEntry

      of xekEnumeration:
        value*: string

      else:
        discard

  XsdVisitContext = object
    groups: Table[string, XsdEntry]
    types: Table[string, XsdEntry]

  XsdDocument* = object
    entry*: XsdEntry

using
  xml: XmlNode
  xsd: XsdEntry
  context: var XsdVisitContext

proc add*(xsd: var XsdEntry, subnode: XsdEntry) =
  xsd.subnodes.add subnode

proc `==`*(xsd; kind: XsdEntryKind): bool =
  xsd.kind == kind

func `[]`*(xsd; idx: int | BackwardsIndex): XsdEntry =
  xsd.subnodes[idx]

iterator items*(xsd): XsdEntry =
  for item in xsd.subnodes:
    yield item

iterator pairs*(xsd): (int, XsdEntry) =
  var idx = 0
  for item in xsd:
    yield (idx, item)
    inc idx

func len*(xsd): int = xsd.subnodes.len
func hasName*(xsd): bool = xsd.xsdName.isSome()
func name*(xsd): string = xsd.xsdName.get()
func hasType*(xsd): bool = xsd.xsdType.isSome()
func getType*(xsd): string = xsd.xsdType.get()
func hasTypeImpl*(xsd): bool = xsd.xsdTypeImpl.isSome()
func getTypeImpl*(xsd): XsdEntry = xsd.xsdTypeImpl.get()
func subnodes*(xsd): seq[XsdEntry] = xsd.subnodes

proc newTree*(kind: XsdEntryKind, subnodes: varargs[XsdEntry]): XsdEntry =
  result = XsdEntry(kind: kind)
  for node in subnodes:
    assert notNil(node)
    result.subnodes.add node

proc isEnumerationType*(xsd): bool =
  if xsd == xekSimpleType and xsd[0] == xekRestriction:
    for node in xsd[0]:
      if node != xekEnumeration:
        return false

      return true

proc isChoiceType*(xsd): bool =
  xsd == xekComplexType and xsd[0] == xekChoice

proc isMixed*(xsd): bool =
  notNil(xsd.attrs) and "mixed" in xsd.attrs and
  xsd.attrs["mixed"] == "true"

proc isOptional*(xsd): bool =
  ## Whether entry described by `xsd` has explicit `use="optional"` tag, or
  ## has `minOccurs` equal to zero
  return (
    notNil(xsd.attrs) and
    "use" in xsd.attrs and
    xsd.attrs["use"] == "optional"
  ) or (
    xsd.xsdMinOccurs.isSome() and
    xsd.xsdMinOccurs.get().isFinite and
    xsd.xsdMinOccurs.get().count == 0
  )

proc isUnboundedRepeat*(xsd): bool =
  ## Whether entry descibed by `xsd` does not have finite upper repeat
  ## bound. Lower repeat bound can be anything
  return xsd.xsdMaxOccurs.isSome() and not xsd.xsdMaxOccurs.get().isFinite

# template isSomeAndExpr*(opt: Option)

proc isFinite*(xsd): bool =
  return
    (
      xsd.xsdMaxOccurs.ifSomeIt(it.isFinite) and
      xsd.xsdMinOccurs.ifSomeIt(it.isFinite)
    ) or (
      xsd.xsdMaxOccurs.isNone() and
      xsd.xsdMinOccurs.isNone()
    )

proc getAttributes*(xsd): seq[XsdEntry] =
  assert xsd.kind in {xekComplexType, xekSimpleType}

  var res: seq[XsdEntry]
  proc aux(xsd: XsdEntry) =
    for node in xsd:
      case node.kind:
        of xekAttribute:
          res.add node

        of xekSimpleContent, xekExtension:
          aux(node)

        else:
          discard

  aux(xsd)
  return res

proc getElements*(xsd): seq[XsdEntry] =
  assert xsd.kind in {xekComplexType}
  if len(xsd) > 0 and xsd[0].kind in {xekChoice, xekSequence}:
    for node in items(xsd):
      if node.kind == xekElement:
        result.add node



##[

* XSD primitives check
  :properties:
  :haxdoc: :group
  :end:

]##


proc isPrimitiveRestriction*(xsd): bool =
  ## Whether entry described by `xsd` is a restriction for some base type
  ## (`xsd:string` that matches particular patern for example)
  xsd == xekSimpleType and
  xsd[0] == xekRestriction and
  xsd[0].allOfIt(it.kind in {xekPattern}) and
  xsd.getAttributes().len == 0


proc isPrimitiveExtension*(xsd): bool =
  ## Whether entry described by `xsd` is a restriction for some base type
  ## (`xsd:string` that matches particular patern for example)
  xsd == xekComplexType and
  len(xsd) > 0 and xsd[0] == xekSimpleContent and
  len(xsd[0]) > 0 and xsd[0][0] == xekExtension and
  xsd[0][0].hasType()

proc isPrimitiveType*(xsdType: string): bool =
  xsdType in [
    "xsd:string",
    "xsd:boolean",
    "xsd:decimal",
    "xsd:integer",
    "xsd:float",
    "xsd:double",
    "xsd:duration",
    "xsd:datetime",
    "xsd:time",
    "xsd:date",
    "xsd:gyearmonth",
    "xsd:gyear",
    "xsd:gmonthday",
    "xsd:gday",
    "xsd:gmonth",
    "xsd:hexbinary",
    "xsd:base64binary",
    "xsd:uri",
    "xsd:anytype",
  ]

proc isAlwaysPrimitiveType*(xsd): bool =
  ##[

Whether entry directly has a primitive type (or primitive type itself), or
it's type is a restriction of some basic type.

For example `xsd:string` is a primitive, type, and so is `DoxBool` -
because it is defined as an enumeration restriction based on primitive
type.

```xml
<xsd:simpleType name="DoxBool">
  <xsd:restriction base="xsd:string">
    <xsd:enumeration value="yes" />
    <xsd:enumeration value="no" />
  </xsd:restriction>
</xsd:simpleType>
```

Any XSD entry that has type `DoxBool` will always be a primitive type.

]##
  if not xsd.hasType():
    return false

  else:
    if xsd.getType().isPrimitiveType():
      return true

    if xsd.hasTypeImpl():
      return isAlwaysPrimitiveType(xsd.getTypeImpl())

    else:
      return false


proc isPossiblePrimitiveType*(xsd): bool =
  ##[

Similar to `isAlwaysPrimitiveType`, but checks whether object might
/potentially/ be represented using primitive entry, or it always has an
tag.

]##
  discard


## #+endsection

proc classifyPrimitiveTypeKind*(xsd): XsdTokenKind =
  case xsd.getType():
    of "xsd:string": xtkString
    of "xsd:boolean": xtkBoolean
    of "xsd:decimal": xtkDecimal
    of "xsd:integer": xtkInteger
    of "xsd:float": xtkFloat
    of "xsd:double": xtkDouble
    of "xsd:duration": xtkDuration
    of "xsd:datetime": xtkDateTime
    of "xsd:time": xtkTime
    of "xsd:date": xtkDate
    of "xsd:gyearmonth": xtkGYearMonth
    of "xsd:gyear": xtkGYear
    of "xsd:gmonthday": xtkGMonthDay
    of "xsd:gday": xtkGDay
    of "xsd:gmonth": xtkGMonth
    of "xsd:hexbinary": xtkHexBinary
    of "xsd:base64binary": xtkBase64Binary
    of "xsd:uri": xtkUri
    of "xsd:anytype": xtkAnyType
    else:
      raiseImplementError(xsd.getType())

proc namePrimitiveTypeKind*(
    kind: XsdTokenKind; withPrefix: bool = false): string =

  if kind in xtkNamedKinds:
    raiseArgumentError($kind & " cannot be named as primitive token kind")

  const map = toMapArray({
    xtkString: "string",
    xtkBoolean: "boolean",
    xtkDecimal: "decimal",
    xtkInteger: "integer",
    xtkFloat: "float",
    xtkDouble: "double",
    xtkDuration: "duration",
    xtkDateTime: "datetime",
    xtkTime: "time",
    xtkDate: "date",
    xtkGYearMonth: "gyearmonth",
    xtkGYear: "gyear",
    xtkGMonthDay: "gmonthday",
    xtkGDay: "gday",
    xtkGMonth: "gmonth",
    xtkHexBinary: "hexbinary",
    xtkBase64Binary: "base64binary",
    xtkUri: "uri",
    xtkAnyType: "anytype",
  })

  if withPrefix:
    result = "xsd:"

  result &= map[kind]

proc getFirstTokens*(xsd): seq[XsdToken] =
  ##[

Return list of tokens that entry described by `xsd` can start with.

]##

  case xsd.kind:
    of xekElement, xekRestriction:
      # `xsd:restriction` must be handled separately, because it might
      # separate set of `xsd:string` values into two subtypes using
      # `xsd:enumeration` for example. This is a placeholder
      # implementation, hopefully it won't blow up instantly
      if xsd.hasType() and xsd.getType().isPrimitiveType():
        let kind = xsd.classifyPrimitiveTypeKind()
        result.add XsdToken(kind: kind)

      elif xsd.hasTypeImpl():
        result.add getFirstTokens(xsd.getTypeImpl())

    of xekSequence:
      result = getFirstTokens(xsd[0])

    of xekChoice:
      for alt in xsd:
        result.add getFirstTokens(alt)

    of xekComplexType:
      if xsd[0].kind in {xekSequence, xekChoice}:
        result = getFirstTokens(xsd[0])

    of xekSimpleType:
      if xsd[0].kind in {xekRestriction}:
        result = getFirstTokens(xsd[0])

    else:
      raiseUnexpectedKindError(xsd)





proc getFirstSet*(xsd): set[XsdTokenKind] =
  for token in getFirstTokens(xsd):
    result.incl token.kind

proc getFirstParsers*(alts: seq[XsdToken]):
  tuple[onTokenKinds: seq[(XsdTokenKind, string)],
        onTokenNames: seq[(string, string)]] =
  ##[

Return token kind -> parser name mapping for each element in `alts`.


- @arg{alts} :: FIRST set of tokens for arbitrary entry. Use
  [[code:/getFirstTokens()]] to generate this set.
# Argument error raises
- @raise{ArgumentError} :: if alts have overlapping FIRST sets
- @ret{onTokenKinds} :: Starting token kind mapped to parser name.
  element start/ends are not added in this list (because element parsing
  is also based on `<tagname`, and so might have mutiple different mapping
  for single token). Primitive parser names are used as defined in
  [[code:xml_ast]] module. # NOTE need to make sure `[[code:xml_ast]]` is
  # resolved correctly into module name. I should look into json pointer
  # implementation and base parser on that.
- @ret{onTokenNames} :: Mapping from `<tagname` to parser name (based on
  type). Parser names are constructed using `"parse" + alt.getType()`

]##

  var used: set[XsdTokenKind]
  for alt in alts:
    if alt.kind in used:
      raiseargumenterror(
        $alt.kind &
          " is already used in first set for this group of tokens")

    else:
      case alt.kind:
        of xtkNamedKinds:
          result.onTokenNames.add((
            alt.name(),
            "parse" & toUpperAscii(alt.getType())
          ))

        else:
          # only add unnamed kinds, because named tokens might appearh
          # multiple times in the input.
          used.incl token.kind
          result.onTokenKinds.add((
            att.kind,
            alt.kind.getParsername()
          ))






proc getextensionsection*(xsd): XsdEntry =
  assert xsd.isprimitiveextension()
  return xsd[0][0]


template treereprrecurse(node: typed, level: int, idx: seq[int]): untyped =
  if len(node) > 0:
    result &= "\n"

  for newidx, subn in pairs(node):
    result &= aux(subn, level + 1, idx & newidx)
    if newidx < len(node) - 1:
      result &= "\n"

func `$`*(occur: xsdoccurscount): string =
  if occur.isfinite:
    $occur.count

  else:
    "unbounded"

proc treerepr*(
    xsd; colored: bool = true,
    indexed: bool = false, maxdepth: int = 120
  ): string =

  proc aux(n: XsdEntry, level: int, idx: seq[int]): string =
    template updatevalue(element: typed) =
      for name, field in fieldpairs(element):
        when field is option and
             field isnot option[XsdEntry]
          :
          if field.issome():
            result &= &[" ", tocyan(name, colored), " = ",
                        toyellow($field.get(), colored)]

        elif field is XsdEntry or
             field is XsdEntrykind or
             field is seq[XsdEntry] or
             field is option[XsdEntry]
          :
          discard

        else:
          if name in ["attrs"]:
            discard

          else:
            result &= &[" ", tocyan(name, colored), " = ",
                        toyellow($field, colored)]

      if notnil(element.attrs):
        for key, value in element.attrs:
          result &= &[" ", tored(key, colored), " = ",
                      toyellow(value, colored)]


    let pref {.inject.} =
      if indexed:
        idx.join("", ("[", "]")) & "    "
      else:
        "  ".repeat(level)

    if isnil(n): return pref & tored("<nil>", colored)
    if level > maxdepth: return pref & " ..."

    result &= pref & togreen(($n.kind)[3 ..^ 1], colored)

    updatevalue(n[])

    case n.kind:
      of xeksimpletype:
        result &= &[" / ", tored(($n[0].kind)[3 ..^ 1], colored)]
        updatevalue(n[0][])

      else:
        discard




    case n.kind:
      of xekelement, xekattribute: discard
      of xeksimpletype:
        treereprrecurse(n[0], level, idx)

      else:
        treereprrecurse(n, level, idx)

  return aux(xsd, 0, @[])

proc xsd*(str: string): string = "xsd:" & str

proc updatebaseattrs*(xsd: var XsdEntry, node: xmlnode) =
  if isnil(node.attrs): return

  for key, value in node.attrs:
    var used = false
    case xsd.kind:
      of xekenumeration:
        if key == "value":
          xsd.value = value
          used = true

      of xekrestriction, xekextension:
        if key == "base":
          xsd.xsdtype = some value
          used = true

      else:
        discard

    case key:
      of "name": xsd.xsdname = some value
      of "type": xsd.xsdtype = some value
      of "minoccurs": xsd.xsdminoccurs =
        some xsdoccurscount(isfinite: true, count: parseint(value))

      of "maxoccurs":
        if value == "unbounded":
          xsd.xsdmaxoccurs = some xsdoccurscount(isfinite: false)

        else:
          xsd.xsdmaxoccurs = some xsdoccurscount(
            isfinite: true, count: parseint(value))


      else:
        if not used:
          if xsd.attrs.isnil():
            xsd.attrs = newstringtable()

          xsd.attrs[key] = value



proc convertelement*(xml): XsdEntry =
  result = xekelement.newtree()
  updatebaseattrs(result, xml)

  if not hastype(result):
    # the type definition corresponding to the <simpletype> or
    # <complextype> element information item in the [children], if either
    # is present, otherwise the type definition ·resolved· to by the
    # ·actual value· of the type [attribute], otherwise the {type
    # definition} of the element declaration ·resolved· to by the ·actual
    # value· of the substitutiongroup [attribute], if present, otherwise
    # the ·ur-type definition·.
    result.xsdtype = some "xsd:anytype"

proc convertattribute*(xml; context): XsdEntry =
  result = xekattribute.newtree()
  updatebaseattrs(result, xml)


proc convertsequence*(xml; context): XsdEntry

proc convertgroup*(xml; context): XsdEntry =
  if xml.hasattr("name"):
    result = xekgroupdeclare.newtree()
    for node in xml:
      case node.safetag():
        of xsd"choice", xsd"sequence":
          result.add convertsequence(node, context)


        else:
          raiseimplementerror(node.safetag())

    context.groups[xml["name"]] = result

  else:
    result = XsdEntry(kind: xekgroupref)
    result.groupname = xml["ref"]
    result.groupref = context.groups[result.groupname]



proc convertsequence*(xml; context): XsdEntry =
  # echo sequence
  case xml.safetag():
    of xsd"sequence": result = XsdEntry(kind: xeksequence)
    of xsd"choice": result = XsdEntry(kind: xekchoice)
    else: raiseImplementError(xml.safeTag())

  for element in xml:
    case element.safeTag():
      of xsd"element":
        result.add convertElement(element)

      of xsd"sequence", xsd"choice":
        result.add convertSequence(element, context)

      of xsd"group":
        result.add convertGroup(element, context)

      else:
        echo element
        raiseImplementError(element.safeTag())

  updateBaseAttrs(result, xml)

proc convertExtension*(xml; context): XsdEntry =
  result = newTree(xekExtension)
  updateBaseAttrs(result, xml)
  for node in xml:
    case node.safeTag():
      of xsd"attribute":
        result.add convertAttribute(node, context)

      else:
        raiseImplementError(node.safeTag())


proc convertRestriction*(xml, context): XsdEntry =
  result = newTree(xekRestriction)
  updateBaseAttrs(result, xml)
  for node in xml:
    let kind =
      case node.safeTag():
        of xsd"enumeration": xekEnumeration
        of xsd"pattern": xekPattern
        else:
          raiseImplementError(node.safeTag())

    var entry: XsdEntry = newTree(kind)
    updateBaseAttrs(entry, node)
    result.add entry


proc convertComplexType*(xml; context): XsdEntry =
  if xml.find(xsd"simpleType") != -1:
    result = xekComplexSimpleType.newTree()

  elif xml.find(xsd"complexType") != -1:
    result = xekComplexComplexType.newTree()

  else:
    result = xekComplexType.newTree()

  updateBaseAttrs(result, xml)

  for subnode in xml:
    case subnode.safeTag():
      of xsd"sequence":
        result.add convertSequence(subnode, context)

      of xsd"attribute":
        result.add convertAttribute(subnode, context)

      of xsd"simpleContent":
        case subnode[0].safeTag():
          of xsd"extension":
            result.add xekSimpleContent.newTree(
              convertExtension(subnode[0], context))

          of xsd"restriction":
            result.add xekSimpleContent.newTree(
              convertRestriction(subnode[0], context))

          else:
            raiseImplementError(subnode[0].safeTag())

      of xsd"choice":
        result.add convertSequence(subnode, context)

      of xsd"group":
        result.add convertGroup(subnode, context)

      of xsd"anyAttribute":
        discard

      else:
        if subnode.kind == xnElement:
          raiseImplementError(subnode.safeTag())


  if result.hasName():
    context.types[result.name()] = result

proc convertSimpleType*(xml, context): XsdEntry =
  result = newTree(xekSimpleType)
  updateBaseAttrs(result, xml)
  case xml[0].safeTag():
    of xsd"restriction":
      result.add convertRestriction(xml[0], context)

    else:
      raiseImplementError(xml[0].safeTag())

  if result.hasName():
    context.types[result.name()] = result


proc convertSchema*(xml): XsdEntry =
  assert xml.tag == xsd"schema", xml.tag
  result = newTree(xekTop)
  var context: XsdVisitContext
  for entry in xml:
    if entry.kind == xnElement:
      case entry.tag:
        of xsd"complexType":
          result.add convertComplexType(entry, context)

        of xsd"simpleType":
          result.add convertSimpleType(entry, context)

        of xsd"import":
          discard

        of xsd"element":
          result.startElement = convertElement(entry)

        of xsd"group":
          result.add convertGroup(entry, context)

  proc updateTypes(xsd) =
    if xsd.hasType() and xsd.xsdType.get() in context.types:
      xsd.xsdTypeImpl = some context.types[xsd.xsdType.get()]

    for node in xsd:
      updateTypes(node)

  updateTypes(result)




proc parseXsd*(file: AbsFile): XsdEntry = parseXml(file).convertSchema()
proc parseXsd*(text: string): XsdEntry = parseXml(text).convertSchema()

proc main() =
  let xsd = parseXsd(AbsFile "/tmp/schema.xsd")
  echo xsd.treeRepr()


when isMainModule:
  main()
