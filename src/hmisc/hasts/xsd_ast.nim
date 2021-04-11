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
    "xsd:float",
    "xsd:double",
    "xsd:integer",
    "xsd:decimal",
    # ... TODO
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

proc getFirstTokens*(xsd): seq[XsdToken] =
  ##[

Return list of tokens that entry described by `xsd` can start with.

]##

  discard


proc getFirstSet*(xsd): set[XsdTokenKind] =
  for token in getFirstTokens(xsd):
    result.incl token.kind

proc getExtensionSection*(xsd): XsdEntry =
  assert xsd.isPrimitiveExtension()
  return xsd[0][0]


template treeReprRecurse(node: typed, level: int, idx: seq[int]): untyped =
  if len(node) > 0:
    result &= "\n"

  for newIdx, subn in pairs(node):
    result &= aux(subn, level + 1, idx & newIdx)
    if newIdx < len(node) - 1:
      result &= "\n"

func `$`*(occur: XsdOccursCount): string =
  if occur.isFinite:
    $occur.count

  else:
    "unbounded"

proc treeRepr*(
    xsd; colored: bool = true,
    indexed: bool = false, maxdepth: int = 120
  ): string =

  proc aux(n: XsdEntry, level: int, idx: seq[int]): string =
    template updateValue(element: typed) =
      for name, field in fieldPairs(element):
        when field is Option and
             field isnot Option[XsdEntry]
          :
          if field.isSome():
            result &= &[" ", toCyan(name, colored), " = ",
                        toYellow($field.get(), colored)]

        elif field is XsdEntry or
             field is XsdEntryKind or
             field is seq[XsdEntry] or
             field is Option[XsdEntry]
          :
          discard

        else:
          if name in ["attrs"]:
            discard

          else:
            result &= &[" ", toCyan(name, colored), " = ",
                        toYellow($field, colored)]

      if notNil(element.attrs):
        for key, value in element.attrs:
          result &= &[" ", toRed(key, colored), " = ",
                      toYellow(value, colored)]


    let pref {.inject.} =
      if indexed:
        idx.join("", ("[", "]")) & "    "
      else:
        "  ".repeat(level)

    if isNil(n): return pref & toRed("<nil>", colored)
    if level > maxdepth: return pref & " ..."

    result &= pref & toGreen(($n.kind)[3 ..^ 1], colored)

    updateValue(n[])

    case n.kind:
      of xekSimpleType:
        result &= &[" / ", toRed(($n[0].kind)[3 ..^ 1], colored)]
        updateValue(n[0][])

      else:
        discard




    case n.kind:
      of xekElement, xekAttribute: discard
      of xekSimpleType:
        treeReprRecurse(n[0], level, idx)

      else:
        treeReprRecurse(n, level, idx)

  return aux(xsd, 0, @[])

proc xsd*(str: string): string = "xsd:" & str

proc updateBaseAttrs*(xsd: var XsdEntry, node: XmlNode) =
  if isNil(node.attrs): return

  for key, value in node.attrs:
    var used = false
    case xsd.kind:
      of xekEnumeration:
        if key == "value":
          xsd.value = value
          used = true

      of xekRestriction, xekExtension:
        if key == "base":
          xsd.xsdType = some value
          used = true

      else:
        discard

    case key:
      of "name": xsd.xsdName = some value
      of "type": xsd.xsdType = some value
      of "minOccurs": xsd.xsdMinOccurs =
        some XsdOccursCount(isFinite: true, count: parseInt(value))

      of "maxOccurs":
        if value == "unbounded":
          xsd.xsdMaxOccurs = some XsdOccursCount(isFinite: false)

        else:
          xsd.xsdMaxOccurs = some XsdOccursCount(
            isFinite: true, count: parseInt(value))


      else:
        if not used:
          if xsd.attrs.isNil():
            xsd.attrs = newStringTable()

          xsd.attrs[key] = value



proc convertElement*(xml): XsdEntry =
  result = xekElement.newTree()
  updateBaseAttrs(result, xml)

  if not hasType(result):
    # The type definition corresponding to the <simpleType> or
    # <complexType> element information item in the [children], if either
    # is present, otherwise the type definition ·resolved· to by the
    # ·actual value· of the type [attribute], otherwise the {type
    # definition} of the element declaration ·resolved· to by the ·actual
    # value· of the substitutionGroup [attribute], if present, otherwise
    # the ·ur-type definition·.
    result.xsdType = some "xsd:anyType"

proc convertAttribute*(xml; context): XsdEntry =
  result = xekAttribute.newTree()
  updateBaseAttrs(result, xml)


proc convertSequence*(xml; context): XsdEntry

proc convertGroup*(xml; context): XsdEntry =
  if xml.hasAttr("name"):
    result = xekGroupDeclare.newTree()
    for node in xml:
      case node.safeTag():
        of xsd"choice", xsd"sequence":
          result.add convertSequence(node, context)


        else:
          raiseImplementError(node.safeTag())

    context.groups[xml["name"]] = result

  else:
    result = XsdEntry(kind: xekGroupRef)
    result.groupName = xml["ref"]
    result.groupRef = context.groups[result.groupName]



proc convertSequence*(xml; context): XsdEntry =
  # echo sequence
  case xml.safeTag():
    of xsd"sequence": result = XsdEntry(kind: xekSequence)
    of xsd"choice": result = XsdEntry(kind: xekChoice)
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
