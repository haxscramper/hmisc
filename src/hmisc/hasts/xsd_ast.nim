import std/[options, strutils, tables, hashes, sets]
export tables

import ./xml_ast
export xml_ast

import ../algo/[hstring_algo, halgorithm, clformat]
import ../other/oswrap
import ../core/all
import ../types/colorstring



type
  XsdEntryKind* = enum
    xekComplexComplexType ## Complex type with complex content
    xekComplexSimpleType ## Complex type with simple content
    xekComplexType
    xekSimpleType ## Simple type
    xekElement ## `xsd:element`
    xekAttribute ## `xsd:attribute`
    xekGroupDecl ## `xsd:group`
    xekChoice ## `xsd:choice`
    xekGroupRef ## `xsd:group ref=`
    xekSequence ## `xsd:sequence`. Ordered list of elements
    xekExtension ## `xsd:extension`
    xekRestriction ## `xsd:restriction`
    xekSimpleContent ## `xsd:simpleContent`
    xekTop ## Toplevel element in schema document
    xekEnumeration ## `xsd:enumeration`
    xekPattern
    xekWhitespace ## Configuration for the whitespace handling

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

const xekGroupDeclare* = xekGroupDecl

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


proc treeRepr*(
    xsd; colored: bool = true,
    indexed: bool = false, maxdepth: int = 120
  ): ColoredText

func len*(xsd): int = xsd.subnodes.len
func hasName*(xsd): bool = xsd.xsdName.isSome()
func name*(xsd): string = xsd.xsdName.get()
func hasType*(xsd): bool = xsd.xsdType.isSome()
func hasAttr*(xsd; attr: string): bool =
  notNil(xsd.attrs) and attr in xsd.attrs

func hasAttr*(xsd; key, value: string): bool =
  xsd.hasAttr(key) and xsd.attrs[key] == value

func `[]`*(xsd; key: string): string = xsd.attrs[key]

func getType*(xsd): string =
  ## Return type name for `xsd` - either `type=""` attribute, or name of
  ## the type itself (if xsd is a simple/complex type)
  case xsd.kind:
    of {xekSimpleType, xekComplexType, xekGroupDecl}:
      xsd.name()

    of {xekGroupRef}:
      xsd["ref"]

    of {xekAttribute}:
      if xsd.xsdType.isSome():
        xsd.xsdType.get()

      else:
        "string"

    else:
      xsd.xsdType.get()

func hasTypeImpl*(xsd): bool = xsd.xsdTypeImpl.isSome()
func getTypeImpl*(xsd): XsdEntry = xsd.xsdTypeImpl.get()
func subnodes*(xsd): seq[XsdEntry] = xsd.subnodes


func `$`*(occur: XsdOccursCount): string =
  if occur.isFinite:
    $occur.count

  else:
    "unbounded"

proc treeRepr*(
    xsd; colored: bool = true,
    indexed: bool = false, maxdepth: int = 120
  ): ColoredText =

  coloredResult()

  proc aux(n: XsdEntry, level: int, idx: seq[int]) =
    template updateValue(element: typed) =
      for name, field in fieldPairs(element):
        when field is Option and
             field isnot Option[XsdEntry]:
          if field.isSome():
            add " "
            add toCyan(name, colored)
            add " = "
            add toYellow($field.get(), colored)

        elif field is XsdEntry or
             field is XsdEntryKind or
             field is seq[XsdEntry] or
             field is Option[XsdEntry]:
          discard

        else:
          if name in ["attrs"]:
            discard

          else:
            add " "
            add toCyan(name, colored)
            add " = "
            add toYellow($field, colored)

      if notNil(element.attrs):
        for key, value in element.attrs:
          add " "
          add toRed(key, colored)
          add " = "
          add toYellow(value, colored)


    let pref {.inject.} =
      if indexed:
        idx.join("", ("[", "]")) & "    "

      else:
        "  ".repeat(level)

    add pref
    if isNil(n):
      add toRed("<nil>", colored)
      return

    if level > maxdepth:
      add " ..."
      return



    add toGreen(($n.kind)[3 ..^ 1], colored)

    updateValue(n[])

    case n.kind:
      of xekSimpleType:
        add " / "
        add toRed(($n[0].kind)[3 ..^ 1], colored)
        updateValue(n[0][])

      else:
        discard


    template treeReprRecurse(node: typed, level: int, idx: seq[int]): untyped =
      if len(node) > 0:
        add "\n"

      for newIdx, subn in pairs(node):
        aux(subn, level + 1, idx & newIdx)
        if newIdx < len(node) - 1:
          add "\n"



    case n.kind:
      of xekElement, xekAttribute: discard
      of xekSimpleType:
        treeReprRecurse(n[0], level, idx)

      else:
        treeReprRecurse(n, level, idx)

  aux(xsd, 0, @[])
  endResult()

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
  xsd == xekComplexType and len(xsd) > 0 and xsd[0] == xekChoice

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
  assertKind(xsd, {xekComplexType, xekSimpleType, xekGroupDecl})

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
  assert xsd.kind in {xekComplexType, xekGroupDecl}
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


func hasXsdNs*(tag: string): bool =
  tag.startsWith("xsd:") or tag.startsWith("xs:")

func getNs*(tag: string): string =
  if tag.startswith("xsd:"): "xsd:"
  elif tag.startsWith("xs:"): "xs:"
  else: raise newArgumentError("Tag name does not start with 'xsd:' or 'xs:'")

func getNs*(node: XmlNode): string = node.tag().getNs()

func dropNs*(node: XmlNode): string =
  let tag = node.tag()
  return tag[getNs(node).len .. ^1]

func dropNs*(tag: string): string = tag[getNs(tag).len .. ^1]

func safeDropNs*(node: XmlNode): string =
  let tag = node.safeTag()
  if tag.len == 0:
    return tag

  else:
    return tag[getNs(node).len .. ^1]

func dropNs*(tag, ns: string): string =
  assertArg(tag, tag.startsWith(ns))
  return tag[ns.len .. ^1]

func addNs*(text: string, parent: XmlNode): string =
  return getNs(parent) & text


proc isPrimitiveType*(xsdType: string): bool =
  let h = toHashSet [
    "string",
    "boolean",
    "decimal",
    "integer",
    "float",
    "double",
    "duration",
    "datetime",
    "time",
    "date",
    "gyearmonth",
    "gyear",
    "gmonthday",
    "gday",
    "gmonth",
    "hexbinary",
    "base64binary",
    "uri",
    "anytype",
    "int",
    "positiveinteger"
  ]

  xsdType.hasXsdNs() and normalize(xsdType.dropNs()) in h

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
  case xsd.kind:
    of xekExtension:
      for attribute in xsd:
        if not attribute.isOptional():
          return false

    of xekComplexType:
      if xsd.hasAttr("mixed") and xsd["mixed"] == "true":
        # https://www.w3.org/TR/REC-xml/#sec-mixed-content " An element
        # type has mixed content when elements of that type may contain
        # character data, optionally interspersed with child elements".
        #
        # I.e. body can start with primitive token (string)
        return true

    of xekSimpleContent:
       if xsd.hasType() and xsd.getType().isPrimitiveType():
         return true

       elif xsd.hasTypeImpl():
         return xsd.getTypeImpl().isPossiblePrimitiveType()

    else:
      raise newImplementKindError(xsd)

## #+endsection

proc classifyPrimitiveTypeKind*(str: string): XsdTokenKind =
  case str.dropNs().normalize():
    of "string":             xtkString
    of "boolean":            xtkBoolean
    of "decimal":            xtkDecimal
    of "integer", "int":     xtkInteger
    of "positiveinteger":    xtkNatural
    of "float":              xtkFloat
    of "double":             xtkDouble
    of "duration":           xtkDuration
    of "datetime":           xtkDateTime
    of "time":               xtkTime
    of "date":               xtkDate
    of "gyearmonth":         xtkGYearMonth
    of "gyear":              xtkGYear
    of "gmonthday":          xtkGMonthDay
    of "gday":               xtkGDay
    of "gmonth":             xtkGMonth
    of "hexbinary":          xtkHexBinary
    of "base64binary":       xtkBase64Binary
    of "uri":                xtkUri
    of "anyType", "anytype": xtkAnyType
    else:
      raise newArgumentError(str)

proc classifyPrimitiveTypeKind*(xsd): XsdTokenKind =
  classifyPrimitiveTypeKind(xsd.getType())

proc namePrimitiveTypeKind*(
    kind: XsdTokenKind; withPrefix: bool = false): string =

  if kind in xtkNamedKinds:
    raise newArgumentError($kind & " cannot be named as primitive token kind")

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
    xtkNatural: "Natural"
  })


  if withPrefix:
    result = "xsd:"

  assert map[kind].len > 0, $kind

  result &= map[kind]


proc getNimName*(kind: XsdTokenKind): string =
  case kind:
    of xtkString: "string"
    of xtkBoolean: "bool"
    of xtkDecimal: "float"
    of xtkInteger: "int"
    of xtkFloat: "float"
    of xtkDouble: "float64"
    of xtkDuration: "Duration"
    of xtkDateTime: "DateTime"
    of xtkTime: "DateTime"
    of xtkDate: "DateTime"
    of xtkGYearMonth: "DateTime"
    of xtkGYear: "DateTime"
    of xtkGMonthDay: "DateTime"
    of xtkGDay: "DateTime"
    of xtkGMonth: "DateTime"
    of xtkHexBinary: "string"
    of xtkBase64Binary: "string"
    of xtkUri: "URI"
    of xtkAnyType: "XmlNode"
    of xtkNatural: "Natural"
    else:
      raise newUnexpectedKindError(kind)

proc getParserName*(kind: XsdTokenKind): string =
  "parseXsd" & capitalizeAscii(namePrimitiveTypeKind(kind))

proc getNimType*(name: string): string =
  if name.startsWith("xsd:"):
    name.classifyPrimitiveTypeKind().getNimName()

  else:
    name.capitalizeAscii()

proc getFirstTokens*(xsd; parent: XsdEntry = nil):
    seq[tuple[token: XsdToken, source: Option[XsdEntry]]] =

  ##[

Return list of tokens that entry described by `xsd` can start with.

]##

  var visited: HashSet[int]

  proc aux(xsd; parent: XsdEntry):
    seq[tuple[token: XsdToken, source: Option[XsdEntry]]] =

    if cast[int](xsd) in visited:
      return

    else:
      visited.incl cast[int](xsd)

    case xsd.kind:
      of xekElement, xekRestriction, xekExtension:
        # `xsd:restriction` must be handled separately, because it might
        # separate set of `xsd:string` values into two subtypes using
        # `xsd:enumeration` for example. This is a placeholder
        # implementation, hopefully it won't blow up instantly

        # I've added `xsd:extension` here because I've seen it used as it all
        # attributes are optional, even without bein explicitly annotated
        # with `use="optional"`. Again - not really sure if that is exactly
        # how it should be implemented.
        if xsd.hasType() and xsd.getType().isPrimitiveType():
          let kind = xsd.classifyPrimitiveTypeKind()
          var source: Option[XsdEntry]
          if xsd.kind == xekElement:
            source = some(xsd)

          elif notNil(parent):
            source = some(parent)

          # result.add (XsdToken(kind: kind), source)

        # elif xsd.hasTypeImpl():
        #   result.add aux(xsd.getTypeImpl(), xsd)

        if xsd.hasName():
          result.add ((
            XsdToken(kind: xtkElementStart, xmlName: xsd.name()),
            some(xsd)
          ))

      of xekSequence:
        result = aux(xsd[0], parent)

      of xekChoice:
        for alt in xsd:
          result.add aux(alt, parent)

      of xekGroupRef:
        result.add aux(xsd.groupRef, parent)

      of xekGroupDecl:
        for item in xsd:
          result.add aux(item, parent)

      of xekComplexType:
        if len(xsd) == 0:
          return

        case xsd[0].kind:
          of xekSequence, xekChoice:
            result.add aux(xsd[0], parent)

          of xekSimpleContent:
            # SImple content with extension
            result.add aux(xsd[0][0], parent)

          of xekAttribute:
            discard

          of xekGroupRef:
            result.add aux(xsd[0], parent)

          else:
            raise newUnexpectedKindError(xsd[0])

      of xekSimpleType:
        if xsd[0].kind in {xekRestriction}:
          result.add aux(xsd[0], parent)

      else:
        raise newUnexpectedKindError(xsd)

  return aux(xsd, parent)





proc getFirstSet*(xsd): set[XsdTokenKind] =
  for (token, _) in getFirstTokens(xsd):
    result.incl token.kind


type
  XsdParser* = object
    case onKind*: bool
      of true:
        kind*: XsdTokenKind

      of false:
        tag*: string

    parser*: string
    entry*: Option[XsdEntry]

  XsdParsers* = object
    onKinds*: seq[XsdParser]
    onNames*: seq[XsdParser]

proc getExpectedKinds*(parsers: XsdParsers): set[XsdTokenKind] =
  if parsers.onNames.len > 0:
    result.incl xtkElementStart
    result.incl xtkElementOpen

  for parser in parsers.onKinds:
    result.incl parser.kind

proc getFirstParsers*(alts: seq[XsdEntry]): XsdParsers =
  ##[

Return token kind -> parser name mapping for each element in `alts`.


#[  - @arg{alts} :: FIRST set of tokens for arbitrary entry. Use  ]#
#[    [[code:/getFirstTokens()]] to generate this set.  ]#
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
    for (token, source) in getFirstTokens(alt):
      if token.kind in used:
        raise newArgumentError(
          $token.kind &
            " is already used in first set for this group of tokens")

      else:
        case token.kind:
          of xtkNamedKinds:
            let parser =
              if alt.getType().isPrimitiveType():
                alt.getType().classifyPrimitiveTypeKind().getParserName()

              else:
                "parse" & source.get().getType().getNimType()

            result.onNames.add(
              XsdParser(
                onKind: false,
                tag: token.name(),
                parser: parser,
                entry: source
            ))

          else:
            # only add unnamed kinds, because named tokens might appearh
            # multiple times in the input.
            used.incl token.kind
            result.onKinds.add(
              XsdParser(
                onKind: true,
                kind: token.kind,
                parser: token.kind.getParserName(),
                entry: source
            ))






proc getExtensionSection*(xsd): XsdEntry =
  assert xsd.isPrimitiveExtension()
  return xsd[0][0]



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
    result = xekGroupDecl.newTree()
    for node in xml:
      case node.safeDropNs():
        of "choice", "sequence":
          result.add convertSequence(node, context)

        else:
          raise newImplementError(node.safeTag())

    context.groups[xml["name"]] = result

  else:
    result = XsdEntry(kind: xekGroupRef)
    result.groupName = xml["ref"]
    result.groupRef = context.groups[result.groupName]

  updateBaseAttrs(result, xml)



proc convertSequence*(xml; context): XsdEntry =
  # echo sequence
  case xml.safeDropNs():
    of "sequence": result = XsdEntry(kind: xekSequence)
    of "choice": result = XsdEntry(kind: xekChoice)
    else: raise newImplementError(xml.safeTag())

  for element in xml:
    case element.safeDropNs():
      of "element":
        result.add convertElement(element)

      of "sequence", "choice":
        result.add convertSequence(element, context)

      of "group":
        result.add convertGroup(element, context)

      else:
        echo element
        raise newImplementError(element.safeTag())

  updateBaseAttrs(result, xml)


proc convertExtension*(xml; context): XsdEntry =
  result = newTree(xekExtension)
  updateBaseAttrs(result, xml)
  for node in xml:
    case node.safeDropNs():
      of "attribute":
        result.add convertAttribute(node, context)

      else:
        raise newImplementError(node.safeTag())


proc convertRestriction*(xml, context): XsdEntry =
  result = newTree(xekRestriction)
  updateBaseAttrs(result, xml)
  for node in xml:
    let kind =
      case node.safeDropNs():
        of "enumeration": xekEnumeration
        of "pattern": xekPattern
        of "whiteSpace": xekWhitespace
        else:
          raise newImplementError(node.safeTag())

    var entry: XsdEntry = newTree(kind)
    updateBaseAttrs(entry, node)
    result.add entry


proc convertComplexType*(xml; context): XsdEntry =
  if xml.find("simpleType".addNs(xml)) != -1:
    result = xekComplexSimpleType.newTree()

  elif xml.find("complexType".addNs(xml)) != -1:
    result = xekComplexComplexType.newTree()

  else:
    result = xekComplexType.newTree()

  updateBaseAttrs(result, xml)

  for subnode in xml:
    case subnode.safeDropNs():
      of "sequence":
        result.add convertSequence(subnode, context)

      of "attribute":
        result.add convertAttribute(subnode, context)

      of "simpleContent":
        case subnode[0].safeDropNs():
          of "extension":
            result.add xekSimpleContent.newTree(
              convertExtension(subnode[0], context))

          of "restriction":
            result.add xekSimpleContent.newTree(
              convertRestriction(subnode[0], context))

          else:
            raise newImplementError(subnode[0].safeTag())

      of "choice":
        result.add convertSequence(subnode, context)

      of "group":
        result.add convertGroup(subnode, context)

      of "anyAttribute":
        discard

      of "annotation":
        discard

      else:
        if subnode.kind == xnElement:
          raise newImplementError(subnode.safeTag())


  if result.hasName():
    context.types[result.name()] = result

proc convertSimpleType*(xml, context): XsdEntry =
  result = newTree(xekSimpleType)
  updateBaseAttrs(result, xml)
  case xml[0].safeDropNs():
    of "restriction":
      result.add convertRestriction(xml[0], context)

    else:
      raise newImplementError(xml[0].safeTag())

  if result.hasName():
    context.types[result.name()] = result


proc convertSchema*(xml): XsdEntry =
  result = newTree(xekTop)
  var context: XsdVisitContext
  for entry in xml:
    if entry.kind == xnElement:
      case entry.safeDropNs():
        of "complexType":
          result.add convertComplexType(entry, context)

        of "simpleType":
          result.add convertSimpleType(entry, context)

        of "import":
          discard

        of "element":
          result.startElement = convertElement(entry)

        of "group":
          result.add convertGroup(entry, context)

        of "annotation":
          discard

        else:
          raise newImplementKindError(entry.tag)

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
