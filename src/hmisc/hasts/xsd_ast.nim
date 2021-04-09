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

    case kind*: XsdEntryKind
      of xekComplexSimpleType:
        cstContent: string

      of xekGroupRef:
        groupRef*: string

      of xekTop:
        startElement: XsdEntry

      of xekEnumeration:
        value*: string

      else:
        discard

  XsdGroups = Table[string, XsdEntry]

  XsdDocument* = object
    entry*: XsdEntry
    groups*: XsdGroups


proc add*(entry: var XsdEntry, subnode: XsdEntry) =
  entry.subnodes.add subnode

proc `==`*(entry: XsdEntry, kind: XsdEntryKind): bool =
  entry.kind == kind

func `[]`*(entry: XsdEntry, idx: int | BackwardsIndex): XsdEntry =
  entry.subnodes[idx]

iterator items*(entry: XsdEntry): XsdEntry =
  for item in entry.subnodes:
    yield item

iterator pairs*(entry: XsdEntry): (int, XsdEntry) =
  var idx = 0
  for item in entry:
    yield (idx, item)
    inc idx

func len*(entry: XsdEntry): int = entry.subnodes.len
func hasName*(entry: XsdEntry): bool = entry.xsdName.isSome()
func name*(entry: XsdEntry): string = entry.xsdName.get()
func hasType*(entry: XsdEntry): bool = entry.xsdType.isSome()
func xsdType*(entry: XsdEntry): string = entry.xsdType.get()
func subnodes*(entry: XsdEntry): seq[XsdEntry] = entry.subnodes

proc newTree*(kind: XsdEntryKind, subnodes: varargs[XsdEntry]): XsdEntry =
  result = XsdEntry(kind: kind)
  for node in subnodes:
    assert notNil(node)
    result.subnodes.add node

proc isEnumerationType*(xsd: XsdEntry): bool =
  if xsd == xekSimpleType and xsd[0] == xekRestriction:
    for node in xsd[0]:
      if node != xekEnumeration:
        return false

      return true

proc isOptional*(xsd: XsdEntry): bool =
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

proc isUnboundedRepeat*(xsd: XsdEntry): bool =
  ## Whether entry descibed by `xsd` does not have finite upper repeat
  ## bound. Lower repeat bound can be anything
  return xsd.xsdMaxOccurs.isSome() and not xsd.xsdMaxOccurs.get().isFinite

# template isSomeAndExpr*(opt: Option)

proc isFinite*(xsd: XsdEntry): bool =
  return
    (
      xsd.xsdMaxOccurs.ifSomeIt(it.isFinite) and
      xsd.xsdMinOccurs.ifSomeIt(it.isFinite)
    ) or (
      xsd.xsdMaxOccurs.isNone() and
      xsd.xsdMinOccurs.isNone()
    )

proc getAttributes*(xsd: XsdEntry): seq[XsdEntry] =
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

proc isPrimitiveRestriction*(xsd: XsdEntry): bool =
  ## Whether entry described by `xsd` is a restriction for some base type
  ## (`xsd:string` that matches particular patern for example)
  xsd == xekSimpleType and
  xsd[0] == xekRestriction and
  xsd[0].allOfIt(it.kind in {xekPattern}) and
  xsd.getAttributes().len == 0


proc isPrimitiveExtension*(xsd: XsdEntry): bool =
  ## Whether entry described by `xsd` is a restriction for some base type
  ## (`xsd:string` that matches particular patern for example)
  xsd == xekComplexType and
  len(xsd) > 0 and xsd[0] == xekSimpleContent and
  len(xsd[0]) > 0 and xsd[0][0] == xekExtension and
  xsd[0][0].hasType()

proc getExtensionSection*(xsd: XsdEntry): XsdEntry =
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
    pnode: XsdEntry, colored: bool = true,
    indexed: bool = false, maxdepth: int = 120
  ): string =

  proc aux(n: XsdEntry, level: int, idx: seq[int]): string =
    template updateValue(element: typed) =
      for name, field in fieldPairs(element):
        when field is Option:
          if field.isSome():
            result &= &[" ", toCyan(name, colored), " = ",
                        toYellow($field.get(), colored)]

        elif field is XsdEntry or
             field is XsdEntryKind or
             field is seq[XsdEntry]
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

  return aux(pnode, 0, @[])

proc xsd*(str: string): string = "xsd:" & str

proc updateBaseAttrs*(entry: var XsdEntry, node: XmlNode) =
  if isNil(node.attrs): return

  for key, value in node.attrs:
    var used = false
    case entry.kind:
      of xekEnumeration:
        if key == "value":
          entry.value = value
          used = true

      of xekRestriction, xekExtension:
        if key == "base":
          entry.xsdType = some value
          used = true

      else:
        discard

    case key:
      of "name": entry.xsdName = some value
      of "type": entry.xsdType = some value
      of "minOccurs": entry.xsdMinOccurs =
        some XsdOccursCount(isFinite: true, count: parseInt(value))

      of "maxOccurs":
        if value == "unbounded":
          entry.xsdMaxOccurs = some XsdOccursCount(isFinite: false)

        else:
          entry.xsdMaxOccurs = some XsdOccursCount(
            isFinite: true, count: parseInt(value))


      else:
        if not used:
          if entry.attrs.isNil():
            entry.attrs = newStringTable()

          entry.attrs[key] = value



proc convertElement*(element: XmlNode): XsdEntry =
  result = xekElement.newTree()
  updateBaseAttrs(result, element)

  if not hasType(result):
    # The type definition corresponding to the <simpleType> or
    # <complexType> element information item in the [children], if either
    # is present, otherwise the type definition ·resolved· to by the
    # ·actual value· of the type [attribute], otherwise the {type
    # definition} of the element declaration ·resolved· to by the ·actual
    # value· of the substitutionGroup [attribute], if present, otherwise
    # the ·ur-type definition·.
    result.xsdType = some "xsd:anyType"

proc convertAttribute*(element: XmlNode, groups: var XsdGroups): XsdEntry =
  result = xekAttribute.newTree()
  updateBaseAttrs(result, element)


proc convertSequence*(sequence: XmlNode, groups: var XsdGroups): XsdEntry

proc convertGroup*(element: XmlNode, groups: var XsdGroups): XsdEntry =
  if element.hasAttr("name"):
    result = xekGroupDeclare.newTree()
    for node in element:
      case node.safeTag():
        of xsd"choice", xsd"sequence":
          result.add convertSequence(node, groups)


        else:
          raiseImplementError(node.safeTag())

    groups[element["name"]] = result

  else:
    result = XsdEntry(kind: xekGroupRef)
    result.groupRef = element["ref"]



proc convertSequence*(sequence: XmlNode, groups: var XsdGroups): XsdEntry =
  # echo sequence
  case sequence.safeTag():
    of xsd"sequence": result = XsdEntry(kind: xekSequence)
    of xsd"choice": result = XsdEntry(kind: xekChoice)
    else: raiseImplementError(sequence.safeTag())

  for element in sequence:
    case element.safeTag():
      of xsd"element":
        result.add convertElement(element)

      of xsd"sequence", xsd"choice":
        result.add convertSequence(element, groups)

      of xsd"group":
        result.add convertGroup(element, groups)

      else:
        echo element
        raiseImplementError(element.safeTag())

  updateBaseAttrs(result, sequence)

proc convertExtension(xml: XmlNode, groups: var XsdGroups): XsdEntry =
  result = newTree(xekExtension)
  updateBaseAttrs(result, xml)
  for node in xml:
    case node.safeTag():
      of xsd"attribute":
        result.add convertAttribute(node, groups)

      else:
        raiseImplementError(node.safeTag())


proc convertRestriction*(xml: XmlNode, groups: var XsdGroups): XsdEntry =
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


proc convertComplexType(xsdType: XmlNode, groups: var XsdGroups): XsdEntry =
  if xsdType.find(xsd"simpleType") != -1:
    result = xekComplexSimpleType.newTree()

  elif xsdType.find(xsd"complexType") != -1:
    result = xekComplexComplexType.newTree()

  else:
    result = xekComplexType.newTree()

  updateBaseAttrs(result, xsdType)

  for subnode in xsdType:
    case subnode.safeTag():
      of xsd"sequence":
        result.add convertSequence(subnode, groups)

      of xsd"attribute":
        result.add convertAttribute(subnode, groups)

      of xsd"simpleContent":
        case subnode[0].safeTag():
          of xsd"extension":
            result.add xekSimpleContent.newTree(
              convertExtension(subnode[0], groups))

          of xsd"restriction":
            result.add xekSimpleContent.newTree(
              convertRestriction(subnode[0], groups))

          else:
            raiseImplementError(subnode[0].safeTag())

      of xsd"choice":
        result.add convertSequence(subnode, groups)

      of xsd"group":
        result.add convertGroup(subnode, groups)

      of xsd"anyAttribute":
        discard

      else:
        if subnode.kind == xnElement:
          raiseImplementError(subnode.safeTag())

proc convertSimpleType(node: XmlNode, groups: var XsdGroups): XsdEntry =
  result = newTree(xekSimpleType)
  updateBaseAttrs(result, node)
  case node[0].safeTag():
    of xsd"restriction":
      result.add convertRestriction(node[0], groups)

    else:
      raiseImplementError(node[0].safeTag())


proc convertSchema(node: XmlNode): XsdDocument =
  assert node.tag == xsd"schema", node.tag
  result.entry = newTree(xekTop)
  for entry in node:
    if entry.kind == xnElement:
      case entry.tag:
        of xsd"complexType":
          result.entry.add convertComplexType(entry, result.groups)

        of xsd"simpleType":
          result.entry.add convertSimpleType(entry, result.groups)

        of xsd"import":
          discard

        of xsd"element":
          result.entry.startElement = convertElement(entry)

        of xsd"group":
          result.entry.add convertGroup(entry, result.groups)



proc parseXsd*(file: AbsFile): XsdDocument = parseXml(file).convertSchema()
proc parseXsd*(text: string): XsdDocument = parseXml(text).convertSchema()

proc main() =
  let xsd= parseXsd(AbsFile "/tmp/schema.xsd")
  echo xsd.entry.treeRepr()


when isMainModule:
  main()
