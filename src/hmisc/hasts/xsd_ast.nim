import std/[options, strutils, tables]
export tables

import ./xml_ast

import ../algo/[hstring_algo, halgorithm, clformat]
import ../other/oswrap
import ../helpers

import ../types/colorstring



type
  XsdEntryKind = enum
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

  XsdEntry = ref object
    name: Option[string] ## `name=""` attribute
    xsdType: Option[string] ## `type=""` attribute
    minOccurs: Option[int] ## `minOccurs`
    maxOccurs: Option[int] ## `maxOccurs`
    subnodes: seq[XsdEntry]
    attrs: StringTableRef

    case kind*: XsdEntryKind
      of xekComplexSimpleType:
        cstContent: string

      of xekGroupRef:
        groupRef: string

      of xekTop:
        startElement: XsdEntry

      of xekEnumeration:
        value: string

      else:
        discard

  XsdGroups = Table[string, XsdEntry]

  XsdDocument* = object
    entry*: XsdEntry
    groups*: XsdGroups


proc add*(entry: var XsdEntry, subnode: XsdEntry) =
  entry.subnodes.add subnode

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

proc newTree*(kind: XsdEntryKind, subnodes: varargs[XsdEntry]): XsdEntry =
  result = XsdEntry(kind: kind)
  for node in subnodes:
    assert notNil(node)
    result.subnodes.add node


template treeReprRecurse(node: typed, level: int, idx: seq[int]): untyped =
  if len(node) > 0:
    result &= "\n"

  for newIdx, subn in pairs(node):
    result &= aux(subn, level + 1, idx & newIdx)
    if newIdx < len(node) - 1:
      result &= "\n"

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
            result &= &[" ", toCyan(name, colored), " = ", toYellow($field, colored)]

    let pref {.inject.} =
      if indexed:
        idx.join("", ("[", "]")) & "    "
      else:
        "  ".repeat(level)

    if isNil(n): return pref & toRed("<nil>", colored)
    if level > maxdepth: return pref & " ..."

    result &= pref & toGreen(($n.kind)[3 ..^ 1], colored)

    case n.kind:
      of xekSimpleType:
        result &= &[" / ", toRed(($n[0].kind)[3 ..^ 1], colored)]
        updateValue(n[0][])

      else:
        discard

    updateValue(n[])

    if notNil(n.attrs):
      for key, value in n.attrs:
        result &= &[" ", toRed(key, colored), " = ", toYellow(value, colored)]


    case n.kind:
      of xekElement, xekAttribute: discard
      of xekSimpleType:
        treeReprRecurse(n[0], level, idx)

      else:
        treeReprRecurse(n, level, idx)

  return aux(pnode, 0, @[])

proc x(str: string): string = "xsd:" & str

func `[]`*(node: XmlNode, property: string): string = node.attrs[property]

func optGet*(node: XmlNode, property: string): Option[string] =
  if node.attrs.isNil() or property notin node.attrs:
    discard

  else:
    return some node.attrs[property]

template mapItOpt(opt: typed, expr: untyped): untyped =
  type ExprType = typeof((var it {.inject.} = opt.get(); expr))

  let value = opt
  if value.isSome():
    let it {.inject.} = value.get()
    some expr

  else:
    none ExprType

proc updateBaseAttrs(entry: var XsdEntry, node: XmlNode) =
  for key, value in node.attrs:
    var used = false
    if entry.kind == xekEnumeration:
      if key == "value":
        entry.value = value
        used = true

    case key:
      of "name": entry.name = some value
      of "type": entry.xsdType = some value
      of "minOccurs": entry.minOccurs = some parseInt(value)
      of "maxOccurs":
        if value != "unbounded":
          entry.maxOccurs = some parseInt(value)

      else:
        if not used:
          if entry.attrs.isNil():
            entry.attrs = newStringTable()

          entry.attrs[key] = value



proc convertElement(element: XmlNode): XsdEntry =
  result = xekElement.newTree()
  updateBaseAttrs(result, element)

proc convertAttribute(element: XmlNode, groups: var XsdGroups): XsdEntry =
  result = xekAttribute.newTree()
  updateBaseAttrs(result, element)


proc convertSequence(sequence: XmlNode, groups: var XsdGroups): XsdEntry

proc convertGroup(element: XmlNode, groups: var XsdGroups): XsdEntry =
  if element.hasAttr("name"):
    result = xekGroupDeclare.newTree()
    for node in element:
      case node.safeTag():
        of x"choice", x"sequence":
          result.add convertSequence(node, groups)


        else:
          raiseImplementError(node.safeTag())

    groups[element["name"]] = result

  else:
    result = XsdEntry(kind: xekGroupRef)
    result.groupRef = element["ref"]



proc convertSequence(sequence: XmlNode, groups: var XsdGroups): XsdEntry =
  case sequence.safeTag():
    of x"sequence": result = XsdEntry(kind: xekSequence)
    of x"choice": result = XsdEntry(kind: xekChoice)
    else: raiseImplementError(sequence.safeTag())

  for element in sequence:
    case element.safeTag():
      of x"element":
        result.add convertElement(element)

      of x"sequence", x"choice":
        result.add convertSequence(element, groups)

      of x"group":
        result.add convertGroup(element, groups)

      else:
        echo element
        raiseImplementError(element.safeTag())

proc convertExtension(xml: XmlNode, groups: var XsdGroups): XsdEntry =
  result = newTree(xekExtension)

proc convertRestriction(xml: XmlNode, groups: var XsdGroups): XsdEntry =
  result = newTree(xekRestriction)

  for node in xml:
    let kind =
      case node.safeTag():
        of x"enumeration": xekEnumeration
        of x"pattern": xekPattern
        else:
          raiseImplementError(node.safeTag())

    var entry: XsdEntry = newTree(kind)
    updateBaseAttrs(entry, node)
    result.add entry


proc convertComplexType(xsdType: XmlNode, groups: var XsdGroups): XsdEntry =
  if xsdType.find(x"simpleType") != -1:
    result = xekComplexSimpleType.newTree()

  elif xsdType.find(x"complexType") != -1:
    result = xekComplexComplexType.newTree()

  else:
    result = xekComplexType.newTree()

  updateBaseAttrs(result, xsdType)

  for subnode in xsdType:
    case subnode.safeTag():
      of x"sequence":
        result.add convertSequence(subnode, groups)

      of x"attribute":
        result.add convertAttribute(subnode, groups)

      of x"simpleContent":
        case subnode[0].safeTag():
          of x"extension":
            result.add xekSimpleContent.newTree(
              convertExtension(subnode[0], groups))

          of x"restriction":
            result.add xekSimpleContent.newTree(
              convertRestriction(subnode[0], groups))

          else:
            raiseImplementError(subnode[0].safeTag())

      of x"choice":
        result.add convertSequence(subnode, groups)

      of x"group":
        result.add convertGroup(subnode, groups)

      of x"anyAttribute":
        discard

      else:
        if subnode.kind == xnElement:
          raiseImplementError(subnode.safeTag())

proc convertSimpleType(node: XmlNode, groups: var XsdGroups): XsdEntry =
  result = newTree(xekSimpleType)
  updateBaseAttrs(result, node)
  case node[0].safeTag():
    of x"restriction":
      result.add convertRestriction(node[0], groups)

    else:
      raiseImplementError(node[0].safeTag())


proc convertSchema(node: XmlNode): XsdDocument =
  assert node.tag == x"schema", node.tag
  result.entry = newTree(xekTop)
  for entry in node:
    if entry.kind == xnElement:
      case entry.tag:
        of x"complexType":
          result.entry.add convertComplexType(entry, result.groups)

        of x"simpleType":
          result.entry.add convertSimpleType(entry, result.groups)

        of x"import":
          discard

        of x"element":
          result.entry.startElement = convertElement(entry)

        of x"group":
          result.entry.add convertGroup(entry, result.groups)



proc parseXsd*(file: AbsFile): XsdDocument = parseXml(file).convertSchema()
proc parseXsd*(text: string): XsdDocument = parseXml(text).convertSchema()

proc main() =
  let xsd= parseXsd(AbsFile "/tmp/schema.xsd")
  echo xsd.entry.treeRepr()


when isMainModule:
  main()
