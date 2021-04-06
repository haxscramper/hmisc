import std/[xmlparser, parsexml, xmltree, options,
            strutils, strtabs, enumerate]
# import hnimast/hast_common
import hmisc/algo/[hstring_algo, halgorithm, clformat]
import hmisc/other/oswrap
import hmisc/helpers

import hmisc/types/colorstring


proc parseXml*(file: AbsFile): XmlNode = parseXml(file.readFile())

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

func len*(entry: XsdEntry): int = entry.subnodes.len

proc newTree*(kind: XsdEntryKind, subnodes: varargs[XsdEntry]): XsdEntry =
  result = XsdEntry(kind: kind)
  for node in subnodes:
    assert notNil(node)
    result.subnodes.add node

proc treeRepr*(
    pnode: XmlNode, colored: bool = true,
    indexed: bool = false, maxdepth: int = 120
  ): string =

  proc aux(n: XmlNode, level: int, idx: seq[int]): string =
    let pref =
      if indexed:
        idx.join("", ("[", "]")) & "    "
      else:
        "  ".repeat(level)

    if isNil(n):
      return pref & toRed("<nil>", colored)

    if level > maxdepth:
      return pref & " ..."



    result &= pref

    case n.kind:
      of xnText, xnVerbatimText, xnCData, xnEntity:
        result &= indent(n.text, level * 2)

      of xnComment:
        result &= indent(n.text, level * 2)

      of xnElement:
        let multiline = n.len > 0
        let separator = tern(multiline, "\n", " ")
        result &= &[" ", toRed(n.tag, colored), separator]
        if level + 1 > maxdepth and n.len > 0:
          result[^1] = ' '
          result &= &["... ", toPluralNoun("subnode", n.len), separator]

        if notNil(n.attrs):
          for key, value in n.attrs:
            if multiline:
              result.add pref

            else:
              result.add " "

            result.add &["  ", key, " = \"", toYellow(value, colored),
                         "\"", separator]

        if multiline:
          if level + 1 > maxdepth:
            discard

          else:
            for i, node in enumerate(n):
              result &= aux(node, level + 1, idx & i)

        else:
          result &= "\n"

  return aux(pnode, 0, @[])

template treeReprInit(
    kind: enum, indexed: bool, maxdepth: int, colored: bool): untyped =

  let pref {.inject.} =
    if indexed:
      idx.join("", ("[", "]")) & "    "
    else:
      "  ".repeat(level)

  if isNil(n):
    return pref & toRed("<nil>", colored)

  if level > maxdepth:
    return pref & " ..."


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
    treeReprInit(n.kind, indexed, maxdepth, colored)

    result &= pref & toGreen(($n.kind)[3 ..^ 1], colored)

    case n.kind:
      of xekSimpleType:
        result &= &[" / ", toRed(($n[0].kind)[3 ..^ 1], colored)]

      else:
        discard

    for name, field in fieldPairs(n[]):
      when field is Option:
        if field.isSome():
          result &= &[" ", name, " = ", toYellow($field.get(), colored)]

      elif field is XsdEntry or
           field is XsdEntryKind or
           field is seq[XsdEntry]
        :
        discard

      else:
        if name in ["attrs"]:
          discard

        else:
          result &= &[" ", name, " = ", toYellow($field, colored)]

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

proc find*(node: XmlNode, subnodeTag: string): int =
  result = -1
  for idx, subnode in enumerate(node):
    if subnode.kind == xnElement and subnode.tag == subnodeTag:
      return idx

func safeTag*(node: XmlNode): string =
  if node.kind == xnElement:
    return node.tag

func hasAttr*(node: XmlNode, attribute: string): bool =
  notNil(node.attrs) and attribute in node.attrs

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



proc parseElement(element: XmlNode): XsdEntry =
  result = xekElement.newTree()
  updateBaseAttrs(result, element)

proc parseAttribute(element: XmlNode): XsdEntry =
  result = xekAttribute.newTree()
  updateBaseAttrs(result, element)


proc parseGroup(element: XmlNode): XsdEntry =
  if element.hasAttr("name"):
    result = xekGroupDeclare.newTree()
    echo "implement group definition parser"

  else:
    result = XsdEntry(kind: xekGroupRef)
    result.groupRef = element["ref"]



proc parseSequence(sequence: XmlNode): XsdEntry =
  case sequence.safeTag():
    of x"sequence": result = XsdEntry(kind: xekSequence)
    of x"choice": result = XsdEntry(kind: xekChoice)
    else: raiseImplementError(sequence.safeTag())

  for element in sequence:
    case element.safeTag():
      of x"element":
        result.add parseElement(element)

      of x"sequence", x"choice":
        result.add parseSequence(element)

      of x"group":
        result.add parseGroup(element)

      else:
        echo element
        raiseImplementError(element.safeTag())

proc parseExtension(xml: XmlNode): XsdEntry =
  result = newTree(xekExtension)

proc parseRestriction(xml: XmlNode): XsdEntry =
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


proc parseComplexType(xsdType: XmlNode): XsdEntry =
  if xsdType.find(x"simpleType") != -1:
    result = XsdEntry(kind: xekComplexSimpleType)

  elif xsdType.find(x"complexType") != -1:
    result = XsdEntry(kind: xekComplexComplexType)

  else:
    result = XsdEntry(kind: xekComplexType)

  for subnode in xsdType:
    case subnode.safeTag():
      of x"sequence":
        result.add parseSequence(subnode)

      of x"attribute":
        result.add parseAttribute(subnode)

      of x"simpleContent":
        case subnode[0].safeTag():
          of x"extension":
            result.add xekSimpleContent.newTree(
              parseExtension(subnode[0]))

          of x"restriction":
            result.add xekSimpleContent.newTree(
              parseRestriction(subnode[0]))

          else:
            raiseImplementError(subnode[0].safeTag())

      of x"choice":
        result.add parseSequence(subnode)

      of x"group":
        result.add parseGroup(subnode)

      of x"anyAttribute":
        discard

      else:
        if subnode.kind == xnElement:
          raiseImplementError(subnode.safeTag())

proc parseSimpleType(node: XmlNode): XsdEntry =
  result = newTree(xekSimpleType)
  case node[0].safeTag():
    of x"restriction":
      result.add parseRestriction(node[0])

    else:
      raiseImplementError(node[0].safeTag())


proc parseSchema(node: XmlNode): XsdEntry =
  assert node.tag == x"schema", node.tag
  result = XsdEntry(kind: xekTop)
  for entry in node:
    if entry.kind == xnElement:
      case entry.tag:
        of x"complexType":
          result.add parseComplexType(entry)

        of x"simpleType":
          result.add parseSimpleType(entry)

        of x"import":
          discard

        of x"element":
          result.startElement = parseElement(entry)





proc main() =
  let schema = parseXml(AbsFile "/tmp/schema.xsd")
  let entries = parseSchema(schema)
  echo entries.treeRepr()


when isMainModule:
  main()
