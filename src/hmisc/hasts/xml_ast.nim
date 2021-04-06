import std/[xmltree, parsexml, xmlparser, enumerate, strtabs, options]
import ../other/oswrap
import ../helpers
import ../types/colorstring
import ../algo/clformat

proc parseXml*(file: AbsFile): XmlNode = parseXml(file.readFile())

export xmltree, strtabs, xmlparser


func add*(xml: var XmlNode, optXml: Option[XmlNode]) =
  if optXml.isSome():
    xml.add optXml.get()

func newXml*(tag: string, args: seq[XmlNode] = @[]): XmlNode =
  newXmlTree(tag, args)



func wrap*(xml: XmlNode, tag: string): XmlNode =
  result = newElement(tag)
  result.add xml


func wrap*(xml: seq[XmlNode], tag: string): XmlNode =
  result = newElement(tag)
  for node in xml:
    result.add node


func add*(xml: var XmlNode, sub: seq[XmlNode]): void =
  for it in sub:
    xml.add it

func `[]=`*(xml: var XmlNode, attrname: string, attrval: string): void =
  var attrs = xml.attrs()
  if attrs != nil:
    attrs[attrname] = attrval
    xml.attrs = attrs
  else:
    xml.attrs = {attrname : attrval}.toXmlAttributes()


iterator pairs*(node: XmlNode): (int, XmlNode) =
  var idx = 0
  for subnode in node:
    yield (idx, subnode)
    inc idx

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


func `[]`*(node: XmlNode, property: string): string = node.attrs[property]

func optGet*(node: XmlNode, property: string): Option[string] =
  if node.attrs.isNil() or property notin node.attrs:
    discard

  else:
    return some node.attrs[property]


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
            for i, node in n:
              result &= aux(node, level + 1, idx & i)

        else:
          result &= "\n"

  return aux(pnode, 0, @[])
