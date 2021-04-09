import std/[xmltree, xmlparser, enumerate, strutils,
            strtabs, options, streams]

# import std/parsexml except charData, elementName, entityName,
#                            attrKey, attrValue, piName, piRest

# export parsexml except charData, elementName, entityName,
#                        attrKey, attrValue, piName, piRest
import parsexml
# import std/parsexml as pxml
export xmltree, strtabs, xmlparser, parsexml

import ../other/oswrap
import ../helpers
import ../types/colorstring
import ../algo/clformat
export oswrap

proc parseXml*(file: AbsFile): XmlNode = parseXml(file.readFile())

type
  HXmlParserEventError* = object of CatchableError
  HXmlParser* = object
    base*: XmlParser
    traceNext*: bool

proc currentEventToStr*(parser: HXmlParser): string =
  case parser.base.kind:
    of {xmlElementStart, xmlElementEnd, xmlElementOpen}:
      $parser.base.kind & " " & parser.base.elementName()

    of xmlAttribute:
      $parser.base.kind & " " & parser.base.attrKey() &
        " = \"" & parser.base.attrValue() & "\""

    of xmlError:
      $parser.base.kind & " " & parser.base.errorMsg()

    else:
      $parser.base.kind

proc displayAt*(parser: HXmlParser): string =
  result = "(" & $parser.base.getLine & ":" & $parser.base.getColumn & ") "
  result.add currentEventToStr(parser)


template expectAt(
    parser: HXmlParser, at: set[XmlEventKind], procname: string
  ): untyped {.dirty.} =

  if parser.base.kind notin at:
    raise newException(
      HXmlParserEventError,
      (
        "Invalid curent parser event kind for " & procname &
        " expected any of " & $at & " & but found " & $parser.base.kind() &
        " at " & displayAt(parser)
      )
    )

proc charData*(my: HXmlParser): string =
  expectAt(my, {
    xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial
  }, "charData")
  charData(my.base)


proc elementName*(my: HXmlParser): string =
  expectAt(my, {
    xmlElementStart, xmlElementEnd, xmlElementOpen}, "elementName")

  elementName(my.base)

proc entityName*(my: HXmlParser): string =
  expectAt(my, {xmlEntity}, "entityName")
  entityName(my.base)

proc attrKey*(my: HXmlParser): string =
  expectAt(my, {xmlAttribute}, "attrKey")
  attrKey(my.base)

proc attrValue*(my: HXmlParser): string =
  expectAt(my, {xmlAttribute}, "attrValue")
  attrValue(my.base)

proc piName*(my: HXmlParser): string =
  expectAt(my, {xmlPi}, "piName")
  piName(my.base)

proc piRest*(my: HXmlParser): string =
  expectAt(my, {xmlPi}, "piRest")
  piRest(my.base)

proc kind*(parser: HXmlParser): XmlEventKind = parser.base.kind()

proc strVal*(parser: HXmlParser): string =
  expectAt(parser, {
    xmlAttribute,
    xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial
  }, "strVal")

  if parser.kind in {xmlAttribute}:
    result = parser.attrValue()

  elif parser.kind in {
    xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial
  }:
    result = parser.charData()

  if parser.traceNext:
    echo "  ? ", parser.currentEventToStr(), " -> ", result



proc next*(parser: var HXmlParser) =
  if parser.traceNext:
    echo "> ", parser.currentEventToStr()

  next(parser.base)

proc skipElementStart*(parser: var HXmlParser, tag: string) =
  parser.expectAt({xmlElementStart}, tag)
  assert parser.elementName == tag
  if parser.traceNext: echo "  + ", "Skipping ", tag, " start"

  next(parser)

proc skipElementEnd*(parser: var HXmlParser, tag: string) =
  parser.expectAt({xmlElementEnd}, tag)
  assert parser.elementName == tag
  if parser.traceNext: echo "  - ", "Skipping ", tag, " end"
  next(parser)


proc newHXmlParser*(file: AbsFile, traceNext: bool = false): HXmlParser =
  var fileStream = newFileStream(file.string, fmRead)
  if isNIl fileStream:
    discard

  else:
    open(result.base, fileStream, file.string)
    result.traceNext = traceNext
    next(result.base)

proc newHXmlParser*(text: string, traceNext: bool = false): HXmlParser =
  var stringStream = newStringStream(text)
  open(result.base, stringStream, "<text>")
  result.traceNext = traceNext
  next(result.base)

type
  HXmlParseError* = object of CatchableError


template raiseUnexpectedAttribute*(parser: HXmlParser): untyped =
  raise newException(
    HXmlParseError,
    "Unexpected attribute \"" & parser.attrKey() & "\" at " &
      parser.displayAt()
  )


template raiseUnexpectedElement*(parser: HXmlParser): untyped =
  {.line: instantiationInfo().}:
    raise newException(
      HXmlParseError,
      "Unexpected element <" & parser.elementName() & "> at " &
        parser.displayAt()
    )


template raiseUnexpectedElement*(parser: HXmlParser, tag: string): untyped =
  {.line: instantiationInfo().}:
    raise newException(
      HXmlParseError,
      "Unexpected element: expected <" & tag &
        ">, but found" & parser.elementName() &
        " at " & parser.displayAt()
    )

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

import std/[times, uri]

type XsdParseTarget[T] = T | seq[T] | Option[T]

proc parseXsdString*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  expectAt(parser, {
    xmlCharData, xmlAttribute, xmlElementStart
  }, "parseXsdString")

  var res: string

  while parser.kind() in {xmlWhitespace, xmlCharData}:
    res &= parser.strVal()
    next(parser)

  when target is seq:    target.add res
  elif target is Option: target = some res
  else:                  target = res


proc parseXsdBoolean*(
    target: var XsdParseTarget[bool], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdDecimal*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdInteger*(
    target: var XsdParseTarget[int], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdFloat*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdDouble*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdDuration*(
    target: var XsdParseTarget[Duration], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdDateTime*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdTime*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdDate*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdGYearMonth*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdGYear*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc paresXsdGMonthDay*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdGDay*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdGMonth*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdHexBinary*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdBase64Binary*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard

proc parseXsdUri*(
    target: var XsdParseTarget[URI], parser: var HXmlParser,
    tag: string = ""
  ) =
  discard


proc parseXsdAnyType*(
    target: var XsdParseTarget[XmlNode], parser: var HXmlParser,
    tag: string = ""
  ) =
  next(parser)


when isMainModule:
  discard
