import std/[
  strformat,
  xmltree,
  xmlparser,
  enumerate,
  strutils,
  tables,
  with,
  strtabs,
  options,
  streams,
  parsexml,
  macros
]

export xmltree, strtabs, xmlparser, parsexml

import
  ../core/all,
  ../other/[oswrap, rx],
  ../types/colorstring,
  ../algo/[clformat, hstring_algo],
  ./base_writer

export oswrap

proc parseXml*(file: AbsFile): XmlNode = parseXml(file.readFile())

type
  HXmlParserEventError* = object of CatchableError
  HXmlParser* = object of RootObj
    base*: XmlParser
    traceNext*: bool
    file*: AbsFile

const xmlAttr* = XmlEventKind.xmlAttribute

proc currentEventToStr*(parser: HXmlParser): string =
  case parser.base.kind:
    of {xmlElementStart, xmlElementEnd, xmlElementOpen}:
      $parser.base.kind & " " & parser.base.elementName()

    of xmlAttribute:
      $parser.base.kind & " " & parser.base.attrKey() &
        " = \"" & parser.base.attrValue() & "\""

    of xmlError:
      $parser.base.kind & " " & parser.base.errorMsg()

    of {
      xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial
    }:
      var chard = parser.base.charData()
      chard = chard[0 .. min(chard.high, 20)]
      $parser.base.kind & " " & chard

    else:
      $parser.base.kind

proc displayAt*(parser: HXmlParser): string =
  result = $parser.file & "(" & $parser.base.getLine &
    ":" & $parser.base.getColumn & ") "
  result.add currentEventToStr(parser)

proc `$`*(p: HXmlParser): string = p.displayAt()


template expectAt*(
    parser: HXmlParser, at: set[XmlEventKind], procname: string
  ): untyped {.dirty.} =

  {.line: instantiationInfo(fullPaths = true).}:
    if parser.base.kind notin at:
      when not defined(nimdoc):
        raise newException(
          HXmlParserEventError,
          (
            "Invalid curent parser event kind for '" & procname &
            "'. Expected any of " & $at & ", but found " & $parser.base.kind() &
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
    xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial,
    xmlElementStart, xmlElementOpen
  }, "strVal")

  if parser.kind in {xmlAttribute}:
    result = parser.attrValue()

  elif parser.kind in {
    xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial
  }:
    result = parser.charData()

  elif parser.kind in {xmlElementStart, xmlElementOpen}:
    result = parser.elementName()

  if parser.traceNext:
    echo "  ? ", parser.currentEventToStr(), " -> ", result


proc traceParse*(parser: var HXmlParser) =
  proc strVal(parser: XmlParser, ind: var int): string =
    result = &"{parser.getLine:>3}:{parser.getColumn:<3}" & "|  ".repeat(clamp(ind, 0, high(int)))
    case parser.kind:
      of {xmlAttribute}:
        result &= parser.attrKey() & " " & parser.attrValue()
      of xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial:
        result &= parser.charData()

      of xmlElementStart:
        result &= "<" & parser.elementName() & ">"
        inc ind

      of xmlElementOpen:
        result &= "<" & parser.elementName()
        inc ind

      of xmlElementClose:
        result &= ">"

      of xmlElementEnd:
        result &= "</" & parser.elementName() & ">"
        dec ind

      else:
        discard

  var ind = 0
  while true:
    next(parser.base)
    echo alignLeft(($parser.kind)[3..^1], 15), " ", strVal(parser.base, ind)
    if parser.base.kind == xmlEof:
      return


proc next*(parser: var HXmlParser) =
  next(parser.base)

  if parser.traceNext:
    echo "> ", parser.displayAt()



proc errorAt*(parser: var HXmlParser): string =
  result = $parser.file & "(" & $parser.base.getLine &
    ":" & $parser.base.getColumn & ") " & currentEventToStr(parser)

  # for i in 0 .. 5:
  #   if i > 0:
  #     result &= " "
  #   result &= "[" & currentEventToStr(parser) & "]"
  #   parser.next()


type
  HXmlParseError* = object of CatchableError


template raiseUnexpectedAttribute*(parser: HXmlParser): untyped =
  raise newException(
    HXmlParseError,
    "Unexpected attribute \"" & parser.attrKey() & "\" at " &
      parser.errorAt()
  )


template raiseUnknownElement*(
  parser: HXmlParser, extra: string = ""): untyped =
  raise newException(
    HXmlParseError,
    "Unknown element <" & parser.elementName() & "> at " &
      parser.errorAt() & " " & extra
  )


template raiseUnexpectedElement*(
    parser: HXmlParser, tag: string, extra: string = ""): untyped =
  raise newException(
    HXmlParseError,
    "Unexpected element: expected <" & tag &
      ">, but found <" & parser.elementName() &
      "> at " & parser.errorAt() & " " & extra
  )


proc skipElementStart*(parser: var HXmlParser, tag: string) =
  parser.expectAt({xmlElementStart, xmlElementOpen}, tag)
  assert parser.elementName == tag
  if parser.traceNext: echo "  + ", "Skipping ", tag, " start"

  next(parser)

proc skipElementEnd*(parser: var HXmlParser, tag: string) =
  parser.expectAt({xmlElementEnd}, tag)
  assert parser.elementName == tag
  if parser.traceNext: echo "  - ", "Skipping ", tag, " end"
  next(parser)


proc `[]`*(r: var HXmlParser, key: string): bool =
  # expectAt(r, {xmlAttr, xmlElementStart, xmlElementOpen, xmlElementEnd}, "[]")
  case r.kind:
    of xmlAttr:
      result = r.attrKey() == key
    of xmlElementStart, xmlElementOpen, xmlElementEnd:
      result = r.elementName() == key

    else:
      result = false

    # else:
    #   discard

proc atClose*(r: var HXmlParser): bool = r.kind in {xmlElementClose}
proc atEnd*(r: var HXmlParser): bool = r.kind in {xmlElementEnd}
proc atOpenStart*(r: var HXmlParser): bool =
  r.kind in {xmlElementStart, xmlElementOpen}

proc isAttribute*(reader: HXmlParser): bool =
  reader.kind == XmlEventKind.xmlAttribute

proc atAttr*(r: var HXmlParser): bool = r.kind in {xmlAttr}
proc atCdata*(r: var HXmlParser): bool = r.kind in {XmlEventKind.xmlCData}


proc skipOpen*(r: var HXmlParser; tag: string) =
  expectAt(r, {xmlElementOpen}, "skipOpen")
  assert r.elementName() == tag
  r.next()

proc skipStart*(r: var HXmlParser; tag: string) =
  expectAt(r, {xmlElementStart}, "skipStart")
  assert r.elementName() == tag
  r.next()

proc skipClose*(r: var HXmlParser) =
  expectAt(r, {xmlElementClose}, "skipClose")
  r.next()


proc skipCloseEnd*(r: var HXmlParser) =
  expectAt(r, {xmlElementClose}, "skipCloseEnd")
  r.next()
  expectAt(r, {xmlElementEnd}, "skipCloseEnd")
  r.next()

proc skipEnd*(r: var HXmlParser, tag: string) =
  expectAt(r, {xmlElementEnd}, "skipEnd")
  if r.elementName() != tag:
    raiseUnexpectedElement(r, tag)

  r.next()



proc newHXmlParser*(file: AbsFile, traceNext: bool = false): HXmlParser =
  assertExists(file)
  var fileStream = newFileStream(file.string, fmRead)
  assert not isNil(fileStream),
      "Failed to open file for reading" & file.string

  open(result.base, fileStream, file.string)
  result.traceNext = traceNext
  result.file = file
  next(result.base)

proc newHXmlParser*(text: string, traceNext: bool = false): HXmlParser =
  var stringStream = newStringStream(text)
  open(result.base, stringStream, "<text>")
  result.traceNext = traceNext
  next(result.base)

proc close*(parser: var HXmlParser) = parser.base.close()
proc `=destroy`*(parser: var HXmlParser) = parser.close()


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
  ): ColoredText =

  coloredResult()

  proc aux(n: XmlNode, level: int, idx: seq[int]) =
    let pref =
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

    case n.kind:
      of xnText, xnVerbatimText, xnCData, xnEntity:
        add indent(n.text, level * 2)

      of xnComment:
        add indent(n.text, level * 2)

      of xnElement:
        let multiline = n.len > 0
        let separator = tern(multiline, "\n", " ")
        add " "
        add toRed(n.tag, colored)
        add separator
        if level + 1 > maxdepth and n.len > 0:
          # result[^1] = clr(' ')
          add "... "
          add toPluralNoun("subnode", n.len)
          add separator

        if notNil(n.attrs):
          for key, value in n.attrs:
            if multiline:
              add pref

            else:
              add " "

            add "  "
            add key
            add " = \""
            add toYellow(value, colored)
            add "\""
            add separator

        if multiline:
          if level + 1 > maxdepth:
            discard

          else:
            for i, node in n:
              aux(node, level + 1, idx & i)

        else:
          add "\n"

  aux(pnode, 0, @[])

import std/[times, uri]

type XsdParseTarget[T] = T | seq[T] | Option[T]

proc setValue[T](target: var Option[T], value: T) =
  target = some value

proc setValue[T](target: var T, value: T) =
  target = value

proc setValue[T](target: var seq[T], value: T) =
  target.add value

proc parseXsdString*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  expectAt(parser, {
    xmlCharData, xmlAttribute, xmlElementStart, xmlWhitespace
  }, "parseXsdString")

  var res: string

  if parser.kind() == xmlAttribute:
    res = parser.strVal()

  else:
    while parser.kind() in {xmlWhitespace, xmlCharData}:
      res &= parser.strVal()
      next(parser)

  setValue(target, res)


proc parseXsdBoolean*(
    target: var XsdParseTarget[bool], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError("")

proc parseXsdDecimal*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError("")

proc parseXsdInteger*(
    target: var XsdParseTarget[int], parser: var HXmlParser,
    tag: string = ""
  ) =
  let res = parser.strVal().parseInt()
  setValue(target, res)
  parser.next()

proc parseXsdNatural*(
    target: var XsdParseTarget[Natural], parser: var HXmlParser,
    tag: string = ""
  ) =
  var tmp: int
  parseXsdInteger(tmp, parser, tag)
  target = Natural(tmp)

proc parseXsdFloat*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdDouble*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdDuration*(
    target: var XsdParseTarget[Duration], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdDateTime*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdTime*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdDate*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdGYearMonth*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdGYear*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc paresXsdGMonthDay*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdGDay*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdGMonth*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdHexBinary*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdBase64Binary*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdUri*(
    target: var XsdParseTarget[URI], parser: var HXmlParser,
    tag: string = ""
  ) =
  raise newImplementError(parser.displayAt())

proc parseXsdAnyType*(
    target: var XmlNode, parser: var HXmlParser,
    tag: string = ""
  ) =
  case parser.kind():
    of xmlCharData, xmlWhitespace:
      target.setValue(newText(parser.strVal()))
      next(parser)

    of xmlElementStart:
      parser.skipStart(tag)
      target = newXmlTree(tag, [])

      while not parser.atEnd():
        var buf: XmlNode
        parseXsdAnyType(buf, parser)
        target.add buf

      parser.skipEnd(tag)

    else:
      raise newImplementKindError(parser)


type
  XsdTokenKind* = enum
    xtkString               ## ``xsd:string``
    xtkBoolean              ## ``xsd:boolean``
    xtkDecimal              ## ``xsd:decimal``
    xtkInteger              ## ``xsd:integer``
    xtkNatural              ## ``xsd:positiveInteger``
    xtkFloat                ## ``xsd:float``
    xtkDouble               ## ``xsd:double``
    xtkDuration             ## ``xsd:duration``
    xtkDateTime             ## ``xsd:datetime``
    xtkTime                 ## ``xsd:time``
    xtkDate                 ## ``xsd:date``
    xtkGYearMonth           ## ``xsd:gyearmonth``
    xtkGYear                ## ``xsd:gyear``
    xtkGMonthDay            ## ``xsd:gmonthday``
    xtkGDay                 ## ``xsd:gday``
    xtkGMonth               ## ``xsd:gmonth``
    xtkHexBinary            ## ``xsd:hexbinary``
    xtkBase64Binary         ## ``xsd:base64binary``
    xtkUri                  ## ``xsd:uri``
    xtkAnyType              ## ``xsd:anytype``

    xtkError                 ## an error occurred during parsing
    xtkEof                   ## end of file reached
    xtkCharData              ## character data
    xtkWhitespace            ## whitespace has been parsed
    xtkComment               ## a comment has been parsed
    xtkPI                    ## processing instruction (``<?name something ?>``)
    xtkElementStart          ## ``<elem>``
    xtkElementEnd            ## ``</elem>``
    xtkElementOpen           ## ``<elem
    xtkAttribute             ## ``key = "value"`` pair
    xtkElementClose          ## ``>``
    xtkCData                 ## ``<![CDATA[`` ... data ... ``]]>``
    xtkEntity                ## &entity;
    xtkSpecial                ## ``<! ... data ... >``

const
  # IDEA associated type constants. I had to split type definitions into
  # two section to put constants inbetween, but in documentation they
  # should appear in exactly the same order as here.
  xtkNamedKinds* = {xtkElementStart .. xtkAttribute}

type
  XsdToken* = object
    case kind*: XsdTokenKind
     of xtkNamedKinds:
       xmlName*: string

     else:
       discard

func name*(token: XsdToken): string = token.xmlName
func `$`*(token: XsdToken): string =
  result &= "(" & $token.kind

  if token.kind in xtkNamedKinds:
    result &= " " & token.xmlName

  result &= ")"


proc classifyXsdString*(
    str: string, expected: set[XsdTokenKind]): XsdTokenKind =

  const
    floatingPointPrefix = &[?nrx({'-', '+'}), *nrx(rskDigit), ?nrx('.'), +nrx(rskDigit)]
    # decimal has a lexical representation consisting of a finite-length
    # sequence of decimal digits (#x30-#x39) separated by a period as a
    # decimal indicator. An optional leading sign is allowed. If the sign
    # is omitted, "+" is assumed. Leading and trailing zeroes are optional.


    floatRx = &[
      floatingPointPrefix,
      ?[nrx({'e', 'E'}), ?nrx({'-', '+'}), +nrx(rskDigit)],
      nrx(rskLineEnd)
    ]
    # Floating point parsing is taken from first random SO answer, but I'm
    # sure it is much more involved than that.

    decimalRx = &[floatingPointPrefix, nrx(rskLineEnd)]
    integerRx = fullLine &[nrx('0') | &[nrx('1', '9'), *nrx('0', '9')]]

    # NOTE this pattern certainly has unnecessary number of comments,
    # that is probably making it harder to understand that simple
    # regex like `-?\d{4}-\d{2}-\d{2}` (and so on), but I wanted to
    # test if it works for something simple.
    digit2 = nrx(rskDigit).repeat(2)

    dateRx = &[
      # `'-'? yyyy`
      ?nrx('-'), nrx(rskDigit).repeat(4),
      # `'-' mm`
      nrx('-'), digit2,
      # `'-' dd`
      nrx('-'), digit2,
    ]

    secondsFractionRx = &[nrx('.'), +nrx(rskDigit)]

    timeRx = &[
      digit2,
      # `':' mm`
      nrx(':'), digit2,
      # `':' ss`
      nrx(':'), digit2,
    ]

    # `(zzzzzz)?` Timezone is a string of the form:
    # `(('+' | '-') hh ':' mm) | 'Z'` where
    timezoneRx = nrx('Z') | &[
      nrx({
        '+', # '+' indicates a nonnegative duration,
        '-', # '-' indicates a nonpositive duration.
      }),
      # `hh` is a two-digit numeral (with leading zeros as
      # required) that represents the hours,
      digit2,
      nrx(':'),
      # `mm` is a two-digit numeral that represents the minutes,
      digit2
    ]

  # echo str, " ", str =~ integerRx, " ", integerRx.toStr()
  # echo integerRx

  if xtkBoolean in expected and str in ["true", "false"]:
    result = xtkBoolean


  elif xtkInteger in expected and str =~ integerRx:
    result = xtkInteger

  elif xtkDecimal in expected and str =~ decimalRx:
    result = xtkDecimal

  elif len({xtkFloat, xtkDouble} * expected) > 0 and
       (str in ["INF"] or str =~ floatRx):
    # For now just use primitive implementation, later I would have to
    # parse the string and determine if it can fit in float or it is a
    # double.

    if xtkFloat in expected:
      result = xtkFloat

    else:
      result = xtkDouble

  elif xtkDuration in expected and str[0] == 'P':
    result = xtkDuration
    # The lexical representation for duration is the [ISO 8601] extended
    # format PnYn MnDTnH nMnS, where nY represents the number of years, nM
    # the number of months, nD the number of days, 'T' is the date/time
    # separator, nH the number of hours, nM the number of minutes and nS
    # the number of seconds. The number of seconds can include decimal
    # digits to arbitrary precision.

  elif xtkDateTime in expected and
       str =~ &[dateRx, nrx('T'), timeRx, ?secondsFractionRx, ?timezoneRx]:
    # dateTime consists of finite-length sequences of characters of the
    # form:
    #
    # `'-'? yyyy '-' mm '-' dd 'T' hh ':' mm ':' ss ('.' s+)? (zzzzzz)?`

    result = xtkDateTime

  elif xtkTime in expected and
       str =~ &[timeRx, ?secondsFractionRx, ?timezoneRx]:
    result = xtkTime

  # Date is undeschipherable without looking into 8601 standard

  # elif xtkDate in expected and
  #      str =~ &[timeRx]

  elif xtkString in expected:
    result = xtkString

  else:
    result = xtkCharData


proc xsdToken*(parser: HXmlParser, expected: set[XsdTokenKind]): XsdToken =
  ## Classify current XSD token
  var kind: XsdTokenKind
  case parser.kind:
    of xmlError:        kind = xtkError
    of xmlEof:          kind = xtkEof
    of xmlWhitespace:   kind = xtkWhitespace
    of xmlComment:      kind = xtkComment
    of xmlPI:           kind = xtkPI
    of xmlAttribute:    kind = xtkAttribute
    of xmlElementClose: kind = xtkElementClose
    of xmlSpecial:      kind = xtkSpecial
    of xmlEntity:       kind = xtkEntity
    of xmlCharData:     kind = parser.strVal().classifyXsdString(expected)
    of xmlCData:        kind = xtkCData


    of xmlElementStart:
      return XsdToken(kind: xtkElementStart, xmlName: parser.strVal())

    of xmlElementEnd:
      return XsdToken(kind: xtkElementEnd, xmlName: parser.strVal())

    of xmlElementOpen:
      return XsdToken(kind: xtkElementOpen, xmlName: parser.strVal())

  return XsdToken(kind: kind)



template raiseUnexpectedToken*(
    parser: HXmlParser, token: XsdToken): untyped =

  {.line: instantiationInfo().}:
    raise newException(
      HXmlParseError,
      "Unexpected token <" & $token & "> at " &
        parser.displayAt()
    )

## * XML generation

type
  XmlWriter* = object of RootObj
    base*: BaseWriter

using
  writer: var XmlWriter
  reader: var HXmlParser

proc `=destroy`*(writer: var XmlWriter) = discard
  # echo "=destroy called"
  # writer.close()

genBaseWriterProcs(XmlWriter)

proc xmlCData*(writer; text: string) =
  writer.writeRaw("<![CDATA[")
  var start = 0
  var pos = text.find("]]>")
  while pos != -1:
    writer.writeRaw(text[start .. pos])
    writer.writeRaw("]]")
    start = pos + 3
    pos = text.find("]]>", start)
    writer.writeRaw("]]><![CDATA[>")

  writer.writeRaw(text[start .. ^1])
  writer.writeRaw("]]>")

proc xmlStart*(writer; elem: string, indent: bool = true) =
  if indent: writer.writeIndent()
  writer.writeRaw("<", elem, ">")
  if indent: writer.line()


proc xmlEnd*(writer; elem: string, indent: bool = true) =
  if indent: writer.writeIndent()
  writer.writeRaw("</", elem, ">")
  if indent: writer.line()

proc xmlOpen*(writer; elem: string, indent: bool = true) =
  if indent: writer.writeIndent()
  writer.writeRaw("<", elem)




proc xmlClose*(writer) = writer.writeRaw(">")



proc xmlCloseEnd*(writer; newline: bool = true) =
  writer.writeRaw("/>")
  if newline: writer.line()


proc xmlWrappedCdata*(writer; text, tag: string) =
  writer.writeIndent()
  writer.xmlStart(tag, false)
  writer.xmlCData(text)
  writer.xmlEnd(tag, false)
  writer.line()



proc writeEscaped*(writer; text: string) =
  writer.writeRaw(xmltree.escape text)


proc xmlAttribute*(writer; key: string, value: string) =
  writer.writeRaw(" ", key, "=\"", xmltree.escape(value), "\"")

proc xmlStart*(
    writer; tag: string, table: openarray[(string, string)],
    indent: bool = true
  ) =
  writer.xmlOpen(tag, indent)
  for (key, value) in table:
    writer.xmlAttribute(key, value)

  writer.xmlClose()
  if indent: writer.line()

proc xmlSingle*(
    writer; tag: string, table: openarray[(string, string)],
    indent: bool = true
  ) =
  writer.xmlOpen(tag, indent)
  for (key, value) in table:
    writer.xmlAttribute(key, value)

  writer.xmlCloseEnd(indent)

proc xmlRawSingle*(writer; text: string, indent: bool = true) =
  if indent:
    writer.writeIndent()

  writer.writeRaw(text)
  if indent:
    writer.line()

proc eopen*(
    writer; elem: string,
    params: openarray[(string, string)] = @[];
    indent: bool = false
  ) =
  writer.xmlStart(elem, params, indent)

proc esingle*(
    writer; elem: string,
    params: openarray[(string, string)] = @[];
    indent: bool = false
  ) =
  writer.xmlSingle(elem, params, indent)

proc etagl*(
    writer; name: string;
    params: openarray[(string, string)] = @[];
  ) =
  xmlSingle(writer, name, params, true)

proc etag*(
    writer; name: string;
    params: openarray[(string, string)] = @[];
  ) =
  xmlSingle(writer, name, params, false)

proc eclose*(writer; elem: string; indent: bool = true) =
  writer.xmlEnd(elem, indent)

template eindent*(writer; body): untyped =
  indent(writer)
  body
  dedent(writer)

template ewrap*(
    writer; name: string;
    params: openarray[(string, string)] = @[];
    body: untyped
  ): untyped =
  eopen(writer, name, params, false)
  body
  eclose(writer, name, false)


template ewrapl*(
    writer; name: string;
    params: openarray[(string, string)] = @[];
    indentBody: bool = true,
    body: untyped
  ): untyped =

  writeIndent(writer)
  eopen(writer, name, params, false)
  line(writer)
  if indentBody:
    writer.indent()

  body

  if indentBody:
    writer.dedent()

  writeIndent(writer)
  eclose(writer, name, false)
  line(writer)

template ewrapl1*(
    writer; name: string;
    params: openarray[(string, string)] = @[];
    body: untyped
  ): untyped =
  ## Open and close tag around body. Newline will be added after whole
  ## block, but not inside. `ewrapl1("h1", ...)` will result in
  ## `<h1> ... </h1>`. Existing indentation is printed out, but not
  ## changed for body.

  writeIndent(writer)
  eopen(writer, name, params, false)
  body
  eclose(writer, name, false)
  line(writer)


when isMainModule:
  import std/unittest
  let t = classifyXsdString

  let all = { low(XsdTokenKind) .. high(XsdTokenKind) }

  check t("12", all) == xtkInteger
  check t("12.0", all) == xtkDecimal
  check t("+0.3E-2", all) == xtkFloat
  check t("2002-10-10T12:00:00+05:00", all) == xtkDateTime
  check t("00:00:00", all) == xtkTime
