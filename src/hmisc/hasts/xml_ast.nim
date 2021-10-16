import std/[
  xmltree, xmlparser, enumerate, strutils,
  tables, with, strtabs, options, streams,
  parsexml
]

export xmltree, strtabs, xmlparser, parsexml

import
  ../core/all,
  ../other/[oswrap, rx],
  ../types/colorstring,
  ../algo/[clformat, hstring_algo]

export oswrap

proc parseXml*(file: AbsFile): XmlNode = parseXml(file.readFile())

type
  HXmlParserEventError* = object of CatchableError
  HXmlParser* = object
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


proc next*(parser: var HXmlParser) =
  if parser.traceNext:
    echo "> ", parser.displayAt()

  next(parser.base)


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
          result[^1] = clr(' ')
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
  raiseImplementError("")

proc parseXsdDecimal*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError("")

proc parseXsdInteger*(
    target: var XsdParseTarget[int], parser: var HXmlParser,
    tag: string = ""
  ) =
  let res = parser.strVal().parseInt()
  setValue(target, res)
  parser.next()

proc parseXsdFloat*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdDouble*(
    target: var XsdParseTarget[float], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdDuration*(
    target: var XsdParseTarget[Duration], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdDateTime*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdTime*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdDate*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdGYearMonth*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdGYear*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc paresXsdGMonthDay*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdGDay*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdGMonth*(
    target: var XsdParseTarget[DateTime], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdHexBinary*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdBase64Binary*(
    target: var XsdParseTarget[string], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

proc parseXsdUri*(
    target: var XsdParseTarget[URI], parser: var HXmlParser,
    tag: string = ""
  ) =
  raiseImplementError(parser.displayAt())

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
  XmlWriter* = object
    stream: Stream
    indentBuf: string
    ignoreIndent: int

using
  writer: var XmlWriter
  reader: var HXmlParser

proc close*(writer: var XmlWriter) = writer.stream.close()

proc `=destroy`*(writer: var XmlWriter) = discard
  # echo "=destroy called"
  # writer.close()


proc newXmlWriter*(): XmlWriter =
  XmlWriter(stream: newStringStream())

proc readAll*(writer): string =
  writer.stream.setPosition(0)
  writer.stream.readAll()

proc newXmlWriter*(stream: Stream): XmlWriter =
  XmlWriter(stream: stream)

proc newXmlWriter*(file: File): XmlWriter =
  newXmlWriter(newFileStream(file))

proc newXmlWriter*(file: AbsFile): XmlWriter =
  assertExists(file.dir())
  newXmlWriter(newFileStream(file, fmWrite))


proc space*(writer) = writer.stream.write(" ")
proc line*(writer) = writer.stream.write("\n")
proc indent*(writer) = writer.indentBuf.add "  "
proc dedent*(writer) =
  writer.indentBuf.setLen(max(writer.indentBuf.len - 2, 0))

proc writeInd*(writer) =
  if writer.ignoreIndent > 0:
    dec writer.ignoreIndent

  else:
    writer.stream.write(writer.indentBuf)

proc ignoreNextIndent*(writer) = inc writer.ignoreIndent

proc xmlCData*(writer; text: string) =
  writer.stream.write("<![CDATA[")
  var start = 0
  var pos = text.find("]]>")
  while pos != -1:
    writer.stream.write(text[start .. pos])
    writer.stream.write("]]")
    start = pos + 3
    pos = text.find("]]>", start)
    writer.stream.write("]]><![CDATA[>")

  writer.stream.write(text[start .. ^1])
  writer.stream.write("]]>")

proc xmlStart*(writer; elem: string, indent: bool = true) =
  if indent: writer.writeInd()
  writer.stream.write("<", elem, ">")
  if indent: writer.line()


proc xmlEnd*(writer; elem: string, indent: bool = true) =
  if indent: writer.writeInd()
  writer.stream.write("</", elem, ">")
  if indent: writer.line()

proc xmlOpen*(writer; elem: string, indent: bool = true) =
  if indent: writer.writeInd()
  writer.stream.write("<", elem)




proc xmlClose*(writer) = writer.stream.write(">")



proc xmlCloseEnd*(writer; newline: bool = true) =
  writer.stream.write("/>")
  if newline: writer.line()


proc xmlWrappedCdata*(writer; text, tag: string) =
  writer.writeInd()
  writer.xmlStart(tag, false)
  writer.xmlCData(text)
  writer.xmlEnd(tag, false)
  writer.line()


template loadPrimitive*(
    stream: var HXmlParser, target: typed, tag: string,
    loadAttr: untyped, loadField: untyped
  ): untyped =

  if stream.isAttribute():
    loadAttr
    stream.next()

  else:
    stream.skipElementStart(tag)
    loadField
    stream.skipElementEnd(tag)

proc loadEnumWithPrefix*[E](
    r: var HXmlParser; kind: var E, tag, prefix: string) =
  loadPrimitive(
    r, kind, tag,
    (kind = parseEnum[E](prefix & r.strVal())),
    (kind = parseEnum[E](prefix & r.strVal()))
  )




proc toXmlString*[T](item: Option[T]): string =
  mixin toXmlString
  if item.isSome():
    return toXmlString(item.get())

proc toXmlString*(item: string): string = item
proc toXmlString*(item: enum | bool | float): string = $item
proc toXmlString*(item: SomeInteger): string = $item
proc writeRaw*(writer; text: string) =
  writer.stream.write(text)

proc writeEscaped*(writer; text: string) =
  writer.stream.write(xmltree.escape text)

proc xmlAttribute*(
    writer; key: string, value: SomeInteger | bool | float | enum | string) =
  writer.stream.write(
    " ", key, "=\"", xmltree.escape(toXmlString(value)), "\"")

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

  writer.xmlCloseEnd()
  if indent: writer.line()


proc xmlAttribute*[T](writer; key: string, value: Option[T]) =
  if value.isSome():
    xmlAttribute(writer, key, value.get())

proc xmlAttribute*[A, B](writer; key: string, value: HSlice[A, B]) =
  xmlAttribute(writer, key, toXmlString(value.a) & ":" & toXmlString(value.b))


proc writeXml*(
  writer; value: string | SomeInteger | bool | SomeFloat | enum, tag: string) =
  writer.writeInd()
  writer.xmlStart(tag, false)
  writer.stream.write(xmltree.escape $value)
  writer.xmlEnd(tag, false)
  writer.line()


proc writeXml*[T](writer; values: seq[T], tag: string) =
  mixin writeXml
  for it in values:
    writer.writeXml(it, tag)

proc writeXml*[K, V](writer; table: Table[K, V], tag: string) =
  mixin writeXml
  for key, value in pairs(table):
    writer.xmlStart(tag)
    writer.writeXml(key, "key")
    writer.writeXml(value, "value")
    writer.xmlEnd(tag)

proc writeXml*[T](writer; opt: Option[T], tag: string) =
  if opt.isSome():
    writeXml(writer, opt.get(), tag)

proc writeXml*(writer; value: Slice[int], tag: string) =
  with writer:
    xmlOpen(tag)
    xmlAttribute("a", $value.a)
    xmlAttribute("b", $value.b)
    xmlCloseEnd()

proc writeXml*[E: enum](writer; values: set[E], tag: string) =
  writer.xmlStart(tag)
  writer.indent()
  writer.writeInd()
  for item in values:
    writer.xmlOpen("item")
    writer.xmlAttribute("val", $item)
    writer.xmlCloseEnd()

  writer.dedent()
  writer.xmlEnd(tag)


proc loadXml*[E: enum](stream: var HXmlParser, target: var E, tag: string) =
  if stream.atAttr():
    target = parseEnum[E](stream.attrValue())
    stream.next()

  else:
    stream.skipStart(tag)
    target = parseEnum[E](stream.strVal())
    stream.next()
    stream.skipEnd(tag)

proc isAttribute*(stream: HXmlParser): bool =
  stream.kind == XmlEventKind.xmlAttribute

proc loadXml*(stream: var HXmlParser, target: var bool, tag: string) =
  loadPrimitive(
    stream, target, tag,
    loadAttr = (target = stream.strVal().parseBool()),
    loadField = (target = stream.strVal().parseBool()),
  )

proc loadXml*(stream: var HXmlParser, target: var SomeFloat, tag: string) =
  loadPrimitive(
    stream, target, tag,
    loadAttr = (target = stream.strVal().parseFloat()),
    loadField = (target = stream.strVal().parseFloat()),
  )

proc loadXml*[E](stream: var HXmlParser, target: var set[E], tag: string) =
  stream.skipElementStart(tag)
  while stream.elementName() == "item":
    stream.next()
    assert stream.attrKey() == "val"
    target.incl parseEnum[E](stream.attrValue())
    stream.next()

  stream.skipElementEnd(tag)

proc loadXml*[T](stream: var HXmlParser, target: var seq[T], tag: string) =
  mixin loadXml
  while stream.kind in {xmlElementOpen, xmlElementStart} and
        stream.elementName() == tag:
    var tmp: T
    loadXml(stream, tmp, tag)
    target.add tmp


template loadXml*[T](
    stream: var HXmlParser, target: var seq[T],
    tag: string, mixedStr: T, fieldAsgn: untyped
  ) =
  mixin loadXml
  while stream.kind in {
    xmlElementOpen, xmlElementStart, xmlCharData, xmlWhitespace
  }:
    case stream.kind:
      of xmlElementOpen, xmlElementStart:
        if stream.elementName() == tag:
          var tmp: T
          loadXml(stream, tmp, tag)
          target.add tmp

        else:
          break

      of xmlElementCharData, xmlWhitespace:
        var next {.inject.} = mixedStr
        var str: string
        loadXml(stream, str, "")
        fieldAsgn = str
        target.add next

      else:
        break

proc loadXml*[A, B](
  stream: var HXmlParser, target: var Table[A, B], tag: string) =

  mixin loadXml
  while stream.elementName() == tag:
    var key: A
    var val: B
    stream.skipElementStart(tag)
    loadXml(stream, key, "key")
    loadXml(stream, val, "value")
    stream.skipElementEnd(tag)
    target[key] = val



proc loadXml*[T](stream: var HXmlParser, target: var Option[T], tag: string) =
  mixin loadXml
  if (stream.isAttribute() and stream.attrKey() == tag) or
     (stream.elementName() == tag):
    var tmp: T
    loadXml(stream, tmp, tag)
    target = some(tmp)

proc loadXml*(stream: var HXmlParser, target: var string, tag: string) =
  if stream.isAttribute():
    target = stream.strVal()
    stream.next()

  else:
    stream.skipElementStart(tag)
    while stream.kind() in {xmlWhitespace, xmlCharData, XmlEventKind.xmlCData}:
      target &= stream.strVal()
      next(stream)

    stream.skipElementEnd(tag)

proc loadXml*(stream: var HXmlparser, target: var AbsFile, tag: string) =
  var tmp: string
  loadXml(stream, tmp, tag)
  target = AbsFile(tmp)

proc loadXml*(stream: var HXmlParser, target: var int, tag: string) =
  if stream.isAttribute():
    target = stream.strVal().parseInt()
    stream.next()

  else:
    stream.skipElementStart(tag)
    target = stream.strVal().parseInt()
    stream.skipElementEnd(tag)


proc loadXml*(reader; target: var Slice[int], tag: string) =
  if reader.atAttr:
    let val = reader.strVal().split(":")
    target.a = parseInt(val[0])
    target.b = parseInt(val[1])
    reader.next()


  else:
    with reader:
      skipOpen(tag)
      loadXml(target.a, "a")
      loadXml(target.b, "b")
      skipClose()


proc loadXml*(reader; target: var XmlNode, tag: string) =
  parseXsdAnyType(target, reader, tag)


when isMainModule:
  import std/unittest
  let t = classifyXsdString

  let all = { low(XsdTokenKind) .. high(XsdTokenKind) }

  check t("12", all) == xtkInteger
  check t("12.0", all) == xtkDecimal
  check t("+0.3E-2", all) == xtkFloat
  check t("2002-10-10T12:00:00+05:00", all) == xtkDateTime
  check t("00:00:00", all) == xtkTime
