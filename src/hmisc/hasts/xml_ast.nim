import std/[xmltree, xmlparser, enumerate, strutils,
            strtabs, options, streams]

# import std/parsexml except charData, elementName, entityName,
#                            attrKey, attrValue, piName, piRest

# export parsexml except charData, elementName, entityName,
#                        attrKey, attrValue, piName, piRest
import parsexml
# import std/parsexml as pxml
export xmltree, strtabs, xmlparser, parsexml

import ../other/[oswrap, rx]
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

    of {
      xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial
    }:
      var chard = parser.base.charData()
      chard = chard[0 .. min(chard.high, 20)]
      $parser.base.kind & " " & chard

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
       xmlName: string

     else:
       discard

proc classifyXsdString*(str: string, expected: set[XsdTokenKind]): XsdTokenKind =
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

  else:
    result = xtkCharData


proc xsdToken*(parser: HXmlParser, expected: set[XsdTokenKind]): XsdToken =
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



when isMainModule:
  import std/unittest
  let t = classifyXsdString

  let all = { low(XsdTokenKind) .. high(XsdTokenKind) }

  check t("12", all) == xtkInteger
  check t("12.0", all) == xtkDecimal
  check t("+0.3E-2", all) == xtkFloat
  check t("2002-10-10T12:00:00+05:00", all) == xtkDateTime
  check t("00:00:00", all) == xtkTime
