import shell
import hmisc/helpers
import sequtils
import json
import strutils, strformat
import options

type
  ListStyleKind = enum
    lskDefaultStyle
    lskExample
    lskDecimal
    lskLowerRoman
    lskUpperRoman
    lskLowerAlpha
    lskUpperAlpha

  ListDelimiterKind = enum
    ldkDefaultDelim
    ldkPeriod
    ldkOneParen
    ldkTwoParens

  ListAttributes = object
    startNumber: int
    numberStyle: ListStyleKind
    delimStyle: ListDelimiterKind

type
  DocFormat = string
  PlainText = string
  DocAttr = object
    identifier: string
    classes: seq[string]
    kvPairs: seq[(string, string)]

  DocLink = object
    url: string
    target: string

  DocCitationMode = enum
    dckAuthorInText
    dckSuppressAuthor
    dckNormalCitation

  DocCitation = object
    id: string
    prefix: FormattedText
    suffix: FormattedText
    mode: DocCitationMode
    noteNum: int
    hash: int

  DocInlineKind = enum
    inkStr #  Text
    inkEmph #  [Inline]
    inkStrong #  [Inline]
    inkStrikeout #  [Inline]
    inkSuperscript #  [Inline]
    inkSubscript #  [Inline]
    inkSmallCaps #  [Inline]
    inkQuoted #  QuoteType [Inline]
    inkCite #  [Citation]  [Inline]
    inkCode #  Attr Text
    inkSpace #
    inkSoftBreak #
    inkLineBreak #
    inkMath #  MathType Text
    inkRawInline #  Format Text
    inkLink #  Attr [Inline] Target
    inkImage #  Attr [Inline] Target
    inkNote #  [Block]
    inkSpan #  Attr [Inline]

  DocInline = object
    case kind: DocInlineKind:
      of inkStr: #  Text
        text: string
      of inkEmph, inkStrong, inkStrikeout,
         inkSuperscript, inkSubscript, inkSmallCaps:
           #  [Inline]
        content: FormattedText
      of inkQuoted: #  QuoteType [Inline]
        singleQuote: bool ## Single/double quote
        qtContent: FormattedText
      of inkCite: #  [Citation]  [Inline]
        citation: DocCitation
        citComment: FormattedText
      of inkCode: #  Attr Text
        cAttr: DocAttr
        source: PlainText ## Source code
      of inkSpace, inkSoftBreak, inkLineBreak: #
        discard
      of inkMath: #  MathType Text
        latex: PlainText
        isInline: bool
      of inkRawInline: #  Format Text
        format: DocFormat
        inline: PlainText
      of inkLink: #  Attr [Inline] Target
        lAttr: DocAttr
        lInline: seq[DocInline]
        lTarget: DocLink
      of inkImage: #  Attr [Inline] Target
        iAttr: DocAttr
        iInline: seq[DocInline]
        iTarget: DocLink
      of inkNote: #  [Block]
        blocks: seq[DocBlock]
      of inkSpan: #  Attr [Inline]
        sAttr: DocAttr
        sInline: seq[DocInline]

  FormattedText = seq[DocInline]

  TableAlignKind = enum
    takAlignLeft
    takAlignRight
    takAlignCenter
    takAlignDefault

  DocBlockKind = enum
    dbkPlain # [Inline]
    dbkPara # [Inline]
    dbkLineBlock # [[Inline]]
    dbkCodeBlock # Attr Text
    dbkRawBlock # Format Text
    dbkBlockQuote # [Block]
    dbkOrderedList # ListAttributes [[Block]]
    dbkBulletList # [[Block]]
    dbkDefinitionList # [([Inline],[[Block]])]
    dbkHeader # Int Attr [Inline]
    dbkHorizontalRule #
    dbkTable # [Inline] [Alignment] [Double] [TableCell] [[TableCell]]
    dbkDiv # Attr [Block]
    dbkNull #

  TableCell = seq[DocBlock]

  DocBlock = object
    case kind*: DocBlockKind:
      of dbkPlain, dbkPara: # [Inline]
        plaintext: seq[DocInline]
      of dbkLineBlock: # [[Inline]]
        lineBlocs: seq[seq[DocInline]]
      of dbkCodeBlock: # Attr Text
        cAttr: DocAttr
        source: string
      of dbkRawBlock: # Format Text
        rAttr: DocAttr
        rSource: string
      of dbkBlockQuote: # [Block]
        quoteBlocsk: seq[DocBlock]
      of dbkOrderedList: # ListAttributes [[Block]]
        lAttr: ListAttributes
        ordBlocks: seq[seq[DocBlock]]
      of dbkBulletList: # [[Block]]
        bullBlocks: seq[seq[DocBlock]]
      of dbkDefinitionList: # [([Inline],[[Block]])]
        definitions: seq[tuple[
          name: seq[DocInline],
          blocks: seq[seq[DocBlock]]
        ]]
      of dbkHeader: # Int Attr [Inline]
        hLevel: int
        hAttr: DocAttr
        hName: FormattedText
      of dbkHorizontalRule, dbkNull: #
        nil
      of dbkTable:
        # [Inline] [Alignment] [Double] [TableCell] [[TableCell]]
        tCaption: FormattedText
        tColAlign: TableAlignKind
        relWidths: float
        headers: TableCell
        rows: seq[seq[TableCell]]
      of dbkDiv: # Attr [Block]
        divAttr: DocAttr
        divBlocks: seq[DocBlock]

type
  RawPandoc = object
    kind: string
    case final: bool
    of true:
      value: string
    of false:
      content: seq[RawPandoc]

func toCleanString(node: JsonNode): string =
  case node.kind:
    of JString: $node.getStr()
    of JInt: $node.getInt()
    of JFloat: $node.getFloat()
    else: $node


func toRawPandoc(node: JsonNode): RawPandoc =
  if node.kind == JObject:
    if node.hasKey("c") and node["c"].kind == JArray:
      RawPandoc(
        final: false,
        kind: node["t"].getStr(),
        content: node["c"].getElems().map(toRawPandoc)
      )
    else:
      RawPandoc(
        final: true,
        kind: node["t"].getStr(),
        value:
          block:
            if node.hasKey("c"): node["c"].toCleanString()
            else: ""
      )
  elif node.kind == JArray:
    RawPandoc(
      final: false,
      kind: "ValueArray",
      content: node.getElems().map(toRawPandoc)
    )
  else:
    RawPandoc(
      final: true,
      kind: "Final",
      value: node.toCleanString()
    )

func maxLen(node: RawPandoc): int =
  case node.final:
    of true: 1
    of false:
      max(
        node.content.len,
        node.content.map(maxLen).max(0)
      )

func dfsJoin(node: RawPandoc, first = true): seq[string] =
  if node.final:
    @[node.value]
  else:
    let sub = node.content[0].dfsJoin(false)
    if first: sub
    else: node.kind & sub


func debugPrint(node: RawPandoc, ind = 0, index = 0): void =
  let pref = "  ".repeat(ind)
  if node.final:
    debugecho &"{pref}#{index} [{node.kind}] {node.value}"
  else:
    if node.maxLen() == 1:
      let subnodes = toSeq(pairs(node.dfsJoin())).mapIt(
        &"#{it[0]} [{it[1]}]"
      ).join(" ")

      debugecho &"{pref}{subnodes}"

    else:
      debugecho &"{pref}#{index} [{node.kind}]"
      for index, child in node.content:
        debugprint(child, ind + 1, index)

# func makePlaintextBlock(text: string): DocBlock =

func findFirst(node: RawPandoc, kind: string): Option[RawPandoc] =
  if node.kind == kind:
    return some(node)

  if node.final:
    return none(RawPandoc)
  else:
    for child in node.content:
      let res = findFirst(child, kind)
      if res.isSome():
        return res


func isEmpty(node: RawPandoc): bool =
  (not node.final) and node.content.len == 0

type
  SimpleTable = object
    header: seq[string]
    cells: seq[seq[string]]

func toTableSimple(node: RawPandoc): SimpleTable =
  assert node.kind == "Table"
  let alignSettings = node.content[1]

  func cellStrVal(cell: RawPandoc): string =
    if cell.isEmpty:
      ""
    else:
      # #1 [ValueArray]
      #   #0 [Plain]
      #     #0 [Str] "$x$"
      cell.content[0].content[0].value


  let tableHeaders: seq[string] =
    node.content[3].content.map(cellStrVal)
  let tableCells: seq[seq[string]] =
    node.content[4].content.mapIt(it.content.map(cellStrVal))

  SimpleTable(
    header: tableHeaders,
    cells: tableCells
  )






proc getTable(parsed: seq[RawPandoc]): SimpleTable =
  for node in parsed:
    let res = findFirst(node, "Table")
    if res.isSome():
      # debugprint res.get()
      return res.get().toTableSimple()


func toLatexTable(tbl: SimpleTable): string =
  let newline = " \\\\"
  let formatting = "||" & tbl.header.mapIt("l").join("|") & "||"
  let headers = tbl.header.join(" & ") & newline
  let cells = tbl.cells.mapIt(it.join(" & ")).join(newline & "\n")


  return fmt"""
\begin{{tabular}}{{{formatting}}}
{headers}
\hline
\hline
{cells}
\end{{tabular}}
"""

proc htmlTableToLatex*(htmlTable: string): string =
  let tmpfile = "/tmp/htmltable.html"
  tmpfile.writeFile(htmlTable)
  let (res, err, code) = shellVerboseErr {dokCommand}:
    pandoc -t json ($tmpfile)

  if code == 0:
    let jnode = res.parseJson()
    let blocks = jnode["blocks"].getElems()
    let tbl = getTable(blocks.map(toRawPandoc))
    return tbl.toLatexTable()
  else:
    echo err
    return ""


when isMainModule:
  discard htmlTableToLatex("test.tmp.html".readFile().string())



# for blc in parsed:
#   print blc
