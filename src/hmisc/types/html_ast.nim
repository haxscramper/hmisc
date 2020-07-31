##[

Statically typed ast for subset of HTML

Originally implemented to generate graphviz html-like labels.

]##

import colors, xmltree, strformat, strutils, strtabs, sequtils
import hprimitives

type
  HtmlElemKind* = enum
    hekTable
    hekRow
    hekCell

    hekImage
    hekOther
    hekText

  HtmlTextProp* = enum
    htpNone
    htpBold
    htpUnderline
    htpItalic


  HtmlElem* = object
    elements*: seq[HtmlElem]
    attrs*: StringTableRef
    case kind*: HtmlElemKind
      of hekTable:
        border*: int
        bgcolor*: Color
        height*: int
        width*: int
      of hekText:
        textProps*: set[HtmlTextProp]
        textColor*: Color
        textStr*: string
      of hekCell:
        cellColor*: Color
        cellBgColor*: Color
        cellSize*: ArrSize
        dotPort*: int
      else:
        discard

func `[]`*(html: HtmlElem, idx: int): HtmlElem =
  html.elements[idx]

func `[]`*(html: var HtmlElem, idx: int): var HtmlElem =
  html.elements[idx]

func `[]=`*(html: var HtmlElem, idx: int, other: HtmlElem): void =
  html.elements[idx] = other

func `[]=`*(html: var HtmlElem, attrname: string, attrval: string): void =
  if html.attrs == nil:
    html.attrs = newStringTable()

  # debugecho "set ", attrname, " as ", attrval
  html.attrs[attrname] = attrval

func add*(html: var HtmlElem, other: HtmlElem): void =
  assert html.kind notin {hekText}
  html.elements.add other

func len*(html: HtmlElem): int = html.elements.len

func toHtmlText*(text: string,
                 color: Color = colNoColor,
                 props: set[HtmlTextProp] = {}): HtmlElem =
  HtmlElem(kind: hekText, textStr: text, textColor: color, textProps: props)


func toHtmlCell*(content: HtmlElem): HtmlElem =
  if content.kind == hekCell:
    content
  else:
    HtmlElem(kind: hekCell, elements: @[content])


func toHtmlCell*(strbl: string): HtmlElem =
  HtmlElem(kind: hekCell, elements: @[strbl.toHtmlText()])


func toHtmlCell*(strbl: StrBlock): HtmlElem =
  toHtmlCell(strbl.join("\n"))

func toHtmlRow*(cell: HtmlElem): HtmlElem =
  HtmlElem(kind: hekRow, elements: @[toHtmlCell(cell)])

func toHtmlRow*(cells: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekRow, elements: cells)

func toHtmlTable*(cells: seq[seq[HtmlElem]]): HtmlElem =
  HtmlElem(kind: hekTable, elements: cells.map(toHtmlRow))

func toHtmlTableHoriz*(cells: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekTable,
           elements: @[toHtmlRow(cells.map(toHtmlCell))])

func toHtmlTableVert*(cells: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekTable,
           elements: cells.mapIt(it.toHtmlCell().toHtmlRow()))

func setOrAddCell*(table: var HtmlElem, pos: ArrPos, cell: HtmlElem): void =
  assert table.kind == hekTable
  if table.elements.len <= pos.row:
    for _ in table.elements.len .. pos.row:
      table.elements.add HtmlElem(kind: hekRow)

  if table.elements[pos.row].len <= pos.col:
    for _ in table.elements[pos.row].len .. pos.col:
      table.elements[pos.row].add HtmlElem(kind: hekCell)

  table[pos.row][pos.col] = cell


func wrap(xml: XmlNode, tag: string): XmlNode =
  result = newElement(tag)
  result.add xml

func `[]=`*(xml: var XmlNode, attrname: string, attrval: string): void =
  var attrs = xml.attrs()
  if attrs != nil:
    attrs[attrname] = attrval
    xml.attrs = attrs
  else:
    xml.attrs = {attrname : attrval}.toXmlAttributes()


func toXml(html: HtmlElem): XmlNode =
  case html.kind:
    of hekTable:
      result = newElement("table")
      result["border"] = $html.border
    of hekRow:
      result = newElement("tr")
    of hekCell:
      result = newElement("td")
      if html.dotPort != 0:
        result["port"] = "t" & $html.dotPort

    of hekText:
      result = newText(html.textStr)
      if html.textColor != colNoColor:
        result = result.wrap("font")
        result["color"] = $html.textColor

      for prop in html.textProps:
        result =
          case prop:
            of htpBold: result.wrap("b")
            of htpUnderline: result.wrap("u")
            of htpItalic: result.wrap("i")
            of htpNone: result
    else:
      discard


  if html.attrs != nil:
    var tmp = result.attrs
    for key, val in pairs(html.attrs):
      tmp[key] = val

    result.attrs = tmp

  for row in html.elements:
    result.add row.toXml()


proc toPrettyStr*(n: XmlNode): string = add(result, n, 0, 2, true)
proc toFlatStr*(n: XmlNode): string = add(result, n, 0, 0, false)
proc toPrettyStr*(n: HtmlElem): string = toPrettyStr(n.toXml())
proc toFlatStr*(n: HtmlElem): string = toFlatStr(n.toXml())

func `$`*(html: HtmlElem): string = $html.toXml()

func toHtmlDoc*(html: HtmlElem): string =
  $html.toXml().wrap("body").wrap("html")
