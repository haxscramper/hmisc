##[

Statically typed ast for subset of HTML

Originally implemented to generate graphviz html-like labels.

]##

import std/[colors, xmltree, strformat, strutils, strtabs, sequtils, terminal,
       macros, options]
import hmisc/[helpers, hexceptions]
import hmisc/types/[hprimitives, colorstring]
export xmltree

type
  Xml* = XmlNode
  HtmlElemKind* = enum
    hekTable
    hekRow
    hekCell

    hekElemList
    hekImage
    hekOther
    hekText

  HtmlTextProp* = enum
    htpNone
    htpBold
    htpUnderline
    htpItalic


  HtmlElem* = ref object
    elements*: seq[HtmlElem]
    attrs*: StringTableRef
    class*: seq[string]
    case kind*: HtmlElemKind
      of hekTable:
        border*: int
        bgcolor*: Color
        height*: int
        width*: int
      of hekText:
        textProps*: set[HtmlTextProp]
        textColor* {.requiresinit.}: Color
        textColorBg* {.requiresinit.}: Color
        textStr*: string
        textPre*: bool
      of hekCell:
        cellColor*: Color
        cellBgColor*: Color
        cellSize*: ArrSize
        dotPort*: int
      of hekOther:
        tagname*: string
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

func add*(html: var HtmlElem, other: HtmlElem | seq[HtmlElem]): void =
  assert html.kind notin {hekText}
  html.elements.add other

func len*(html: HtmlElem): int = html.elements.len

func toHtmlText*(text: string,
                 color: Color = colNoColor,
                 props: set[HtmlTextProp] = {},
                 bgColor: Color = colNoColor
                ): HtmlElem =
  HtmlElem(kind: hekText,
           textStr: text,
           textColor: color,
           textProps: props,
           textColorBg: bgColor,
           # textPre: pre
  )

func newElementHtml*(tag: string): HtmlElem =
  HtmlElem(kind: hekOther, tagname: tag)

func newHtmlText*(text: string): HtmlElem =
  HtmlElem(kind: hekText, textStr: text,
           textColor: colNoColor,
           textColorBg: colNoColor,
           # textPre: pre
  )

# func newHtmlPre*(subitems: seq[HtmlElem]): HtmlElem =
#   HtmlElem(kind: hekPre, elements: subitems)

const xmlSeqTag* = "xml-seq-tag-ignore-when-export"

func add*(xml: var Xml, optXml: Option[Xml]) =
  if optXml.isSome():
    xml.add optXml.get()

func newXml*(tag: string, args: seq[Xml] = @[]): Xml =
  newXmlTree(tag, args)

func newXmlSeq*(elems: seq[Xml] = @[]): Xml = newXmlTree(xmlSeqTag, elems)

func newHtmlPre*(text: string): HtmlElem =
  HtmlElem(kind: hekOther, tagname: "pre", elements: @[newHtmlText(text)])

func newHtmlCode*(text: string): Xml =
  newXmlTree("code", @[newXmlTree("pre", @[newText(text)])])

func newTree*(tag: string,
             subitems: openarray[HtmlElem],
             attrs: openarray[(string, string)] = @[]): HtmlElem =

  result = newElementHtml(tag)
  for it in subitems:
    result.add it

  # for attr in attrs:
  result.attrs = attrs.toXmlAttributes()

func newHtmlLink*(
    target: string,
    description: Option[seq[HtmlElem]] = none(seq[HtmlElem])
  ): HtmlElem =

  if description.isNone():
    result = newTree("a", @[], {"href" : target})

  else:
    result = newTree("a", description.get(), {"href" : target})


func newTree(subitems: seq[HtmlElem]): HtmlElem =
  HtmlElem(kind: hekElemList, elements: subitems)

func newTree*(subitems: seq[HtmlElem], tag: string): HtmlElem =
  newTree(tag, subitems)

func newElemList*(): HtmlElem = HtmlElem(kind: hekElemList, elements: @[])

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


func wrap*(xml: HtmlElem, tag: string): HtmlElem =
  result = newElementHtml(tag)
  result.add xml

func wrap*(xml: XmlNode, tag: string): XmlNode =
  result = newElement(tag)
  result.add xml


func wrap*(xml: seq[XmlNode], tag: string): XmlNode =
  result = newElement(tag)
  for node in xml:
    result.add node


func wrap*(xml: seq[HtmlElem], tag: string): HtmlElem =
  result = newElementHtml(tag)
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


macro orCond*(body: untyped): untyped =
  var conditions: seq[(int, NimNode)]

  result = newStmtList()
  for idx, element in body:
    case element.kind:
      of nnkIfStmt:
        let
          condId = ident &"cond{idx}"
          body = element[0][1]
          cond = element[0][0]

        conditions.add (idx, cond)

        result.add quote do:
          if `condId`:
            `body`
      else:
        result.add element

  var
    tmp = newStmtList()
    condVars: seq[NimNode]
  for (idx, cond) in conditions:
    let condId = ident &"cond{idx}"
    condVars.add condId
    tmp.add quote do:
      let `condId` = `cond`


  let topCond = condVars.foldl(nnkInfix.newTree(ident "or", a, b))

  result = quote:
    `tmp`
    if `topCond`:
      `result`

macro condIncl*(rtype, body: untyped): untyped =
  let resn = ident "res"
  result = newStmtList()

  for elem in body:
    if elem.kind != nnkBracket or elem.len != 2:
      raise toCodeError(
        elem, "Expected brace with two elements: `[cond, val]`")


    let
      cond = elem[0]
      rval = elem[1]

    result.add:
      quote do:
        if `cond`:
          `resn`.incl `rval`

  result = quote do:
    block:
      var `resn`: `rtype`
      `result`
      `resn`


# proc addIndent*(result: var string, indent: int, addNewLines: bool) =
#   if addNewLines:
#     result.add("\n")
#   for i in 1 .. indent:
#     result.add(' ')

template add2Aux(xmm: untyped) {.dirty.} =
  ## Adds the textual representation of `n` to string `result`.
  proc noWhitespace(n: xmm.XmlNode): bool =
    for i in 0 ..< n.len:
      if n[i].kind in {xmm.xnText, xmm.xnEntity}:
        return true

  func addEscapedAttr(result: var string, s: string) =
    # `addEscaped` alternative with less escaped characters.
    # Only to be used for escaping attribute values enclosed in double quotes!
    for c in items(s):
      case c
      of '<': result.add("&lt;")
      of '>': result.add("&gt;")
      of '&': result.add("&amp;")
      of '"': result.add("&quot;")
      else: result.add(c)

  if n == nil: return

  case n.kind
    of xmm.xnElement:
      if n.tag == xmlSeqTag:
        for node in n:
          add2(
            result, node, level, indWidth = indWidth,
            addNewlines = addNewlines,
            escape = escape
          )

        return

      let pref = "" # " ".repeat(level * indWidth)
      result.add(pref & '<' & n.tag)
      if not isNil(n.attrs):
        for key, val in pairs(n.attrs):
          result.add(' ')
          result.add(key)
          result.add("=\"")
          if escape:
            result.addEscapedAttr(val)
          else:
            result.add(val)
          result.add('"')

      if n.len == 0:
        result.add("> </" & n.tag & ">")

      else:
        result.add(">")
        if addNewlines:
          result.add("\n")

        let addNewLines = addNewlines and (not n.noWhitespace())

        for i in 0 ..< n.len:
          let escape = escape and (
            n[i].kind == xmm.xnElement and n[i].tag != "pre")

          result.add2(n[i], level + 1, indWidth, addNewlines, escape)

        result.add(pref & "</" & n.tag & ">")

      if addNewlines:
        result.add("\n")

    of xmm.xnText:
      if escape:
        xmm.addEscaped(result, n.text)
      else:
        result.add n.text

      if addNewlines:
        result.add ("\n")

    of xmm.xnComment:
      result.add("<!-- ")
      xmm.addEscaped(result, n.text)
      result.add(" -->")

    of xmm.xnCData:
      result.add("<![CDATA[")
      result.add(n.text)
      result.add("]]>")

    of xmm.xnEntity:
      result.add('&')
      result.add(n.text)
      result.add(';')

    of xmm.xnVerbatimText:
      result.add(n.text)


func add2*(result: var string, n: XmlNode, level = 0, indWidth = 2,
          addNewLines = true,
          escape: bool = true) =
  add2Aux(xmltree)

proc toPrettyStr*(n: XmlNode): string = add2(result, n, 0, 2, true)

func toXml*(html: HtmlElem): seq[XmlNode]
proc toFlatStr*(n: XmlNode, escape: bool = true): string =
  add2(result, n, 0, 0, false, escape)

proc toPrettyStr*(n: HtmlElem): string =
  n.toXml().mapIt(toPrettyStr(it)).join("\n")

proc toFlatStr*(n: HtmlElem, escape: bool = true): string =
  n.toXml().mapIt(toFlatStr(it, escape)).join("\n")


func toXml*(html: HtmlElem): seq[XmlNode] =
  var tmpres: XmlNode
  case html.kind:
    of hekTable:
      tmpres = newElement("table")
      tmpres["border"] = $html.border
    of hekRow:
      tmpres = newElement("tr")
    of hekCell:
      tmpres = newElement("td")
      if html.dotPort != 0:
        tmpres["port"] = "t" & $html.dotPort

    of hekText:
      tmpres = newText(html.textStr)
      # if html.textPre:
      #   tmpres = tmpres.wrap("pre")

      orCond:
        tmpres = tmpres.wrap("font")
        if html.textColor != colNoColor:
          tmpres["color"] = $html.textColor

        if html.textColorBg != colNoColor:
          tmpres["style"] = &"background-color:{html.textColorBg}"


      for prop in html.textProps:
        tmpres =
          case prop:
            of htpBold: tmpres.wrap("b")
            of htpUnderline: tmpres.wrap("u")
            of htpItalic: tmpres.wrap("i")
            of htpNone: tmpres
    of hekOther:
      tmpres = newElement(html.tagname)
    of hekElemList:
      return html.elements.mapIt(it.toXML()).concat()
    # of hekPre:
    #   let text = html.elements.mapIt(toFlatStr(it, false)).join("")
    #   # tmpres = newText(text).wrap("pre")
    else:
      tmpres = newElement(html.tagname)

  if html.class.len > 0:
    if tmpres.kind == xnText:
      tmpres = tmpres.wrap("font")

    tmpres["class"] = html.class.join(" ")

  if html.attrs != nil:
    var tmp = tmpres.attrs
    for key, val in pairs(html.attrs):
      tmp[key] = val

    tmpres.attrs = tmp

  for row in html.elements:
    tmpres.add row.toXml()

  return @[ tmpres ]



func `$`*(html: HtmlElem): string =
  html.toXml().mapIt($it).join("\n")

func toHtmlDoc*(html: HtmlElem): string =
  $html.toXml().wrap("body").wrap("html")


func toDocument*(html: HtmlElem, styleSection: string = ""): string =
  var buf = newElemList()
  if styleSection.len > 0:
    buf.add newHtmlText("\n" & styleSection ).wrap("style")

  buf.add html.wrap("body")
  buf.wrap("html").toPrettyStr()

func toDocument*(html: seq[HtmlElem], styleSection: string = ""): string =
  html.newTree().toDocument(styleSection)

#=======================  Colored string to html  ========================#


func toHTML*(str: ColoredString, selector: bool = true): HtmlElem =
  if selector:
    result = newElementHtml("span")
    result.class.add case str.fg:
      of fgDefault:
        @[]
      else:
        @["term-" & ($str.fg).toLowerAscii()]

    result.class.add case str.bg:
      of bgDefault:
        @[]
      else:
        @["term-" & ($str.bg).toLowerAscii()]

    result.add str.str.toHtmlText(
      props = block:
        condIncl(set[HtmlTextProp]):
          [styleBright in str.styling, htpBold]
          [styleItalic in str.styling, htpItalic]
          [styleUnderscore in str.styling, htpUnderline]
    )

  else:
    result = newHtmlText(str.str)
    result.textColor = case str.fg:
      of fgDefault: colNoColor
      of fgRed: colRed
      of fgGreen: colGreen
      else: colBlue

    result.textColorBg = case str.bg:
      of bgDefault: colNoColor
      of bgRed: colRed
      of bgGreen: colGreen
      else: colBlue

func toHtml*(strs: openarray[ColoredString],
             selector: bool = true): HtmlElem =
  result = strs.mapIt(it.toHTML(selector).toFlatStr(false)
  ).join("").newHtmlText()

func toHtml*(strs: seq[seq[ColoredString]],
             selector: bool = true): HtmlElem =

  var buf: seq[string]
  for line in strs:
    let xml = line.toHtml(selector)
    let tmp = xml.toFlatStr(false)
    buf.add tmp

  result = buf.join("\n").newHtmlPre()

template testComp(expr, asExpr: untyped): untyped =
  compiles:
    import expr as asExpr


when testComp(fusion/htmlparser/xmltree, fus_xmltree):
  static:
    echo "Can import as"

  import fusion/htmlparser/xmltree as fus_xmltree
  func add2*(
    result: var string, n: fus_xmltree.XmlNode,
    level = 0, indWidth = 2, addNewLines = true, escape: bool = true) =

    add2Aux(fus_xmltree)

  proc toPrettyStr*(n: fus_xmltree.XmlNode): string =
    add2(result, n, 0, 2, true)
