import std/[
  colors, options, strtabs, ropes, sequtils, sets,
  strutils, strformat, os, hashes, tables, sugar
]

export colors

import
  ../core/all,
  ../types/[hprimitives, colorstring],
  ../algo/halgorithm,
  ../other/hshell

import
  ./html_ast

export RawHtml

#================================  TODO  =================================#

##[

Statically typed wrapper on top of graphviz node description
format.

Large portions of documentation copied from original site and added to
relevant places.

]##


type
  DotPortPosition* = enum
    dppNone = ""
    dppTop = "n"
    dppBotton = "s"

    dppTopLeft = "nw"
    dppLeft = "w"
    dppBottomLeft = "sw"

    dppTopRight = "ne"
    dppRight = "e"
    dppBottonRight = "se"


  DotNodeId* = object
    path: seq[int]
    record: seq[int]
    port: DotPortPosition


func `hash`*(id: DotNodeId): Hash =
  !$(hash(id.path) !& hash(id.record))

func isRecord*(id: DotNodeId): bool =
  id.record.len > 0

func isEmpty*(id: DotNodeId): bool  =
  id.path.len == 0 and id.record.len == 0

func toStr*(id: DotNodeId, isRecord: bool = false): string =
  if id.isRecord or isRecord:
    result.add ""

  result.add id.path.mapIt("t" & replace($it, "-", "neg")).join("_")

  if id.record.len > 0:
    result.add ":"
    result.add id.record.mapIt("t" & replace($it, "-", "neg")).join(":")

    if id.port != dppNone:
      result.add ":" & $id.port

func `$`(id: DotNodeId): string = toStr(id)


# converter toDotNodeId*(id: int): DotNodeId =
#   ## Create single node id
#   DotNodeId(path: @[id])

converter toDotNodeId*(hash: Hash | int): DotNodeId =
  when hash is int:
    DotNodeId(path: @[hash])

  else:
    DotNodeId(path: @[hash.int])

converter toDotNodeId*(ids: seq[int]): seq[DotNodeId] =
  ## Create multiple node ids
  ids.mapIt(DotNodeId(path: @[it]))


func toDotPath*(
  top: int, sub: int, port: DotPortPosition = dppNone): DotNodeId =
  DotNodeId(path: @[top], record: @[sub], port: port)


converter toDotNodeId*(path: tuple[top, sub: int]): DotNodeId =
  toDotPath(path.top, path.sub)

converter toDotNodeId*(path: tuple[top, sub: int, port: DotPortPosition]): DotNodeId =
  toDotPath(path.top, path.sub, path.port)

func toDotPath*(
  top, sub: DotNodeId, port: DotPortPosition = dppNone): DotNodeId =
  DotNodeId(path: @[top.path[0]], record: @[sub.path[0]], port: port)

converter toDotNodeId*[T](p: ptr T): DotNodeId =
  DotNodeId(path: @[cast[int](p)])

func addIdShift*(node: DotNodeId, shift: int): DotNodeId =
  result = node
  if shift != 0:
    result.path.add shift

func addRecord*(node: DotNodeId, rec: int): DotNodeId =
  result = node
  result.record.add rec

converter toDotNodeId*(ids: seq[seq[int]]): seq[DotNodeId] =
  ## Create multile node ids for record nodes
  # debugecho ids
  # defer: debugecho result
  ids.mapIt(DotNodeId(path: it))

func quoteGraphviz*(str: string): string =
  result.add '"'
  for idx, ch in pairs(str):
    if ch in {'\'', '"'}:
      result.add "\\"

    elif ch in {'\\'} and (
      # Do not escape `\l` and `\r`
      idx < str.high and str[idx + 1] notin {'l', 'r'}):

      result.add "\\"

    result.add ch

  result.add '"'

func escapeHtmlGraphviz*(str: string): string =
  str.multiReplace({
    "]": "&#93;",
    "[": "&#91;",
    "<": "&lt;",
    ">": "&gt;"
  })



type
  DotNodeShape* = enum
    # Copied from https://graphviz.org/doc/info/shapes.html
    # (Polygon-based models) and converted using
    # `xclip -out | tr '\t' '\n' | sort | uniq | perl -pne 's/^(.*)$/nsa\u$1 = "$1"/' > /tmp/res.txt`

    nsaDefault = "" ## No shape specified
    nsaRecord = "record" ## Record node
    nsaMRecord = "Mrecord"

    nsaAssembly = "assembly"
    nsaBox = "box"
    nsaBox3d = "box3d"
    nsaCds = "cds"
    nsaCircle = "circle"
    nsaComponent = "component"
    nsaCylinder = "cylinder"
    nsaDiamond = "diamond"
    nsaDoublecircle = "doublecircle"
    nsaDoubleoctagon = "doubleoctagon"
    nsaEgg = "egg"
    nsaEllipse = "ellipse"
    nsaFivepoverhang = "fivepoverhang"
    nsaFolder = "folder"
    nsaHexagon = "hexagon"
    nsaHouse = "house"
    nsaInsulator = "insulator"
    nsaInvhouse = "invhouse"
    nsaInvtrapezium = "invtrapezium"
    nsaInvtriangle = "invtriangle"
    nsaLarrow = "larrow"
    nsaLpromoter = "lpromoter"
    nsaMcircle = "Mcircle"
    nsaMdiamond = "Mdiamond"
    nsaMsquare = "Msquare"
    nsaNone = "none"
    nsaNote = "note"
    nsaNoverhang = "noverhang"
    nsaOctagon = "octagon"
    nsaOval = "oval"
    nsaParallelogram = "parallelogram"
    nsaPentagon = "pentagon"
    nsaPlain = "plain"
    nsaPlaintext = "plaintext"
    nsaPoint = "point"
    nsaPolygon = "polygon"
    nsaPrimersite = "primersite"
    nsaPromoter = "promoter"
    nsaProteasesite = "proteasesite"
    nsaProteinstab = "proteinstab"
    nsaRarrow = "rarrow"
    nsaRect = "rect"
    nsaRectangle = "rectangle"
    nsaRestrictionsite = "restrictionsite"
    nsaRibosite = "ribosite"
    nsaRnastab = "rnastab"
    nsaRpromoter = "rpromoter"
    nsaSeptagon = "septagon"
    nsaSignature = "signature"
    nsaSquare = "square"
    nsaStar = "star"
    nsaTab = "tab"
    nsaTerminator = "terminator"
    nsaThreepoverhang = "threepoverhang"
    nsaTrapezium = "trapezium"
    nsaTriangle = "triangle"
    nsaTripleoctagon = "tripleoctagon"
    nsaUnderline = "underline"
    nsaUtr = "utr"

  DotNodeStyle* = enum
    ## The style attribute can be used to modify the appearance of a node.

    nstDefault = "" ## No explicitly specified style
    nstFilled = "filled"
    nstInvisible = "invisible"
    nstDiagonals = "diagonals"
    nstRounded = "rounded"
    nstDashed = "dashed"
    nstDotted = "dotted"
    nstSolid = "solid"
    nstBold = "bold"
    nstWedged = "wedged"
    nstStriped = "filled"

  DotNodeCommonStyle = enum
    ## Commonly encountered node stules
    ncsDfaState ## DFA/NFA intermediate state. Single circle node.
    ncsDfaAccept ## DFA/NFA accept state. Double circle node.
    ncsAstTerminal ## AST tree terminal node. First line italic, second
                   ## quoted bold. Light gray background, square node.
    ncsAstNonTerminal ## AST tree nonterminal (intermediate) node. White
                      ## background, square node.

  DotNodeLabelAlign* = enum
    nlaDefault = "\\n"

    nlaLeft = "\\l"
    nlaCenter = "\\n"
    nlaRight = "\\r"

type # Enumerations for arrows
  ArrowShape = enum
    ## https://graphviz.org/doc/info/arrows.html
    ashDefault = "" ## Shape not explicitly specified
    ashBox = "box"
    ashCrow = "crow"
    ashCurve = "curve"
    ashIcurve = "icurve"
    ashDiamond = "diamond"
    ashDot = "dot"
    ashInv = "inv"
    ashNone = "none"
    ashNormal = "normal"
    ashTee = "tee"
    ashVee = "vee"

  ArrowShapeModifier = enum
    asmNone = ""
    asmLeftClip = "l" ## Clip the shape, leaving only the part to the
                      ## left of the edge.
    asmRightClip = "r" ## Clip the shape, leaving only the part to the
                       ## right of the edge.

  ArrowDirType = enum
    dtDefault = "" ## Direction not explicitly specified
    dtForward = "forward"
    dtBack = "back"
    dtBoth = "both"
    dtNone = "none"

type
  DotEdgeStyle* = enum
    edsDefault = ""
    edsSolid = "solid"
    edsDotted = "dotted"
    edsDashed = "dashed"
    edsBold = "bold"
    edsInvis = "invis"
    edsTapered = "tapered" # TODO NOTE

  DotSplineStyle* = enum
    spsDefault = ""
    spsOrtho = "ortho"
    spsNone = "none"
    spsLine = "line"
    spsPolyline = "polyline"
    spsCurved = "curved"
    spsSplines = "spline"

type
  ClusterStyles* = enum
    clsDefault = ""
    clsSolid = "solid"
    clsDashed = "dashed"
    clsStriped = "striped"
    clsDotted = "dotted"
    clsBold = "bold"
    clsRounded = "rounded"
    clsFilled = "filled"


type
  RecordField* = object
    id*: DotNodeId ## Record field id
    text*: string ## Text in record field
    # REVIEW allow use of html directly?
    vertical*: bool ## Orientation direction
    subfields*: seq[RecordField]

  DotNode* = object
    ##[

## Fields

:colorListp: Type of color filling

   If true - use color list: sequence of weighted color values. Total
   `weight` should summ up to `1`. Each value is in range [0; 1]. If
   no values specified gradient will be soft. If the colorList value
   specifies multiple colors, with no weights, and a filled style is
   specified, a linear gradient fill is done using the first two
   colors. If weights are present, a degenerate linear gradient fill
   is done. This essentially does a fill using two colors, with the
   weights specifying how much of region is filled with each color.

    ]##
    id*: DotNodeId
    width*: float
    height*: float
    fontname*: string
    penwidth*: Option[float]
    case style*: DotNodeStyle
      # NOTE not clear what happens with 'filled' node that uses color
      # list
      of nstStriped, nstWedged:
        colorList*: seq[tuple[
          color: Color,
          weight: Option[float]
        ]] ## Weighted color values
        gradientAngle*: int
        isRadial*: bool ## Two fill styles: linear and radial.
      else:
        color*: Option[Color] ## DotNode color
        fillcolor*: Option[Color]

    labelAlign*: DotNodeLabelAlign
    case shape*: DotNodeShape
      of nsaRecord, nsaMRecord:
        # NOTE top-level record is always horizontal (?)
        flds*: seq[RecordField]

      of nsaPlaintext:
        htmlLabel*: HtmlElem

      else:
        labelLeftPad*: string
        label*: Option[string] ## DotNode label

type
  Arrow* = object
    ##[

## Fields

:shapes: Arrow shapes

  Multiple arrow shapes can be used for a single node. Subsequent
  arrow shapes, if specified, occur further from the node.

  :shape: Left/right or non-clipped
  :isOpen: Filled or not filled

    ]##

    shapes*: seq[
      tuple[
        modifs: ArrowShapeModifier,
        shape: ArrowShape,
        isOpen: bool
      ]
    ] ## Arrow shapes [1]

type
  DotEdge* = object
    lhead*: Option[string]
    rhead*: Option[string]
    ltail*: Option[string]
    rtail*: Option[string]

    fontname*: string
    style*: DotEdgeStyle
    arrowSpec*: Arrow
    src*: DotNodeId
    to*: seq[DotNodeId]
    color*: Color
    weight*: Option[float]
    minlen*: Option[float]
    label*: Option[string]

type
  DotGraphPresets* = enum
    dgpAutomata
    dgpAutomataAccept
    dgpRecords

  DotGraphNodeRank* = enum
    gnrDefault = ""
    gnrSame = "same"

  DotGraphRankDir* = enum
    grdDefault = ""
    grdTopBottom = "TB"
    grdLeftRight = "LR"

  DotGraph* = object
    # spline*: SplineStyle
    compound*: Option[bool] ## If true, allow edges between clusters. Must
                            ## be specified if `lhead` or `ltais` is used
    rankdir*: DotGraphRankDir
    styleNode*: DotNode
    styleEdge*: DotEdge
    topNodes*: seq[DotNode]
    attrs*: Table[string, string]

    ranksep*: Option[float]
    noderank*: DotGraphNodeRank
    isUndirected*: bool

    # Cluster graph
    name*: string
    isCluster*: bool
    isWrapper*: bool
    idshift*: int

    label*: string
    labelOnBottom*: bool
    fontsize*: int
    fontcolor*: Color
    fontname*: string
    color*: Option[Color]
    splines*: DotSplineStyle

    subgraphs*: seq[DotGraph]
    nodes*: seq[DotNode]
    edges*: seq[DotEdge]
    recordIds*: HashSet[DotNodeId]
    bgColor*: Option[Color]

#============================  constructors  =============================#
func initDotNode*(): DotNode = DotNode(style: nstDefault)

func `[]=`*(graph: var DotGraph, key, value: string) =
  graph.attrs[key] = value

func makeRectConsolasNode*(): DotNode =
  DotNode(shape: nsaRect, fontname: "Consolas", labelAlign: nlaLeft)
  # result.fontname = "Consolas"
  # result.shape = nsaRect
  # result.labelAlign = nlaLeft

func makeRectConsolasEdge*(): DOtEdge =
  result.fontname = "Consolas"

func makeCircleConsolasNode*(): DotNode =
  DotNode(shape: nsaCircle, fontname: "Consolas")
  # result.fontname = "Consolas"
  # result.shape = nsaCircle

func makeDotNode*(style: DotGraphPresets): DotNode =
  case style:
    of dgpRecords:
      discard

    of dgpAutomata:
      result = DotNode(shape: nsaCircle)

    of dgpAutomataAccept:
      result = DotNode(shape: nsaDoubleCircle)

  result.fontname = "Consolas"

func makeDotEdge*(style: DotGraphPresets): DotEdge =
  result.fontname = "Consolas"
  case style:
    else:
      discard

func makeDotEdge*(style: DotEdgeStyle): DotEdge =
  result = DotEdge(style: style)
  result.fontname = "Consolas"


func setProps*(node: sink DotNode, id: DotNodeId, label: string): DotNode =
  result = node
  result.id = id
  result.label = some(label)

func makeDotGraph*(
    name: string = "G",
    nodes: seq[DotNode] = @[],
    edges: seq[DotEdge] = @[],
    styleNode: DotNode = makeRectConsolasNode(),
    styleEdge: DotEdge = makeRectConsolasEdge()
  ): DotGraph =

  DotGraph(
    name: name,
    nodes: nodes,
    edges: edges,
    styleNode: styleNode,
    styleEdge: styleEdge,
    fontname: "Consolas"
  )

func makeDotGraph*(style: DotGraphPresets, name: string = "G"): DotGraph =
  result = DotGraph(
    name: name,
    styleNode: makeDotNode(style),
    styleEdge: makeDotEdge(style)
  )

  case style:
    of dgpAutomata:
      result.rankdir = grdLeftRight

    of dgpRecords:
      result.rankdir = grdLeftRight

    else:
      discard


func add*(graph: var DotGraph, node: DotNode): void =
  graph.nodes.add node
  if node.shape in {nsaPlaintext}:
    graph.recordIds.incl node.id


func add*(graph: var DotGraph, sub: DotGraph): void =
  var subg = sub
  subg.isCluster = true
  # if subg.name.len == 0:
  #   subg.name = &"cluster_{graph.subgraphs.len}{graph.name}"

  # else:
  #   if not subg.name.startsWith("cluster_"):
  #     subg.name = "cluster_" & subg.name

  graph.subgraphs.add subg

func addSubgraph*(graph: var DotGraph, subg: DotGraph)
    {.deprecated: "Use `HGraph.add` instead".} =

  graph.add subg

func add*(graph: var DotGraph, edge: DotEdge): void =
  var edge = edge
  graph.edges.add edge

func addEdge*(graph: var DotGraph, edge: DotEdge): void {.deprecated.} =
  graph.add edge

func addNode*(graph: var DotGraph, node: DotNode): void {.deprecated.} =
  graph.add node


func makeDotEdge*(idFrom, idTo: DotNodeId): DotEdge =
  DotEdge(src: idFrom, to: @[idTo])

func makeDotEdge*(idFrom, idTo: DotNodeId, label: string): DotEdge =
  result = DotEdge(src: idFrom, to: @[idTo], label: some(label))

func makeAuxEdge*(idFrom, idTo: DotNodeId): DotEdge =
  DotEdge(src: idFrom, to: @[idTo], weight: some(0.0), style: edsInvis)

func applyStyle*(to: var DotNode, source: DotNode): void =
  if source.shape != nsaDefault:
    to.shape = source.shape

  if source.fontname != "":
    to.fontname = source.fontname

func makeConstraintEdge*(idFrom, idTo: DotNodeId): DotEdge =
  DotEdge(
    src: idFrom,
    to: @[idTo],
    weight: some(1000.0),
    style: edsInvis
    # minlen: some(0.0)
  )



func makeDotNode*(
    id: DotNodeId,
    label: string,
    shape: DotNodeShape = nsaDefault,
    color: Color = colNoColor,
    style: DotNodeStyle = nstDefault,
    width: float = -1.0,
    height: float = -1.0,
    fillcolor: Color = colNoColor
  ): DotNode =

  if fillcolor != colNoColor and shape == nsaDefault:
    result = DotNode(shape: shape, style: nstFilled)

  else:
    result = DotNode(shape: shape, style: style)

  result.id = id
  result.label = some(label)
  if style notin {nstStriped, nstWedged}:
    if color != colNoColor:
      result.color = some(color)

    if fillcolor != colNoColor:
      result.fillcolor = some(fillcolor)



func makeDotNode*(id: DotNodeId, text: RawHtml): DotNode =
  DotNode(id: id, shape: nsaPlaintext, htmlLabel: newHtmlRaw(text))

func makeDotNode*(id: DotNodeId, html: HtmlElem): DotNode =
  DotNode(id: id, shape: nsaPlaintext, htmlLabel: html)

func add*(
    node: var DotNode, other: DotNode,
    attrs: openarray[(string, string)] = @[]
  ) =
  when not defined(nimdoc):
    var attrs = toSeq(attrs)
    attrs.add ("port", $other.id)
    node.htmlLabel.add newTree(
      "tr", @[newTree("td", @[other.htmlLabel], attrs)])

const defaultDotBackgroundMap*: array[BackgroundColor, Color] =
  block:
    var res: array[BackgroundColor, Color]

    res[bgDefault] = colNoColor
    res[bgRed]     = Color(0x964C7B) # colRed
    res[bgGreen]   = Color(0x74DFC4) # colGreen
    res[bgCyan]    = Color(0x6D7E8A) # colCyan
    res[bgMagenta] = Color(0xB381C5) # colMagenta
    res[bgYellow]  = Color(0xFFE261) # colYellow
    res[bgBlack]   = colBlack
    res[bgWhite]   = colWhite
    res[bgBlue]    = Color(0x336A79) # colBlue

    res

const defaultDotForegroundMap*: array[ForegroundColor, Color] =
  block:
    var res: array[ForegroundColor, Color]

    res[fgDefault] = colNoColor
    res[fgRed]     = Color(0x964C7B) # colRed
    res[fgGreen]   = Color(0x74DFC4) # colGreen
    res[fgCyan]    = Color(0x6D7E8A) # colCyan
    res[fgMagenta] = Color(0xB381C5) # colMagenta
    res[fgYellow]  = Color(0xFFE261) # colYellow
    res[fgBlack]   = colBlack
    res[fgWhite]   = colWhite
    res[fgBlue]    = Color(0x336A79) # colBlue

    res


const lightDotForegroundMap*: array[ForegroundColor, Color] = toMapArray({
  fgRed: Color(0xEA9999),
  fgYellow: Color(0xFFE599),
  fgGreen: Color(0xB6D7A8),
  fgCyan: Color(0xA4C2F4),
  fgBlue: Color(0x9FC5E8),
  fgMagenta: Color(0xD5A6BD),
  fgDefault: colNoColor,
  fgBlack: colBlack,
  fgWhite: colWhite
})


const brightDotForegroundMap*: array[ForegroundColor, Color] = toMapArray({
  fgRed: Color(0xff0000),
  fgYellow: Color(0xffff00),
  fgGreen: Color(0x00ff00),
  fgCyan: Color(0x00ffff),
  fgBlue: Color(0x0000ff),
  fgMagenta: Color(0xff00ff),
  fgDefault: colNoColor,
  fgBlack: colBlack,
  fgWhite: colWhite
})

func dotColor*(
    col: ForegroundColor,
    map: array[ForegroundColor, Color] = defaultDotForegroundMap): Color =
  map[col]

func makeHtmlDotNodeContents*(
    cellItems: seq[HtmlElem],
    tableAttrs: openarray[(string, string)] = {"border": "1"},
    cellAttrs: openarray[(string, string)] = {"balign": "left", "border": "0"}):
  HtmlElem =

    newTree("table", @[
        newTree("tr", @[
          newTree("td", cellItems, cellAttrs)
        ]),
      ], tableAttrs)

func makeDotNode*(
    id: DotNodeId, label: string,
    fg: ForegroundColor, bg: BackgroundColor,
    bgColorMap: array[BackgroundColor, Color] = defaultDotBackgroundMap,
    fgColorMap: array[ForegroundColor, Color] = defaultDotForegroundMap
  ): DotNode =

  result = DotNode(id: id, shape: nsaPlaintext)
  var label = newHtmlText(escapeHtmlGraphviz(label))

  label.textColor = fgColorMap[fg]
  # label.textColorBg =
  result.htmlLabel = makeHtmlDotNodeContents(@[label])
  result.htmlLabel["bgcolor"] = $bgColorMap[bg]


func makeDotRecord*(
    id: DotNodeId, text: string, subfields: seq[RecordField] = @[]
  ): RecordField =

  RecordField(id: id, text: text, subfields: subfields)

func makeRecordDotNode*(id: DotNodeId, records: seq[RecordField]): DotNode =
  DotNode(shape: nsaRecord, id: id, flds: records, labelAlign: nlaLeft)

func add*(node: var DotNode, field: RecordField) =
  node.flds.add field

func makeColoredDotNode*(
    id: DotNodeId, label: string,
    tableAttrs: openarray[(string, string)] = {"border": "1"},
    cellAttrs: openarray[(string, string)] = {"balign": "left", "border": "0"},
    style: DotNodeStyle = nstDefault,
  ): DotNode =
  ## Create graphviz with colored note text. `label` is allowed to contain
  ## terminal ANSI SGR control codes like `\e[32`

  var escaped: seq[HtmlElem]
  var split = label.splitSGR_sep()
  for chunk in mitems(split):
    for elem in mitems(chunk):
      elem.str = escapeHtmlGraphviz(elem.str)
      escaped.add elem.toHtml(selector = false)

    escaped.add newTree("br", @[])

  DotNode(
    id: id,
    shape: nsaPlaintext,
    htmlLabel: makeHtmlDotNodeContents(escaped, tableAttrs, cellAttrs)
  )


func makeTableDotNode*(
    id: DotNodeId, label: RawHtml | string,
    tableAttrs: openarray[(string, string)] = {"border": "1"},
    cellAttrs: openarray[(string, string)] = {"balign": "left", "border": "0"},
    style: DotNodeStyle = nstDefault,
  ): DotNode =

  DotNode(
    id: id,
    shape: nsaPlaintext,
    htmlLabel: makeHtmlDotNodeContents(
      @[newHtmlRaw(label)], tableAttrs, cellAttrs)
  )

type
  DotTreeKind = enum
    dtkNodeDef
    dtkSubgraph
    dtkEdgeDef
    dtkProperty
    dtkComment

  DotTree = ref object
    # IR for conversion to string
    case kind: DotTreeKind
      of dtkNodeDef:
        nodeId: string
        nodeAttributes: StringTableRef
      of dtkEdgeDef:
        origin: DotNodeId
        targets: seq[DotNodeId]
        edgeAttributes: StringTableRef
      of dtkProperty:
        key, val: string
        globalProp: bool
      of dtkSubgraph:
        section: seq[string]
        elements: seq[DotTree]
      of dtkComment:
        text: string

func toString(record: RecordField, align: DotNodeLabelAlign): string =
  # TODO keep track of graph direction to ensure correct rotation
  if record.subfields.len > 0:
    "{" & record.subfields.mapIt(
      toString(it, align)).join("|") & "}"

  else:
    let text = record.text.split({'\n'}).join($align) & $align
    &"<{record.id}>{text}"

func toTree(node: DotNode, idshift: int, level: int = 0): DotTree =
  var attr = newStringTable()
  result = DotTree(kind: dtkNodeDef)
  result.nodeId = node.id.addIdShift(idshift).toStr(
   node.shape in {nsaPlaintext})

  if node.width > 0: attr["width"] = $node.width
  if node.height > 0: attr["height"] = $node.height
  if node.fontname.len > 0: attr["fontname"] = node.fontname.quoteGraphviz()
  if node.penwidth.isSome(): attr["penwidth"] = $node.penWidth.get()

  case node.style:
    of nstStriped, nstWedged:
      if node.isRadial:
        attr["style"] = "radial"

      attr["colorList"] = node.colorList.mapIt(
        $it.color & (it.weight.isSome()).tern(";" & $it.weight.get(), "")
      ).join(":")
    else:
      if node.style != nstDefault:
        attr["style"] = $node.style

      if node.shape != nsaPlaintext:
        if node.color.isSome():
          attr["color"] = ($node.color.get()).quoteGraphviz()

        if node.fillcolor.isSome():
          attr["fillcolor"] = ($node.fillcolor.get()).quoteGraphviz()

  case node.shape:
    of nsaRecord, nsaMRecord:
      attr["label"] = quoteGraphviz(
        node.flds.mapIt(toString(it, node.labelAlign)).join("|"))

    of nsaPlaintext:
      attr["label"] = " <\n" & (node.htmlLabel.toFlatStr() & "> ").
        split("\n").
        mapIt("  ".repeat(level + 1) & it).
        joinl()

    else:
      if node.label.isSome():
        if node.labelAlign != nlaDefault:
          let str = node.label.get().
            split("\n").
            mapIt(node.labelLeftPad & it).
            join($node.labelAlign)

          attr["label"] = quoteGraphviz(
            case node.labelAlign:
              of nlaLeft, nlaRight: str & $node.labelAlign
              else: str
          )

        else:
          if node.labelLeftPad.len > 0:
            attr["label"] = node.label.get().
              split("\n").
              mapIt(node.labelLeftPad & it).
              join("\n").
              quoteGraphviz()
          else:
            attr["label"] = node.label.get().quoteGraphviz()

  if node.shape != nsaDefault: attr["shape"] = $node.shape

  result.nodeAttributes = attr

func setSome[T](
  table: StringTableRef,
  name: string, val: Option[T]): void =
  if val.isSome():
    table[name] = $val.get

func toTree(edge: DotEdge, idshift: int, level: int = 0): DotTree =
  result = DotTree(kind: dtkEdgeDef)
  var attrs = newStringTable()

  if edge.color != colBlack:
    # HACK black color is omitted unconditionally. need to IMPLEMENT
    # check whether or not this is allowed.
    attrs["color"] = ($edge.color).quoteGraphviz()

  attrs.setSome("minlen", edge.minlen)
  if edge.weight.isSome(): attrs["weight"] = ($edge.weight.get())
  if edge.label.isSome(): attrs["label"] = edge.label.get().quoteGraphviz()
  if edge.style != edsDefault: attrs["style"] = ($edge.style)
  if edge.fontname.len > 0: attrs["fontname"] = edge.fontname.quoteGraphviz()
  if edge.lhead.isSome(): attrs["lhead"] = "cluster_" & edge.lhead.get()
  if edge.rhead.isSome(): attrs["rhead"] = "cluster_" & edge.rhead.get()
  if edge.ltail.isSome(): attrs["ltail"] = "cluster_" & edge.ltail.get()
  if edge.rtail.isSome(): attrs["rtail"] = "cluster_" & edge.rtail.get()

  result.origin = edge.src.addIdShift(idshift)
  result.targets = edge.to.mapIt(it.addIdShift(idshift))

  result.edgeAttributes = attrs

func toTreeComment(itms: varargs[string, `$`]): DotTree =
  DotTree(kind: dtkComment, text: itms.join(" "))

func toTree(attrs: StringTableRef): seq[DotTree] =
  for key, val in attrs:
    result.add DotTree(kind: dtkProperty, key: key, val: val)

func toTree(
    graph: DotGraph, idshift: int,
    level: int = 0,
    clusterIdx: int = 0
  ): DotTree =
  result = DotTree(kind: dtkSubgraph)
  var attrs = newStringTable()

  if graph.attrs.len > 0:
    result.elements.add DotTree(
      kind: dtkProperty, globalProp: true,
      key: "graph",
      val: collect(newSeq, for lhs, rhs in graph.attrs: &"{lhs}={rhs}").join(", ")
    )

  if level == 0:
    if graph.isUndirected:
      result.section.add "graph"
    else:
      result.section.add "digraph"

    result.section.add graph.name
  else:
    result.section.add "subgraph"

  if graph.isCluster:
    result.section.add &"cluster_{graph.name}_{level}_{clusterIdx}"

  if graph.ranksep.isSome(): attrs["ranksep"] = $graph.ranksep.get()
  if graph.splines != spsDefault: attrs["splines"] = $graph.splines
  if graph.noderank != gnrDefault: attrs["rank"] = $graph.noderank
  if graph.rankdir != grdDefault: attrs["rankdir"] = $graph.rankdir
  if graph.label.len > 0: attrs["label"] = quoteGraphviz(graph.label)
  if graph.color.isSome(): attrs["color"] = &"\"{$graph.color.get()}\""
  if graph.fontname.len > 0: attrs["fontname"] = graph.fontname
  if graph.compound.isSome(): attrs["compound"] = $graph.compound.get()
  if graph.bgColor.isSome(): attrs["bgcolor"] = &"\"{graph.bgcolor.get()}\""

  result.elements &= toTree(attrs)
  block:
    let styleNode = graph.styleNode.toTree(idshift, level + 1)
    if styleNode.nodeAttributes.len > 0:
      result.elements.add DotTree(
        kind: dtkProperty, globalProp: true,
        key: "node",
        val: collect(newSeq,
          for lhs, rhs in (styleNode.nodeAttributes):
            &"{lhs}={rhs}").join(", "))

  block:
    let styleEdge = graph.styleEdge.toTree(idshift, level + 1)
    if styleEdge.edgeAttributes.len > 0:
      when not defined(nimdoc):
        result.elements.add DotTree(
          kind: dtkProperty, globalProp: true,
          key: "edge",
          val: collect(newSeq,
            for lhs, rhs in styleEdge.edgeAttributes:
              &"{lhs}={rhs}").join(", "))

  if graph.topNodes.len > 0:
    var nodeIds: seq[DotNodeId]
    for node in graph.topNodes:
      let tree = node.toTree(idshift, level + 1)
      result.elements.add tree
      nodeIds.add node.id

    block:
      var prevId: DotNodeId = nodeIds[0]
      for nodeId in nodeIds[1..^1]:
        result.elements.add toTreeComment("Constraint edge idshift:", idshift)

        # debugecho "-----"
        # debugecho makeConstraintEdge(prevId, nodeId).toTree(0, level + 1)[]
        # debugecho makeConstraintEdge(prevId, nodeId).toTree(idshift, level + 1)[]
        result.elements.add makeConstraintEdge(
          prevId, nodeId).toTree(idshift, level + 1)

        prevId = nodeId

    if graph.nodes.len > 0:
      result.elements.add toTreeComment("Link from constraint id node")
      result.elements.add:
        makeAuxEdge(nodeIds[^1], graph.nodes[0].id).toTree(idshift, level + 1)

    # Edge(
    #     src: nodeIds[^1].id,
    #     to: @[graph.nodes[0].id],
    #     weight: some(0.0),
    #     style: edsInvis
    #   ).toTree(level + 1)


  # debugecho idshift
  result.elements.add graph.nodes.mapIt(toTree(it, idshift, level + 1))
  result.elements.add graph.edges.mapIt(toTree(it, idshift, level + 1))
  for idx, sub in graph.subgraphs:
    result.elements.add toTree(
      sub,
      idshift = (sub.idshift != 0).tern(sub.idshift, graph.idshift),
      level = level + 1,
      clusterIdx = idx
    )
  # result.elements.add graph.subgraphs.mapIt(
  #   toTree(it,
  #          ,
  #          level = level + 1))



proc join(ropes: openarray[Rope], sep: string = " "): Rope =
  new(result)
  for idx, rope in ropes:
    if idx != 0:
      result.add sep

    result.add rope

proc toRope(tree: DotTree, level: int = 0): Rope =
  let pref = "  ".repeat(level)
  case tree.kind:
    of dtkComment:
      rope(pref & "//" & tree.text)
    of dtkSubgraph:
      pref & tree.section.join(" ") & " {\n" &
        tree.elements.mapIt(toRope(it, level + 1)).join("\n") &
      "\n" & pref & "}"
    of dtkProperty:
      if tree.globalProp:
        rope(&"{pref}{tree.key}[{tree.val}];")
      else:
        rope(&"{pref}{tree.key} = {tree.val};")
    of dtkNodeDef:
      let attrs = collect(newSeq, for l, r in tree.nodeAttributes: &"{l}={r}").join(", ")
      # mapPairs(&"{lhs}={rhs}").join(", ")
      if attrs.len == 0:
        rope(&"{pref}{tree.nodeId};")
      else:
        rope(&"{pref}{tree.nodeId}[{attrs}];")
    of dtkEdgeDef:
      let attrs = collect(newSeq, for l, r in tree.edgeAttributes: &"{l}={r}").join(", ")
      if tree.targets.anyOfIt(it.isRecord):
        var res: Rope
        # TODO Generate muliple edegs for record types, one edge per target
        # TODO test of thsi works on graphviz first
        for to in tree.targets:
          if attrs.len == 0:
            res.add rope(&"{pref}{tree.origin} -> {to};\n")
          else:
            res.add rope(&"{pref}{tree.origin} -> {to}[{attrs}];\n")
          # res.add &"{pref}{tree.origin} -> {to};\n"

        res
      else:
        let rhs =
          if tree.targets.len == 1:
            $tree.targets[0]
          else:
            tree.targets.mapIt($it).join(", ").wrap(("{", "}"))

        if attrs.len == 0:
          rope(&"{pref}{tree.origin} -> {rhs};")
        else:
          rope(&"{pref}{tree.origin} -> {rhs}[{attrs}];")

proc `$`*(graph: DotGraph): string =
  $graph.toTree(graph.idshift).toRope()

#===============================  testing  ===============================#

# let res = DotGraph(
#   name: "G",
#   nodes: @[
#     DotNode(id: 12),
#     DotNode(id: 25),
#     DotNode(id: 23),
#     DotNode(
#       id: 77,
#       shape: nsaRecord,
#       flds: @[
#         RecordField(id: 8, text: "Hello"),
#         RecordField(id: 9, text: "world"),
#         RecordField(id: 10, text: "world")
#       ]
#     )
#   ],
#   edges: @[
#     DotEdge(src: 12, to: @[23, 25]),
#     DotEdge(src: 999, to: @[@[77, 8], @[77, 9], @[77, 10], @[25]]),
#     DotEdge(src: 999, to: @[77, 12], color: colGreen)
#   ],
#   subgraphs: @[
#     DotGraph(
#       name: "ZZ",
#       idshift: 8880000,
#       isCluster: true,
#       nodes: @[
#         DotNode(id: 999)
#       ]
#     )
#   ]
# )

# echo $res

# {.define(shellThrowException).}
# import shell

# FIXME use `AbsFile` etc.

import hmisc/other/oswrap
export oswrap

proc toPng*(
    graph: DotGraph,
    resfile: AbsFile,
    resolution: int = 300,
    tmpfile: AbsFile = getAppTempFile("dotTmpFile.dot"),
    tmpimage: AbsFile = getAPpTempFile("dotTmpImage.png")
  ): void =
  ## Generate file from graph
  assertExists resFile.dir()
  tmpfile.writeFile($graph)

  try:
    when not defined(nimdoc):
      discard runShell makeX11ShellCmd("dot").withIt do:
        it - ("T", "", "png")
        it - ("o", "", $tmpimage)
        it.arg tmpfile


    cpFile tmpimage, resfile
  except ShellError:
    printShellError()
    raise


proc toXDot*(
    graph: DotGraph,
    resfile: AbsFile,
    tmpfile: AbsFile = AbsFile "/tmp/dot-file.dot"
  ): void =
  ## Generate file from graph

  tmpfile.writeFile($graph)

  when not defined(nimdoc):
    discard runShell makeGnuShellCmd("dot").withIt do:
      it - ("T", "", "xdot")
      it - ("o", "", $resfile)
      it.arg tmpfile

proc toPng*(
    graph: DotGraph,
    resfile: string,
    resolution: int = 300,
    tmpfile: string = "/tmp/dot-file.dot",
    tmpimage: string =  "/tmp/dot-image-tmp.png"
  ): void {.deprecated: "Use overload with `AbsFile`".} =

  toPng(graph, AbsFile resFile, resolution,
        AbsFile tmpFile, AbsFile tmpImage)
