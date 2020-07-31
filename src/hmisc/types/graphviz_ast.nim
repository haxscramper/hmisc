import colors, options, strtabs, ropes, sequtils, strutils, strformat
import ../helpers
# import ../halgorithm

import html_ast, hprimitives

#================================  TODO  =================================#
#[

REVIEW use `DotGraph` instead of `Graph`

]#


##[
Statically typed wrapper on top of graphviz node description
format.

Large portions of documentation copied from original site and added to
relevant places.

]##


type
  NodeId* = object
    path: seq[int]
    record: seq[int]

func isRecord*(id: NodeId): bool =
  id.record.len > 0



func toStr*(id: NodeId, isRecord: bool = false): string =
  (id.isRecord or isRecord).tern("struct", "") &
  id.path.mapIt("t" & $it).join("_") &
    (id.record.len > 0).tern(
      ":" & id.record.mapIt("t" & $it).join(":"), "")

func `$`(id: NodeId): string = toStr(id)


converter toNodeId*(id: int): NodeId =
  ## Create single node id
  NodeId(path: @[id])

converter toNodeId*(ids: seq[int]): seq[NodeId] =
  ## Create multiple node ids
  ids.mapIt(NodeId(path: @[it]))

converter toNodeId*[T](p: ptr T): NodeId =
  NodeId(path: @[cast[int](p)])

func addIdShift*(node: NodeId, shift: int): NodeId =
  result = node
  if shift != 0:
    result.path.add shift

func addRecord*(node: NodeId, rec: int): NodeId =
  result = node
  result.record.add rec

converter toNodeId*(ids: seq[seq[int]]): seq[NodeId] =
  ## Create multile node ids for record nodes
  # debugecho ids
  # defer: debugecho result
  ids.mapIt(NodeId(path: it))

func quote*(input: string): string =
  input.multiReplace([
    ("\"", "\\\"")
  ]).wrap(("\"", "\""))





type
  NodeShape* = enum
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

  NodeStyle* = enum
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

  NodeLabelAlign* = enum
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
  EdgeStyle = enum
    edsDefault = ""
    edsSold = "solid"
    edsDotted = "dotted"
    edsDashed = "dashed"
    edsBold = "bold"
    edsInvis = "invis"
    edsTapered = "tapered" # TODO NOTE

  SplineStyle = enum
    spsDefault = ""
    spsOrtho = "ortho"
    spsNone = "none"
    spsLine = "line"
    spsPolyline = "polyline"
    spsCurved = "curved"
    spsSplines = "spline"

type
  ClusterStyles = enum
    clsDefault = ""
    clsSolid = "solid"
    clsDashed = "dashed"
    clsStriped = "striped"
    clsDotted = "dotted"
    clsBold = "bold"
    clsRounded = "rounded"
    clsFilled = "filled"


type
  RecordField = object
    id*: NodeId ## Record field id
    text*: string ## Text in record field
    # REVIEW allow use of html directly?
    vertical*: bool ## Orientation direction
    subfields*: seq[RecordField]

  Node* = object
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
    id*: NodeId
    width*: float
    height*: float
    fontname*: string
    case style: NodeStyle
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
        color*: Color ## Node color

    case shape*: NodeShape
      of nsaRecord, nsaMRecord:
        # NOTE top-level record is always horizontal (?)
        flds*: seq[RecordField]
      of nsaPlaintext:
        htmlLabel*: HtmlElem
      else:
        labelLeftPad*: string
        labelAlign*: NodeLabelAlign
        label*: Option[string] ## Node label

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
  Edge* = object
    style*: EdgeStyle
    spline*: SplineStyle
    arrowSpec*: Arrow
    src*: NodeId
    to*: seq[NodeId]
    color*: Color
    weight*: Option[float]
    minlen*: Option[float]

type
  GraphNodeRank* = enum
    gnrDefault = ""
    gnrSame = "same"

  Graph* = object
    styleNode*: Node
    styleEdge*: Edge
    topNodes*: seq[Node]

    noderank*: GraphNodeRank
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
    splines*: SplineStyle

    subgraphs*: seq[Graph]
    nodes*: seq[Node]
    edges*: seq[Edge]

#============================  constructors  =============================#
func initNode*(): Node =
  Node(color: colNoColor, style: nstDefault)

func makeDotGraph*(name: string = "G",
                   nodes: seq[Node] = @[],
                   edges: seq[Edge] = @[]): Graph =
  Graph(nodes: nodes,
        edges: edges,
        styleNode: initNode())

func addEdge*(graph: var Graph, edge: Edge): void =
  graph.edges.add edge

func addNode*(graph: var Graph, node: Node): void =
  graph.nodes.add node

func makeEdge*(idFrom, idTo: NodeId): Edge =
  Edge(src: idFrom, to: @[idTo])

func makeAuxEdge*(idFrom, idTo: NodeId): Edge =
  Edge(src: idFrom, to: @[idTo], weight: some(0.0), style: edsInvis)


func makeConstraintEdge*(idFrom, idTo: NodeId): Edge =
  Edge(
    src: idFrom,
    to: @[idTo],
    weight: some(1000.0),
    style: edsInvis
    # minlen: some(0.0)
  )

func addSubgraph*(graph: var Graph, subg: Graph): void =
  graph.subgraphs.add subg



func makeNode*(
  id: NodeId,
  label: string,
  shape: NodeShape = nsaDefault,
  color: Color = colNoColor,
  style: NodeStyle = nstDefault,
  width: float = -1.0,
  height: float = -1.0): Node =
  result = Node(shape: shape, style: style)
  result.id = id
  result.label = some(label)
  result.color = color

func makeNode*(id: NodeId, html: HtmlElem): Node =
  Node(id: id, shape: nsaPlaintext, htmlLabel: html)


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
        origin: NodeId
        targets: seq[NodeId]
        edgeAttributes: StringTableRef
      of dtkProperty:
        key, val: string
        globalProp: bool
      of dtkSubgraph:
        section: seq[string]
        elements: seq[DotTree]
      of dtkComment:
        text: string

func toString(record: RecordField): string =
  # TODO keep track of graph direction to ensure correct rotation
  &"<{record.id}>{record.text}"

func toTree(node: Node, idshift: int, level: int = 0): DotTree =
  var attr = newStringTable()
  result = DotTree(kind: dtkNodeDef)
  result.nodeId = node.id.addIdShift(idshift).toStr(
   node.shape in {nsaPlaintext})

  if node.width > 0: attr["width"] = $node.width
  if node.height > 0: attr["height"] = $node.height
  if node.fontname.len > 0: attr["fontname"] = node.fontname.quote()

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
        if node.color != colNoColor:
          attr["color"] = ($node.color).quote()

  case node.shape:
    of nsaRecord, nsaMRecord:
      attr["label"] = node.flds.mapIt(toString(it)).join("|").quote()
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

          attr["label"] = quote(
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
              quote()
          else:
            attr["label"] = node.label.get().quote()

  if node.shape != nsaDefault: attr["shape"] = $node.shape

  result.nodeAttributes = attr

func setSome[T](
  table: StringTableRef,
  name: string, val: Option[T]): void =
  if val.isSome():
    table[name] = $val.get

func toTree(edge: Edge, idshift: int, level: int = 0): DotTree =
  result = DotTree(kind: dtkEdgeDef)
  var attrs = newStringTable()

  if edge.color != colBlack:
    # HACK black color is omitted unconditionally. need to IMPLEMENT
    # check whether or not this is allowed.
    attrs["color"] = ($edge.color).quote

  attrs.setSome("minlen", edge.minlen)
  if edge.weight.isSome(): attrs["weight"] = ($edge.weight.get())
  if edge.style != edsDefault: attrs["style"] = ($edge.style)

  result.origin = edge.src.addIdShift(idshift)
  result.targets = edge.to.mapIt(it.addIdShift(idshift))

  result.edgeAttributes = attrs

func toTreeComment(itms: varargs[string, `$`]): DotTree =
  DotTree(kind: dtkComment, text: itms.join(" "))

func toTree(attrs: StringTableRef): seq[DotTree] =
  for key, val in attrs:
    result.add DotTree(kind: dtkProperty, key: key, val: val)

func toTree(graph: Graph, idshift: int, level: int = 0): DotTree =
  result = DotTree(kind: dtkSubgraph)
  var attrs = newStringTable()

  if level == 0:
    if graph.isUndirected:
      result.section.add "graph"
    else:
      result.section.add "digraph"

    result.section.add graph.name
  else:
    result.section.add "subgraph"

  if graph.isCluster: result.section.add &"cluster_{graph.name}"
  if graph.splines != spsDefault: attrs["splines"] = $graph.splines
  if graph.noderank != gnrDefault: attrs["rank"] = $graph.noderank

  result.elements &= toTree(attrs)
  block:
    let styleNode = graph.styleNode.toTree(idshift, level + 1)
    if styleNode.nodeAttributes.len > 0:
      result.elements.add DotTree(
        kind: dtkProperty, globalProp: true,
        key: "node",
        val: styleNode.nodeAttributes.mapPairs(&"{lhs}={rhs}").join(", ")
      )

  if graph.topNodes.len > 0:
    var nodeIds: seq[NodeId]
    for node in graph.topNodes:
      let tree = node.toTree(idshift, level + 1)
      result.elements.add tree
      nodeIds.add node.id

    block:
      var prevId: NodeId = nodeIds[0]
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
  result.elements.add graph.subgraphs.mapIt(
    toTree(it,
           idshift = (it.idshift != 0).tern(it.idshift, graph.idshift),
           level = level + 1))



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
      let attrs = tree.nodeAttributes.mapPairs(&"{lhs}={rhs}").join(", ")
      if attrs.len == 0:
        rope(&"{pref}{tree.nodeId};")
      else:
        rope(&"{pref}{tree.nodeId}[{attrs}];")
    of dtkEdgeDef:
      let attrs = tree.edgeAttributes.mapPairs(&"{lhs}={rhs}").join(", ")
      if tree.targets.anyOfIt(it.isRecord):
        var res: Rope
        # TODO Generate muliple edegs for record types, one edge per target
        # TODO test of thsi works on graphviz first
        for to in tree.targets:
          res.add &"{pref}{tree.origin} -> {to};\n"

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

proc `$`*(graph: Graph): string =
  $graph.toTree(graph.idshift).toRope()

#===============================  testing  ===============================#

let res = Graph(
  name: "G",
  nodes: @[
    Node(id: 12),
    Node(id: 25),
    Node(id: 23),
    Node(
      id: 77,
      shape: nsaRecord,
      flds: @[
        RecordField(id: 8, text: "Hello"),
        RecordField(id: 9, text: "world"),
        RecordField(id: 10, text: "world")
      ]
    )
  ],
  edges: @[
    Edge(src: 12, to: @[23, 25]),
    Edge(src: 999, to: @[@[77, 8], @[77, 9], @[77, 10], @[25]]),
    Edge(src: 999, to: @[77, 12], color: colGreen)
  ],
  subgraphs: @[
    Graph(
      name: "ZZ",
      idshift: 8880000,
      isCluster: true,
      nodes: @[
        Node(id: 999)
      ]
    )
  ]
)

# echo $res

{.define(shellThrowException).}
import shell

proc topng*(
  graph: Graph,
  resfile: string,
  tmpfile: string = "/tmp/dot-file.dot",
  tmpimage: string = "/tmp/dot-image-tmp.png"
     ): void =
  ## Generate file from graph

  tmpfile.writeFile($graph)
  let shellcmd = &"dot -Tpng -o{tmpimage} {tmpfile}"
  shell:
    ($shellcmd)
    cp ($tmpimage) ($resfile)

# res.topng("/tmp/file.png")
