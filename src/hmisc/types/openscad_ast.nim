import strtabs
import strformat
import sequtils
import strutils
import hmisc/helpers
import math
import common

type
  GenerateWhat = enum
   onlyBase
   baseAndKeys
   wholeKeyboard

const generateWhat = wholeKeyboard

type
  ScadNodeType = enum
    sntInvoke
    sntComment
    sntInclude
    sntGroup
    sntModule
    sntVariable

  GroupModeType = enum
    gmtRegular
    gmtDebug
    gmtBackground
    gmtRoot

  ScadNode = object
    # DOC
    case kind: ScadNodeType
    of sntInvoke:
      name: string
      params: StringTableRef
      children: seq[ScadNode]
    of sntComment:
      text: string
    of sntInclude:
      path: string
    of sntGroup:
      elements: seq[ScadNode]
      groupMod: GroupModeType
    of sntModule:
      modName: string
      argList: StringTableRef ## Argument and it's default value
      body: seq[ScadNode]
    of sntVariable:
      varName: string
      defValue: string

proc toString*(node: ScadNode): string =
  case node.kind:
    of sntInvoke:
      result = node.name
      result &= "(" &
        toSeq(node.params.pairs).mapIt(&"{it.key} = {it.value}").join(",") &
        ")"

      if node.children.len == 0: result &= ";\n"
      else:
        result &= "{\n" &
          node.children.mapIt(it.toString).join("\n") &
          "}\n";
    of sntComment:
      result = &"//{node.text}\n"
    of sntInclude:
      result = &"""
// clang-format off
use <{node.path}>;
// clang-format on
"""
    of sntModule:
      result = &"module {node.modName} (" &
        toSeq(node.argList.pairs).mapIt(&"{it.key} = {it.value}").join(",") &
        ") {\n" &
        node.body.map(toString).join("\n") &
        "\n}"
    of sntVariable:
      result = &"{node.varName} = {node.defValue};"
    of sntGroup:
      result = node.elements.map(toString).join("\n")
      case node.groupMod:
        of gmtRegular: discard
        of gmtDebug, gmtBackground, gmtRoot:
          let symbol =
            case node.groupMod:
              of gmtRegular: ""
              of gmtDebug: "# union()"
              of gmtBackground: "% union()"
              of gmtRoot: "! union()"

          result = &"""
// clang-format off
{symbol}{{
  {result}
}}
// clang-format on
"""

proc makeScadComment*(text: string): ScadNode =
  ScadNode(text: text, kind: sntComment)

proc makeScadInclude*(path: string): ScadNode =
  ScadNode(path: path, kind: sntInclude)

proc makeScadVar*(name, defValue: string): ScadNode =
  ScadNode(kind: sntVariable, varName: name, defValue: defValue)

proc makeScadModule*(
  name: string,
  body: openarray[ScadNode],
  args: varargs[tuple[key, val: string]]
     ): ScadNode =
  ScadNode(
    modName: name,
    body: toSeq(body),
    argList: newStringTable(args),
    kind: sntModule
  )

proc makeGroup*(
  elements: openarray[ScadNode],
  gType: GroupModeType =  gmtRegular
     ): ScadNode =
  ## Make group node
  ScadNode(elements: toSeq(elements), kind: sntGroup, groupMod: gType)

proc makeGroupWith*(
  node: ScadNode,
  other: openarray[ScadNode],
  reverse: bool = false,
     ): ScadNode =
  ## Add scad node to group with `other` elements. It will be added as
  ## first or last based on `reverse` value: `true` to add as last,
  ## `false` otherwise
  ScadNode(elements:
    reverse.tern(
      toSeq(other) & @[node], @[node] & toSeq(other)
    ),
    kind: sntGroup)

proc addComment*(node: ScadNode, comment: string): ScadNode =
  makeScadComment(comment).makeGroupWith([node])

proc wrapComment*(node: ScadNode, comment: string): ScadNode =
  if comment.find("\n") == -1:
    makeScadComment(" --- begin " & comment & " ---").makeGroupWith([
      node,
      makeScadComment(" --- end " & comment & " ---")
    ])
  else:
    let commLines = comment.split("\n")
    let commHead = " --- begin " & commLines[0] & "\n" &
      commLines[1..^1].mapIt("// " & it).join("\n") & "---"

    makeScadComment(commHead).makeGroupWith([
      node,
      makeScadComment(" --- end " & commLines[0] & "---")
    ])

proc makeScad*(
  name: string,
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
  ScadNode(
    name: name,
    params: newStringTable(params),
    kind: sntInvoke
  )

proc makeScadTree*(
  name: string,
  children: openarray[ScadNode],
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
  ScadNode(
    name: name,
    params: newStringTable(params),
    children: children.toSeq(),
    kind: sntInvoke
  )

proc setColor*(
  node: ScadNode, r = 0.0, g = 0.0, b = 0.0, a = 1.0
     ): ScadNode =
  makeScadTree(
    "color",
    [node],
    {"c" : &"[{r}, {r}, {b}]", "alpha" : $a}
  )


proc setColor*(node: ScadNode, colorname: string,  a = 1.0): ScadNode =
  makeScadTree("color", [node], {"c" : &"\"{colorname}\"", "alpha" : $a})



proc scadOperator*(
  node: ScadNode,
  name: string,
  params: varargs[tuple[key, val: string]]
     ): ScadNode =
    makeScadTree(name, [node], params)

proc scadTranslate*(node: ScadNode, x = 0.0, y = 0.0, z = 0.0): ScadNode =
  makeScadTree("translate", [node], {"v" : &"[{x}, {y}, {z}]"})

proc scadTranslate*(node: ScadNode, pos: string): ScadNode =
  makeScadTree("translate", [node], {"v" : pos})

proc scadTranslate*(node: ScadNode, pos: Vec3): ScadNode =
  scadTranslate(node, x = pos.x, y = pos.y, z = pos.z)

proc scadTranslate*(node: ScadNode, pos: Vec): ScadNode =
  scadTranslate(node, x = pos.x, y = pos.y, 0)

proc scadRotate*(
  node: ScadNode,
  angle: float | string,
  x = 0.0, y = 0.0, z = 1.0
     ): ScadNode =
  makeScadTree("rotate", [node], {
    "a" : (when angle is float: $(angle.radToDeg()) else: angle),
    "v" : &"[{x}, {y}, {z}]"
  })


proc scadSubtract*(node: ScadNode, subtract: varargs[ScadNode]): ScadNode =
  makeScadTree(name = "difference", children = @[node] & subtract.toSeq())


proc scadSubtract*(
  node: ScadNode,
  subtract: seq[ScadNode],
  traceColor: string
     ): ScadNode =
  @[
    scadSubtract(node, subtract.toSeq()),
    [makeScadTree(name = "union", children = subtract)
      .setColor(colorname = traceColor, a = 0.3)]
      .makeGroup(gType = gmtDebug)
  ].makeGroup()


proc scadSubtract*(
  node: ScadNode,
  subtract: ScadNode,
  traceColor: string
     ): ScadNode =
    scadSubtract(node, @[subtract], traceColor)

proc scadUnion*(node: ScadNode, subtract: varargs[ScadNode]): ScadNode =
    makeScadTree(name = "union", children = @[node] & subtract.toSeq())

proc `$`*(size: Size3): string = &"[{size.w}, {size.d}, {size.h}]"

proc makeCube*(size: Size3, center: bool = false): ScadNode =
  makeScad("cube", {
    "size" : $size,
    "center": $center
  })

proc makeCube*(size: Size3, center: Vec3): ScadNode =
  makeCube(size, false).scadTranslate(center)


proc makeCube*(w,d,h: float): ScadNode =
  makeCube(makeSize3(w = w, d = d, h = h), false)
