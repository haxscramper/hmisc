import sequtils, xmltree, strtabs, strformat, strutils


const svgMulti* = 50 ## Multiplication ratio for converting float to
                     ## svg coordinates

proc toSVGsize*(num: float): string =
  ## Convert float size to svg (multiply by `svgMulti`)
  $(num * svgMulti).toInt()

proc `<->`*(comm: string): XmlNode =
  ## Create xml comment
  newComment(comm)

proc newXmlTree*(
  node: string,
  children: openarray[XmlNode],
  attributes: varargs[tuple[key, val: string]]
     ): XmlNode =
    newXmlTree(node, children, attributes.toXmlAttributes())

proc flipUp*(node: XmlNode): XmlNode =
  ## Multiply `y` axis by `-1`
  newXmlTree(
    "g", [node],
    {
      "transform" : "scale(1, -1)"
    }.toXmlAttributes())

# TODO proc for condensing transformations into single <g> tag

proc svgScale*(node: XmlNode, scale: (float, float)): XmlNode =
  ## Scale both dimensions of svg node
  newXmlTree(
    "g", [node], {
      "transform" : &"scale({scale[0]}, {scale[1]})"
    })

proc svgScale*(node: XmlNode, x, y: float | int): XmlNode =
  ## Scale both dimensions of svg node
  node.svgScale(
    when x is float: (x,y)
    else: (x.toFloat(), y.toFloat()))

proc svgRotate*(node: XmlNode, deg: int | float): XmlNode =
  ## Wrap node into `g` tag with `trasnform` attribute set to
  ## `rotate(deg)`
  newXmlTree(
    "g", [node], {
      "transform" : &"rotate({deg})"
    })

proc makeSVGGroup*(nodes: openarray[XmlNode]): XmlNode =
  newXmlTree("g", nodes)

proc svgTranslate*(
  node: XmlNode,
  x, y: int | float | string): XmlNode =
  ## Wrap node into `g` tag with `transform` attribute set to
  ## `translate(...)`. Basically move node to position `x, y`
  when (x is int) or (x is float):
    newXmlTree(
      "g", [node], {
      "transform" : &"translate({x.toSVGsize()}, {y.toSVGsize()})"
    })
  elif x is string:
    newXmlTree(
      "g", [node], {
      "transform" : &"translate({x}, {y})"
    })

proc makeSVG*(
  name: string,
  attributes: varargs[tuple[key, val: string]],
  text: Option[string] = none(string)
     ): XmlNode =
    ## Create svg node with `name`, `attributes` and possible child
    ## text node that contains `text`
    newXmlTree(
      name,
      tern(
        text.isSome,
        @[newText(text.get())],
        @[],
      ),
      attributes.toXmlAttributes())


proc makeStyle*(input: varargs[
  tuple[key: string, val: string]]
               ): string =
    input.mapIt(&"{it.key}: {it.val};").join(" ")


proc makeText*(text: string, p: Vec, textClass = "coordinate"): XmlNode =
  ## Create text at position `p`
  makeSVG(
    "text",
    {"x" : "0", "y" : "0", "class" : textClass},
    text)
  .svgTranslate(p.x, -p.y)
  .svgScale(1, -1)

proc `&`*(attrs, addition: XmlAttributes): XmlAttributes =
  # var kvPairs: seq[tuple[key, val: string]]
  # for attr, val in attrs:
  #   kvPairs[attr] = val

  for attr, val in addition:
    attrs[attr] = val

  return attrs

proc toSVG*(key: Key): XmlNode =
  newXmlTree(
    "rect", [],
    {
      "width" : $(key.length * svgMulti).toInt(),
      "height" : $(key.width * svgMulti).toInt(),
      "class" : "key-box"
    }.toXmlAttributes()
  )

proc toSVG*(row: Row): XmlNode =
  var shift = 0.0
  let keys: seq[XmlNode] =
      row.keys.mapIt(
        block:
          var keyXml = it.key.toSVG()
          shift += it.space
          keyXml.attrs = keyXml.attrs &
            {"x" : $(shift * svgMulti).toInt() }.toXmlAttributes()
          shift += it.key.length
          keyXml
      )

  newXmlTree("g",
    newComment("row start") & keys & newComment("row end")
  )



proc toSVG*(
  p: Vec,
  annotate: static[char] = 'n',
  style: Option[string] = none(string)
          ): XmlNode =
  ## Generate svg circle at position `p` and possibly annotate it with
  ## coordinates. Coordinate annotation is controlled using `annoate`
  ## and can take several values ('n' for no annotation, 'r' and 'l'
  ## for right and left respectively)
  static:
    const allowed: set[char] = {'r', 'n', 'l'}
    assert annotate in allowed,
          "Value of annotation position can only be one of " &
            $allowed

  let circle = makeSVG("circle", {
    "cx" : p.x.toSVGsize(),
    "cy" : p.y.toSVGsize(),
    "r" : $8,
    "style" :
      if style.isSome: style.get()
      else: "fill: black;"
  })
  case annotate:
    of 'n': circle
    of 'r': newXmlTree(
      "g",
      [circle, makeText(&"{p.x} {p.y}", p + (0.0, 0.1))])
    else: circle

proc toSVG*(line: Line): XmlNode =
  let a: Vec = (0,0)
  makeSVG("line", {
      "x1" : line.x1.toSVGsize(),
      "y1" : line.y1.toSVGsize(),
      "x2" : line.x2.toSVGsize(),
      "y2" : line.y2.toSVGsize(),
      "stroke" : "green",
      "stroke-width" : "3"
    })

proc toSVG*(geom: Geometry): XmlNode =
  case geom.kind:
    of gkLine: geom.l.toSVG()
    of gkVec: geom.v.toSVG()

proc toSVG*(geoms: seq[Geometry]): XmlNode =
  geoms.mapIt(it.toSVG).makeSVGGroup()

proc toSVGImage*(
  body: seq[XmlNode],
  width: int = 480,
  height: int = 480): XmlNode =
    newXmlTree(
      "svg",
      [
        newXmlTree("style", @[newText("\n" & """
.key-box {
  fill : rgb(255,255,255);
  stroke-width:3;
  stroke:rgb(0,0,0)
}
.base-control-dot {
  fill : rgb(255,0,0);
}
.coordinate {
  font: bold 16px sans-serif;
}
""")]),
        newXmlTree(
          "g", body,
          {
            # Since 2d will be mapped to 3d it is better to worh in
            # coordinate system that won't melt your brain when you
            # try to think how one thing maps to another.
            "transform" : &"""
scale(1, -1)
translate(100, 100)
translate(0, -{height})
"""
          }.toXmlAttributes())
      ],
      {
        "width" : $width,
        "height" : $height,
        "version" : "1.1",
        "xmlns" : "http://www.w3.org/2000/svg"
      }.toXmlAttributes()
    )
