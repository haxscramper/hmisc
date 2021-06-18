## Nim reimplementation of layout algorithm from
## https://github.com/google/rfmt.

# Thanks to nim being syntactically close to python this is mostly
# just blatant copy-paste of python code with added type annotations.

## .. include:: blockfmt-doc.rst

# TODO grid layout block

import std/[
  strutils, sequtils, macros, tables, strformat,
  lenientops, options, hashes, math, sugar, streams
]

import
  ../base_errors,
  ../algo/[hmath, halgorithm, hseq_mapping],
  ../types/colorstring,
  ../hdebug_misc,
  ../hexceptions

const infty = 1024 * 1024 * 1024 * 1024

func inf(a: int): bool = (infty - 4096 <= a) and (a <= infty + 4096)

func `*`(a: SomeNumber, b: bool): SomeNumber = (if b: a else: 0)
func get[T](inseq: seq[Option[T]]): seq[T] =
  for elem in inseq:
    if elem.isSome():
      result.add elem.get()


#*************************************************************************#
#****************************  Format policy  ****************************#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  LayoutElement = object
    ## An element of a layout object - a directive to the console.
    ##
    ## This class sports a collection of static methods, each of which
    ## returns an anonymous function invoking a method of the console
    ## to which it is applied.
    ##
    ## Refer to the corresponding methods of the LytConsole class for
    ## descriptions of the methods involved.
    text: string ## Layout element text
    debug: string ## Textual reprsentation of layout element for
                  ## pretty-printing

  Layout* = object
    ## An object containing a sequence of directives to the console.
    elements: seq[LayoutElement]

  LytSolution* = ref object
    knots: seq[int]
    spans: seq[int]
    intercepts: seq[float]
    gradients: seq[float]
    layouts*: seq[Layout]
    index: int

  LytBlockKind* = enum
    bkText ## Single line text block
    bkLine ## Horizontally stacked lines
    bkChoice ## Several alternating layouts
    bkStack ## Vertically stacked layouts
    bkWrap ## Mulitple blocks wrapped to create lowerst-cost layout
    bkVerb ## Multiple lines verbatim
    bkEmpty ## Empty layout block - ignored by `add` etc.

  LytBlock* = ref object
    layout_cache: Table[Option[LytSolution], Option[LytSolution]]
    isBreaking*: bool ## Whether or not this block should end the line
    breakMult*: int
    width*: int ## Number of characters on the longest line in the block
    height*: int ## Number of lines in block

    case kind*: LytBlockKind
      of bkVerb:
        textLines*: seq[string] ## Lines of text
        firstNl*: bool ## Insert newline at the block start

      of bkText:
        text*: string ## Line of text

      of bkWrap:
        prefix*: Option[string]
        sep*: string ## Separator for block wraps
        wrapElements*: seq[LytBlock]

      of bkStack, bkChoice, bkLine:
        elements*: seq[LytBlock]

      of bkEmpty:
        discard

  LytFormatPolicy = object
    breakElementLines: proc(blc: seq[seq[LytBlock]]): seq[seq[LytBlock]] ## Hook


proc printOn*(self: Layout, buf: var string): void =
  for elem in self.elements:
    buf &= elem.text

proc write*(stream: Stream | File, self: Layout, indent: int = 0) =
  if indent == 0:
    for elem in self.elements:
      stream.write(elem.text)

  else:
    for elem in self.elements:
      for ch in elem.text:
        if ch == '\n':
          stream.write " ".repeat(indent)

        stream.write ch


proc debugOn*(self: Layout, buf: var string): void =
  for elem in self.elements:
    buf &= elem.debug

proc `$`*(le: LayoutElement): string = le.text


proc `$`*(lt: Layout): string =
  lt.debugOn(result)


proc `$`*(sln: LytSolution): string =
  result &= "<"
  var idx: int = 0
  for s in zip(
    sln.knots, sln.spans, sln.intercepts, sln.gradients, sln.layouts
  ):
    if idx > 0:
      result &= ", "

    result &= &"{s[0]}/({s[1]}, {s[2]:.2}, {s[3]}, \"{s[4]}\")"
    inc idx

  result &= ">"

proc `$`*(sln: Option[LytSolution]): string =
  if sln.isSome(): return $sln.get()

proc `$`*(blc: LytBlock): string =
  case blc.kind:
    of bkText:
      (if blc.is_breaking: "*" else: "") & &"\"{blc.text}\""

    of bkStack:
      "V[" & blc.elements.mapIt($it).join(" ↕ ") & "]"

    of bkLine:
      "H[" & blc.elements.mapIt($it).join(" ↔ ") & "]"

    of bkChoice:
      blc.elements.mapIt($it).join(" ? ").wrap("()")

    of bkWrap:
      blc.wrapElements.mapIt($it).join(" ").wrap("[]")

    of bkVerb:
      blc.textLines[0] & "..."

    of bkEmpty:
      "<empty>"
      # &""">>{blc.textLines.join("⮒")}<<"""

func treeRepr*(inBl: LytBlock): string =
  func aux(bl: LytBlock, level: int): string =
    let name =
      case bl.kind:
        of bkLine: "L"
        of bkChoice: "C"
        of bkText: "T"
        of bkWrap: "W"
        of bkStack: "S"
        of bkVerb: "V"
        of bkEmpty: "E"

    let pref = align(
      name & &" {toCyan($bl.height)}x{toCyan($bl.width)} " & " ",
      level * 2
    )

    let pref2 = repeat(" ", level * 2 + 2)

    case bl.kind:
      of bkLine, bkChoice, bkStack, bkWrap:
        for isFirst, isLast, elem in itemsIsFirstLast(bl.elements):
          result &=
            (if isFirst: pref & "\n" & pref2 else: pref2) &
            elem.aux(level + 1) &
            (if isLast: "" else: "\n")

      of bkText:
        result = &"{pref}{bl.text.escape()}"

      of bkEmpty:
        result = "<empty>"

      of bkVerb:
        for isLast, line in itemsIsLast(bl.textLines):
          result &= &"{pref}|  {line.escape()}" & (if isLast: "" else: "\n")

  return aux(inBl, 0)





#*************************************************************************#
#************************  LytOptions configuration  ************************#
#*************************************************************************#
type
  LytOptions* = object
    leftMargin*: int ## position of the first right margin. Expected `0`
    rightMargin*: int ## position of the second right margin. Set for `80` to
            ## wrap on default column limit.
    leftMarginCost*: float ## cost (per character) beyond margin 0. Expected value
              ## `~0.05`
    rightMarginCost*: float ## cost (per character) beyond margin 1. Should be much
              ## higher than `c0`. Expected value `~100`
    linebreakCost*: int ## cost per line-break
    indentSpaces*: int ## spaces per indent
    # adj_comment: int
    # adj_flow: int #
    # adj_call: int
    # adj_arg: int
    cpack*: float ## cost (per element) for packing justified layouts.
                 ## Expected value `~0.001`
    format_policy*: LytFormatPolicy

func hash(elem: LayoutElement): Hash = hash(elem.text)
func hash(lyt: Layout): Hash = hash(lyt.elements)

func hash(sln: Option[LytSolution]): Hash =
  if sln.isNone():
    return
  else:
    let sln = sln.get()
    result = !$(
      hash(sln.knots) !&
      hash(sln.spans) !&
      hash(sln.intercepts) !&
      hash(sln.gradients) !&
      hash(sln.layouts) !&
      hash(sln.index)
    )


#*************************************************************************#
#*******************************  Layout  ********************************#
#*************************************************************************#
func lytString(s: string): LayoutElement =
  result.debug = s
  result.text = s

func lytNewline(indent: bool = true): LayoutElement =
  result.text = "\n"
  result.debug = "\\n"

proc lytPrint(lyt: Layout): LayoutElement =
  result.debug = &"[{lyt}]"
  lyt.printOn(result.text)

func getStacked(layouts: seq[Layout]): Layout =
  ## Return the vertical composition of a sequence of layouts.

  ## Args:
  ##   layouts: a sequence of Layout objects.
  ## Returns:
  ##   A new Layout, stacking the arguments.
  var l_elts: seq[LayoutElement]
  for l in layouts:
    for e in l.elements:
      l_elts.add e

    l_elts.add lytNewLine()

  return Layout(elements: l_elts[0 .. ^2])  # Drop the last NewLine()

func initLayout(elems: openarray[LayoutElement]): Layout =
  result.elements = toSeq(elems)

#*************************************************************************#
#******************************  LytSolution  *******************************#
#*************************************************************************#

func initSolution(
    knots: seq[int], spans: seq[int], intercepts: seq[float],
    gradients: seq[float], layouts: seq[Layout]): LytSolution =

  LytSolution(
    knots: knots, spans: spans, intercepts: intercepts,
    gradients: gradients, layouts: layouts)

#===========================  Helper methods  ============================#
func reset(self: var LytSolution) =
  ## Begin iteration.
  self.index = 0

func advance(self: var LytSolution) =
  ## Advance to the next knot.
  self.index += 1

func retreat(self: var LytSolution) =
  ## Move back a knot.
  self.index -= 1

func curKnot(self: LytSolution): int =
  ## The currently indexed knot.
  return self.knots[self.index]

func curSpan(self: LytSolution): int =
  return self.spans[self.index]

func curIntercept(self: LytSolution): float =
  return self.intercepts[self.index]

func curGradient(self: LytSolution): float =
  return self.gradients[self.index]

func curLayout(self: LytSolution): Layout =
  return self.layouts[self.index]

func curIndex(self: LytSolution): int =
  return self.index

func curValueAt(self: LytSolution, margin: int): float =
  ## The value (cost) extrapolated for margin m from the current knot.
  # Since a LytSolution's cost is represented by a piecewise linear function,
  # the extrapolation in this case is linear, from the current knot.
  return self.curIntercept() + self.curGradient() * float(margin - self.curKnot())

func nextKnot(self: LytSolution): int =
  ## The knot after the once currently indexed.
  if self.index + 1 >= self.knots.len:
    infty
  else:
    self.knots[self.index + 1]

proc moveToMargin(self: var LytSolution, margin: int) =
  ## Adjust the index so m falls between the current knot and the next.
  if self.curKnot() > margin:
    while self.curKnot() > margin:
      self.retreat()
  else:
    while self.nextKnot() <= margin and self.nextKnot() != infty:
      self.advance()
      # info "Advancing to position", self.curIndex(),
      #   "next knot is", self.nextKnot(), "margin is", margin,
      #   self.nextKnot() <= margin


#==========================  LytSolution factory  ===========================#

type
  LytSolutionFactory = object
    ## A factory object used to construct new LytSolution objects.
    ##
    ## The factory performs basic consistency checks, and eliminates
    ## redundant segments that are linear extrapolations of those that
    ## precede them.
    entries: seq[tuple[
      knot, span: int, intercept, gradient: float, lyt: Layout]]

proc add(
  self: var LytSolutionFactory,
  knot, span: int, intercept, gradient: float, layout: Layout): void =
  ## Add a segment to a LytSolution under construction.
  # debug "Add layout", layout, "to solution"
  if self.entries.len > 0:
    # Don't add a knot if the new segment is a linear extrapolation of
    # the last.
    let (k_last, s_last, i_last, g_last, _) = self.entries[^1]
    if (span == s_last and gradient == g_last and
        i_last + (knot - k_last) * g_last == intercept):
      return

  if knot < 0 or span < 0 or intercept < 0 or gradient < 0:
    raiseAssert(
      "Internal error: bad layout: " &
        &"(k {knot}, s {span}, i {intercept}, g {gradient})")

  self.entries.add (knot, span, intercept, gradient, layout)

proc makeSolution(self: LytSolutionFactory): LytSolution =
  ## Construct and return a new LytSolution with the data in this
  ## object
  new(result)
  # debug "Constructing solution with", self.entries.len, "elements"
  # debug self.entries.mapIt(it.lyt)
  for (k, s, i, g, l) in self.entries:
    result.knots.add k
    result.spans.add s
    result.intercepts.add i
    result.gradients.add g
    result.layouts.add l

#=====================  LytSolution manipulation logic  =====================#


proc minSolution(solutions: seq[LytSolution]): Option[LytSolution] =
  ## Form the piecewise minimum of a sequence of LytSolutions.

  ## Args:
  ##   solutions: a non-empty sequence of LytSolution objects
  ## Returns:
  ##   values LytSolution object whose cost is the piecewise minimum of the LytSolutions
  ##   provided, and which associates the minimum-cost layout with each piece.
  # debug "Minimal solution out of #", solutions.len
  if len(solutions) == 1:
    return some(solutions[0])

  var
    factory: LytSolutionFactory
    solutions = solutions

  for s in mitems(solutions):
    s.reset()

  let n = len(solutions)
  var
    k_l = 0
    last_i_min_soln = -1  # Index of the last minimum solution
    last_index = -1  # Index of the current knot in the last minimum
    # solution Move through the intervals [k_l, k_h] defined by the
    # glb of the partitions defined by each of the solutions.

  while k_l < infty:
    let
      k_h = min(solutions.map(nextKnot)) - 1
      gradients = solutions.map(curGradient)

    while true:
      let values = solutions.mapIt(it.curValueAt(k_l))
      # Use the index of the corresponding solution to break ties.
      let (min_value, min_gradient, i_min_soln) =
        (0 ..< n).mapIt((values[it], gradients[it], it)).min()

      let min_soln = solutions[i_min_soln]
      if i_min_soln != last_i_min_soln or min_soln.curIndex() != last_index:
        # Add another piece to the new LytSolution
        factory.add(
          k_l,
          min_soln.curSpan(),
          min_value,
          min_gradient,
          min_soln.curLayout()
        )

        last_i_min_soln = i_min_soln
        last_index = min_soln.curIndex()
      # It's possible that within the current interval, the minimum
      # solution may change, should a solution with a lower initial
      # value but greater gradient surpass the value of one with a
      # higher initial value but lesser gradient. In such instances,
      # we need to add an extra piece to the new solution.
      let distances_to_cross = collect(newSeq):
        for i in 0 ..< n:
          if gradients[i] < min_gradient:
            ceil((values[i] - min_value) / (min_gradient - gradients[i]))

      # Compute positions of all crossovers in [k_l, k_h]
      let crossovers = collect(newSeq):
        for d in distances_to_cross:
          if k_l + d <= k_h:
            k_l + d

      if crossovers.len > 0:  # Proceed to crossover in [k_l, k_h]
        k_l = min(crossovers).int # XXXX
      else:  # Proceed to next piece
        k_l = k_h + 1
        if k_l < infty:
          for s in mitems(solutions):
            s.moveToMargin(k_l)
        break

  return some factory.makeSolution()

proc vSumSolution(solutions: seq[LytSolution]): LytSolution =
  ## The layout that results from stacking several LytSolutions vertically.
  ## Args:
  ##   solutions: a non-empty sequence of LytSolution objects
  ## Returns:
  ##   A LytSolution object that lays out the solutions vertically, separated by
  ##   newlines, with the same left margin.

  # echov "vSumSolutions"
  # for s in solutions:
  #   echov s

  assert solutions.len > 0
  # debug "Vertical stack #", solutions.len, "solution"

  if len(solutions) == 1:
    return solutions[0]

  var solutions = solutions # XXXX

  var col: LytSolutionFactory

  for s in mitems(solutions):
    s.reset()

  var margin = 0  # Margin for all components
  while true:
    col.add(
      margin,
      solutions[^1].curSpan(),
      solutions.mapIt(it.curValueAt(margin)).sum(),
      solutions.mapIt(it.curGradient()).sum(),
      solutions.mapIt(it.curLayout()).getStacked()
    )

    # The distance to the closest next knot from the current margin.
    let d_star = min(
      solutions.
      filterIt(it.nextKnot() >= margin).
      mapIt(it.nextKnot() - margin))  # TODO(pyelland): Redundant check?

    if d_star.inf:
      break

    margin += d_star

    for s in mitems(solutions):
      s.moveToMargin(margin)

  return col.makeSolution()

proc hPlusSolution(s1, s2: var LytSolution, opts: LytOptions): LytSolution =
  ## The LytSolution that results from joining two LytSolutions side-by-side.

  ## Args:
  ##   `s1`: LytSolution object
  ##   `s2`: LytSolution object
  ## Returns:
  ##   A new LytSolution reflecting a layout in which `s2` ('s layout) is
  ##   placed immediately to the right of `s1`.

  ## The resulting LytSolution object maps each prospective left margin m
  ## to the span, cost and layout information that would result from
  ## siting LytSolution `s1` at m, and then placing `s2` at margin `m +
  ## sp1(m)`, where `sp1(m)` is the span of characters occupied by the
  ## layout to which `s1` maps m. In general, of course, both s1 and
  ## `s2`'s layouts may occupy multiple lines, in which case `s2`'s
  ## layout begins at the end of the last line of `s1`'s layout---the
  ## span in this case is the span of `s1`'s last line.
  var col: LytSolutionFactory
  s1.reset()
  s2.reset()
  var
    s1_margin = 0
    s2_margin = s1.curSpan()

  s2.moveToMargin(s2_margin)

  while true:
    # When forming the composite cost gradient and intercept, we must
    # eliminate the over-counting of the last line of the s1, which is
    # attributable to its projection beyond the margins.
    let
      g1 = s1.curGradient()
      g2 = s2.curGradient()
      overhang0 = s2_margin - opts.leftMargin  # s2_margin = rightMargin + span of s1
      overhang1 = s2_margin - opts.rightMargin  # s2_margin = rightMargin + span of s1
      g_cur = (g1 + g2 -
               opts.leftMarginCost * (overhang0 >= 0) -
               opts.rightMarginCost * (overhang1 >= 0))
      i_cur = (s1.curValueAt(s1_margin) + s2.curValueAt(s2_margin) -
               opts.leftMarginCost * max(overhang0, 0) -
               opts.rightMarginCost * max(overhang1, 0))

    # The Layout computed by the following implicitly sets the margin
    # for s2 at the end of the last line printed for s1.
    col.add(
      s1_margin, s1.curSpan() + s2.curSpan(), i_cur, g_cur,
      initLayout([lytPrint(s1.curLayout()), lytPrint(s2.curLayout())]))

    # Move to the knot closest to the margin of the corresponding
    # component.
    let
      kn1 = s1.nextKnot()
      kn2 = s2.nextKnot()

    if kn1.inf and kn2.inf:
      break

    # Note in the following that one of kn1 or kn2 may be infinite.
    if kn1 - s1_margin <= kn2 - s2_margin:
      s1.advance()
      s1_margin = kn1
      s2_margin = s1_margin + s1.curSpan()
      # Note that s1.CurSpan() may have changed, and s2_margin may
      # decrease, so we cannot simply increment s2's index.
      s2.moveToMargin(s2_margin)
    else:
      s2.advance()
      s2_margin = kn2
      s1_margin = s2_margin - s1.curSpan()

  return col.makeSolution()




func plusConst(self: LytSolution, val: float): LytSolution =
  ## Add a constant to all values of this LytSolution.
  result = self
  for a in mitems(result.intercepts):
    a += val

proc withRestOfLine(
    self: var Option[LytSolution],
    rest_of_line: var Option[LytSolution], opts: LytOptions
  ): Option[LytSolution] =
  ## Return a LytSolution that joins the rest of the line right of this one.

  ## Args:
  ##   rest_of_line: a LytSolution object representing the code laid out on the
  ##     remainder of the line, or None, if the rest of the line is empty.
  ## Returns:
  ##   A new LytSolution object juxtaposing the layout represented by this
  ##   LytSolution to the immediate right of the remainder of the line.
  if restOfLine.isNone():
    self
  else:
    some self.get().hplusSolution(restOfLine.get(), opts)


#*************************************************************************#
#*****************************  LytBlock type  ******************************#
#*************************************************************************#
proc elements(self: LytBlock): seq[LytBlock] =
  if contains({bkWrap}, self.kind):
    return self.elements
  if contains({bkStack, bkChoice, bkLine}, self.kind):
    return self.elements
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `elements=`(self: var LytBlock; it: seq[LytBlock]) =
  var matched: bool = false
  if contains({bkWrap}, self.kind):
    if true:
      matched = true
      self.wrapElements = it
  if contains({bkStack, bkChoice, bkLine}, self.kind):
    if true:
      matched = true
      self.elements = it
  if not matched:
    raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

func len*(blc: LytBlock): int =
  case blc.kind:
    of bkWrap: blc.wrapElements.len()
    else: blc.elements.len()

iterator items*(blc: LytBlock): LytBlock =
  for item in blc.elements:
    yield item


#============================  Constructors  =============================#
# TODO support reassembling horizontal lines of stacks in different forms,
# in any of tese forms
#
# ```
#   proc (line 1
#         line 2
#         line 3 = line 1
#                  line 2
#                  line 3)
# ```
#
# ```
#   proc (line 1 = line 1)
#         line 2   line 2
#         line 3   line 3
# ```

func makeBlock*(kind: LytBlockKind): LytBlock =
  LytBlock(kind: kind)

func makeTextBlock*(text: string): LytBlock =
  LytBlock(kind: bkText, text: text, width: text.len, height: 1)

func makeEmptyBlock*(): LytBlock =
  LytBlock(kind: bkEmpty)

func makeTextBlock*(text: ColoredString): LytBlock =
  LytBlock(kind: bkText, text: $text, width: text.len, height: 1)

proc makeTextBlocks*(text: openarray[string]): seq[LytBlock] =
  text.mapIt(makeTextBlock(it))


func makeIndentBlock*(blc: LytBlock, indent: int): LytBlock


func makeLineBlock*(elems: openarray[LytBlock]): LytBlock =
  result = LytBlock(
    kind: bkLine,
    elements: filterIt(elems, it.kind != bkEmpty))
  result.height = elems.maxIt(it.height)
  result.width = elems.sumIt(it.width)


func makeIndentBlock*(blc: LytBlock, indent: int): LytBlock =
  if indent == 0:
    blc

  else:
    makeLineBlock(@[makeTextBlock(" ".repeat(indent)), blc])

template findSingle*(elems: typed, targetKind: typed): untyped =
  var
    countEmpty = 0
    countFull = 0
    idx = -1

  for item in elems:
    if item.kind == bkEmpty: inc countEmpty
    if item.kind == targetKind:
      if idx != -1:
        idx = -1
        break

      else:
        idx = countFull

    inc countFull

  idx


func makeChoiceBlock*(elems: openarray[LytBlock]): LytBlock =
  if (let idx = findSingle(elems, bkChoice); idx != -1):
    result = elems[idx]

  else:
    result = LytBlock(
      kind: bkChoice,
      elements: filterIt(elems, it.kind != bkEmpty))

    result.width = elems.maxIt(it.width)
    result.height = elems.maxIt(it.height)

func makeStackBlock*(elems: openarray[LytBlock]): LytBlock =
  if (let idx = findSingle(elems, bkStack); idx != -1):
    result = elems[idx]

  else:
    result = LytBlock(
      kind: bkStack,
      elements: filterIt(elems, it.kind != bkEmpty))

    result.height = elems.sumIt(it.height)
    result.width = elems.maxIt(it.width)


func makeWrapBlock*(elems: openarray[LytBlock]): LytBlock =
  LytBlock(kind: bkWrap, wrapElements: toSeq(elems))

func makeVerbBlock*(
    textLines: openarray[string], breaking: bool = true,
    firstNl: bool = false
  ): LytBlock =

  LytBlock(
    kind: bkVerb, textLines: toSeq(textLines),
    isBreaking: breaking, firstNl: firstNl
  )

func makeForceLinebreak*(text: string = ""): LytBlock =
  makeVerbBlock(@[text], true, false)

# func makeIndentBlock*(element: LytBlock, indent: int = 0): LytBlock =
#   ## Create line block with `indent` leading whitespaces
#   makeLineBlock(@[
#     makeTextBlock(" ".repeat(indent)),
#     element
#   ])

func makeLineCommentBlock*(
  text: string, prefix: string = "# "): LytBlock =
  makeVerbBlock(@[prefix & text])

func add*(target: var LytBlock, other: varargs[LytBlock]) =
  for bl in other:
    assert not isNil(bl)
    if bl.kind != bkEmpty:
      case target.kind:
        of bkStack:
          target.height += bl.height
          target.width = max(target.width, bl.width)

        of bkLine:
          target.width += bl.width
          target.height = max(target.height, bl.height)

        else:
          discard

      if bl.kind == target.kind and bl.kind in {bkStack, bkLine}:
        target.elements.add bl.elements

      else:
        target.elements.add bl

#============================  Layout logic  =============================#

proc doOptLayout*(
  self: var LytBlock,
  rest_of_line: var Option[LytSolution], opts: LytOptions): Option[LytSolution]

proc optLayout(
  self: var LytBlock,
  rest_of_line: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =
  ## Retrieve or compute the least-cost (optimum) layout for this block.
  ##
  ## Args:
  ##   rest_of_line: a LytSolution object representing the text to the
  ##     right of this block.
  ##
  ## Returns:
  ##   A LytSolution object representing the optimal layout for this
  ##   block and the rest of the line.
  # Deeply-nested choice block may result in the same continuation
  # supplied repeatedly to the same block. Without memoisation, this
  # may result in an exponential blow-up in the layout algorithm.
  if rest_of_line notin self.layout_cache:
    self.layout_cache[rest_of_line] = self.doOptLayout(rest_of_line, opts)

  return self.layout_cache[rest_of_line]

proc doOptTextLayout(
  self: LytBlock,
  rest_of_line: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =

  let
    span = len(self.text)
    layout = initLayout([lytString(self.text)])
  # The costs associated with the layout of this block may require 1, 2 or 3
  # knots, depending on how the length of the text compares with the two
  # margins (leftMargin and rightMargin) in opts. Note that we assume
  # opts.rightMargin >= opts.leftMargin >= 0, as asserted in base.Options.Check().
  if span >= opts.rightMargin:
    result = some initSolution(
      @[0],
      @[span],
      @[float((span - opts.leftMargin) * opts.leftMarginCost + (span - opts.rightMargin) * opts.rightMargin)],
      @[float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout]
    )

  elif span >= opts.leftMargin:
    result = some initSolution(
      @[0, opts.rightMargin - span],
      @[span, span], # XXXX
      @[float((span - opts.leftMargin) * opts.leftMarginCost),
        float((opts.rightMargin - opts.leftMargin) * opts.leftMarginCost)],
      @[float(opts.leftMarginCost), float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout, layout] # XXXX
    )
  else:
    result = some initSolution(
      @[0, opts.leftMargin - span, opts.rightMargin - span],
      @[span, span, span], # XXXX
      @[float(0), float(0), float((opts.rightMargin - opts.leftMargin) * opts.leftMarginCost)],
      @[float(0), float(opts.leftMarginCost), float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout, layout, layout] # XXXX
    )

  return result.withRestOfLine(rest_of_line, opts)


proc doOptLineLayout(
  self: var LytBlock,
  rest_of_line: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =
  # echov "doOptLineLayout"
  assert self != nil
  # echov self
  if self.elements.len == 0:
    return rest_of_line

  var element_lines: seq[seq[LytBlock]] = @[]
  element_lines.add @[]

  for i, elt in self.elements:
    element_lines[^1].add elt

    if i < len(self.elements) - 1 and elt.is_breaking:
      element_lines.add @[]

  if len(element_lines) > 1:
    assert opts.format_policy.breakElementLines != nil
    element_lines = opts.format_policy.breakElementLines(element_lines)

  var line_solns: seq[Option[LytSolution]]

  # echov element_lines
  # pprintStackTrace()
  for i, ln in rmpairs(element_lines):
    var ln_layout = none(LytSolution)

    if i == element_lines.high:
      ln_layout = rest_of_line
      # if i < len(element_lines) - 1:
      #   none(LytSolution)
      # else:
      #   rest_of_line

    for idx, elt in rmpairs(ln):
      # echov "--", elt
      ln_layout = elt.optLayout(ln_layout, opts)

    line_solns.add ln_layout

  let soln = vSumSolution(line_solns.filterIt(it.isSome()).mapIt(it.get()))

  return some soln.plusConst(float(opts.linebreakCost * (len(line_solns) - 1)))


proc doOptChoiceLayout(
  self: var LytBlock, rest_of_line: var Option[LytSolution],
  opts: LytOptions): Option[LytSolution] =
  # The optimum layout of this block is simply the piecewise minimum of its
  # elements' layouts.
  return minSolution(
    self.elements.mutMapIt(
      it.optLayout(rest_of_line, opts)
    ).get()
  )


proc doOptStackLayout(
  self: var LytBlock,
  rest_of_line: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =
  # The optimum layout for this block arranges the elements vertically. Only
  # the final element is composed with the continuation provided---all the
  # others see an empty continuation ("None"), since they face the end of
  # a line.
  if self.elements.len == 0:
    return rest_of_line

  let soln = vSumSolution: get: collect(newSeq):
    for idx, elem in mpairs(self.elements):
      if idx < self.elements.high:
        none(LytSolution).withResIt do:
          optLayout(elem, it, opts)
      else:
      # if idx == self.elements.len - 1:
        elem.optLayout(restOfLine, opts)
      # else:


  # Under some odd circumstances involving comments, we may have a
  # degenerate solution.
  # WARNING
  if soln.layouts.len == 0:
    return rest_of_line
  # if soln.isNone():


  # Add the cost of the line breaks between the elements.
  return some soln.plusConst float(
    opts.linebreakCost * self.break_mult *
    max(len(self.elements) - 1, 0))


proc doOptWrapLayout(
  self: var LytBlock,
  rest_of_line: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =
  # Computing the optimum layout for this class of block involves
  # finding the optimal packing of elements into lines, a problem
  # which we address using dynamic programming.
  var sep_layout = (makeTextBlock(self.sep), none(LytSolution)).withResIt do:
    it[0].optLayout(it[1], opts)

  # TODO(pyelland): Investigate why OptLayout doesn't work here.
  var prefix_layout: Option[LytSolution] =
    if self.prefix.isSome():
      (makeTextBlock(self.prefix.get()), none(LytSolution)).withResIt do:
        it[0].doOptLayout(it[1], opts)
    else:
      none(LytSolution)

  var elt_layouts: seq[Option[LytSolution]] = self.wrapElements.mutMapIt(
    block:
      var tmp = none(LytSolution)
      it.optLayout(tmp, opts)
  )

  # Entry i in the list wrap_solutions contains the optimum layout for the
  # last n - i elements of the block.
  var wrap_solutions: seq[Option[LytSolution]] =
    self.len.newSeqWith(none(LytSolution))

  # Note that we compute the entries for wrap_solutions in reverse
  # order, at each iteration considering all the elements from i ... n
  # - 1 (the actual number of elements considered increases by one on
  # each iteration). This means that the complete solution, with
  # elements 0 ... n - 1 is computed last.
  for i in countdown(self.len - 1, 0): # XXXX
    # To calculate wrap_solutions[i], consider breaking the last n - i
    # elements after element j, for j = i ... n - 1. By induction,
    # wrap_solutions contains the optimum layout of the elements after
    # the break, so the full layout is calculated by composing a line
    # with the elements before the break with the entry from
    # wrap_solutions corresponding to the elements after the break.
    # The optimum layout to be entered into wrap_solutions[i] is then
    # simply the minimum of the full layouts calculated for each j.
    var solutions_i: seq[LytSolution]
    # The layout of the elements before the break is built up incrementally
    # in line_layout.
    var line_layout: Option[LytSolution] =
      if prefix_layout.isNone():
        elt_layouts[i]
      else:
        prefix_layout.withRestOfLine(elt_layouts[i], opts)

    var last_breaking: bool = self.wrapElements[i].is_breaking
    for j in i ..< self.len - 1: # XXXX
      let full_soln = vSumSolution(
        @[line_layout, wrap_solutions[j + 1]].get())
      # We adjust the cost of the full solution by adding the cost of
      # the line break we've introduced, and a small penalty
      # (_options.cpack) to favor (ceteris paribus) layouts with
      # elements packed into earlier lines.
      solutions_i.add(full_soln.plusConst float(
        opts.linebreakCost * self.break_mult + opts.cpack * (self.len - j)))
      # If the element at the end of the line mandates a following
      # line break, we're done.
      if last_breaking:
        break
      # Otherwise, add a separator and the next element to the line
      # layout and continue.
      var sep_elt_layout = sep_layout.withRestOfLine(
        elt_layouts[j + 1], opts)

      line_layout = line_layout.withRestOfLine(sep_elt_layout, opts)
      last_breaking = self.wrapElements[j + 1].is_breaking

    if not last_breaking:
      solutions_i.add line_layout.withRestOfLine(rest_of_line, opts).get()

    wrap_solutions[i] = minSolution(solutions_i)
  # Once wrap_solutions is complete, the optimum layout for the entire
  # block is the optimum layout for the last n - 0 elements.
  return wrap_solutions[0]

proc doOptVerbLayout(
  self: var LytBlock,
  rest_of_line: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =
  # The solution for this block is essentially that of a TextBlock(''), with
  # an abberant layout calculated as follows.
  var l_elts: seq[LayoutElement]

  for i, ln in self.textLines:
    if i > 0 or self.first_nl:
      l_elts.add lytNewLine()

    l_elts.add lytString(ln)

  let layout = initLayout(l_elts)
  let span = 0
  var sf: LytSolutionFactory
  if opts.leftMargin > 0:  # Prevent incoherent solutions
    sf.add(0, span, 0, 0, layout)
  # opts.rightMargin == 0 is absurd
  sf.add(opts.leftMargin - span, span, 0, opts.leftMarginCost, layout)
  sf.add(
    opts.rightMargin - span, span,
    (opts.rightMargin - opts.leftMargin) * opts.leftMarginCost,
    opts.leftMarginCost + opts.rightMarginCost, layout)

  return some sf.makeSolution()

proc doOptLayout*(
    self: var LytBlock,
    rest_of_line: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =

  case self.kind:
    of bkText:   result = self.doOptTextLayout(restOfLine, opts)
    of bkLine:   result = self.doOptLineLayout(restOfLine, opts)
    of bkChoice: result = self.doOptChoiceLayout(restOfLine, opts)
    of bkStack:  result = self.doOptStackLayout(restOfLine, opts)
    of bkWrap:   result = self.doOptWrapLayout(restOfLine, opts)
    of bkVerb:   result = self.doOptVerbLayout(restOfLine, opts)
    of bkEmpty: discard

const defaultFormatOpts* = LytOptions(
  leftMargin: 0,
  rightMargin: 80,
  leftMarginCost: 0.05,
  rightMarginCost: 100,
  linebreakCost: 2,
  indentSpaces: 2,
  cpack: 0.001,
  formatPolicy: LytFormatPolicy(
    breakElementLines: (
      proc(blc: seq[seq[LytBlock]]): seq[seq[LytBlock]] =
        echov blc
        let spaceText = makeTextBlock(" ")
        func strippedLine(line: seq[LytBlock]): LytBlock =
          var leftSpaces, rightSpaces: int

          for idx, bl in pairs(line):
            if bl == spaceText:
              leftSpaces = idx
            else:
              break

          for idx, bl in rpairs(line):
            if bl == spaceText:
              rightSpaces = idx
            else:
              break

          return makeLineBlock(line[(leftSpaces)..(rightSpaces)])


        if blc.len > 1:
          result.add @[makeIndentBlock(
            makeStackBlock(blc[1..^1].map(strippedLine)),
            2 * 2 # FIXME use indentation from
          )]

        result.add @[blc[0]]
    )
  )
)




proc layoutBlock*(blc: LytBlock, opts: LytOptions = defaultFormatOpts): string =
  ## Perform search optimal block layout and return string
  ## reprsentation of the most cost-effective one
  var blocks = blc
  let sln = none(LytSolution).withResIt do:
    blocks.doOptLayout(it, opts).get()

  sln.layouts[0].printOn(result)

proc wrapBlocks*(blocks: seq[LytBlock],
                 opts: LytOptions = defaultFormatOpts,
                 margin: int = opts.rightMargin
                ): string =
  ## Create wrap block and return most optimal layout for it
  var opts = opts
  opts.rightMargin = margin
  layoutBlock(makeWrapBlock(blocks), opts)

proc stackBlocks*(
  blocks: seq[LytBlock], opts: LytOptions = defaultFormatOpts): string =
  ## Return string representation of the most optimal layout for
  ## vertically stacked blocks.
  layoutBlock(makeStackBlock(blocks), opts)



type
  LytBuilderKind* = enum
    blkLine
    blkStack
    blkText
    blkIndent
    blkSpace
    blkChoice
    blkEmpty

proc `[]`*(b: static[LytBuilderKind], s: seq[LytBlock]): LytBlock =
  static: assert b in {blkLine, blkStack, blkChoice}, $b

  case b:
    of blkLine:
      makeLineBlock(s)

    of blkStack:
      makeStackBlock(s)

    of blkChoice:
      makeChoiceBlock(s)

    else:
      raiseAssert("#[ IMPLEMENT ]#")


proc `[]`*(b: static[LytBuilderKind], bl: LytBlock, args: varargs[LytBlock]): LytBlock =
  b[@[ bl ] & toSeq(args)]

proc `[]`*(b: static[LytBuilderKind], a: string | ColoredString): LytBlock =
  staticAssert(
    b == blkText,
    "Single-argument block builder for string must use `T[\"somestring\"]`",
    "Change builder kind to `T` (current kind is " & $b & ")",
    hxInfo()
  )

  return makeTextBlock(a)

proc `[]`*(b: static[LytBuilderKind], tlen: int = 1): LytBlock =
  case b:
    of blkSpace: result = makeTextBlock(" ".repeat(tlen))
    of blkEmpty: result = makeEmptyBlock()
    of blkLine: result = makeLineBlock(@[])
    of blkChoice: result = makeChoiceBlock(@[])
    of blkStack: result = makeStackBlock(@[])
    else:
      staticAssert(
        b in {blkSpace, blkLine, blkChoice, blkStack, blkEmpty},
        "Block builder without arguments must use space or " &
          "combinator layouts",
        "Unexpected builder kind",
        hxInfo()
      )




proc `[]`*(b: static[LytBuilderKind], i: int, bl: LytBlock): LytBlock =
  static: assert b == blkIndent
  return makeIndentBlock(bl, i)

func `&?`*(bl: LytBlock, added: tuple[condOk: bool, bl: LytBlock]): LytBlock =
  result = bl
  if added.condOk:
    result.add added.bl

func join*(
  blocks: LytBlock, sep: LytBlock, vertLines: bool = true): LytBlock =
  assert blocks.kind in {bkLine, bkStack},
    "Only stack or line layouts can be joined"

  result = makeBlock(blocks.kind)

  case blocks.kind:
    of bkLine:
      result.height = max(blocks.elements.maxIt(it.height), sep.height)
      result.width = blocks.sumIt(it.width) + (blocks.len - 1) * sep.width

    of bkStack:
      if vertLines:
        result.height = blocks.sumIt(it.height)
        result.width = max(blocks.maxIt(it.width), sep.width)

      else:
        result.height = blocks.sumIt(it.height) + (blocks.len - 1) * sep.height
        result.width = blocks.maxIt(it.width) + sep.width

    else:
      discard





  for isLast, item in itemsIsLast(blocks):
    if blocks.kind == bkStack and vertLines:
      if not isLast:
        result.add makeLineBlock([item, sep])

      else:
        result.add item

    else:
      result.add item
      if not isLast:
        result.add sep


func padSpaces*(bl: var LytBlock) =
  func aux(bl: var LytBlock, indent: var int, first: bool) =
    let baseIndent = indent
    if first and bl.height == 1: return

    case bl.kind:
      of bkText:
        if indent > 0 and not first:
          bl.text = repeat(" ", indent) & bl.text

      of bkLine:
        if bl.height == 1 and indent > 0:
          aux(bl.elements[0], indent, first)

        else:
          for toplevel in mitems(bl.elements):
            aux(toplevel, indent, first)
            indent += toplevel.width
            # if toplevel.height > 1:
            #   if toplevel.kind == bkStack:

            #   elif toplevel.kind == bkChoice:
            #     for choice in mitems(toplevel.elements):
            #       var indent = indent
            #       aux(choice, indent, first)


      of bkStack:
        for idx, item in mpairs(bl.elements):
          var indent = indent
          aux(item, indent, idx == 0)


      of bkChoice:
        for item in mitems(bl.elements):
          var indent = baseIndent
          aux(item, indent, first)

      else:
        raiseImplementError(&"For kind {bl.kind} {instantiationInfo()}")

  var indent = 0
  aux(bl, indent, true)

proc toString*(
  bl: LytBlock,
  rightMargin: int = 80, fixLyt: bool = true): string =
  var bl = bl
  if fixLyt:
    padSpaces(bl)

  let opts = defaultFormatOpts.withIt do:
    it.rightMargin = rightMargin

  let sln = none(LytSolution).withResIt do:
    bl.doOptLayout(it, opts).get()

  sln.layouts[0].printOn(result)

func codegenRepr*(inBl: LytBlock): string =
  func aux(bl: LytBlock, level: int): string =
    let pref = repeat("  ", level)
    let name =
      case bl.kind:
        of bkEmpty: "empty"
        of bkLine: "hsb"
        of bkChoice: "choice"
        of bkText: "txb"
        of bkWrap: "wrap"
        of bkStack: "vsb"
        of bkVerb: "verb"

    case bl.kind:
      of bkLine, bkChoice, bkStack, bkWrap:
        result = pref & name & "([\n"
        for isLast, elem in itemsIsLast(bl.elements):
          result &= elem.aux(level + 1) & (if isLast: "\n" else: ",\n")

        result &= pref & "])"

      of bkText:
        result = &"{pref}txb({bl.text.escape()})"

      of bkVerb:
        result = pref & name & "([\n"
        for isLast, line in itemsIsLast(bl.textLines):
          result &= &"{pref}  \"{line}\"" & (if isLast: "\n" else: ",\n")

        result &= pref & "])"

      of bkEmpty:
        result = "empty"

  return "str(" & aux(inBl, 0) & ")"


template initBlockFmtDSL*() {.dirty.} =
  const
    H = blkLine
    V = blkStack
    T = blkText
    I = blkIndent
    S = blkSpace
    C = blkChoice
    E = blkEmpty



when isMainModule:
  block:
    let content = (0 .. 10).mapIt(makeTextBlock &"[ {it} ]")

    for blocks in @[
      makeLineBlock(content), makeStackBlock(content), makeWrapBlock(content)]:
      var blocks = blocks
      let sln = none(LytSolution).withResIt do:
        blocks.doOptLayout(it, defaultFormatOpts).get()

      var c = LytConsole()
      sln.layouts[0].printOn(c)
      echo c.text

  block:
    let content = (0 .. 5).mapIt(makeTextBlock &"[ {it} ]") & @[
      makeVerbBlock(@["hello", "world"], firstNl = true)
    ] & (5 .. 10).mapIt(makeTextBlock &"[ {it} ]")

    var blocks = makeWrapBlock(content)
    let sln = none(LytSolution).withResIt do:
      blocks.doOptLayout(it, defaultFormatOpts).get()

    var c = LytConsole()
    sln.layouts[0].printOn(c)
    echo c.text
