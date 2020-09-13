## Nim reimplementation of layout algorithm from
## https://github.com/google/rfmt.

# Thanks to nim being syntactically close to python this is mostly
# just blatant copy-paste of python code with added type annotations.

import strutils, sequtils, macros, tables, strformat, lenientops,
       options, hashes, math, sugar

import nimtraits
import ../algo/[hmath, halgorithm]
import colorlogger

startColorLogger()

const infty = 1024 * 1024 * 1024 * 1024

func inf*(a: int): bool = (infty - 4096 <= a) and (a <= infty + 4096)

func `*`(a: SomeNumber, b: bool): SomeNumber = (if b: a else: 0)
func get*[T](inseq: seq[Option[T]]): seq[T] =
  for elem in inseq:
    if elem.isSome():
      result.add elem.get()

#*************************************************************************#
#***************************  Printer console  ***************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

type
  Console = object
    margins: seq[int]

proc printString(c: Console, str: string): void =
  stdout.write(str)

proc printSpace(c: Console, n: int): void =
  ## Write a string of `n` spaces on the console.
  c.printString(" ".repeat(n))

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
    ## Refer to the corresponding methods of the Console class for
    ## descriptions of the methods involved.
    impl: proc(pr: Console)

  Layout = object
    ## An object containing a sequence of directives to the console.
    elements: seq[LayoutElement]

  Solution = object
    knots: seq[int]
    spans: seq[int]
    intercepts: seq[float]
    gradients: seq[float]
    layouts: seq[Layout]
    index: int

  BlockKind = enum
    bkText
    bkLine
    bkChoice
    bkStack
    bkWrap
    bkVerb

  Block = object
    layout_cache: Table[Option[Solution], Option[Solution]]
    isBreaking: bool
    case kind: BlockKind
      of bkVerb:
        textLines: seq[string]
        firstNl: bool
      of bkText:
        text: string
      of bkWrap:
        prefix: Option[string]
        sep: string
        breakMult: int
        wrapElements: seq[Block]
      of bkStack, bkChoice, bkLine:
        elements: seq[Block]

  FormatPolicy[Tree] = object
    ## Formatter protocol
    ##
    ## Generally speaking, a parse tree is formatted from the leaves
    ## up, with the appropriate formatting methods called on the
    ## fields of a node before the results are provided (along with
    ## the node itself) to the formatting method for the node. Some
    ## policies, however, may choose to format nodes in a
    ## context-dependent manner (the right-hand side of an assignment,
    ## for example, might be formatted differently from the identical
    ## expression that appears as a statement by itself). In such
    ## instances, information about the context may be (temporarily)
    ## stored in the policy object, before some or all of the node's
    ## fields are formatted again. By default, the results of
    ## formatting a parse node are memoised by this superclass, so use
    ## the method Reformat() to effect the reformatting of a node's
    ## field. See the class GoogleFormatPolicy (in package
    ## google_format) for examples.
    ##
    ## This kind of bottom-up/top-down formatting does result in some
    ## redundant computation, in as far as the formatted versions of
    ## the fields passed into a formatting method may be discarded.
    ## However, memoisation (which ensures that the grandchildren of a
    ## reformatted node are by default not themselves reformatted)
    ## keeps such redundant computation to a minimum. Furthermore,
    ## considerable convenience is afforded writers of FormatPolicy
    ## (sub)classes by preformatting a node's fields rather than
    ## having policy writer format them explicitly.

    blocksFor: proc(tree: Tree): seq[Block] ## A LayoutBlock at the
    ## root of nested collection of such blocks that yields the layout
    ## of the code represented in the parse tree.

    breakElementLines: proc(blc: seq[seq[Block]]): seq[seq[Block]] ## Hook
    ## for formatting around comment-induced line breaks.

    # formats: Table[string, ]

proc printOn(self: Layout, console: Console): void =
  for elem in self.elements:
    elem.impl(console)

proc printLayout(self: Console, layout: Layout): void =
  layout.printOn(self)

proc `$`(blc: Block): string =
  case blc.kind:
    of bkText: &"\"{blc.text}\""
    of bkStack: blc.elements.mapIt($it).join(" ↕ ").wrap("()")
    of bkLine: blc.elements.mapIt($it).join(" ↔ ").wrap("()")
    of bkChoice: blc.elements.mapIt($it).join(" ? ").wrap("()")
    of bkWrap: blc.wrapElements.mapIt($it).join(" ").wrap("[]")
    of bkVerb: &">>{blc.text}<<"



#*************************************************************************#
#************************  Options configuration  ************************#
#*************************************************************************#
type
  StrTree = object
    val: string
    subn: seq[StrTree]

type
  Options = object
    m0: int ## position of the first right margin
    m1: int ## position of the second right margin
    c0: float ## cost (per character) beyond margin 0
    c1: float ## cost (per character) beyond margin 1
    cb: int ## cost per line-break
    ind: int ## spaces per indent
    # adj_comment: int
    # adj_flow: int #
    # adj_call: int
    # adj_arg: int
    cpack: int ## cost (per element) for packing justified layouts
    format_policy: FormatPolicy[StrTree]

let opts = Options(
  m0: 0,
  m1: 80,
  c0: 0.05,
  c1: 100,
  cb: 2,
  ind: 2
)

func hash(elem: LayoutElement): Hash = hash(elem.impl)
func hash(lyt: Layout): Hash = hash(lyt.elements)

func hash(sln: Option[Solution]): Hash =
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
  result.impl = proc(c: Console) = stdout.write s

func lytNewline(indent: bool = true): LayoutElement =
  result.impl = proc(c: Console) = stdout.write "\n"

func lytPrint(lyt: Layout): LayoutElement =
  result.impl = proc(c: Console) = c.printLayout(lyt)

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

func initLayout*(elems: openarray[LayoutElement]): Layout =
  result.elements = toSeq(elems)

#*************************************************************************#
#******************************  Solution  *******************************#
#*************************************************************************#

func initSolution*(
    knots: seq[int], spans: seq[int], intercepts: seq[float],
    gradients: seq[float], layouts: seq[Layout]): Solution =

  Solution(
    knots: knots, spans: spans, intercepts: intercepts,
    gradients: gradients, layouts: layouts)

#===========================  Helper methods  ============================#
func reset(self: var Solution) =
  ## Begin iteration.
  self.index = 0

func advance(self: var Solution) =
  ## Advance to the next knot.
  self.index += 1

func retreat(self: var Solution) =
  ## Move back a knot.
  self.index -= 1

func curKnot(self: Solution): int =
  ## The currently indexed knot.
  return self.knots[self.index]

func curSpan(self: Solution): int =
  return self.spans[self.index]

func curIntercept(self: Solution): float =
  return self.intercepts[self.index]

func curGradient(self: Solution): float =
  return self.gradients[self.index]

func curLayout(self: Solution): Layout =
  return self.layouts[self.index]

func curIndex(self: Solution): int =
  return self.index

func curValueAt(self: Solution, margin: int): float =
  ## The value (cost) extrapolated for margin m from the current knot.
  # Since a Solution's cost is represented by a piecewise linear function,
  # the extrapolation in this case is linear, from the current knot.
  return self.curIntercept() + self.curGradient() * float(margin - self.curKnot())

func nextKnot(self: Solution): int =
  ## The knot after the once currently indexed.
  if self.index + 1 >= self.knots.len:
    infty
  else:
    self.knots[self.index + 1]

proc moveToMargin(self: var Solution, margin: int) =
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


#==========================  Solution factory  ===========================#

type
  SolutionFactory = object
    ## A factory object used to construct new Solution objects.
    ##
    ## The factory performs basic consistency checks, and eliminates
    ## redundant segments that are linear extrapolations of those that
    ## precede them.
    entries: seq[tuple[
      knot, span: int, intercept, gradient: float, lyt: Layout]]

func add(
  self: var SolutionFactory,
  knot, span: int, intercept, gradient: float, layout: Layout): void =
  ## Add a segment to a Solution under construction.
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

func makeSolution(self: SolutionFactory): Solution =
  ## Construct and return a new Solution with the data in this
  ## object
  for (k, s, i, g, l) in self.entries:
    result.knots.add k
    result.spans.add s
    result.intercepts.add i
    result.gradients.add g
    result.layouts.add l

#=====================  Solution manipulation logic  =====================#


proc minSolution(solutions: seq[Solution]): Option[Solution] =
  ## Form the piecewise minimum of a sequence of Solutions.

  ## Args:
  ##   solutions: a non-empty sequence of Solution objects
  ## Returns:
  ##   values Solution object whose cost is the piecewise minimum of the Solutions
  ##   provided, and which associates the minimum-cost layout with each piece.
  if len(solutions) == 1:
    return some(solutions[0])

  var
    factory: SolutionFactory
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
        # Add another piece to the new Solution
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

proc vSumSolution(solutions: seq[Solution]): Solution =
  ## The layout that results from stacking several Solutions vertically.
  ## Args:
  ##   solutions: a non-empty sequence of Solution objects
  ## Returns:
  ##   A Solution object that lays out the solutions vertically, separated by
  ##   newlines, with the same left margin.


  assert solutions.len > 0

  if len(solutions) == 1:
    return solutions[0]

  var solutions = solutions # XXXX

  var col: SolutionFactory

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

proc hPlusSolution(s1, s2: Solution): Solution =
  ## The Solution that results from joining two Solutions side-by-side.

  ## Args:
  ##   `s1`: Solution object
  ##   `s2`: Solution object
  ## Returns:
  ##   A new Solution reflecting a layout in which `s2` ('s layout) is
  ##   placed immediately to the right of `s1`.

  ## The resulting Solution object maps each prospective left margin m
  ## to the span, cost and layout information that would result from
  ## siting Solution `s1` at m, and then placing `s2` at margin `m +
  ## sp1(m)`, where `sp1(m)` is the span of characters occupied by the
  ## layout to which `s1` maps m. In general, of course, both s1 and
  ## `s2`'s layouts may occupy multiple lines, in which case `s2`'s
  ## layout begins at the end of the last line of `s1`'s layout---the
  ## span in this case is the span of `s1`'s last line.
  var
    s1 = s1
    s2 = s2

  var col: SolutionFactory
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
      overhang0 = s2_margin - opts.m0  # s2_margin = m1 + span of s1
      overhang1 = s2_margin - opts.m1  # s2_margin = m1 + span of s1
      g_cur = (g1 + g2 -
               opts.c0 * (overhang0 >= 0) -
               opts.c1 * (overhang1 >= 0))
      i_cur = (s1.curValueAt(s1_margin) + s2.curValueAt(s2_margin) -
               opts.c0 * max(overhang0, 0) -
               opts.c1 * max(overhang1, 0))

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

    if kn1 == infty and kn2 == infty:
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




func plusConst(self: Solution, val: float): Solution =
  ## Add a constant to all values of this Solution.
  result = self
  for a in mitems(result.intercepts):
    a += val

proc withRestOfLine(
  self: Option[Solution],
  rest_of_line: Option[Solution]): Option[Solution] =
  ## Return a Solution that joins the rest of the line right of this one.

  ## Args:
  ##   rest_of_line: a Solution object representing the code laid out on the
  ##     remainder of the line, or None, if the rest of the line is empty.
  ## Returns:
  ##   A new Solution object juxtaposing the layout represented by this
  ##   Solution to the immediate right of the remainder of the line.
  if restOfLine.isNone():
    self
  else:
    some self.get().hplusSolution(restOfLine.get())


#*************************************************************************#
#*****************************  Block type  ******************************#
#*************************************************************************#
proc elements(self: Block): seq[Block] =
  if contains({bkWrap}, self.kind):
    return self.elements
  if contains({bkStack, bkChoice, bkLine}, self.kind):
    return self.elements
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `elements=`(self: var Block; it: seq[Block]) =
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

func len*(blc: Block): int =
  case blc.kind:
    of bkWrap: blc.wrapElements.len()
    else: blc.elements.len()


#============================  Constructors  =============================#

func makeTextBlock*(text: string): Block =
  Block(kind: bkText, text: text)

func makeIndentBlock*(blc: Block, indent: int): Block =
  Block(kind: bkLine, elements: @[makeTextBlock(" ".repeat(indent)), blc])

func makeChoiceBlock*(elems: seq[Block]): Block =
  Block(kind: bkChoice, elements: elems)

func makeStackBlock*(elems: seq[Block]): Block =
  Block(kind: bkStack, elements: elems)

func makeLineBlock*(elems: seq[Block]): Block =
  Block(kind: bkLine, elements: elems)

func makeWrapBlock*(elems: seq[Block]): Block =
  Block(kind: bkWrap, wrapElements: elems)

#============================  Layout logic  =============================#

proc doOptLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution]

proc optLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution] =
  ## Retrieve or compute the least-cost (optimum) layout for this block.
  ##
  ## Args:
  ##   rest_of_line: a Solution object representing the text to the
  ##     right of this block.
  ##
  ## Returns:
  ##   A Solution object representing the optimal layout for this
  ##   block and the rest of the line.
  # Deeply-nested choice block may result in the same continuation
  # supplied repeatedly to the same block. Without memoisation, this
  # may result in an exponential blow-up in the layout algorithm.
  if rest_of_line notin self.layout_cache:
    self.layout_cache[rest_of_line] = self.doOptLayout(rest_of_line)

  return self.layout_cache[rest_of_line]

proc doOptTextLayout(
  self: Block, rest_of_line: Option[Solution]): Option[Solution] =

  let
    span = len(self.text)
    layout = initLayout([lytString(self.text)])
  # The costs associated with the layout of this block may require 1, 2 or 3
  # knots, depending on how the length of the text compares with the two
  # margins (m0 and m1) in opts. Note that we assume
  # opts.m1 >= opts.m0 >= 0, as asserted in base.Options.Check().
  if span >= opts.m1:
    result = some initSolution(
      @[0],
      @[span],
      @[float((span - opts.m0) * opts.c0 + (span - opts.m1) * opts.m1)],
      @[float(opts.c0 + opts.c1)],
      @[layout]
    )

  elif span >= opts.m0:
    result = some initSolution(
      @[0, opts.m1 - span],
      @[span, span], # XXXX
      @[float((span - opts.m0) * opts.c0),
        float((opts.m1 - opts.m0) * opts.c0)],
      @[float(opts.c0), float(opts.c0 + opts.c1)],
      @[layout, layout] # XXXX
    )
  else:
    result = some initSolution(
      @[0, opts.m0 - span, opts.m1 - span],
      @[span, span, span], # XXXX
      @[float(0), float(0), float((opts.m1 - opts.m0) * opts.c0)],
      @[float(0), float(opts.c0), float(opts.c0 + opts.c1)],
      @[layout, layout, layout] # XXXX
    )

  return result.withRestOfLine(rest_of_line)


proc doOptLineLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution] =
  if self.elements.len == 0:
    return rest_of_line

  var element_lines: seq[seq[Block]] = @[]
  element_lines.add @[]

  for i, elt in self.elements:
    element_lines[^1].add elt

    if i < len(self.elements) - 1 and elt.is_breaking:
      element_lines.add @[]

  if len(element_lines) > 1:
    element_lines = opts.format_policy.breakElementLines(element_lines)

  var line_solns: seq[Option[Solution]]

  for i, ln in mpairs(element_lines):
    var ln_layout: Option[Solution] =
      if i < len(element_lines) - 1:
        none(Solution)
      else:
        rest_of_line

    for idx, elt in mpairs(ln):
      if idx < ln.len - 1: # XXXX
        ln_layout = elt.optLayout(ln_layout)

    line_solns.add ln_layout

  let soln = vSumSolution(line_solns.filterIt(it.isSome()).mapIt(it.get()))

  return some soln.plusConst(float(opts.cb * (len(line_solns) - 1)))


proc doOptChoiceLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution] =
  # The optimum layout of this block is simply the piecewise minimum of its
  # elements' layouts.
  return minSolution(
    self.elements.mapIt(
      it.withResIt do:
        it.optLayout(rest_of_line)
    ).get()
  )


proc doOptStackLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution] =
  discard


proc doOptWrapLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution] =
  # Computing the optimum layout for this class of block involves
  # finding the optimal packing of elements into lines, a problem
  # which we address using dynamic programming.
  let sep_layout = makeTextBlock(self.sep).withResIt do:
    it.optLayout(none(Solution))

  # TODO(pyelland): Investigate why OptLayout doesn't work here.
  let prefix_layout: Option[Solution] =
    if self.prefix.isSome():
      makeTextBlock(self.prefix.get()).withResIt do:
        it.doOptLayout(none(Solution))
    else:
      none(Solution)

  var elt_layouts: seq[Option[Solution]] = self.wrapElements.mapIt(
    it.withResIt do: it.optLayout(none(Solution)))

  # Entry i in the list wrap_solutions contains the optimum layout for the
  # last n - i elements of the block.
  var wrap_solutions: seq[Option[Solution]] =
    self.len.newSeqWith(none(Solution))

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
    var solutions_i: seq[Solution]
    # The layout of the elements before the break is built up incrementally
    # in line_layout.
    var line_layout: Option[Solution] =
      if prefix_layout.isNone():
        elt_layouts[i]
      else:
        prefix_layout.withRestOfLine(elt_layouts[i])

    var last_breaking: bool = self.wrapElements[i].is_breaking
    for j in i ..< self.len - 1: # XXXX
      let full_soln = vSumSolution(
        @[line_layout, wrap_solutions[j + 1]].get())
      # We adjust the cost of the full solution by adding the cost of
      # the line break we've introduced, and a small penalty
      # (_options.cpack) to favor (ceteris paribus) layouts with
      # elements packed into earlier lines.
      solutions_i.add(full_soln.plusConst float(
        opts.cb * self.break_mult + opts.cpack * (self.len - j)))
      # If the element at the end of the line mandates a following
      # line break, we're done.
      if last_breaking:
        break
      # Otherwise, add a separator and the next element to the line
      # layout and continue.
      let sep_elt_layout = sep_layout.withRestOfLine(elt_layouts[j + 1])

      line_layout = line_layout.withRestOfLine(sep_elt_layout)
      last_breaking = self.wrapElements[j + 1].is_breaking

    if not last_breaking:
      solutions_i.add line_layout.withRestOfLine(rest_of_line).get()

    wrap_solutions[i] = minSolution(solutions_i)
  # Once wrap_solutions is complete, the optimum layout for the entire
  # block is the optimum layout for the last n - 0 elements.
  return wrap_solutions[0]

proc doOptVerbLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution] =
  discard

proc doOptLayout(
  self: var Block, rest_of_line: Option[Solution]): Option[Solution] =

  logIdented:
    result = case self.kind:
      of bkText: self.doOptTextLayout(restOfLine)
      of bkLine: self.doOptLineLayout(restOfLine)
      of bkChoice: self.doOptChoiceLayout(restOfLine)
      of bkStack: self.doOptStackLayout(restOfLine)
      of bkWrap: self.doOptWrapLayout(restOfLine)
      of bkVerb: self.doOptVerbLayout(restOfLine)


func tree(head: string, elems: varargs[StrTree]): StrTree =
  result.val = head
  result.subn = toSeq(elems)


let content = (makeTextBlock "[eeee]").repeat(4)
echo content
var blocks = makeChoiceBlock(
  @[
    makeLineBlock(content),
    makeStackBlock(content)
  ]
)

let sln = blocks.doOptLayout(none(Solution)).get()

let c = Console()

echo "---"
sln.layouts[0].printOn(c)

echo "\n---"

