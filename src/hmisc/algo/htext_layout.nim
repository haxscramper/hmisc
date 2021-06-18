import std/[math, lenientops]

func `+=`(f: var float, i: int) = f += float(i)

type
  NodeKind = enum
    tBox
    tGlue
    tPenalty

  Node = ref object
    position: int
    line: int

    kind: NodeKind
    width: int
    shrinkability: int
    stretchability: int
    penalty: int
    flaggedPenalty: int



    fitness: int
    totalshrink: int
    totaldemerits: int
    totalstretch: int
    totalwidth: int

    previous: Node
    link: Node

var
  activeFirst: Node = Node(
    position: 0,
    line: 0,
    fitness: 0,
    totalwidth: 0,
    totalstretch: 0,
    totalshrink: 0,
    totaldemerits: 0,
    previous: nil,
    link: nil
  )

  passiveFirst: Node

var
  sumW, sumY, sumZ = 0

var paragraph: seq[Node]

const
  t = 0
  w = 1
  l = 2
  p = 3
  f = 4
  y = 5
  z = 6
  posInfty = 9000
  negInfty = -9000
  alpha = 1 # QUESTION
  gamma = 1 # QUESTION

template `[]`(selector, idx: int): untyped =
  when selector == t:
    paragraph[idx].kind

  elif selector == w:
    paragraph[idx].width

  elif selector == l:
    # WARNING FIXME find out what `l` refers to
    999

  elif selector == p:
    paragraph[idx].penalty

  elif selector == f:
    paragraph[idx].flaggedPenalty

  elif selector == y:
    paragraph[idx].stretchability

  elif selector == z:
    paragraph[idx].shrinkability

  else:
    error("IANAL" & selector.astToStr())

let m = paragraph.len

template computeDemerits {.dirty.} =
  var d: float
  if p[b] >= 0:
    d = (1 + 100 * abs(r^3) + p[b])^2
  elif p[b] != negInfty:
    d = (1 + 100 * abs(r^3))^2 - p[b]^2
  else:
    d = (1 + 100 * abs(r^3))^2

  d += alpha * f[b] * f[a.position]
  var c: int # Line class
  if r < -5: c = 0
  elif r <= 0.5: c = 1
  elif r <= 1: c = 2
  else: c = 3

  if abs(c - a.fitness) > 1:
    d += gamma

  d += a.totaldemerits

template computeTwyz {.dirty.} =
  tw = sumW
  ty = sumY
  tz = sumZ
  i = b
  while true:
    if i > m: break
    if t[i] == tBox: break
    if t[i] == tGLue:
      tw += w[i]
      ty += y[i]
      tz += z[i]

    elif p[i] == `-infty` and i > b:
      break

    inc i

template deactivateNode {.dirty.} =
  if preva == nil:
    activeFirst = nexta
  else:
    preva.link = nexta

  a.link = passiveFirst
  passiveFirst = a

template insertNewActive {.dirty.} =
  computeTwyz()

  for c in 0 .. 3:
    if D_c <= D + gamma:
      s = Node(
        position: b,
        line: line(A_c) + 1,
        fitness: c,
        totalwidth: tw,
        totalstretch: ty,
        totalshrink: tz,
        totaldemerits: D,
        previous: A_c,
        link: a
      )

      if preva == nil: activeFirst = d else: preva.link = s
      preva = s


template computeAdjustments {.dirty.} =
  var L = sumW - a.totalwidth
  var r = 0.0 # Adjustment ratio
  if t[b] == tPenalty:
    L += w[b]
  let j = a.line + 1
  if L < l[j]:
    var Y = sumY - a.totalstretch
    if Y > 0:
      r = (l[j] - L) / Y
    else:
      r = posInfty
  elif L > l[j]:
    var Z = sumZ - a.totalshrink
    if Z > 0:
      r = (l[j] - L) / Z
    else:
      r = posInfty
  else:
    r = 0


template mainLoop {.dirty.} =
  var
    a = activeFirst
    prevA: Node

  while true:
    var
      D_0 = posInfty
      D_1 = posInfty
      D_2 = posInfty
      D_3 = posInfty
      D = posInfty.float


    while true:
      var nexta = a.link
      computeAdjustments()
      if r < -1 or p[b] == negInfty:
        deactivateNode()
      else:
        prevA = a

      if -1 <= r and r <= p:
        var D_c = float(a.totaldemerits) # QUESTION
        var A_c: Node = nil
        computeDemerits()
        if d < D_c:
          D_c = d
          A_c = a
          if d < D:
            D = d

      a = nexta
      if a == nil: break
      var j_0 = 0 # FIXME page `1159` contains exmplanation for the logic of `j_0`
      if a.line >= j and j < j_0: break

    if D < negInfty:
      # TODO insert new active nodes for breaks from `A_c` to `b`
      discard

    if a == nil:
      break

  if activeFirst == nil:
    raiseAssert("#[ IMPLEMENT ]#")


template chooseFewestDemerits {.dirty.} =
  ## Choose the active node with fewest total demerits
  # WARNING original code was `if a := b := A`
  a = activeFirst
  b = activeFirst
  d = a.totaldemerits
  while true:
    a = a.link
    if a == nil: break
    if a.totaldemerits < d:
      d = totaldemerits
      b = a
  k = b.line

template chooseActive {.dirty.} =
  # Choose appropriate active nodes
  a = activeFirst
  s = 0
  while true:
    delta = a.line - k
    if (q <= delta and delta < s) or
        (s < delta and delta <= q):
        s = delta
        d = a.totaldemerits
        b = a
    elif delta == s and a.totaldemerits < d:
        d = a.totaldemerits
        b = a
    a = a.link
    if a == nil:
        break

  k = b.line

template breakpoinSequence {.dirty.} =
  # Use the chosen node to determine the optimum breakpoint
  # sequence
  for j in countdown(k, 1):
    b[j] = b.position
    b = b.previous

for b in 0 ..< m: # nim has zero-based index for arrays
    if t[b] == tBox:
      sumW += sumW + w[b]
    elif t[b] == tGLue:
      if t[b - 1] == tBox:
        mainLoop()

      sumW += w[b]
      sumY += y[b]
      sumZ += z[b]
    elif p[b] != posInfty:
      mainLoop()
