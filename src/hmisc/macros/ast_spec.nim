import
  ../core/[all, code_errors],
  ../types/colorstring,
  ../algo/clformat,
  ./argpass

import
  std/[options, macros, sequtils, strutils]

type
  AstRangeKind = enum
    akPoint ## `idx`
    akInversePoint ## `^idx`
    akDirectSlice ## `idx1 .. idx2`
    akInverseSlice ## `^idx1 .. ^idx2`
    akMixedSlice ## `idx1 .. ^idx2`


  AstRange = object
    optional*: bool
    case kind*: AstRangeKind
      of akPoint, akInversePoint:
        idx*: int

      of akDirectSlice, akInverseSlice, akMixedSlice:
        start*: int
        finish*: int

  AstCheckFail[K] = object
    isMissing*: bool
    msg*: string
    path*: seq[int]
    parent*: K
    expected*: set[K]
    arange*: AstRange
    nested*: seq[AstCheckFail[K]]

  AstCheckProc*[N, K] = proc(node: N): Option[AstCheckFail[K]]

  AstPatternRange[N, K] = tuple[arange: AstRange, alts: seq[AstPattern[N, K]]]

  AstPattern*[N, K] = object
    check*: AstCheckProc[N, K]
    expected*: set[K]
    ranges*: seq[AstPatternRange[N, K]]

  AstSpec[N, K] = object
    spec: array[K, Option[AstPattern[N, K]]]

func astPattern*[N, K](
    expected: set[K], check: AstCheckProc[N, K] = nil): AstPattern[N, K] =
  AstPattern[N, K](expected: expected, check: check)


func astPattern*[N, K](
    expected: set[K],
    alts: openarray[AstPatternRange[N, K]],
    check: AstCheckProc[N, K] = nil
  ): AstPattern[N, K] =

  AstPattern[N, K](expected: expected, ranges: @alts, check: check)

func astPattern*[N, K](
    alts: openarray[AstPatternRange[N, K]], check: AstCheckProc[N, K] = nil
  ): AstPattern[N, K] =

  AstPattern[N, K](ranges: @alts, check: check)

func astPattern*[N, K](): AstPattern[N, K] =
  AstPattern[N, K]()

func astSpec*[N, K](
    patterns: openarray[(K, AstPattern[N, K])]): AstSpec[N, K] =
  for (kind, pattern) in patterns:
    result.spec[kind] = some pattern

func astRange*(idx: int, optional: bool = false): AstRange =
  AstRange(kind: akPoint, idx: idx)

func astRange*(
    idx: BackwardsIndex,
    optional: bool = false): AstRange =
  AstRange(kind: akInversePoint, idx: idx.int, optional: optional)

func astRange*(
    slice: Slice[int],
    optional: bool = false): AstRange =
  AstRange(
    kind: akDirectSlice, start: slice.a,
    finish: slice.b, optional: optional)

func astRange*(
    slice: HSlice[int, BackwardsIndex],
    optional: bool = false): AstRange =
  AstRange(
    kind: akMixedSlice, start: slice.a,
    finish: slice.b.int, optional: optional)

func astRange*(
    slice: Slice[BackwardsIndex],
    optional: bool = false): AstRAnge =
  AstRange(
    kind: akInverseSlice, start: slice.a.int,
    finish: slice.b.int, optional: optional)

macro astSpec*(nodeType, kindType, body: untyped): untyped =
  let
    nodeType = copyNimNode(nodeType)
    kindType = copyNimNode(kindType)

  proc call(name: string): NimNode =
    nnkBracketExpr.newTree(ident(name), nodeType, kindType)


  proc aux(node: NimNode): NimNode
  proc rangeCurly(node: NimNode): NimNode

  proc altList(node: NimNode): NimNode =
    result = nnkBracket.newTree()
    for kind in node:
      case kind.kind:
        of nnkIdent:
          if kind.eqIdent("_"):
            discard

          else:
            result.add newCall(call"astPattern", nnkCurly.newTree(kind))

        of nnkCall:
          result.add newCall(
            call"astPattern",
            nnkCurly.newTree(kind[0]),
            rangeCurly(kind[1]))

        of nnkObjConstr:
          var check = newStmtList()
          let nodeId = ident("node")

          result.add newCall(
            call"astPattern",
            nnkCurly.newTree(kind[0]),
            quote do:
              proc node(`nodeId`: `nodeType`): Option[AstCheckFail[`kindType`]] =
                discard

              node
          )

        else:
          assertNodeKind(kind, {
            nnkInfix, nnkCall, nnkObjConstr})


    result = nnkPrefix.newTree(ident"@", result)



  proc rangeCurly(node: NimNode): NimNode =
    result = nnkTableConstr.newTree()
    for check in node:
      case check.kind:
        of nnkInfix:
          if check.len == 4:
            result.add newEcE(
              newCall(
                ident"astRange",
                nnkInfix.newTree(check[0 .. 2])), altList(check[3]))

          else:
            result.add newEcE(
              newCall(ident"astRange", check),
              nnkPrefix.newTree(
                ident"@",
                nnkBracket.newTree(
                  newCall(call"astPattern"))))

        of nnkCall:
          result.add newEcE(
            newCall(ident"astRange", check[0]), altList(check[1]))

        of nnkPrefix:
          case check[0].strVal():
            of "?":
              result.add newEcE(
                newCall(ident"astRange", check[1], newLit(true)),
                altList(check[2]))

            else:
              raise check[0].toCodeError(
                "Unexpected prefix - want ?<idx> or ^<idx>")

        else:
          assertNodeKind(check, {nnkInfix, nnkCall})


  proc aux(node: NimNode): NimNode =
    case node.kind:
      of nnkStmtList:
        if allIt(
          node, it.kind == nnkIdent or
          (it.kind == nnkCall and it[0].kind == nnkIdent)):
          result = newCall(call"astPattern", altList(node))

        else:
          result = newCall(call"astPattern", rangeCurly(node))

      else:
        raise newImplementKindError(node, node.treeRepr())



  result = nnkTableConstr.newTree()
  for pattern in body:
    assertNodeKind(pattern, {nnkCall})
    result.add newEcE(pattern[0], aux(pattern[1]))

  result = newCall(call"astSpec", result)

func contains*(arange: AstRange, idx, maxLen: int): bool =
  case arange.kind:
    of akPoint: idx == arange.idx
    of akInversePoint: idx == maxLen - arange.idx
    of akDirectSlice: arange.start <= idx and idx <= arange.finish
    of akInverseSlice: (maxLen - arange.start) <= idx and idx <= (maxLen - arange.finish)
    of akMixedSlice: arange.start <= idx and idx <= (maxLen - arange.finish)

func `$`*(arange: AstRange): string =
  case arange.kind:
    of akPoint: $arange.idx
    of akInversePoint: "^" & $arange.idx
    of akDirectSlice: $arange.start & ".." & $arange.finish
    of akInverseSlice: "^" & $arange.start & "..^" & $arange.finish
    of akMixedSlice: $arange.start & "..^" & $arange.finish

func `$`*[N, K](spec: AstPattern[N, K]): string =
  result.add $spec.expected

proc toPath*[N](ast: N, path: seq[int]): string =
  mixin `[]`
  proc aux(a: N, path: seq[int]): seq[string] =
    result.add $a.kind
    if path.len > 1:
      result.add aux(a[path[0]], path[1..^1])

    elif path.len == 1:
      result.add "[" & $path[0] & "]"

  return join(aux(ast, path), ".")

proc isEmpty*[K](fail: AstCheckFail[K], withNested: bool = true): bool =
  fail.isMissing.not() and
  fail.msg.len == 0 and
  fail.expected.len == 0 and
  (if withNested: fail.nested.len == 0 else: true)

proc findMissing*[N, K](
    spec: AstPattern[N, K], node: N, path: seq[int] = @[]): AstCheckFail[K] =
  result.path = path
  # Validate all subnodes of `node` against specified subranges.
  if spec.ranges.len > 0:
    var altFound = newSeqWith(spec.ranges.len, false)

    var idx = 0
    while idx < node.len:
      # Find matching node range for index
      for rangeIdx, arange in spec.ranges:
        if arange.arange.contains(idx, node.len):
          # Mark as found
          altFound[rangeIdx] = true

          for alt in arange.alts:
            let n = findMissing(alt, node, path & @[idx])
            if not n.isEmpty():
              result.nested.add n

          break

      inc idx

    # Show all missing ranges
    for rangeIdx, found in altFound:
      if (not found) and (not spec.ranges[rangeIdx].arange.optional):
        var expected: set[K]
        for alt in spec.ranges[rangeIdx].alts:
          expected.incl alt.expected

        result.nested.add AstCheckFail[K](
          isMissing: true,
          parent: node.kind,
          path: path & rangeIdx,
          arange: spec.ranges[rangeIdx].arange,
          expected: expected)


proc validateAst*[N, K](
    spec: AstPattern[N, K], node, subnode: N,
    idx: int, path: seq[int] = @[]
  ): AstCheckFail[K] =

  result.path = path
  # Find range in which given subnode is found
  for arange in spec.ranges:
    if arange.arange.contains(idx, node.len):
      for alt in arange.alts:
        if subnode.kind notin alt.expected:
          result.nested.add AstCheckFail[K](
            parent: node.kind,
            path: path,
            expected: alt.expected,
            arange: arange.arange)


proc formatFail*[N, K](fail: AstCheckFail[K], node: N): ColoredText =
  coloredResult()

  proc aux(fail: AstCheckFail[K], level: int) =
    addIndent(level)
    if fail.isEmpty(withNested = false):
      add toPath(node, fail.path).toYellow()

    else:
      if fail.msg.len > 0:
        add fail.msg
        add " "

      if fail.expected.len > 0:
        if fail.isMissing:
          add "missing subnode "
          add toGreen($fail.arange)
          add " "
          add toRed($fail.expected)

        else:
          add "wanted "
          add toRed($fail.expected)
          add " in "
          add toGreen($fail.arange)

      else:
        if fail.isMissing:
          add "missing subnode "
          add toGreen($fail.arange)

      if fail.path.len > 0:
        add " on path "
        add toPath(node, fail.path).toGreen()

      else:
        add " for "
        add toGreen($fail.parent)

    for nested in fail.nested:
      if not nested.isEmpty():
        add "\n"
        aux(nested, level + 1)

  aux(fail, 0)
  endResult()


proc validateAst*[N, K](
    spec: AstSpec[N, K], node: N, subnode: N, idx: int): ColoredText =

  if spec.spec[node.kind].isSome():
    result.add formatFail(
      validateAst(spec.spec[node.kind].get(), node, subnode, idx, @[idx]), node)

    result.add "\n"
    result.add formatFail(
      findMissing(spec.spec[node.kind].get(), node), node)

proc validateAst*[N, K](spec: AstSpec[N, K], node: N): ColoredText =
  if spec.spec[node.kind].isSome():
    result.add formatFail(
      findMissing(spec.spec[node.kind].get(), node), node)
