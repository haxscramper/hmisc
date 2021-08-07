import
  std/[macros, strformat, sequtils, strutils, times, options]

import
  ../macros/nim_ast_help,
  ../algo/[hseq_distance, htemplates, halgorithm],
  ../other/[hpprint],
  ../core/[all, code_errors],
  ../types/colorstring,
  ./oswrap



type
  TestBackend = enum tbC, tbCpp, tbJs, tbNims

  TestGlob = object
    suiteGlob: GitGlob
    testGlob: GitGlob

  TestConf = object
    isSuite: bool
    stderr, stdout, stdin: string
    backends: seq[TestBackend]
    name: string
    timeExecution: bool

  TestLocation = object
    line, column: int
    file: string

  TestFailKind = enum
    tfkNone
    tfkException
    tfkManualFail

    tfkStringDiff
    tfkStructDiff
    tfkAstDiff
    tfkMatchDiff
    tfkStructEq
    tfkStructNeqNoDiff

  TestReportKind = enum
    trkCheckpoint

    trkCheckOk
    trkCheckFail

    trkExpectOk
    trkExpectFail

    trkTestStart
    trkTestEnd
    trkTestFail
    trkTestOk

    trkSuiteStart
    trkSuiteEnd
    trkSuiteFail

const
  trkSectionKinds = { trkTestStart .. trkSuiteFail }
  trkFailKinds = { trkSuiteFail, trkTestFail, trkExpectFail, trkCheckFail }

type
  TestMatchFail = tuple[path, expected, got: string]

  TestReport = object
    location {.requiresinit.}: TestLocation
    msg: string
    case failKind: TestFailKind
      of tfkStringDiff:
        textDiff: ShiftedDiff[string]

      of tfkStructDiff, tfkStructEq:
        structDiff: PPrintTree

      of tfkException:
        exception: ref Exception

      of tfkMatchDiff:
        paths: seq[TestMatchFail]

      else:
        discard


    case kind: TestReportKind
      of trkCheckOk, trkCheckFail:
        strs: seq[tuple[expr, value: string]]

      of trkSectionKinds:
        conf: TestConf

      else:
        discard

  TestContext* = ref object
    globs: seq[TestGlob]
    startTime: float
    sourceOnError: bool
    failCount: int


  TestProcPrototype = proc(testContext: TestContext)

func allBackends(conf: TestConf): bool =
  let b = conf.backends
  b.len == 0 or ((tbC in b) and (tbCpp in b) or (tbJs in b) or (tbNims in b))

import ./hlogger

proc report(context: TestContext, report: TestReport) =
  let (file, line, column) = (
    report.location.file,
    report.location.line,
    report.location.column)

  let (dir, filename, expr) = splitFile(AbsFile file)

  let
    width = 8
    pad = " " |<< width
    pFail = toRed("[FAIL] " |>> width)
    pOk = toGreen("[OK] " |>> width)
    pSuite = toMagenta("[SUITE] " |>> width)

  case report.kind:
    of trkSectionKinds:
      let name = report.conf.name
      case report.kind:
        of trkFailKinds:
          echo pFail, name

        of trkSuiteEnd:
          echo pSuite, name

        of trkTestEnd:
          if context.failCount > 0:
            echo pFail, name

          else:
            echo pOk, name

          context.failCount = 0

        else:
          echo pOk, name

      if report.failKind == tfkException:
        newTermLogger().logStackTrace(
          report.exception, source = context.sourceOnError)


    of trkCheckFail:
      inc context.failCount
      var msg: ColoredText
      msg.add pad
      msg.add toYellow("[CHECK FAIL]")
      msg.add " in "
      msg.add toGreen(&"{filename}:{line}")


      case report.failKind:
        of tfkStructDiff:
          msg.add " - structural comparison mismatch"
          echo msg, "\n"
          echo report.structDiff.pstring().indent(width + 3), "\n"

        of tfkMatchDiff:
          msg.add " - pattern matchig fail"
          echo msg, "\n"
          for (path, expected, got) in report.paths:
            echo pad, "  expected ", toGreen(expected), " at '", path,
              "', but got ", toRed(got)

          echo ""

        else:
          echo msg, "\n"
          let exprWidth  = report.strs.maxIt(it.expr.len)
          for (expr, value) in report.strs:
            echo pad, "  ", toGreen(expr |<< exprWidth), " was ", value

          echo ""


    else:
      discard


func suiteFailed(
    conf: TestConf, loc: TestLocation, fail: ref Exception): TestReport =

  TestReport(
    kind: trkSuiteFail, failKind: tfkException,
    msg: fail.msg, conf: conf, location: loc)

func testFailed(
    conf: TestConf, loc: TestLocation, fail: ref Exception): TestReport =
  result = TestReport(
    kind: trkTestFail, failKind: tfkException,
    conf: conf, location: loc)

  if fail of CatchableError:
    result.msg = (ref CatchableError)(fail).msg

func testEnd(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(kind: trkTestEnd, conf: conf, location: loc)

func suiteStarted(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(
    kind: trkSuiteStart, location: loc, conf: conf)

proc suiteEnded(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(kind: trkSuiteEnd, location: loc, conf: conf)


proc checkFailed(
    loc: TestLocation,
    strs: openarray[(string, string)]
  ): TestReport =
  TestReport(kind: trkCheckFail, location: loc, strs: @strs)


proc checkOk(loc: TestLocation): TestReport =
  TestReport(kind: trkCheckOk, location: loc)

proc matchCheckFailed(
    loc: TestLocation, fails: seq[TestMatchFail]): TestReport =
  TestReport(
    kind: trkCheckFail, location: loc,
    failKind: tfkMatchDiff, paths: fails)


func getWhen(conf: TestConf): NimNode =
  if conf.allBackends():
    result = newLit(true)

  else:
    var conds: seq[NimNode]

    for b in conf.backends:
      conds.add case b:
        of tbC: newCall("defined", ident"c")
        of tbCpp: newCall("defined", ident"cpp")
        of tbjs: newCall("defined", ident"js")
        of tbNims: newcall("defined", ident"nims")

    result = foldl(conds, nnkInfix.newTree(ident"or", a, b))

func testLocation(node: NimNode): TestLocation =
  let iinfo = node.lineInfoObj()
  TestLocation(file: iinfo.filename, line: iinfo.line, column: iinfo.column)

proc stringdiff*(str1, str2: string, loc: TestLocation): TestReport =
  if str1 != str2:
    result = TestReport(
      location: loc,
      kind: trkCheckFail,
      failKind: tfkStringDiff,
    )

    let
      text1 = str1.split()
      text2 = str2.split()

    result.textDiff = shiftDiffed(
      text1, text2, myersDiff(text1, text2), "")

  else:
    result = TestReport(location: loc, kind: trkCheckOk)

proc structdiff*(ptree1, ptree2: PPrintTree, loc: TestLocation): TestReport =
  let
    diff = treeDiff(ptree1, ptree2)

  if isSome(diff):
    result = TestReport(
      location: loc,
      kind: trkCheckFail,
      failKind: tfkStructDiff,
      structDiff: diff.get())

  else:
    result = TestReport(location: loc, kind: trkCheckOk)


proc structdiff*[T](struct1, struct2: T, loc: TestLocation): TestReport =
  bind pptree
  structdiff(pptree(struct1), pptree(struct2), loc)


proc structeq*[T](struct1, struct2: T, loc: TestLocation): TestReport =
  bind pptree
  if struct1 == struct2:
    let diff = treeDiff(pptree(struct1), pptree(struct2))
    if diff.isSome():
      result = TestReport(
        location: loc,
        kind: trkCheckFail,
        failKind: tfkStructDiff,
        structDiff: diff.get())

    else:
      result = TestReport(
        location: loc, kind: trkCheckFail, failKind: tfkStructNeqNoDiff)

  else:
    result = TestReport(location: loc, trkCheckOk)



proc hasKind[A, K](ast: A, kind: K): bool =
  ast.kind == kind

proc toPath[A](ast: A, path: seq[int]): string =
  mixin `[]`
  proc aux(a: A, path: seq[int]): seq[string] =
    result.add $a.kind
    if path.len > 1:
      result.add aux(a[path[0]], path[1..^1])

    elif path.len == 1:
      result.add "[" & $path[0] & "]"

  return aux(ast, path).join(".")


macro astdiff*(ast: typed, match: untyped, loc: TestLocation): untyped =
  let
    fails = genSym(nskVar, "fails")
    expr = ident("expr")
    toPath = bindSym("toPath")

  proc aux(path, pattern: NimNode, pathIdx: seq[int]): NimNode =
    let idxLit = pathIdx.newLit()
    case pattern.kind:
      of nnkCall, nnkBracketExpr:
        let kind = pattern[0]
        let kindLit = kind.toStrLit()
        let wantLen = newLit(pattern.len - 1)
        result = quote do:
          if not hasKind(`path`, `kind`):
            `fails`.add((
              `idxLit`,
              `kindLit`,
              $`path`.kind
            ))

          if `path`.len < `wantLen`:
            `fails`.add((
              `idxLit`,
              "missing subnodes - wanted len " & $`wantLen`,
              $`path`.len
            ))

        for idx, check in pattern[1..^1]:
          let
            idxLit = newLit(idx)
            auxImpl = aux(
              nnkBracketExpr.newTree(path, idxLit),
              check, pathIdx & idx
            )

          result.add quote do:
            if `idxLit` < `path`.len:
              `auxImpl`


      else:
        raise newImplementKindError(pattern)

  let impl = aux(ast, match, @[])

  result = quote do:
    var `fails`: seq[(seq[int], string, string)]
    let `expr` = `ast`

    `impl`

    if `fails`.len > 0:
      var failDescs: seq[TestMatchFail]
      for (path, desc, got) in `fails`:
        failDescs.add((`toPath`(`expr`, path), desc, got))

      matchCheckFailed(`loc`, failDescs)

    else:
      checkOk(`loc`)



macro matchdiff*(
    obj: typed, match: untyped, loc: static[TestLocation]): untyped =

  let
    loc = newLit(loc)
    fails = genSym(nskVar, "fails")
    tmp = ident("expr")
    hasKind = bindSym("hasKind")


  func pathLit(item: NimNode): NimNode =
    item.toStrLit().strVal()["expr".len .. ^1].newLit()

  proc callCmp(item, call: NimNode, desc: NimNode): NimNode =
    let pathLit = item.pathLit()

    result = quote do:
      if not(`call`):
        `fails`.add (`pathLit`, $`desc`, $`item`)

  proc itemCmp(item, value: NimNode): NimNode =
    let pathLit = item.pathLit()
    case value.kind:
      of nnkIntLit:
        result = quote do:
          if not(`item` == `value`):
            `fails`.add (`pathLit`, $`value`, $`item`)

      of nnkPrefix:
        let
          cmpExpr = nnkInfix.newTree(value[0], item, value[1])
          valueLit = value.toStrLit()

        result = quote do:
          if not(`cmpExpr`):
            `fails`.add (`pathLit`, `valueLit`, $`item`)

      else:
        raise newImplementKindError(value, value.treeRepr())

  proc aux(item, pattern, path: NimNode): NimNode =
    result = newStmtList()
    case pattern.kind:
      of nnkPar:
        for check in pattern:
          result.add aux(item, check, path)

      of nnkExprColonExpr:
        assertNodeKind(pattern[0], {nnkIdent})
        result.add itemCmp(nnkDotExpr.newTree(path, pattern[0]), pattern[1])

      of nnkBracket:
        result.add itemCmp(
          nnkDotExpr.newTree(path, ident"len"), newLit(pattern.len))

        for idx, item in pattern:
          result.add itemCmp(
            nnkBracketExpr.newTree(path, newLit(idx)), item)

      of nnkBracketExpr, nnkCall:
        let
          pathLit = path.pathLit()
          wantKind = pattern[0]
          kindLit = wantKind.toStrLit()

        result.add quote do:
          if not(`hasKind`(`path`, `wantKind`)):
            `fails`.add (`pathLit`, `kindLit`, $`path`.kind)


        for idx, check in pattern[1..^1]:
          if check.kind in { nnkIntLit, nnkPrefix }:
            result.add itemCmp(
              nnkBracketExpr.newTree(path, newLit(idx)), check)

          else:
            result.add aux(
              item,
              check,
              nnkBracketExpr.newTree(path, newLit(idx)))


      else:
        raise newImplementKindError(pattern)


  let impl = aux(tmp, match, tmp)

  result = quote do:
    block:
      var `fails`: seq[TestMatchFail]
      let `tmp` = `obj`
      `impl`

      if `fails`.len > 0:
        matchCheckFailed(`loc`, `fails`)

      else:
        checkOk(`loc`)





















template hasTestContext(): untyped {.dirty.} =
  mixin testContext
  when declared(testContext):
    testContext is TestContext

  else:
    false

template wantContext*(): untyped {.dirty.} =
  when not hasTestContext():
    {.error: ""}

proc canRunTest(
    context: TestContext, conf: TestConf): bool =
  if context.globs.len == 0:
    return true

  for glob in context.globs:
    if conf.isSuite:
      if glob.suiteGlob.match(conf.name):
        return true

    else:
      if glob.testGlob.match(conf.name):
        return true

proc maybeRunSuite(
    context: TestContext,
    suiteProc: TestProcPrototype,
    conf: TestConf,
    loc: TestLocation
  ) =

  if context.canRunTest(conf):
    try:
      suiteProc(context)

      context.report suiteEnded(conf, loc)
    except:
      report(context, suiteFailed(conf, loc, getCurrentException()))


proc checkpoint(loc: TestLocation, msg: string): TestReport =
  TestReport(location: loc, msg: msg, kind: trkCheckpoint)

macro configureTests(conf: untyped): untyped =
  discard


proc newTestLogger*(): HLogger =
  result = HLogger(
    logPrefix: toMapArray({
      logTrace:  toWhite("[trace]").format(),
      logDebug:  toYellow("[debug]").format(),
      logInfo:   toMagenta("[info]").format(),
      logNotice: toGreen("[notice]").format(),
      logWarn:   toYellow("[warn]").format(),
      logError:  toRed("[error]").format(),
      logFatal:  toMagenta("[fatal]").format(),
    }),
    eventPrefix: toMapArray({
      logEvSuccess:   toGreen("[OK]").format(),
      logEvFail:      toRed("[FAIL]").format(),
      logEvWaitStart: toYellow("[wait]").format(),
      logEvWaitDone:  toMagenta("[DONE]").format(),
      logEvExprDump:  toItalic("[expr]").format()
    })
  )

  result.openScope(hskMain, "main")

  result.prefixLen = max(
    result.logPrefix.maxIt(it.str.len),
    result.eventPrefix.maxIt(it.str.len))

proc newTestContext*(): TestContext =
  TestContext(
    sourceOnError: true
  )

proc getTestGlobs(): seq[TestGlob] =
  for param in paramStrs():
    let split = param.split("::")
    result.add TestGlob(
      suiteGlob: toGitGlob(split[0]),
      testGlob: toGitGlob(
        if split.len > 1 and split[1].len > 0:
          split[1]
        else:
          "*"
      )
    )

proc getTestContext(): TestContext =
  var context {.global, threadvar.}: TestContext
  if isNil(context):
    context = newTestContext()
    context.globs = getTestGlobs()

  return context

macro suite*(name: static[string], body: untyped): untyped =
  var suiteConf: TestConf
  suiteConf.isSuite = true
  suiteConf.name = name


  let
    line = lineIInfo(body)
    suiteProc = genSym(nskProc, "suite")
    suiteLit = newLit(suiteConf)
    whenSuite = suiteConf.getWhen()
    suiteFailed = bindSym("suiteFailed")
    locLit = testLocation(body).newLit()

  result = quote do:
    when `whenSuite`:
      let conf = `suiteLit`
      let loc = `locLit`
      proc `suiteProc`(testContext {.inject.}: TestContext) =
        {.line: `line`.}:
          `body`

      when hasTestContext():
        maybeRunSuite(testContext, `suiteProc`, conf, loc)

      else:
        maybeRunSuite(getTestContext(), `suiteProc`, conf, loc)



macro test*(name: static[string], body: untyped): untyped =
  var testConf: TestConf

  testConf.name = name

  let
    line = lineIInfo(body)
    testLit = newLit(testConf)
    canRun = bindSym("canRunTest")
    testFailed = bindSym("testFailed")
    testOk = bindSym("testEnd")
    report = bindSym("report")
    testLoc = testLocation(body).newLit()


  result = quote do:
    block:
      let
        conf = `testLit`
        loc = `testLoc`

      if `canRun`(testContext, conf):
        try:
          {.line: `line`.}:
            `body`

          `report`(testContext, `testOk`(conf, loc))

        except:
          `report`(testContext, `testFailed`(
            conf, loc, getCurrentException()))





proc buildCheck(expr: NimNode): NimNode =
  let
    line = lineIINfo(expr)
    loc = testLocation(expr).newLit()

  case expr.kind:
    of nnkInfix:
      let (lhs, rhs) = (expr[1], expr[2])
      let (lhsLit, rhsLit) = (expr[1].toStrLit(), expr[2].toStrLit())
      let (lhsId, rhsId) = (genSym(nskLet), genSym(nskLet))

      let doOp = nnkInfix.newTree(expr[0], lhsId, rhsId)

      result = quote do:
        block:
          {.line: `line`.}:
            let
              `lhsId` = `lhs`
              `rhsId` = `rhs`

            if not (`doOp`):
              `report`(testContext, checkFailed(`loc`, {
                `lhsLit`: $`lhsId`,
                `rhsLit`: $`rhsId`
              }))


    else:
      if expr.kind in { nnkCall, nnkCommand } and
         expr[0].eqIdent([
           "stringdiff", "structdiff",
           "matchdiff", "astdiff"
         ]):
        expr.add loc
        result = quote do:
          block:
            {.line: `line`.}:
              `report`(testContext, `expr`)

      else:
        let lit = repr(expr).newLit()
        result = quote do:
          block:
            {.line: `line`.}:
              if not(`expr`):
                testContext.checkFailed(`loc`, `lit`)



  # echo result.repr()




macro check*(expr: untyped): untyped =
  result = newStmtList()
  case expr.kind:
    of nnkStmtList:
      for expr in expr:
        result.add buildCheck(expr)

    else:
      result.add buildCheck(expr)

macro parametrizeOnType*(
    varname: untyped, types: typed, body: untyped): untyped =
  result = newStmtList()
  for typename in types:
    result.add newStmtList(
      nnkTypeSection.newTree(
        nnkTypeDef.newTree(varname, newEmptyNode(), typename)),
      body.copyNimTree())

macro parametrizeOnConst*(varname, values, body: untyped): untyped =
  result = newStmtList()
  for value in values:
    let varname = ident(varname.strVal())
    result.add nnkBlockStmt.newTree(newEmptyNode(), newStmtList(
      nnkConstSection.newTree(
        nnkConstDef.newTree(varname, newEmptyNode(), value)),
      body.copyNimTree()))

macro parametrizeOnValue*(
    varname: untyped, values: typed, body: untyped): untyped =

  result = nnkForStmt.newTree(varname, values, body)



macro expect*(exceptions: varargs[typed]; body: untyped): untyped =
  discard

macro fail*(): untyped = discard
macro skip*(): untyped = discard
template checkpoint*(mgs: string): untyped =
  block:
    wantContext()
    let (file, line, column) = instantiationInfo(fullpaths = true)
    testContext.report checkpoint(
      TestLocation(line: line, column: column, file: file), msg)
