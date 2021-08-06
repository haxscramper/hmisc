import
  std/[macros, strformat, sequtils, strutils, times, options]

import
  ../macros/nim_ast_help,
  ../algo/hseq_distance,
  ../other/[hpprint],
  ../core/[all, code_errors],
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

const trkSectionKinds = { trkTestStart .. trkSuiteFail }

type
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


  TestProcPrototype = proc(testContext: TestContext)

func allBackends(conf: TestConf): bool =
  let b = conf.backends
  b.len == 0 or ((tbC in b) and (tbCpp in b) or (tbJs in b) or (tbNims in b))

proc report(context: TestContext, report: TestReport) =
  echo report.kind, " ", report.msg
  case report.kind:
    of trkSectionKinds:
      echo ">> ", report.conf.name
      if report.failKind == tfkException:
        pprintStackTrace(report.exception)

    of trkCheckFail:
      case report.failKind:
        of tfkStructDiff:
          echo report.structDiff.pstring()

        else:
          discard


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

func testOk(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(kind: trkTestOk, conf: conf, location: loc)

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



template astdiff*[T](ast1, ast2: T, loc: TestLocation): TestReport =
  TestReport(location: loc)

macro matchdiff*(obj: typed, match: untyped, loc: TestLocation): TestReport =
  discard



















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

proc newTestContext(): TestContext =
  discard

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
    context = TestContext(globs: getTestGlobs())

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
    testOk = bindSym("testOk")
    report = bindSym("report")
    testLoc = testLocation(body).newLit()


  result = quote do:
    {.line: `line`.}:
      let
        conf = `testLit`
        loc = `testLoc`

      if `canRun`(testContext, conf):
        try:
          `body`

          `report`(testContext, `testOk`(conf, loc))

        except:
          `report`(testContext, `testFailed`(conf, loc, getCurrentException()))





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
           "stringdiff", "structdiff", "astdiff", "matchdiff"]):
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
