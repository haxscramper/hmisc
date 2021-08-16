import
  std/[
    macros, strformat, sequtils, monotimes,
    strutils, times, options, stats, tables
  ]

export tables

from fusion/matching import hasKind

import
  ../macros/[nim_ast_aux, ast_spec],
  ../algo/[hseq_distance, htemplates, halgorithm, clformat],
  ../other/[hpprint],
  ../core/[all, code_errors],
  ../types/colorstring,
  ./oswrap,
  ./hlogger,
  ./hcoverage




type
  TestBackend = enum tbC, tbCpp, tbJs, tbNims

  TestGlob = object
    suiteGlob: GitGlob
    testGlob: GitGlob

  TestCoverageWant = object
    procname: CovProcName
    onlyMissing: bool

  TestConf = object
    isSuite: bool
    stderr, stdout, stdin: string
    backends: seq[TestBackend]
    name: string
    timeExecution: bool

    showCoverage: seq[TestCoverageWant]

  TestLocation = object
    line, column: int
    file: string

  TestFailKind = enum
    tfkNone
    tfkException
    tfkManualFail
    tfkOpCheck
    tfkCallCheck
    tfkPredicateFail
    tfkNoExceptionRaised
    tfkUnexpectedExceptionRaised


    tfkStrdiff
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
    trkShow

    trkBlockStart
    trkBlockEnd

    trkTestComment
    trkSuiteComment
    trkBlockComment

    trkTimeStats

const
  trkSectionKinds = { trkTestStart .. trkSuiteFail }
  trkFailKinds = { trkSuiteFail, trkTestFail, trkExpectFail, trkCheckFail }

type
  TestMatchFail = tuple[path, expected, got: string]

  TestValueKind = enum
    tvkString
    tvkStringified
    tvkTypename

  TestValueContext = enum
    tvcNone
    tvcEqCompare
    tvcParameter
    tvcTypeCompare

  TestValue = object
    context: TestValueContext
    case kind: TestValueKind
      of tvkString, tvkStringified, tvkTypename:
        str: string

  TestReport = object
    location {.requiresinit.}: TestLocation
    msg: string
    case failKind: TestFailKind
      of tfkStructDiff, tfkStructEq:
        structDiff: It[PPrintTree]

      of tfkException, tfkUnexpectedExceptionRaised:
        exception: ref Exception

      of tfkMatchDiff:
        paths: seq[TestMatchFail]

      of tfkOpCheck:
        checkOp: string

      else:
        discard


    strs: seq[tuple[expr: string, value: TestValue]]
    name: string
    case kind: TestReportKind
      of trkSectionKinds:
        conf: TestConf

      of trkTimeStats:
        stat: RunningStat

      of trkTestComment, trkSuiteComment, trkBlockComment:
        text: string

      else:
        discard

  TestContext* = ref object
    globs: seq[TestGlob]
    startTime: float
    sourceOnError: bool
    failCount: int
    shownHeader: bool
    contextValues: seq[(string, TestValue)]
    skipAfterException: bool
    skipAfterManual: bool
    skipAfterCheckFail: bool
    skipNext: bool
    logger: HLogger


  TestProcPrototype = proc(testContext: TestContext)

func allBackends(conf: TestConf): bool =
  let b = conf.backends
  b.len == 0 or ((tbC in b) and (tbCpp in b) or (tbJs in b) or (tbNims in b))

proc addValue(ctx: TestContext, name: string, value: TestValue) =
  ctx.contextValues.add((name, value))

proc removeValue(ctx: TestContext, name: string) =
  discard ctx.contextValues.pop()

proc testValue(str: string, context: TestValueContext): TestValue =
  TestValue(str: str, kind: tvkString, context: context)

proc testValue[T](str: T, context: TestValueContext): TestValue =
  TestValue(str: $str, kind: tvkStringified, context: context)

proc testValue[T](str: typedesc[T], context: TestValueContext): TestValue =
  TestValue(str: $str, kind: tvkTypename, context: context)

proc `$`(val: TestValue): string =
  result = val.str


proc newTestLogger*(): HLogger =
  let pref = "     ] "
  result = HLogger(
    logPrefix: toMapArray({
      logTrace:  toWhite(pref & "[trace]").format(),
      logDebug:  toYellow(pref & "[debug]").format(),
      logInfo:   toMagenta(pref & "[info]").format(),
      logNotice: toGreen(pref & "[notice]").format(),
      logWarn:   toYellow(pref & "[warn]").format(),
      logError:  toRed(pref & "[error]").format(),
      logFatal:  toMagenta(pref & "[fatal]").format(),
    }),
    eventPrefix: toMapArray({
      logEvSuccess:   toGreen(pref & "[ok]").format(),
      logEvFail:      toRed(pref & "[fail]").format(),
      logEvWaitStart: toYellow(pref & "[wait]").format(),
      logEvWaitDone:  toMagenta(pref & "[DONE]").format(),
      logEvExprDump:  toItalic(pref & "[expr]").format()
    })
  )

  result.openScope(hskMain, "main")
  result.groupPrefix = true

  result.prefixLen = max(
    result.logPrefix.maxIt(it.str.len),
    result.eventPrefix.maxIt(it.str.len))


proc newTestContext*(): TestContext =
  TestContext(sourceOnError: true, logger: newTestLogger())

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

proc getTestLogger*(): HLogger =
  getTestContext().logger

proc configureDefaultTestContext*(
    skipAfterException: bool = false,
    skipAfterManual: bool = false,
    skipAfterCheckFail: bool = false
  ) =

  var default = getTestContext()
  default.skipAfterException = skipAfterException
  default.skipAfterManual = skipAfterManual
  default.skipAfterCheckFail = skipAfterCheckFail

proc showCoverage*(
    conf: var TestConf, procname: string, onlyMissing: bool = true) =
  conf.showCoverage.add TestCoverageWant(
    procname: CovProcName(name: procname), onlyMissing: onlyMissing)


proc report(context: TestContext, report: TestReport) =
  let (file, line, column) = (
    report.location.file,
    report.location.line,
    report.location.column)

  let (dir, filename, expr) = splitFile(AbsFile file)

  let
    width = 8
    pad = " " |<< width
    pFail  = toRed("[FAIL] " |>> width)
    pOk    = toGreen("[OK] " |>> width)
    pBLock = toCyan("[BLOCK] " |>> width)
    pSuite = toMagenta("[SUITE] " |>> width)
    pShow  = toYellow("[SHOW] ")
    pRun   = to8Bit("[RUN] " |>> width, 3, 1, 1)
    pCover = toYellow("[COVER] " |>> width)

  if report.kind != trkTimeStats:
    context.shownHeader = false

  if report.failKind == tfkException and context.skipAfterException:
    context.skipNext = true

  if report.failKind == tfkManualFail and context.skipAfterManual:
    context.skipNext = true

  if report.kind == trkCheckFail and context.skipAfterCheckFail:
    context.skipNext = true

  case report.kind:
    of trkTestComment, trkSuiteComment, trkBlockComment:
      for line in report.text.split('\n'):
        echo pad, "# ".to8Bit(0, 2, 2), to8Bit(line, 0, 2, 2)

    of trkTimeStats:
      if not context.shownHeader:
        echo pad, "[TIME] "
        context.shownHeader = true

      echo pad, toGreen(report.name |<< 30), &"{report.stat.min:<10.3f}"
      let w = context.contextValues.maxIt(it[0].len)
      for (name, value) in context.contextValues:
        echo pad, "  ", toYellow(name |<< w), " = ", toRed($value)

    of trkShow:
      let w = report.strs.maxIt(it[0].width())
      let padW = width + pShow.len()
      for idx, (arg, val) in report.strs:
        let split = split(strip(
          $val, leading = false, chars = {'\n'}), '\n')

        var pref: ColoredText
        pref.add pad
        if idx == 0: pref.add pShow else: pref.add "     ] "
        pref.add indentBody(toGreen(arg |<< w), padW, prefix = "] ")

        if split.len > 1:
          echo pref, " = "

        elif pref.len + split[0].len > 80:
          echo pref
          echo pad, "         = ", split[0]
          echo pad, "       "


        else:
          if split[0] == " ":
            echo pref, " = ", "' '"

          else:
            echo pref, " = ", split[0]


        if split.len > 1:
          for line in split:
            echo pad, "        ", line

          echo pad, "       "

    of trkBlockStart:
      echo pBlock, report.name.to8Bit(0, 4, 3)

    of trkSectionKinds:
      let name = report.conf.name
      case report.kind:
        of trkFailKinds:
          echo pFail, name, " ", report.msg

        of trkSuiteEnd:
          echo pSuite, name

        of trkTestStart:
          echo pRun, name

        of trkTestEnd:
          if context.failCount > 0:
            echo pFail

          else:
            echo pOk

          context.failCount = 0

        else:
          echo pOk, name


      if report.failKind == tfkException:
        assertRef report.exception
        logStackTrace(
          getTestLogger(),
          report.exception,
          showError = true,
          source = context.sourceOnError,
          skipFirst = 2
        )


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

        of tfkStrDiff:
          let
            text1 = report.strs[0].value.str.split("\n")
            text2 = report.strs[1].value.str.split("\n")

          let formatted = myersDiff(text1, text2).
            shiftDiffed(text1, text2).
            formatDiffed(text1, text2)

          echo msg, " - string mismatch\n"
          echo pad, toGreen(report.strs[0].expr), " != ",
            toGreen(report.strs[1].expr), "\n"

          echo formatted.indent(pad.len)
          echo ""

        else:
          if report.failKind == tfkOpCheck:
            let
              s1 = report.strs[0][1].str
              s2 = report.strs[1][1].str

            echo msg, " - ", toGreen(report.strs[0][0]), " was \n"
            echo pad, "  '", s1.indentBody(pad.len + 2), "'\n"
            echo pad, "but expected\n"
            echo pad, "  '", s2.indentBody(pad.len + 2), "'"
            echo ""

          else:
            echo msg, " ", report.failKind, "\n"
            let exprWidth  = report.strs.maxIt(it.expr.len)
            for (expr, value) in report.strs:
              echo pad, "  ", toGreen(expr |<< exprWidth),
                " was '", value, "'"

            echo ""


    else:
      discard


  if report.kind in { trkTestEnd, trkSuiteEnd }:
    for cover in report.conf.showCoverage:
      let coverReport = getCoverage(cover.procname)
      var lastNotExecuted = 0
      echo pCover, "proc '", toGreen(coverReport.procname.fullname.name), "' defined in ",
        coverReport.file.name(), " ", hshow(coverReport.lineRange), "\n"

      var nocover = 0
      for line in coverReport.chunks:
        case line.kind:
          of cckNotExecuted:
            inc nocover
            echo pad, &"{line.line:<4} {toRed(line.text)}"
            lastNotExecuted = line.line

          of cckEmpty:
            if line.line == lastNotExecuted + 1:
              lastNotExecuted = line.line
              echo pad, &"{line.line:<4}"

          of cckExecuted:
            if not cover.onlyMissing:
              echo pad, &"{line.line:<4} {toGreen(line.text)}"

      if nocover == 0:
        echo pad, "full coverage"





func testLocation(node: NimNode): TestLocation =
  let iinfo = node.lineInfoObj()
  TestLocation(file: iinfo.filename, line: iinfo.line, column: iinfo.column)


func testLocation(pos: (string, int, int)): TestLocation =
  TestLocation(file: pos[0], line: pos[1], column: pos[2])


proc flattenArgs(args: NimNode): seq[NimNode] =
  for arg in args:
    case arg.kind:
      of nnkArgList:
        result.add flattenArgs(arg)

      of nnkStmtList:
        for stmt in arg:
          result.add stmt

      else:
        result.add arg


macro show*(args: varargs[untyped]): untyped =
  ## Show value in the unit test
  ##
  ## - WARNING :: Due to parser handling with `proc arg = value` case
  ##   it is not possible to use `show name = value` without parentheses,
  ##   nor is it possible to detect correctly. Instead you should either
  ##   add wrapping parens (`show(name = value)`), or use block syntax:
  ##   ```nim
  ##   show:
  ##     name = value
  ##   ```


  let args = flattenArgs(args)
  let
    report = bindSym("report")
    testValue = bindSym("testValue")
    tNone = bindSym("tvcNone")

  var argpass = nnkTableConstr.newTree()
  for arg in args:
    let (lit, arg) =
      if arg.kind in { nnkExprEqExpr, nnkAsgn }:
        (arg[0].toStrLit(), arg[1])

      else:
        (arg.tostrLit(), arg)

    let pass = nnkExprColonExpr.newTree(
      lit,
      newCall(
        testValue,
        nnkPrefix.newTree(ident"$", arg),
        tNone))

    argpass.add pass

  let loc = args[0].testLocation().newLit()

  result = quote do:
    `report`(testContext, TestReport(
      kind: trkShow,
      strs: @`argpass`,
      location: `loc`))





func suiteFailed(
    conf: TestConf, loc: TestLocation, fail: ref Exception): TestReport =

  TestReport(
    kind: trkSuiteFail, failKind: tfkException,
    exception: fail,
    msg: fail.msg, conf: conf, location: loc)

func testFailed(
    conf: TestConf, loc: TestLocation, fail: ref Exception): TestReport =
  result = TestReport(
    kind: trkTestFail, failKind: tfkException,
    conf: conf, location: loc, exception: fail)

  if fail of CatchableError:
    result.msg = (ref CatchableError)(fail).msg


func testFailManual(loc: TestLocation, msg: string): TestReport =
  result = TestReport(
    kind: trkTestFail, failKind: tfkManualFail,
    location: loc, msg: msg)


func expectOk(loc: TestLocation): TestReport =
  TestReport(kind: trkExpectOk, location: loc)

func expectFail(unexpected: ref Exception, loc: TestLocation): TestReport =
  TestReport(
    kind: trkExpectFail,
    failKind: tfkUnexpectedExceptionRaised,
    exception: unexpected,
    location: loc)

func expectFail(loc: TestLocation): TestReport =
  TestReport(
    kind: trkExpectFail,
    failKind: tfkNoExceptionRaised,
    location: loc)


func testEnd(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(kind: trkTestEnd, conf: conf, location: loc)

func testStart(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(kind: trkTestStart, conf: conf, location: loc)

func blockStart(name: string, loc: TestLocation): TestReport =
  TestReport(kind: trkBlockStart, location: loc, name: name)

func blockEnd(name: string, loc: TestLocation): TestReport =
  TestReport(kind: trkBlockEnd, location: loc, name: name)

func testComment(text: string, loc: TestLocation): TestReport =
  TestReport(kind: trkTestComment, location: loc, text: text)

func blockComment(text: string, loc: TestLocation): TestReport =
  TestReport(kind: trkBlockComment, location: loc, text: text)

func suiteComment(text: string, loc: TestLocation): TestReport =
  TestReport(kind: trkSuiteComment, location: loc, text: text)

func suiteStarted(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(
    kind: trkSuiteStart, location: loc, conf: conf)

proc suiteEnded(conf: TestConf, loc: TestLocation): TestReport =
  TestReport(kind: trkSuiteEnd, location: loc, conf: conf)


proc checkFailed(
    loc: TestLocation,
    strs: openarray[(string, TestValue)],
    failKind: TestFailKind,
    checkOp: string = ""
  ): TestReport =
    result = TestReport(
      kind: trkCheckFail,
      strs: @strs,
      location: loc, failKind: failKind)

    if result.failKind == tfkOpCheck:
      result.checkOp = checkOp




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

proc strdiffImpl(
    str1, str2: string, loc: TestLocation,
    expr1, expr2: string
  ): TestReport =

  if str1 != str2:
    result = TestReport(
      location: loc,
      kind: trkCheckFail,
      failKind: tfkStrdiff,
      strs: @{
        expr1: testValue(str1, tvcEqCompare),
        expr2: testValue(str2, tvcEqCompare)})


  else:
    result = TestReport(location: loc, kind: trkCheckOk)

template strdiff*(str1, str2: string, loc: TestLocation): TestReport =
  bind strdiffImpl
  strdiffImpl(str1, str2, loc, astToStr(str1), astToStr(str2))

proc structdiff*(ptree1, ptree2: PPrintTree, loc: TestLocation): TestReport =
  let
    diff = treeDiff(ptree1, ptree2)

  if isSome(diff):
    result = TestReport(
      location: loc,
      kind: trkCheckFail,
      failKind: tfkStructDiff,
      structDiff: diff.get().newIt())

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



proc hasKindSimple[A, K](ast: A, kind: K): bool =
  ast.kind == kind



macro astdiff*(ast: typed, match: untyped, loc: TestLocation): untyped =
  let
    fails = genSym(nskVar, "fails")
    expr = ident("expr")
    toPath = bindSym("toPath")
    kindPredicate = bindSym("hasKind")

  proc aux(path, pattern: NimNode, pathIdx: seq[int]): NimNode =
    let idxLit = pathIdx.newLit()
    case pattern.kind:
      of nnkCall, nnkBracketExpr:
        let kind = pattern[0]
        let kindLit = kind.toStrLit()
        let wantLen = newLit(pattern.len - 1)
        result = quote do:
          if not `kindPredicate`(`path`, `kind`):
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

  if match.kind == nnkIdent:
    result = quote do:
      echo validateAst(`match`, `ast`)
      checkOk(`loc`)

  else:
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

  func splicePath(path, part: NimNode): NimNode =
    case part.kind:
      of nnkIdent:
        result = nnkDotExpr.newTree(path, part)

      of nnkBracket:
        result = nnkBracketExpr.newTree(path, part)

      of nnkStrLit, nnkIntLit:
        result = nnkBracketExpr.newTree(path, part)

      of nnkDotExpr:
        result = splicePath(
          splicePath(path, part[0]), part[1])

      else:
        raise newImplementKindError(part, part.lispRepr())


  func pathLit(path: NimNode): NimNode =
    path.toStrLit().strVal()["expr".len .. ^1].newLit()

  func checkedIndex(path: NimNode, idx: int, body: NimNode): NimNode =
    let
      idxLit = newLit(idx)
      pathLit = pathLit(path)

    quote do:
      if `idxLit` < len(`path`):
        `body`

      else:
        `fails`.add((
          `pathLit`,
          "Missing element at index " & $`idxLit`,
          $len(`path`)))


  proc callCmp(path, call: NimNode, desc: NimNode): NimNode =
    let pathLit = path.pathLit()

    result = quote do:
      if not(`call`):
        `fails`.add (`pathLit`, $`desc`, $`path`)


  proc aux(pattern, path: NimNode): NimNode
  proc itemCmp(path, value: NimNode): NimNode


  const litKinds = { nnkIntLit, nnkStrLit, nnkPrefix }

  proc auxOrCmp(pattern, path: NimNode): NimNode =
    if pattern.kind in litKinds:
      result = itemCmp(path, pattern)

    else:
      result = aux(pattern, path)



  proc itemCmp(path, value: NimNode): NimNode =
    let pathLit = path.pathLit()
    case value.kind:
      of litKinds - nnkPrefix, nnkIdent:
        result = quote do:
          if not(`path` == `value`):
            `fails`.add (`pathLit`, $`value`, $`path`)

      of nnkPrefix:
        let
          cmpExpr = nnkInfix.newTree(value[0], path, value[1])
          valueLit = value.toStrLit()

        result = quote do:
          if not(`cmpExpr`):
            `fails`.add (`pathLit`, `valueLit`, $`path`)

      of nnkPar:
        result = aux(value, path)

      else:
        raise newImplementKindError(value, value.treeRepr())

  proc kindHead(a: NimNode): NimNode =
    result = a
    while result.kind in { nnkCall, nnkObjConstr, nnkBracketExpr }:
      result = result[0]

  startHaxComp()


  var stmtLevel = 0
  proc aux(pattern, path: NimNode): NimNode =
    result = newStmtList()
    case pattern.kind:
      of nnkPar:
        for idx, check in pattern:
          result.add aux(check, path)

      of nnkStmtList:
        inc stmtLevel
        for idx, check in pattern:
          if stmtLevel > 1:
            result.add newCommentStmtNode(here())
            result.add checkedIndex(
              path, idx, aux(check, path.splicePath(newLit(idx))))

          else:
            result.add aux(check, path)

        dec stmtLevel


      of nnkExprColonExpr:
        result.add itemCmp(splicePath(path, pattern[0]), pattern[1])

      of nnkBracket:
        result.add itemCmp(
          splicePath(path, ident"len"), newLit(pattern.len))

        for idx, check in pattern:
          result.add auxOrCmp(check, nnkBracketExpr.newTree(path, newLit(idx)))

      of nnkTableConstr:
        for idx, check in pattern:
          result.add check[1].auxOrCmp(splicePath(path, check[0]))

      of nnkBracketExpr, nnkCall, nnkObjConstr:
        let
          pathLit = path.pathLit()
          wantKind = pattern.kindHead()
          kindLit = wantKind.toStrLit()


        result.add quote do:
          if not(`hasKind`(`path`, `wantKind`)):
            `fails`.add (`pathLit`, `kindLit`, $`path`.kind)


        inc stmtLevel
        for idx, check in pattern[1..^1]:
          case check.kind:
            of nnkIntLit, nnkStrLit, nnkPrefix:
              result.add itemCmp(
                nnkBracketExpr.newTree(path, newLit(idx)), check)

            of nnkExprEqExpr:
              result.add aux(check[1], nnkBracketExpr.newTree(path, check[0]))

            of nnkExprColonExpr:
              let path = splicePath(path, check[0])
              result.add check[1].auxOrCmp(path)

            of nnkStmtList:
              result.add aux(check, path)

            else:
              if stmtLevel > 3:
                result.add newCommentStmtNode(here() & $stmtLevel)
                result.add checkedIndex(
                  path, idx, aux(check, path.splicePath(newLit(idx))))

              else:
                result.add aux(check, path)

        dec stmtLevel

      else:
        raise newImplementKindError(pattern)


  let impl = aux(match, tmp)

  result = quote do:
    block:
      var `fails`: seq[TestMatchFail]
      let `tmp` = `obj`
      `impl`

      if `fails`.len > 0:
        matchCheckFailed(`loc`, `fails`)

      else:
        checkOk(`loc`)


  # echov result.repr()





















template hasTestContext(): untyped {.dirty.} =
  mixin testContext
  when declared(testContext):
    testContext is TestContext

  else:
    false

template wantContext*(): untyped {.dirty.} =
  bind hasTestContext
  when not hasTestContext():
    static:
      {.error: ""}

proc canRunTest(
    context: TestContext, conf: TestConf): bool =

  if context.skipNext:
    return false

  if context.globs.len == 0:
    return true

  for glob in context.globs:
    if conf.isSuite:
      if glob.suiteGlob.match(conf.name):
        return true

    else:
      if glob.testGlob.match(conf.name):
        return true

proc canRunBlock(context: TestContext): bool =
  not context.skipNext

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




proc makeConfigs(confIdent, configSection: NimNode): NimNode =
  result = newStmtList()
  if notNil(configSection):
    for node in configSection:
      case node.kind:
        of nnkCall:
          node.insert(1, confIdent)
          result.add node

        else:
          raise newImplementKindError(node)



macro suite*(name: static[string], body: untyped): untyped =
  var suiteConf: TestConf
  suiteConf.isSuite = true
  suiteConf.name = name


  let
    line = lineIInfo(body)
    suiteProc = genSym(nskProc, "`[" & name & "]`")
    suiteLit = newLit(suiteConf)
    whenSuite = suiteConf.getWhen()
    suiteFailed = bindSym("suiteFailed")
    locLit = testLocation(body).newLit()

  var
    newBody = newStmtList()
    configSection: NimNode

  for node in body:
    if node.kind == nnkPragmaBlock and node[0][0].eqIdent("configure"):
      configSection = node[1]

    else:
      newBody.add node

  let
    confIdent = genSym(nskVar, "conf")
    callConfigs = makeConfigs(confIdent, configSection)

  result = quote do:
    when `whenSuite`:
      var `confIdent` = `suiteLit`
      `callConfigs`

      let loc = `locLit`
      proc `suiteProc`(testContext {.inject.}: TestContext) =
        `newBody`

      when hasTestContext():
        maybeRunSuite(testContext, `suiteProc`, `confIdent`, loc)

      else:
        maybeRunSuite(getTestContext(), `suiteProc`, `confIdent`, loc)



macro test*(name: static[string], body: untyped): untyped =
  let
    line = lineIInfo(body)
    canRun = bindSym("canRunTest")
    testFailed = bindSym("testFailed")
    testStart = bindSym("testStart")
    testOk = bindSym("testEnd")
    report = bindSym("report")
    testLoc = testLocation(body).newLit()
    blockStart = bindSym("blockStart")
    blockEnd = bindSym("blockEnd")
    testComment = bindSym("testComment")

  proc newCommentReport(stmt, call: NimNode): NimNode =
    newCall(
      report,
      ident"testContext",
      newCall(
        call, stmt.strVal().newLit(),
        stmt.testLocation().newLit()))

  var newBody = newStmtList()
  var configSection: NimNode
  for stmt in body:
    case stmt.kind:
      of nnkPragmaBlock:
        # echo stmt.treeRepr2(pathIndexed = true)
        if stmt[0][0].eqIdent("configure"):
          configSection = stmt[1]

        else:
          newBody.add stmt

      of nnkBlockStmt:
        if stmt[0].kind != nnkEmpty:
          let name = stmt[0].toStrLit()
          let blockLoc = testLocation(stmt[1]).newLit()
          var blockBody = newStmtList()
          for stmt in stmt[1]:
            if stmt.kind == nnkCommentStmt:
              blockBody.add newCommentReport(stmt, bindSym("blockComment"))

            else:
              blockBody.add stmt

          newBody.add nnkIfStmt.newTree(
            nnkElifBranch.newTree(
              newCall(bindSym"canRunBlock", ident"testContext"),
              nnkBlockStmt.newTree(
                stmt[0],
                newStmtList(
                  newCall(report, ident"testContext",
                          newCall(blockStart, name, blockLoc)),
                  blockBody,
                  newCall(report, ident"testContext",
                          newCall(blockEnd, name, blockLoc))))))

        else:
          newBody.add stmt

      of nnkCommentStmt:
        newBody.add newCommentReport(stmt, bindSym"testComment")

      else:
        newBody.add stmt

  let
    testLit = newLit(TestConf(name: name))
    confIdent = genSym(nskVar, "conf")
    callConfigs = makeConfigs(confIdent, configSection)


  result = quote do:
    block:
      var
        `confIdent` = `testLit`
        loc = `testLoc`

      `callConfigs`

      if `canRun`(testContext, `confIdent`):
        try:
          `report`(testContext, `testStart`(`confIdent`, loc))
          `newBody`

          `report`(testContext, `testOk`(`confIdent`, loc))

        except:
          `report`(testContext, `testFailed`(
            `confIdent`, loc, getCurrentException()))


proc nowMs(): float64 =
  ## Gets current milliseconds.
  getMonoTime().ticks.float64 / 1_000_000.0

proc removeOutliers(s: var seq[SomeNumber]) =
  ## Remove numbers that are above 2 standard deviation.
  let avg = mean(s)
  let std = standardDeviation(s)
  var i = 0
  while i < s.len:
    if abs(s[i] - avg) > std*2:
      s.delete(i)
      continue
    inc i


template timeIt*(
    testName: string,
    params: seq[(string, TestValue)],
    iterations: untyped,
    body: untyped
  ): untyped =
  bind TestReport
  block:
    var
      num = 0
      total: float64
      deltas: seq[float64]


    while true:
      inc num
      let start = nowMs()

      block:
        body

      let finish = nowMs()

      let delta = finish - start
      total += delta
      deltas.add(delta)

      when iterations != 0:
        if num >= iterations:
          break

      else:
        if total > 5_000.0 or num >= 1000:
          break

    var minDelta = min(deltas)
    removeOutliers(deltas)
    var stat: RunningStat
    stat.push(deltas)

    report(testContext, TestReport(
      kind: trkTimeStats,
      name: testName,
      location: instantiationInfo(fullpaths = true).testLocation(),
      stat: stat))

template timeIt*(name: string, body: untyped): untyped =
  timeIt(name, @[], 0, body)


proc buildCheck(expr: NimNode): NimNode =
  # echo expr.treeRepr()
  let
    line = lineIINfo(expr)
    loc = testLocation(expr).newLit()
    testValue = bindSym("testValue")

  case expr.kind:
    of nnkInfix:
      let (lhs, rhs) = (expr[1], expr[2])
      let (lhsLit, rhsLit) = (expr[1].toStrLit(), expr[2].toStrLit())
      let (lhsId, rhsId) = (genSym(nskLet), genSym(nskLet))

      let
        doOp = nnkInfix.newTree(expr[0], lhsId, rhsId)
        opLit = expr[0].toStrLit()
        context = newLit(
          tern(expr[0].eqIdent("=="), tvcEqCompare, tvcNone))

      if expr[0].eqIdent(["is", "isnot", "of"]):
        result = quote do:
          block:
            {.line: `line`.}:
              let `lhsId` = `lhs`
              if not(`expr`):
                `report`(testContext, checkFailed(`loc`, {
                  `lhsLit`: `testValue`(typeof(`lhsId`), tvcTypeCompare),
                  `rhsLit`: `testValue`(`rhs`, tvcTypeCompare)
                }, tfkOpCheck, checkOp = `opLit`))


      else:
        result = quote do:
          block:
            {.line: `line`.}:
              let
                `lhsId` = `lhs`
                `rhsId` = `rhs`

              if not (`doOp`):
                `report`(testContext, checkFailed(`loc`, {
                  `lhsLit`: `testValue`($`lhsId`, `context`),
                  `rhsLit`: `testValue`($`rhsId`, `context`),
                }, tfkOpCheck, checkOp = `opLit`))


    else:
      if expr.kind in { nnkCall, nnkCommand } and
         expr[0].eqIdent([
           "strdiff", "structdiff",
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
                `report`(testContext, `checkFailed`(
                  `loc`, {"expr": `testValue`(`lit`, tvcNone)},
                  tfkPredicateFail))







macro check*(expr: untyped): untyped =
  result = newStmtList()
  case expr.kind:
    of nnkStmtList:
      for expr in expr:
        if expr.kind != nnkCommentStmt:
          result.add buildCheck(expr)

    else:
      result.add buildCheck(expr)

# import unittest
# proc testEq*[A, B](lhs: A, rhs: B) =
#   # TODO use LCS to highlight only parts that are different in red
#   # static:
#   #   assert compiles(lhs == rhs),
#   #    "Cannot directly compare objects of type" & $typeof(lhs) &
#   #      " and " & $typeof(rhs)

#   mixin fmt
#   if lhs != rhs:
#     let
#       lhsStr = ($lhs).replace("\e", "\\e")
#       rhsStr = ($rhs).replace("\e", "\\e")

#     # testEnded(
#     #   ConsoleOutputFormatter(colorOutput: true, isInSuite: true),
#     #   TestResult(testName: "Equality comparison", status: FAILED)
#     # )

#     let diffPos = mismatchStart(lhsStr, rhsStr)
#     if '\n' in lhsStr or '\n' in rhsStr:
#       let
#         linesA = lhsStr.split('\n')
#         linesB = rhsStr.split('\n')

#       var hadAny = false
#       for idx, line in zip(linesA, linesB):
#         if line[0] != line[1]:
#           echo fmt("LHS #{idx}: '{line[0]}'")
#           echo fmt("RHS #{idx}: '{line[1]}'")
#           hadAny = true
#           break

#       if not hadAny:
#         echo fmt("LHS: '{lhsStr}'")
#         echo fmt("RHS: '{rhsStr}'")
#         # else:
#         #   echo &"#{idx}: '{line[0]}' == '{line[1]}'"

#     else:
#       if (lhsStr.len > 50 or rhsStr.len > 50):
#         let start = max(diffPos - 20, 0)
#         let diffPos = max(diffPos, 1)
#         echo "LHS: ...\e[32m", lhsStr[start ..< diffPos], "\e[39m",
#           "\e[31m", lhsStr[diffPos ..< min(diffPos + 40, lhsStr.len)],
#           "\e[39m..."

#         echo "RHS: ...\e[32m", rhsStr[start ..< diffPos], "\e[39m",
#           "\e[31m", rhsStr[diffPos ..< min(diffPos + 40, rhsStr.len)],
#           "\e[39m..."

#         echo " ".repeat(28), "^".repeat(10)
#       else:
#         echo fmt("LHS: {lhsStr}")
#         echo fmt("RHS: {rhsStr}")

#         echo "    ", " ".repeat(diffPos + 1),
#                  "^".repeat(rhsStr.len() - diffPos + 1)

#     echo ""

# template assertEq*(lhs, rhs: untyped): untyped =
#   let lhsVal = lhs
#   let rhsVal = rhs
#   testEq(lhsVal, rhsVal)
#   let lInfo = instantiationInfo()
#   if not (lhsVal == rhsVal):
#     echo lhs.astToStr(), " == ", rhs.astToStr()
#     raiseAssert("Comparison failed on line " & $lInfo.line)


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


  let name = toStrLit(varname)
  result = quote do:
    for `varname` in `values`:
      addValue(testContext, `name`, testValue(`varname`, tvcParameter))
      `body`
      removeValue(testContext, `name`)



macro expect*(args: varargs[untyped]): untyped =
  var
    exceptionTypes: seq[NimNode]
    asVarType: NimNode
    asVar: NimNode

  for ex in args[0 .. ^2]:
    case ex.kind:
      of nnkInfix:
        exceptionTypes.add ex[1]
        asVarType = ex[1]
        asVar = ex[2]

      else:
        exceptionTypes.add ex

  result = nnkTryStmt.newTree()
  result.add newStmtList(args[^1])
  let
    report = bindSym"report"
    fail = bindSym"expectFail"
    ok = bindSym"expectOk"
    loc = testLocation(args[0]).newLit()

  result[^1].add quote do:
    `report`(testContext, `fail`(`loc`))


  if isNil(asVar):
    result.add nnkExceptBranch.newTree(exceptionTypes)
    result[^1].add quote do:
      `report`(testContext, `ok`(`loc`))

  else:
    # echo asVarType.treeRepr2()
    let tmp = genSym(nskLet)
    result.add nnkExceptBranch.newTree(nnkInfix.newTree(
      ident"as", asVarType, tmp))

    result[^1].add quote do:
      `report`(testContext, `ok`(`loc`))
      `asVar` = `tmp`

  result.add nnkExceptBranch.newTree()
  result[^1].add quote do:
    `report`(testContext, `fail`(getCurrentException(), `loc`))

  if notNil(asVar):
    result = quote do:
      var `asVar`: ref `asVarType`
      `result`




macro skip*(): untyped = discard

template fail*(msg: string = ""): untyped =
  bind report, testFailed, testLocation
  block:
    wantContext()
    report(testContext, testFailManual(
      instantiationInfo(fullpaths = true).testLocation(), msg))


template checkpoint*(mgs: string): untyped =
  block:
    wantContext()
    testContext.report checkpoint(
      instantiationInfo(fullpaths = true).testLocation(), msg)
