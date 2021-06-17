import
  std/[macros, strtabs, strutils, enumerate],
  ../algo/[clformat, htemplates, halgorithm],
  ../types/colorstring

type
  HLogLevel* = enum
    logAll
    logTrace
    logDebug
    logInfo
    logNotice
    logWarn
    logError
    logFatal
    logNone

  HLogEvent* = enum
    logEvNone
    logEvSuccess
    logEvFail
    logEvWaitStart
    logEvWaitDone
    logEvExprDump

  HLogScopeKind = enum
    hskTask
    hskScope

  HLogScope = object
    file: string
    line, column: int
    name: string
    kind: HLogScopeKind

  HLogFormat = object
    str: ColoredString

  HLogger = ref object
    minLogLevel: HLogLevel
    lastLog: HLogLevel
    lastEvent: HLogEvent
    scopes: seq[HLogScope]

    eventPrefix: array[HLogEvent, HLogFormat]
    logPrefix: array[HLogLevel, HLogFormat]

    prefixLen: int

proc format(str: ColoredString): HLogFormat =
  HLogFormat(str: str)

proc newTermLogger*(): HLogger =
  result = HLogger(
    logPrefix: toMapArray({
      logTrace:  toColored("trace:", initStyle(styleDim)).format(),
      logDebug:  toColored("debug:",   initStyle(fgYellow)).format(),
      logInfo:   toColored("info:",     initStyle(fgMagenta)).format(),
      logNotice: toColored("notice:", initStyle(fgGreen)).format(),
      logWarn:   toColored("warn:",     initStyle(fgYellow)).format(),
      logError:  toColored("error:",   initStyle(fgRed)).format(),
      logFatal:  toColored("fatal:",   initStyle(bgMagenta, fgCyan)).format(),
    }),
    eventPrefix: toMapArray({
      logEvSuccess:   toColored("ok:",     initStyle(fgMagenta)).format(),
      logEvFail:      toColored("fail:", initStyle(fgGreen)).format(),
      logEvWaitStart: toColored("wait:",     initStyle(fgYellow)).format(),
      logEvWaitDone:  toColored("done:",   initStyle(fgGreen)).format(),
      logEvExprDump:  toColored("expr:", initStyle(styleItalic)).format()
    })
  )

  result.prefixLen = max(
    result.logPrefix.maxIt(it.str.len),
    result.eventPrefix.maxIt(it.str.len)
  )

import hpprint

proc prepareText*(logger: HLogger, text: varargs[string]): string =
  result = text.join(" ")
  if '\n' in result:
    var res = ""
    for line in result.split('\n'):
      res.add "\n"
      res.add repeat(" ", logger.scopes.len() * 2 + logger.prefixLen)
      res.add line

    return res



proc log*(
    logger: HLogger,
    level: HLogLevel,
    event: HLogEvent,
    position: (string, int, int),
    args: varargs[string, `$`]
  ) =

  let prefix =
    if (event == logEvExprDump):
      align($position[1] & ":", logger.prefixLen)

    elif (level == logTrace):
      logger.logPrefix[level].str.alignRight(logger.prefixLen) & " " &
        $position[1]

    elif (level == logger.lastLog and event == logEvNone) or
       (level == logNone and event == logger.lastEvent):
      repeat(" ", logger.prefixLen)

    elif event != logEvNone:
      logger.eventPrefix[event].str.alignRight(logger.prefixLen)

    else:
      logger.logPrefix[level].str.alignRight(logger.prefixLen)

  let indent = repeat("  ", logger.scopes.len())

  echo indent, prefix, " ", logger.prepareText(args)
  logger.lastLog = level
  logger.lastEvent = event

proc openScope*(
    logger: HLogger, kind: HLogScopeKind,
    file: string, line, column: int,
    scopeName: string
  ) =

  var scope = HLogScope(
    file: file, line: line, column: column, kind: kind)

  scope.name = scopeName
  logger.scopes.add scope



proc closeScope*(logger: HLogger) =
  let scope = logger.scopes.pop()

template thisScope*(
    logger: HLogger, name: string,
    kind: HLogScopeKind = hskScope
  ) =

  let (file, line, column) = instantiationInfo(fullPaths = true)
  openScope(logger, kind, file, line, column, name)
  defer: closeScope(logger)

template openScope*(logger: HLogger, kind: HLogScopeKind, name: string) =
  let (file, line, column) = instantiationInfo(fullPaths = true)
  openScope(logger, kind, file, line, column, name)

macro logScope*(varname: untyped, pr: untyped): untyped =
  result = pr

  result[^1] = newStmtList(
    newCall("openScope", varname, ident("hskTask"),
            newLit(basename(pr[0]).strVal())),

    pr[^1],
    newCall("closeScope", varname)
  )

template check*(
    logger: HLogger, expr: bool, desc: string,
    expected: bool = true,
    onMatch: HLogLevel = logInfo,
    onFail: HLogLevel = logNotice
  ): bool =

  let exprRes = expr

  if exprRes == expected:
    logger.log(onMatch, logEvSuccess, instantiationInfo(),
               desc, "was", hshow(exprRes))

  else:
    logger.log(onFail, logEvFail, instantiationInfo(),
               desc, "was", hshow(exprRes))

  exprRes

proc prepareDump*[T](expr: T): string =
  result =
    when expr is NimNode:
      toGreen(($expr.kind)[3..^1]) "\n" &
        expr.toStrLit().strVal().multiReplace({
          "\\\"" : "\"",
          "\\n" : "\n"
        })
    else:
      $expr

  when expr is string:
    if '\n' notin result:
      result = "\"" & result & "\""

  elif (expr is char):
    result =
      case expr:
        of '\n': "\\n"
        of '\t': "\\t"
        of '\r': "\\r"
        else: result

    result = "'" & result & "'"


template dump*(logger: HLogger, expr: untyped, other: varargs[string, `$`]): untyped =
  var args: seq[string]
  args.add astToStr(expr)
  args.add prepareDump(expr)
  for arg in other:
    args.add arg

  log(logger, logNone, logEvExprDump, instantiationInfo(), args)

template pdump*(logger: HLogger, expr: untyped, other: varargs[string, `$`]): untyped =
  var args: seq[string]
  args.add astToStr(expr)
  args.add "\n"
  args.add pstring(expr)
  for arg in other:
    args.add arg

  log(logger, logNone, logEvExprDump, instantiationInfo(), args)


template debug*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logDebug, logEvNone, instantiationInfo(), args)

template trace*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logTrace, logEvNone, instantiationInfo(), args)

template info*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logInfo, logEvNone, instantiationInfo(), args)

template notice*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logNotice, logEvNone, instantiationInfo(), args)

template warn*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logWarn, logEvNone, instantiationInfo(), args)

template error*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logError, logEvNone, instantiationInfo(), args)

template fatal*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logFatal, logEvNone, instantiationInfo(), args)

template wait*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logNone, logEvWaitStart, instantiationInfo(), args)

template waitFor*(logger: HLogger, name: string): untyped =
  log(logger, logNone, logEvWaitStart, instantiationInfo(),
      "Waiting for " & name & " to finish...")

template done*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logNone, logEvWaitDone, instantiationInfo(), args)

template fail*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logNone, logEvFail, instantiationInfo(), args)

template success*(logger: HLogger, args: varargs[string, `$`]): untyped =
  log(logger, logNone, logEvSuccess, instantiationInfo(), args)

import hpprint

when isMainModule:
  proc task(log: HLogger) {.logScope(log).} =
    if log.check(false, "test"): echo 123
    if log.check(true, "test"): echo 123
    log.info("hello")

  var l = newTermLogger()
  task(l)

  l.waitFor("test")
  l.done("Successfully ran 4 tests")
  l.done("Teardown ok")

  l.dump 90 + 2

  l.pdump [(0 + 90), (3)]
  l.trace "Test"
