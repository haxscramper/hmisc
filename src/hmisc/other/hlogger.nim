import
  std/[macros, strtabs, strutils, enumerate, options, sequtils],
  ../algo/[clformat, htemplates, halgorithm, hlex_base],
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
    logEvPass
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
    for (idx, line) in result.split('\n').enumerate():
      res.add "\n"
      if idx == 0: res.add " "
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
    if (event == logEvPass):
      repeat(" ", logger.prefixLen)

    elif (event == logEvExprDump):
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



proc debug*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logDebug, logEvNone, pos, args)

proc trace*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logTrace, logEvNone, pos, args)

proc info*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logInfo, logEvNone, pos, args)

proc notice*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNotice, logEvNone, pos, args)

proc warn*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logWarn, logEvNone, pos, args)

proc error*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logError, logEvNone, pos, args)

proc fatal*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logFatal, logEvNone, pos, args)

proc wait*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvWaitStart, pos, args)

proc done*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvWaitDone, pos, args)

proc fail*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvFail, pos, args)

proc success*(logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvSuccess, pos, args)

template debug*(logger: HLogger, args: varargs[string, `$`]): untyped =
  debug(logger, instantiationInfo(), args)

template trace*(logger: HLogger, args: varargs[string, `$`]): untyped =
  trace(logger, instantiationInfo(), args)

template info*(logger: HLogger, args: varargs[string, `$`]): untyped =
  info(logger, instantiationInfo(), args)

template notice*(logger: HLogger, args: varargs[string, `$`]): untyped =
  notice(logger, instantiationInfo(), args)

template warn*(logger: HLogger, args: varargs[string, `$`]): untyped =
  warn(logger, instantiationInfo(), args)

template error*(logger: HLogger, args: varargs[string, `$`]): untyped =
  error(logger, instantiationInfo(), args)

template fatal*(logger: HLogger, args: varargs[string, `$`]): untyped =
  fatal(logger, instantiationInfo(), args)

template wait*(logger: HLogger, args: varargs[string, `$`]): untyped =
  wait(logger, instantiationInfo(), args)

template done*(logger: HLogger, args: varargs[string, `$`]): untyped =
  done(logger, instantiationInfo(), args)

template fail*(logger: HLogger, args: varargs[string, `$`]): untyped =
  fail(logger, instantiationInfo(), args)

template success*(logger: HLogger, args: varargs[string, `$`]): untyped =
  success(logger, instantiationInfo(), args)

template waitFor*(logger: HLogger, name: string): untyped =
  log(logger, logNone, logEvWaitStart, instantiationInfo(),
      "Waiting for " & name & " to finish...")


import ./hshell

proc loggerOutConverter*(
    stream: var PosStr,
    cmd: ShellCmd, state: var Option[HLogger]): Option[bool] =

  state.get().debug(stream.readLine())

proc loggerErrConverter*(
    stream: var PosStr,
    cmd: ShellCmd, state: var Option[HLogger]): Option[bool] =

  state.get().warn(stream.readLine())

proc runShell*(
    logger: HLogger, pos: (string, int, int), shellCmd: ShellCmd,
    outLog: StreamConverter[ShellCmd, bool, HLogger] = loggerOutConverter,
    errLog: StreamConverter[ShellCmd, bool, HLogger] = loggerErrConverter
  ) =
  info(logger, pos, "Running shell", "'" & shellCmd.bin & "'")
  debug(logger, pos, shellCmd.toLogStr())

  let (outIter, errIter) = makeShellRecordIter(
    shellCmd, outLog, errLog, state = some logger)

  for _ in outIter:
    discard

  for _ in errIter:
    discard

  done(logger)

template runShell*(logger: HLogger, shellCmd: ShellCmd): untyped =
  runShell(logger, instantiationInfo(), shellCmd)






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

  l.runShell shellCmd(ls, "/tmp")
