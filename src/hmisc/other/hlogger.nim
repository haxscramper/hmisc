import
  ../algo/[clformat, htemplates, halgorithm, hlex_base],
  ../types/[colorstring, colortext]

import
  std/[
    macros, strtabs, strutils, enumerate,
    options, sequtils, tables, parseutils
  ]

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
    hskIndent

  HLogScope = object
    file: string
    line, column: int
    name: string
    kind: HLogScopeKind

  HLogFormat = object
    str: ColoredString

  HLogger* = ref object
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
      res.add "\n "
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

proc indent*(logger: HLogger) =
  openScope(logger, hskIndent, "", 0, 0, "")


proc dedent*(logger: HLogger) =
  closeScope(logger)


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
  args.add "\n="
  args.add pstring(expr)
  for arg in other:
    args.add arg

  log(logger, logNone, logEvExprDump, instantiationInfo(), args)



proc debug*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logDebug, logEvNone, pos, args)

template debug*(logger: HLogger, args: varargs[string, `$`]): untyped =
  debug(logger, instantiationInfo(), args)

proc trace*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logTrace, logEvNone, pos, args)

template trace*(logger: HLogger, args: varargs[string, `$`]): untyped =
  trace(logger, instantiationInfo(), args)

proc info*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logInfo, logEvNone, pos, args)

template info*(logger: HLogger, args: varargs[string, `$`]): untyped =
  info(logger, instantiationInfo(), args)

proc notice*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNotice, logEvNone, pos, args)

template notice*(logger: HLogger, args: varargs[string, `$`]): untyped =
  notice(logger, instantiationInfo(), args)

proc warn*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logWarn, logEvNone, pos, args)

template warn*(logger: HLogger, args: varargs[string, `$`]): untyped =
  warn(logger, instantiationInfo(), args)

proc err*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logError, logEvNone, pos, args)

template err*(logger: HLogger, args: varargs[string, `$`]): untyped =
  err(logger, instantiationInfo(), args)

proc fatal*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logFatal, logEvNone, pos, args)

template fatal*(logger: HLogger, args: varargs[string, `$`]): untyped =
  fatal(logger, instantiationInfo(), args)

proc wait*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvWaitStart, pos, args)

template wait*(logger: HLogger, args: varargs[string, `$`]): untyped =
  wait(logger, instantiationInfo(), args)

proc done*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvWaitDone, pos, args)

template done*(logger: HLogger, args: varargs[string, `$`]): untyped =
  done(logger, instantiationInfo(), args)

proc fail*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvFail, pos, args)

template fail*(logger: HLogger, args: varargs[string, `$`]): untyped =
  fail(logger, instantiationInfo(), args)

proc success*(
  logger: HLogger, pos: (string, int, int), args: varargs[string, `$`]) =
  log(logger, logNone, logEvSuccess, pos, args)

template success*(logger: HLogger, args: varargs[string, `$`]): untyped =
  success(logger, instantiationInfo(), args)

template waitFor*(logger: HLogger, name: string): untyped =
  log(logger, logNone, logEvWaitStart, instantiationInfo(),
      "Waiting for " & name & " to finish...")


func findLineRange*(
    base: string,
    start: Slice[int],
    around: (int, int) = (0, 0)
  ): Slice[int] =
  result = start

  var
    before = around[0]
    after = around[1]

  while result.a > 0 and base[result.a] != '\n':
    dec result.a

  while before > 0:
    dec result.a
    while result.a > 0 and base[result.a] != '\n':
      dec result.a

    dec before

  if base[result.a] == '\n':
    inc result.a


  while result.b < base.len and base[result.b] != '\n':
    inc result.b

  while after > 0:
    inc result.b
    while result.b < base.len and base[result.b] != '\n':
      inc result.b

    dec after

  if result.b >= base.len or base[result.b] == '\n':
    dec result.b


func lineTextAround*(
    base: string, charRange: Slice[int], around: (int, int) = (1, 1)):
  tuple[text: string, startPos, endPos: int] =
  var slice = base.findLineRange(charRange, around)
  result.text = base[slice]
  result.startPos = charRange.a - slice.a
  result.endPos = result.startPos + (charRange.b - charRange.a)

func linesAround*(
    base: string, line: int, around: (int, int) = (1, 1)):
  seq[string] =

  var
    currLine = 1
    pos = 0

  while pos < base.len and currLine < line:
    if base[pos] == '\n': inc currLine
    inc pos

  let (text, _, _) = lineTextAround(base, pos .. pos, around)
  return text.split('\n')


proc logLines*(
    logger: HLogger, base: string, center: int, lang: string) =

  var lineIdx = center - 1
  for line in base.linesAround(center, (1, 1)):
    let arrow = if lineIdx == center: toGreen("#>") else: "  "
    logger.debug(alignLeft($lineIdx, 3), arrow, colorizeToStr(line, lang))
    inc lineIdx

    # REVIEW maybe also raw arrow from `#>` and annotate it:
    #
    # 763        if arg > 0:
    # 764 #>       mainProc(arg - 1) # Comment
    # 765 |      raise newException(OSError, "123123123")
    #     |
    #     +- Annotation for an arrow?



import os

proc logStackTrace*(logger: HLogger, e: ref Exception) =
  let stackEntries =
    if e != nil:
      e.getStackTraceEntries()
    else:
      getStackTraceEntries()

  let choosenim = getHomeDir() & ".choosenim"

  var fileW = 0
  for tr in stackEntries:
    let (_, name, ext) = splitFile($tr.filename)
    fileW = max(name.len, fileW)

  var
    files: Table[string, string]
    foundErr: bool = false
    lastPos: (string, int)
    repeated = -1

  let maxIdx = stackEntries.high()
  for idx, tr in stackEntries:
    let filename: string = $tr.filename
    let prefix =
      if not filename.startsWith(choosenim):
        if ($tr.procname).startsWith(@["expect", "assert"]):
          "(asr) ".toBlue()
        else:
          "(usr) ".toGreen()
      else:
        "(sys) "

    var (_, name, ext) = filename.splitFile()
    ext = ext[1 ..^ 1]
    var filePref = $name.alignLeft(fileW)
    if (not foundErr) and idx + 1 < stackEntries.len:
      let next = stackEntries[idx + 1]
      let nextFile = $next.filename
      if nextFile.startsWith(choosenim) or startsWith(
        $next.procname, @["expect", "assert"]):
        filePref = filePref.toRed()
        foundErr = true

    logger.debug prefix & (filePref) & " :" &
      $(($tr.line).alignLeft(4)) &
      " " &
      $($tr.procname).toYellow()

    proc logEntry(idx: int) =
      let
        filename = $stackEntries[idx].filename
        line = stackEntries[idx].line

      let fileText =
        block:
          if filename notin files:
            files[filename] = filename.readFile()

          files[filename]

      logger.debug("")
      logger.indent()
      logger.logLines(fileText, line, ext)
      logger.dedent()
      logger.debug("")


    let nowPos = (filename, tr.line)
    if lastPos[0].len == 0:
      lastPos = nowPos
      logEntry(idx)
      repeated = 0

    elif lastPos != nowPos and idx < maxIdx:
      lastPos = nowPos
      logEntry(idx)
      repeated = 0

    elif idx == maxIdx:
      logEntry(idx)

    else:
      lastPos = nowPos
      inc repeated


import ./hshell

proc loggerOutConverter*(
    stream: var PosStr,
    cmd: ShellCmd, state: var Option[HLogger]): Option[bool] =

  state.get().debug(stream.readLine())

proc loggerErrConverter*(
    stream: var PosStr,
    cmd: ShellCmd, state: var Option[HLogger]): Option[bool] =

  state.get().warn(stream.readLine())

method log*(ex: ref Exception, logger: HLogger) {.base.} =
  logger.err ex.msg

method log*(ex: ShellError, logger: HLogger) =
  logger.err ex.msg

proc runShell*(
    logger: HLogger, pos: (string, int, int), shellCmd: ShellCmd,
    outLog: StreamConverter[ShellCmd, bool, HLogger] = loggerOutConverter,
    errLog: StreamConverter[ShellCmd, bool, HLogger] = loggerErrConverter,
    logRaised: bool = true
  ) =
  info(logger, pos, "Running shell", "'" & shellCmd.bin & "'")
  debug(logger, pos, shellCmd.toLogStr())

  let (outIter, errIter) = makeShellRecordIter(
    shellCmd, outLog, errLog, state = some logger)

  if logRaised:
    try:
      for _ in outIter:
        discard

      for _ in errIter:
        discard

    except ShellError as e:
      log(e, logger)
      e.wasLogged = true
      raise e

  else:
    for _ in outIter:
      discard

    for _ in errIter:
      discard

  done(logger)

template runShell*(logger: HLogger, shellCmd: ShellCmd): untyped =
  runShell(logger, instantiationInfo(), shellCmd)






# import hpprint

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

  l.pdump [(0 + 90), (3)]
  l.trace "Test"
  l.runShell shellCmd(ls, "/tmp")
  l.runShell shellCmd(ls, "/;skldfj;aslkdffjj;alskdjjf;")
