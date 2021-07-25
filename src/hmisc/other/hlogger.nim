import
  ../algo/[
    clformat, htemplates, halgorithm, hlex_base, htext_algo],
  ../macros/argpass,
  ../other/[hpprint, oswrap],
  ../types/[colorstring, colortext],
  ../hdebug_misc,
  ../base_errors,
  ../helpers

import
  std/[
    macros, strtabs, strutils, enumerate, strformat,
    options, sequtils, tables, parseutils
  ]

# - TODO :: `traceIf` implementation for logger with dumps for all conditional
#   expressions and maybe their subparts.
# - TODO :: supports `raw` output to logger - immediately redirect to output


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
    logEvProcCall

  HLogScopeKind* = enum
    hskTask
    hskScope
    hskIndent
    hskChDir

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
    lastLogFile: string
    lastLogLine: int
    scopes: seq[HLogScope]

    eventPrefix: array[HLogEvent, HLogFormat]
    logPrefix: array[HLogLevel, HLogFormat]
    skipNl: int

    prefixLen: int

    showFile*: bool
    showLine*: bool
    leftAlignFiles*: int

proc format(str: ColoredString): HLogFormat =
  HLogFormat(str: str)

proc newTermLogger*(file: bool = false, line: bool = false): HLogger =
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

  result.showLine = line
  result.showFile = file

  result.prefixLen = max(
    result.logPrefix.maxIt(it.str.len),
    result.eventPrefix.maxIt(it.str.len)
  )

proc prepareText*(
    logger: HLogger,
    text: varargs[string],
    join: string
  ): string =

  result = text.join(join)
  var indent = logger.scopes.len() * 2 + logger.prefixLen
  if logger.leftAlignFiles > 0:
    indent += logger.leftAlignFiles + 2

  if '\n' in result:
    var res = ""
    for (idx, line) in result.split('\n').enumerate():
      res.add "\n "
      res.add repeat(" ", indent)
      res.add line

    return res



proc logImpl*(
    logger: HLogger,
    level: HLogLevel,
    event: HLogEvent,
    position: (string, int, int),
    args: seq[string],
    join: string = " "
  ) =

  assertRef logger
  var prefix =
    if (event == logEvPass):
      repeat(" ", logger.prefixLen)

    elif (event == logEvExprDump):
      " " |<< logger.prefixLen

    elif (level == logTrace):
      $logger.logPrefix[level].str.alignRight(logger.prefixLen) &
        (if logger.showFile.not(): " " & $position[1] else: "")

    elif (level == logger.lastLog and event == logEvNone) or
       (level == logNone and event == logger.lastEvent):
      repeat(" ", logger.prefixLen)

    elif event != logEvNone:
      $logger.eventPrefix[event].str.alignRight(logger.prefixLen)

    else:
      $logger.logPrefix[level].str.alignRight(logger.prefixLen)

  var filePrefix = ""
  if logger.showFile:
    fileprefix &= AbsFile(position[0]).name
    if logger.showLine:
      fileprefix &= ":"

  if logger.showLine:
    fileprefix &= $position[1]


  var indent = ""
  if logger.leftAlignFiles > 0:
    let lineRange =
      logger.lastLogLine .. logger.lastLogLine + 1

    # echo lineRange, " ", logger.lastLogLine
    # echo position
    if logger.lastLogFile == position[0] and
       position[1] in lineRange:

      indent.add " " |<< logger.leftAlignFiles

    else:
      logger.lastLogFile = position[0]
      indent.add toLink(
        position, filePrefix |<< logger.leftAlignFiles)

    logger.lastLogLine = position[1]

  else:
    prefix.add " "
    prefix.add toLink(position, filePrefix)

  indent.add repeat("  ", logger.scopes.len())

  if logger.skipNl > 0:
    dec logger.skipNl
    stdout.write indent, prefix, " ", logger.prepareText(args, join)

  else:
    stdout.writeline indent, prefix, " ", logger.prepareText(args, join)

  logger.lastLog = level
  logger.lastEvent = event

proc writeln*(logger: HLogger, text: varargs[string, `$`]) =
  stdout.writeline join(text, " ")

proc write*(logger: HLogger, text: varargs[string, `$`]) =
  stdout.write join(text, " ")

proc skipNl*(logger: HLogger, count: int = 1) =
  logger.skipNl += count

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

template indented*(logger: HLogger, body: untyped): untyped =
  indent(logger)
  body
  dedent(logger)

template withPositions*(logger: untyped): untyped =
  let (file, line) = (logger.showFile, logger.showLine)
  logger.showFile = false
  logger.showLine = false

  body

  logger.showFile = file
  logger.showLine = line



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
    logger.logImpl(
      onMatch, logEvSuccess, instantiationInfo(fullPaths = true), @[desc, "was", hshow(exprRes)])

  else:
    logger.logImpl(
      onFail, logEvFail, instantiationInfo(fullPaths = true), @[desc, "was", hshow(exprRes)])

  exprRes

proc prepareDump*[T](
  head: string, expr: T, other: sink seq[string]): seq[string] =

  result.add toGreen(head & ":")
  var tmp =
    when expr is NimNode:
      toGreen(($expr.kind)[3..^1]) "\n" &
        expr.toStrLit().strVal().multiReplace({
          "\\\"" : "\"",
          "\\n" : "\n"
        })
    else:
      $expr

  when expr is string:
    if '\n' notin tmp:
      tmp = "\"" & tmp & "\""

    else:
      tmp = "\n" & tmp

  elif (expr is char):
    tmp =
      case expr:
        of '\n': "\\n"
        of '\t': "\\t"
        of '\r': "\\r"
        else: tmp

    tmp = "'" & tmp & "'"

  result.add tmp
  result.add other

proc toStrSeq[T](s: T): seq[string] =
  for item in items(s):
    result.add $item

proc toStrSeq(s: varargs[string]): seq[string] =
  for item in items(s):
    result.add item


proc preparePDump*[T](
  head: string, expr: T, args: sink seq[string]): seq[string] =

  result.add toGreen(head & ":")
  result.add "\n="
  result.add pstring(expr)
  result.add args

proc dumpImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logNone, logEvExprDump, pos, args)

template dump*(
  logger: HLogger, expr: untyped, args: varargs[string, `$`]): untyped =
  dumpImpl(logger, instantiationInfo(fullPaths = true),
    prepareDump(astToStr(expr), expr, toStrSeq(args)))

template pdump*(
    logger: HLogger, expr: untyped, args: varargs[string, `$`]): untyped =
  dumpImpl(logger, instantiationInfo(fullPaths = true),
           preparePDump(astToStr(expr), expr, toStrSeq(args)))

proc debugImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logDebug, logEvNone, pos, args)

template debug*(logger: HLogger, args: varargs[string, `$`]): untyped =
  debugImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc traceImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logTrace, logEvNone, pos, args)

template trace*(logger: HLogger, args: varargs[string, `$`]): untyped =
  traceImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc infoImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logInfo, logEvNone, pos, args)

template info*(logger: HLogger, args: varargs[string, `$`]): untyped =
  infoImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc noticeImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logNotice, logEvNone, pos, args)

template notice*(logger: HLogger, args: varargs[string, `$`]): untyped =
  noticeImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc warnImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logWarn, logEvNone, pos, args)

template warn*(logger: HLogger, args: varargs[string, `$`]): untyped =
  warnImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc errImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logError, logEvNone, pos, args)

template err*(logger: HLogger, args: varargs[string, `$`]): untyped =
  errImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc fatalImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logFatal, logEvNone, pos, args)

template fatal*(logger: HLogger, args: varargs[string, `$`]): untyped =
  fatalImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc waitImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logNone, logEvWaitStart, pos, args)

template wait*(logger: HLogger, args: varargs[string, `$`]): untyped =
  waitImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc doneImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logNone, logEvWaitDone, pos, args)

template done*(logger: HLogger, args: varargs[string, `$`]): untyped =
  doneImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc failImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logNone, logEvFail, pos, args)

template fail*(logger: HLogger, args: varargs[string, `$`]): untyped =
  failImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

proc successImpl*(
  logger: HLogger, pos: (string, int, int), args: seq[string]) =
  logImpl(logger, logNone, logEvSuccess, pos, args)

template success*(logger: HLogger, args: varargs[string, `$`]): untyped =
  successImpl(logger, instantiationInfo(fullPaths = true), toStrSeq(args))

template waitFor*(logger: HLogger, name: string): untyped =
  logImpl(logger, logNone, logEvWaitStart, instantiationInfo(fullPaths = true),
    @["Waiting for " & name & " to finish..."])

macro loggerField*(
    T: typed, field: untyped,
    doExport: static[bool] = false,
    module: string = "hlogger"
  ): untyped =

  let
    module = ident(module.strVal)
    vas = nnkBracketExpr.newTree(ident"varargs", ident"string", ident"$")

  var
    iinfo = newCall("instantiationInfo")

  iinfo.add nnkExprEqExpr.newTree(ident("fullPaths"), newLit(true))

  result = newStmtList()
  for name in ["success", "fail", "done", "wait", "notice",
               "fatal", "err", "warn", "info", "trace", "debug"]:

    let
      implId = ident(name & "Impl")
      nameId = if doExport:
                 nnkPostfix.newTree(ident"*", ident(name))

               else:
                 ident(name)

    result.add quote do:
      template `nameId`(o: `T`, args: `vas`): untyped =
        `module`.`implId`(o.`field`, `iinfo`, toStrSeq(args))

  let (dumpId, pdumpId) = (ident"dump", ident"pdump")

  if doExport:
    result.add quote do:
      template `dumpId`*(o: `T`, expr: untyped, args: `vas`): untyped =
        dumpImpl(o.`field`, `iinfo`,
                 prepareDump(astToStr(expr), expr, toStrSeq(args)))

      template `pdumpId`*(o: `T`, expr: untyped, args: `vas`): untyped =
        dumpImpl(o.`field`, `iinfo`,
                 preparePDump(astToStr(expr), expr, toStrSeq(args)))
          # @[astToStr(expr), "\n=", pstring(expr)] & toStrSeq(args))

  else:
    result.add quote do:
      template `dumpId`(o: `T`, expr: untyped, args: `vas`): untyped =
        dumpImpl(o.`field`, args, `iinfo`,
          prepareDump(astToStr(expr), expr, toStrSeq(args)))

      template `pdumpId`(o: `T`, expr: untyped, args: `vas`): untyped =
        dumpImpl(o.`field`, args, `iinfo`,
          @[astToStr(expr), "\n=", pstring(expr)] & toStrSeq(args))


template changeDir*(logger: HLogger, dir: AbsDir, body: untyped): untyped =
  let (file, line, column) = instantiationInfo(fullPaths = true)
  openScope(logger, hskChDir, file, line, column, "")
  withNewDir dir:
    trace(l, (file, line, column), "Changed dir to", dir)
    body

  closeScope(logger)



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

  if result.a < 0: result.a = 0
  if base[result.a] == '\n':
    inc result.a


  while result.b < base.len and base[result.b] != '\n':
    inc result.b

  while after > 0:
    inc result.b
    while result.b < base.len and base[result.b] != '\n':
      inc result.b

    dec after

  if result.b > base.high: result.b = base.high
  if base[result.b] == '\n': dec result.b



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
  if line == 1 and around[0] > 0:
    result &= @[""]

  result &= text.split('\n')


proc logLines*(
    logger: HLogger, base: string, center: int,
    lang: string, column: int = -1
  ) =

  var lineIdx = center - 1
  for line in base.linesAround(center, (1, 1)):
    let arrow =
      if lineIdx == center:
        toGreen(&"{lineIdx:<4}#>")
      else:
        &"{lineIdx:<4}  "


    logger.debug(arrow, colorizeToStr(line, lang))
    if lineIdx == center and column > 0:
      logger.debug "      " & repeat(" ", column) & "^"

    inc lineIdx

    # REVIEW maybe also raw arrow from `#>` and annotate it:
    #
    # 763        if arg > 0:
    # 764 #>       mainProc(arg - 1) # Comment
    # 765 |      raise newException(OSError, "123123123")
    #     |
    #     +- Annotation for an arrow?

proc logLines*(
  logger: HLogger, file: AbsFile, center: int,
  lang: string, column: int = -1) =

  logger.logLines(file.readFile(), center, lang, column)

from os import nil

method log*(ex: ref Exception, logger: HLogger) {.base.} =
  if '\n' notin ex.msg:
    for line in wrapOrgLines(ex.msg, 80, simple = true):
      logger.err line

  else:
    logger.err ex.msg


import ./hshell

method log*(ex: ShellError, logger: HLogger) =
  logger.err ex.msg

proc logStackTrace*(
    logger: HLogger,
    e: ref Exception,
    showError: bool = true,
    ignoreAssert: bool = true,
    source: bool = true
  ) =

  let (showFile, showLine, leftAlignFiles) =
    (logger.showFile, logger.showLine, logger.leftAlignFiles)

  logger.showFile = false
  logger.showLine = false
  logger.leftAlignFiles = 0

  if not isNil(e) and showError:
    e.log(logger)
    # logger.err e.msg

  let stackEntries =
    if not isNil(e):
      e.getStackTraceEntries()

    else:
      getStackTraceEntries()

  let choosenim = os.getHomeDir() & ".choosenim"

  var fileW = 0
  for tr in stackEntries:
    let (_, name, ext) = os.splitFile($tr.filename)
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

    var (_, name, ext) = os.splitFile(filename)
    if ext.len > 0:
      ext = ext[1 ..^ 1]

    var filePref = toLink(($tr.filename, tr.line, 0), $name.alignLeft(fileW))
    if (not foundErr) and idx + 1 < stackEntries.len:
      let next = stackEntries[idx + 1]
      let nextFile = $next.filename
      if nextFile.startsWith(choosenim) or startsWith(
        $next.procname, @["expect", "assert"]):
        filePref = filePref.toRed()
        foundErr = true

    if $tr.procname == "failedAssertImpl" and ignoreAssert:
      return

    logger.debug prefix & (filePref) & " " &
      $($tr.procname).toYellow()

    if filename == "":
      continue

    # proc logEntry(idx: int) =

    let line = tr.line

    if source:
      let fileText =
        block:
          let
            filename = $tr.filename

          if filename notin files:
            files[filename] = filename.readFile()

          files[filename]

      logger.debug("")
      logger.indent()
      logger.logLines(fileText, line, ext)
      logger.dedent()
      logger.debug("")

  logger.showFile = showFile
  logger.showLine = showLine
  logger.leftAlignFiles = leftALignFiles

proc loggerOutConverter*(
    stream: var PosStr,
    cmd: ShellCmd, state: var Option[HLogger]): Option[bool] =

  let line = stream.readLine()
  if ?stream or line notin ["", "\n"]:
    state.get().debug("|", line)

proc loggerErrConverter*(
    stream: var PosStr,
    cmd: ShellCmd, state: var Option[HLogger]): Option[bool] =

  state.get().warn(stream.readLine())

proc prettyShellCmd(cmd: ShellCmd): string =
  result = cmd.bin
  let max = 80

  var lineLen = cmd.bin.len

  for arg in cmd:
    let
      str = arg.toStr(cmd.conf, true)
      len = str.termLen()

    if lineLen + len + 1 > max:
      result &= repeat(" ", clamp(max - lineLen, 0, high(int))) & "\\\n    "
      lineLen = 4

    elif result.len > 0:
      result &= " "
      inc lineLen

    result &= str
    lineLen += len

proc logShellCmd(
    logger: HLogger, pos: (string, int, int), shellCmd: ShellCmd) =
  infoImpl(logger, pos, @["Running shell", "'" & shellCmd.bin & "'"])
  debugImpl(logger, pos, @[shellCmd.prettyShellCmd()])


proc execShell*(
    logger: HLogger, pos: (string, int, int), shellCmd: ShellCmd,
    outLog: StreamConverter[ShellCmd, bool, HLogger] = loggerOutConverter,
    errLog: StreamConverter[ShellCmd, bool, HLogger] = loggerErrConverter,
    logRaised: bool = false,
    execTimeoutMs: int = high(int)
  ) =

  logShellCmd(logger, pos, shellCmd)

  let (outIter, errIter) = makeShellRecordIter(
    shellCmd, outLog, errLog,
    state = some logger,
    execTimeoutMs = execTimeoutMs
  )

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

template execShell*(logger: HLogger, shellCmd: ShellCmd): untyped =
  execShell(logger, instantiationInfo(fullPaths = true), shellCmd)


proc runShell*(
    logger: HLogger, pos: (string, int, int), shellCmd: ShellCmd,
    stdin: string = ""
  ): ShellExecResult =
  logShellCmd(logger, pos, shellCmd)
  result = argpass(runShell(shellCmd), stdin)
  done(logger)

proc runShellResult*(
    logger: HLogger, pos: (string, int, int), shellCmd: ShellCmd,
    stdin: string = "",
    execTimeoutMs: int = high(int)
  ): ShellResult =
  logShellCmd(logger, pos, shellCmd)
  result = argpass(shellResult(shellCmd), stdin, execTimeoutMs)

  if result.resultOk:
    done(logger)

template runShell*(
    logger: HLogger, shellCmd: ShellCmd,
    stdin: string = ""
  ): ShellExecResult =
  argpass runShell(logger, instantiationInfo(fullPaths = true), shellCmd),
     stdin

template runShellResult*(
    logger: HLogger, shellCmd: ShellCmd,
    stdin: string = "",
    execTimeoutMs: int = high(int)
  ): ShellResult =
  argpass runShellResult(logger, instantiationInfo(fullPaths = true), shellCmd),
     stdin, execTimeoutMs


func typedArgs*(call: NimNode): seq[NimNode] =
  for arg in call[1..^1]:
    case arg.kind:
      of nnkHiddenStdConv:
        case arg[1].kind:
          of nnkBracket:
            for elem in arg[1]:
              case elem.kind:
                of nnkHiddenCallConv:
                  result.add elem[1]
                else:
                  result.add elem
          else:
            raise newUnexpectedKindError(arg[1])

      else:
        result.add arg

macro execCode*(
    logger: HLogger,
    inCall: typed,
    dryRun: bool = false,
    lvl: static[HLogLevel] = logInfo
  ): untyped =

  var args = newCall("logImpl")
  let iinfo = inCall.lineInfoObj()

  args.add logger
  args.add newLit(lvl)
  args.add newLit(logEvProcCall)
  args.add newLit((iinfo.filename, iinfo.line, iinfo.column))

  var strArgs = nnkBracket.newTree()
  for idx, arg in inCall.typedArgs():
    var buf = if idx == 0: inCall[0].strVal() & " " else: ", "

    case arg.kind:
      of nnkIdent, nnkSym:
        buf.add arg.toStrLit().strVal() & " ="
        strArgs.add newLit(buf)
        strArgs.add nnkPrefix.newTree(ident "$", arg)

      else:
        if buf.len > 0:
          strArgs.add newLit(buf)

        strArgs.add arg


  args.add nnkPrefix.newTree(ident("@"), strArgs)

  result = quote do:
    `args`
    if not `dryRun`:
      `inCall`
