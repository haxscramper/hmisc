import std/[macros]

type
  HLogLevel* = enum
    logAll
    logDebug
    logInfo
    logNotice
    logWarn
    logError
    logFatal
    logNone

  HLogScopeKind = enum
    hskTask

  HLogScope = object
    file: string
    line, column: string
    case kind: HLogScopeKind
      of hskTask:
        taskname: string

  HLogger = ref object
    minLogLevel: HLogLevel
    lastLog: HLogLevel
    scopes: HLogScope

proc log*(logger: HLogger, args: varargs[string, `$`]) =
  discard

proc openScope*(
    logger: HLogger, kind: HLogScopeKind,
    file: string, line, column: int
  ) =

  echo "Open scope", file, line, " ", column


proc closeScope*(logger: HLogger) =
  discard


template openScope*(logger: HLogger, kind: HLogScopeKind) =
  let (file, line, column) = instantiationInfo(fullPaths = true)
  openScope(logger, kind, file, line, column)


macro logScope*(varname: untyped, pr: untyped): untyped =
  result = pr

  result[^1] = newStmtList(
    newCall("openScope", varname, ident("hskTask")),
    pr[^1],
    newCall("closeScope", varname)
  )


when isMainModule:
  proc task(log: HLogger) {.logScope(log).} =
    echo "123123"

  var logger = HLogger()
  task(logger)
