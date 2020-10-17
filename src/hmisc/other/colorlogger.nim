const cbackend = not (defined(nimscript) or defined(js))

import ../types/colorstring
import ../algo/hstring_algo
import macros, strutils

when cbackend:
  import logging
  export info, debug, notice, warn
  export Level

  type
    ColorLogger = ref object of Logger
      ident: int
      prevBuf: seq[string]

else:
  type
    ColorLogger = object
      ident: int
      prevBuf: seq[string]

    Level* = enum
      lvlAll
      lvlDebug
      lvlInfo
      lvlNotice
      lvlWarn
      lvlError
      lvlFatal
      lvlNone



when not cbackend:
  var globalLog = ColorLogger()

macro err*(args: varargs[untyped]): untyped =
  result = newCall("logError")
  for arg in args:
    result.add arg


proc identLog* =
  when cbackend:
    for handler in getHandlers():
      if ColorLogger(handler) != nil:
        inc ColorLogger(handler).ident
  else:
    inc globalLog.ident

proc dedentLog* =
  when cbackend:
    for handler in getHandlers():
      if ColorLogger(handler) != nil:
        dec ColorLogger(handler).ident
  else:
    dec globalLog.ident

proc getIdent*(): int =
  when cbackend:
    for handler in getHandlers():
      if ColorLogger(handler) != nil:
        return ColorLogger(handler).ident
  else:
    globalLog.ident

template logIdented*(body: untyped): untyped =
  try:
    identLog()
    body
  finally:
    dedentLog()

template logDefer*(logCmd, before: untyped): untyped =
  logCmd before
  identLog()
  defer: dedentLog()

proc startColorLogger*(): void =
  when cbackend:
    var logger = ColorLogger(ident: 0)
    addHandler logger
  else:
    discard

proc makeLogString(level: Level, args: varargs[string]): string =
  let ident = "  ".repeat(getIdent())
  let prefix =
    case level:
      of lvlDebug: "DEBUG"
      of lvlInfo: "\e[94mINFO\e[39m"
      of lvlNotice: "\e[32mNOTICE\e[39m"
      of lvlWarn: "\e[33mWARN\e[39m"
      of lvlError: "\e[31mERROR\e[39m"
      of lvlFatal: "\e[1m\e[35mFATAL\e[39m\e[21m"
      of lvlAll: "ALL"
      of lvlNone: ""

  let mlines = args.msgjoin().split("\n")
  result &= ident & prefix & " " & mlines[0] & "\n"

  for line in mlines[1 .. ^1]:
    result &= ident & " ".repeat(prefix.termLen()) & " " & line & "\n"

  result = result[0..^2]

proc logImpl(
  logger: var ColorLogger, level: Level, args: varargs[string, `$`]) =
  let args = args.join()
  if logger.prevBuf.len > 0 and logger.prevBuf[^1] != args:
    echo makeLogString(level, args)
    logger.prevBuf = @[]
  else:
    discard
    # echo logger.prevBuf.len, " times"

  logger.prevBuf.add args


when cbackend:
  method log(
    logger: var ColorLogger, level: Level, args: varargs[string, `$`]) =
    logger.logImpl(level, args)
else:
  proc log(
    logger: var ColorLogger, level: Level, args: varargs[string, `$`]) =
    logger.logImpl(level, args)

when not cbackend:
  proc log*(level: Level, args: varargs[string, `$`]) =
    globalLog.log(level, args)

  template debug*(args: varargs[string, `$`]) =
    log(lvlDebug, args)

  template info*(args: varargs[string, `$`]) =
    log(lvlInfo, args)

  template notice*(args: varargs[string, `$`]) =
    log(lvlNotice, args)

  template warn*(args: varargs[string, `$`]) =
    log(lvlWarn, args)

  template err*(args: varargs[string, `$`]) =
    log(lvlError, args)

  template fatal*(args: varargs[string, `$`]) =
    log(lvlFatal, args)
else:
  export log

proc logError*(args: varargs[string, `$`]): void =
  when cbackend:
    logging.error(args)
  else:
    globalLog.log(lvlError, args)
