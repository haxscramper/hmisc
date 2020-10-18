const cbackend = not (defined(nimscript) or defined(js))

import ../types/colorstring
import ../algo/hstring_algo
import macros, strutils, strformat

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
      of lvlDebug: " DEBUG"
      of lvlInfo:   "\e[94m  INFO\e[39m"
      of lvlNotice: "\e[32mNOTICE\e[39m"
      of lvlWarn:   "\e[33m  WARN\e[39m"
      of lvlError:  "\e[31m ERROR\e[39m"
      of lvlFatal:  "\e[1m\e[35m FATAL\e[39m\e[21m"
      of lvlAll:    "ALL"
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
    # echo "  ".repeat(getIdent()), " ... "
    # echo logger.prevBuf.len, " times"

  logger.prevBuf.add args


when cbackend:
  method log(
    logger: ColorLogger, level: Level, args: varargs[string, `$`]) =
    (cast[var ColorLogger](logger)).logImpl(level, args)
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
            raiseAssert(&"#[ IMPLEMENT for kind {arg[1].kind} ]#")
      else:
        result.add arg

macro logcall*(
  inCall: typed, testRun: bool = false,
  lvl: static[Level] = lvlInfo): untyped =
  var args = newCall("log")
  args.add newCall(ident "Level", ident $lvl)
  args.add inCall[0].toStrLit()
  args.add newLit("(")

  for arg in inCall.typedArgs():
    if args.len > 4:
      args.add newLit(", ")

    case arg.kind:
      of nnkIdent, nnkSym:
        args.add newLit(arg.toStrLit().strVal() & " = ")
        args.add nnkPrefix.newTree(ident "$", arg)
      else:
        args.add arg

  args.add newLit(")")

  result = quote do:
    `args`
    `inCall`
