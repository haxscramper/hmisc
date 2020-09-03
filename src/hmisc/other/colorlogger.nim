import logging, macros, strutils

macro err*(args: varargs[untyped]): untyped =
  result = newCall(newDotExpr(ident "logging", ident "error"))
  for arg in args:
    result.add arg



type
  ColorLogger = ref object of Logger
    ident: int

proc identLog* =
  for handler in getHandlers():
    if ColorLogger(handler) != nil:
      inc ColorLogger(handler).ident

proc dedentLog* =
  for handler in getHandlers():
    if ColorLogger(handler) != nil:
      dec ColorLogger(handler).ident

method log*(logger: ColorLogger, level: Level, args: varargs[string, `$`]) =
  let ident = "  ".repeat(logger.ident)
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

  for line in args.join(" ").split("\n"):
    echo ident, prefix, " ", line

proc startColorLogger*(): void =
  var logger = ColorLogger(ident: 2)
  addHandler logger
