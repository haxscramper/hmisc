import colechopkg/lib
import macros
import terminal
import re
import helpers
import tables
import sequtils
import strformat
import os
export os
import strutils
import logging
import shell
import types/colorstring

export shell
export helpers

##[

.. contents::

   Panacea for paranoia

]##

## Collection of helper functions to provide verbose messages,
## additional exception types and logging

#=======================  more verbose exceptions  =======================#

type
  VerboseError = ref object of CatchableError
    causes: seq[string]

#============================  shell logging  ============================#


const noShellMsg*: set[DebugOutputKind] = {}

var defLogIndentation {.inject.} = 0

proc die*() {.discardable, noreturn.} =
  ceUserInfo2("Terminating program", defLogIndentation)
  quit 1


proc findFirstFile*(
  pattern: string, filePurpose: string, debug = true): string =
  ## Find first file that matches glob
  let notebooks = toSeq(walkFiles(pattern))
  if notebooks.len < 1:
    ceUserError0(&"No {filePurpose}s found in directory")
    die()
  elif notebooks.len > 1:
    ceUserWarn(&"Multiple {filePurpose}s found in directory, using first")
    for n in notebooks:
      ceUserLog0(n, 2)
    notebooks[0]
  else:
    ceUserInfo0(&"Found single {filePurpose}")
    notebooks[0]


#===========================  logging support  ===========================#

type
  LoggingConf* = enum
    lcFileLogging
    lcPrintLogging

    lcUsePrefix
    lcUseFileName
    lcUseLine
    lcUseLogging
    lcUseReflow

  IInfo = tuple[filename: string, line: int, column: int]

var defLogPrefixMap: Table[string, string]
var defLogConfMap: Table[string, set[LoggingConf]]
var defLogLoggerMap: Table[string, FileLogger]
var defLogLevelMap: Table[string, Level]

# Global set of configurations with hard override for any printing
var globalDisabled: set[LoggingConf]
var globalEnabled: set[LoggingConf]

proc isExplicitlyDisabled*(c: LoggingConf): bool =
  c in globalDisabled

proc isExplicitlyEnabled*(c: LoggingConf): bool =
  (not c.isExplicitlyDisabled()) or
  (c in globalEnabled)

proc increaseLogIndent*() = defLogIndentation += 4
proc decreaseLogIndent*() = defLogIndentation -= 4

template runIndentedLog*(body: untyped): untyped =
  increaseLogIndent()
  body
  decreaseLogIndent()


template getLogConf*(f: string): untyped =
  assert defLogConfMap.hasKey(f),
         "Attempt to use logging without init configuration"

  defLogConfMap[f]

template runTempConfigLock*(
  newConf: openarray[(LoggingConf, bool)], body: untyped): untyped =
  ## Add/remove configuration from global config, run body and then
  ## restore configuration back (by using reverse operation)

  let file = instantiationInfo().filename

  let startGlobalDis = globalDisabled
  let startGlobalEnb = globalEnabled

  for item in newConf:
    if item[1]: # true => adding to conf
      globalEnabled.incl item[0]
    else:
      globalDisabled.incl item[0]

  body

  globalDisabled = startGlobalDis
  globalEnabled = startGlobalEnb
  # globalConfig = startConf
  # for item in newConf:
  #   # true & was actually added => must remove
  #   if item[1] and (item[0] notin startConf):
  #     # globalIncluded.excl item[0]

  #   # was actually removed
  #   elif (item[0] in startConf):
  #     globalConf.incl item[0]
  #     # globalConfig.excl item[0]

template getLogLevel*(f: string): untyped =
  if defLogLevelMap.hasKey(f):
    defLogLevelMap[f]
  else:
    raise newException(
      AssertionError,
      "Attempt to use loggint without init configuration"
    )

template saveLog*(text: string, logLevel = lvlAll): void =
  let f = instantiationInfo().filename
  if lcFileLogging in getLogConf(f):
    defLogLoggerMap[f].log(lvlAll, text)

template getDefPrefix(iinfo: IInfo): string =
  mixin isExplicitlyEnabled, splitFile
  block:
    let file = iinfo.filename
    let confPrefix =
      if defLogPrefixMap.hasKey(file):
        defLogPrefixMap[file]
      else:
        ""

    proc isEnabled(c: LoggingConf): bool =
      (c.isExplicitlyEnabled()) and (c in getLogConf(file))

    (if isEnabled(lcUsePrefix): confPrefix & ": " else: "") &
    (
      block:
        if isEnabled(lcUseFileName):
          let (_, name, ext) = iinfo.filename.splitFile()
          "[" & name & "] "
        else:
          ""
    ) &
    (if isEnabled(lcUseLine): "(" & $iinfo.line & ") " else: "")

proc printingAllowed(iinfo: IInfo): bool =
  (lcUseLogging.isExplicitlyEnabled()) or
  (lcUseLogging in getLogConf(iinfo.filename))

proc reflowNeeded(iinfo: IInfo): bool =
  (lcUseReflow.isExplicitlyEnabled()) or
  (lcUseReflow in getLogConf(iinfo.filename))

template showError*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")

  let iinfo = instantiationInfo()
  if printingAllowed(iinfo):
    ceUserError0(
      getDefPrefix(iinfo = iinfo) & text,
      ind = defLogIndentation,
      doReflow = reflowNeeded(iinfo)
    )

  saveLog(text, lvlError)

template showLog*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")

  let iinfo = instantiationInfo()
  if printingAllowed(iinfo):
    ceUserLog0(
      getDefPrefix(iinfo = iinfo) & text,
      ind = defLogIndentation,
      doReflow = reflowNeeded(iinfo)
    )

  saveLog(text, lvlDebug)

template showInfo*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")

  let iinfo = instantiationInfo()
  if printingAllowed(iinfo):
    ceUserInfo2(
      getDefPrefix(iinfo = iinfo) & text,
      ind = defLogIndentation,
      doReflow = reflowNeeded(iinfo)
    )

  saveLog(text, lvlInfo)

template showWarn*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")

  let iinfo = instantiationInfo()
  if printingAllowed(iinfo):
    ceUserWarn(
      getDefPrefix(iinfo = iinfo) & text,
      ind = defLogIndentation,
      doReflow = reflowNeeded(iinfo)
    )

  saveLog(text, lvlWarn)
  # fileLogger.log(lvlWarn, text)

template showPlain*(msgs: varargs[string, `$`]) =
  let text = msgs.join(" ")
  echo text


template initDefense*(
  logfile: string = "",
  prefix: string = "",
  logPath: bool = false,
  logLine: bool = false
         ): untyped =
  ##[ Create necessary variables and procs for debugging.

  All settings and procedures are scope-local. `logile` is a name of
  the file to use for creating file logger - if empty (default) file
  logging is disabled. `prefix` is a prefix string used for log
  messages.

  .. code-block::
      initDefence(prefix = "FFF")
      showLog("hello", "world")
      showError("test", "ddd")

  .. code-block:: text
      >>> FFF: hello world
      !!! FFF: test ddd
  ]##

  let file = instantiationInfo().filename
  defLogConfMap[file] = {lcPrintLogging}

  if logfile != "":
    defLogConfMap[file].incl lcFileLogging

  if prefix != "":
    defLogConfMap[file].incl lcUsePrefix
    defLogPrefixMap[file] = prefix

  if logLine: defLogConfMap[file].incl lcUseLine
  if logPath: defLogConfMap[file].incl lcUseFileName

  if lcFileLogging in getLogConf(file):
    defLogLoggerMap[file] = newFileLogger(logfile)


#================  shell interaction without exceptions  =================#

template safeRunCommand*(
  msg: string,
  runConf: set[DebugOutputKind],
  hideerror: bool = false,
  body: untyped): bool =
  ## Execute shell command and return `true/false` based on execution
  ## results. This template internally uses syntax from `shell`
  ## module. Execution results are not returned.
  ##
  ## :hideerror: if `true` do not print stderr after error in shell
  ## :runConf: passed to `shellVerboseErr`
  ## :msg: Print 'msg' and execution succeded/failed. Leave empty (`""`)
  ##       for not messages.
  runnableExamples:
    initDefense()
    let res = safeRunCommand("test", {dokCommand}):
      ls

    assert res == true

  block:
    var resOk = false
    let (res, err, code) = shellVerboseErr runConf:
      body

    if code != 0:
      if msg.len > 0:
        showError(msg, "exited with non-zero output code")


      if not hideerror:
        showError("Error output")
        showPlain(err)
        saveLog(err)
        saveLog(res)
    else:
      if msg.len > 0:
        showInfo(msg, "exited with output code 0")

      resOk = true

    resOk


#=====================  exception-related features  ======================#




when isMainModule:
  proc e() = raise newException(AssertionError, """
  Ass SeRtIoN eRrOr with easf sadfas
  Ass SeRtIoN eRrOr with easf sadfas
  Ass SeRtIoN eRrOr with easf sadfas
  Ass SeRtIoN eRrOr with easf sadfas
  dfsdf as dfa sdf asdf asd fa sdf adsf as df
  as f sadf asdf w f dsvafgd aerfg a rgffd ga
  dfsdf as dfa sdf asdf asd fa sdf adsf as df
  as f sadf asdf w f dsvafgd aerfg a rgffd ga
  dfsdf as dfa sdf asdf asd fa sdf adsf as df
  as f sadf asdf w f dsvafgd aerfg a rgffd ga
  dfsdf as dfa sdf asdf asd fa sdf adsf as df
  as f sadf asdf w f dsvafgd aerfg a rgffd ga
  asd asdf asdfasdf sf sadf asdf werfdfasdf waf asd fasdf
  fa""")
  proc d() = e()
  proc c() = d()
  proc b() = c()
  proc a() = b()


  setCurrentDir("/tmp")
  initDefense(prefix = "FFF: ")
  showWarn("hello", "world")
  showError("test", "ddd")

  let res = safeRunCommand("test", {dokCommand}, false):
    ls

  assert res == true
  pprintErr():
    a()
