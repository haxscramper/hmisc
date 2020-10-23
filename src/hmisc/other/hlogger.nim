
type
  Level* = enum
    lvlAll
    lvlDebug
    lvlInfo
    lvlNotice
    lvlWarn
    lvlError
    lvlFatal
    lvlNone

  LogType* = enum
    lkAction
    lkValue
    lkInformation

  LogActKind* = enum
    lakStarted
    lakOngoing
    lakStopped

  LogFormat* = object
    ## Descrive formatting for json values in log
    discard

  Log* = object
    logPos*: LogPos
    format*: LogFormat
    case kind*: LogType
      of lkValue:
        key*: string
        value*: JsonNode
      of lkInformation:
        level*: Level
      of lkAction:
        actKind*: LogActionKind

  LogPos* = object
    filename*: AbsFile
    line*: int
    column*: int
