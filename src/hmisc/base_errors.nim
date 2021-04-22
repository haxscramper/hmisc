import std/macros

## Commonly used exceptions definitions. More target-specific errors are
## implemented in respective submodules such as [[code:hshell.ShellError]]
## for shell-related operations, [[code:oswrap.PathError]] for unexpected
## situations related to filesystem path handling and
## [[code:oswrap.EnvVarError]] for environment variable handling.

type
  ArgumentError*       = object of CatchableError
    ## Invalid argument passed to a functions

  LogicError*          = object of CatchableError

  UnexpectedKindError* = object of ArgumentError
    ## More concrete specializaton of argument error for handling variant
    ## objects

  ImplementError*      = object of CatchableError
    ## I use this error as a more active restrictive `TODO` tag.

  ImplementKindError*  = object of ImplementError
    ## Specialization of implementation placeholder for handling error
    ## kinds


  GlobalSubstring* = object
    ## Part of main string (from file or user input)

    str*: string ## Substring slice
    line*: int ## Line number for substring in main file
    column*: int ## Column number for substring in file
    filename*: string ## Name of the file being parsed

  ErrorAnnotation* = object
    case fromString*: bool
      of true:
        offset*: int ## Offset from the start of main substring

      of false:
        errpos*: LineInfo

    expr*: string
    annotation*: string
    linerange*: int


  InstantiationInfo* = tuple[filename: string, line: int, column: int]
  CodeError* = ref object of CatchableError
    raisepos*: LineInfo
    case fromString*: bool
      of true:
        substr*: GlobalSubstring
        offset*: int

      of false:
        errpos*: LineInfo ## Position of original error

    annots*: seq[ErrorAnnotation] ## Additional error annotations
    postannot*: string


template raiseLogicError*(errMsg: string) {.dirty.} =
  raise newException(LogicError, errMsg)

template raiseArgumentError*(errMsg: string) {.dirty.} =
  raise newException(ArgumentError, errMsg)

template assertKind*(expr, expected: typed) {.dirty.} =
  let kind = expr.kind
  var msg: string
  when compiles(kind notin expected):
    var anyOf: string
    when kind is set:
      if len(kind) > 1:
        anyOf = "any of "



    if kind notin expected:
      msg = "Unexpected kind - got " & $kind &
        ", but expected " & anyOf & $expected

  elif expected is enum:
    if kind != expected:
      msg = "Unexpected kind - got " & $kind &
        ", but expected " & $expected

  if msg.len > 0:
    {.line: instantiationInfo(fullPaths = true).}:
      raise newException(UnexpectedKindError, msg)


template raiseImplementError*(errMsg: string) {.dirty.} =
  raise newException(
    ImplementError, errMsg & " @" & $instantiationInfo())


proc prepareMsg*(userMsg: string): string =
  var msg: string
  if userMsg.len > 0: msg &= " "
  if '\n' in userMsg: msg &= "\n"
  msg &= userMsg

  return msg


template raiseImplementKindError*(
  node: untyped, userMsg: string = "") {.dirty.} =

  raise newException(ImplementKindError,
    "\nUnhandled entry kind: " &
      astToStr(node) &
      " has kind \e[32m" & $node.kind & "\e[39m" &
      prepareMsg(userMsg) & " @" & $instantiationInfo() & "\n"
  )


template raiseUnexpectedKindError*(
  node: untyped, userMsg: string = "") {.dirty.} =
  var msg: string
  if userMsg.len > 0: msg &= " "
  if '\n' in userMsg: msg &= "\n"
  msg &= userMsg

  when node is enum:
    let kind = $node

  else:
    let kind = $node.kind

  raise newException(UnexpectedKindError,
    "\nUnexpected entry kind: " &
      astToStr(node) &
      " has kind \e[32m" & kind & "\e[39m" & prepareMsg(userMsg)
  )


template canImport*(x: untyped): untyped =
  compiles:
    import x
