import std/[macros, strutils, options, tables]
export options

import ./msgbuilder
export msgbuilder

## Commonly used exceptions definitions. More target-specific errors are
## implemented in respective submodules such as [[code:hshell.ShellError]]
## for shell-related operations, [[code:oswrap.PathError]] for unexpected
## situations related to filesystem path handling and
## [[code:oswrap.EnvVarError]] for environment variable handling.

type
  ArgumentError*       = object of CatchableError
    ## Invalid argument passed to a functions

  EnvironmentAssertionError* = object of ArgumentError
  GetterError*               = object of ArgumentError
  SetterError*               = object of ArgumentError
  NilArgumentError*          = object of ArgumentError
  NoneArgumentError*         = object of ArgumentError
  ParseError*                = object of CatchableError
  LogicError*                = object of CatchableError
  UnexpectedKindError*       = object of ArgumentError
    ## More concrete specializaton of argument error for handling variant
    ## objects

  ImplementError*      = object of CatchableError
    ## I use this error as a more active restrictive `TODO` tag.

  ImplementBaseError* = object of ImplementError

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


# template raiseLogicError*(errMsg: string) {.dirty, deprecated.} =
#   raise newException(LogicError, errMsg)

# template raiseArgumentError*(errMsg: string) {.dirty, deprecated.} =
#   raise newException(ArgumentError, errMsg)

proc newLogicError*(msg: varargs[string, `$`]): ref LogicError =
  newException(LogicError, msg.join(""))

template logicAssert*(cond: untyped, msg: string): untyped =
  {.line: instantiationInfo(fullPaths = true).}:
    if not cond:
      raise newLogicError(
        "Condition failed ",
        astToStr(cond),
        " evaluated as 'false'. ",
        msg)

proc newArgumentError*(msg: varargs[string, `$`]): ref ArgumentError =
  newException(ArgumentError, msg.join(""))

proc newEnvironmentAssertionError*(
    msg: varargs[string, `$`]): ref EnvironmentAssertionError =
  newException(EnvironmentAssertionError, msg.join(""))

template assertArg*(arg: untyped, cond: bool, msg: string = ""): untyped =
  if not cond:
    {.line: instantiationInfo(fullpaths = true).}:
      raise newArgumentError(
        "Invalid value for argument ", astToStr(arg), " ",
        "', check '", astToStr(cond), "'",
        (if msg.len > 0: ". " & msg else: msg))


proc newGetterError*(msg: varargs[string, `$`]): ref GetterError =
  newException(GetterError, msg.join(""))

proc newHException*[T: object](
    ex: typedesc[T], msg: varargs[string, `$`]): ref T =
  new(result)
  result.msg = msg.join("")


template assertHasIdx*(
    item: untyped,
    access: int,
    msg: string = "",
    exception: untyped = GetterError,
  ): untyped =

  bind newHException

  let l = len(item)
  if not(access < l):
    {.line: instantiationInfo(fullPaths = true).}:
      raise newHException(
        Exception,
        "Cannot get element at index ",
        access,
        " from '", astToStr(item), "', which has len() = ",
        l, (if len(msg) > 0: ". " & msg else: ""))

proc newSetterError*(msg: varargs[string, `$`]): ref SetterError =
  newException(SetterError, msg.join(""))

proc newImplementError*(msgs: varargs[string, `$`]): ref ImplementError =
  newException(ImplementError, join(msgs, ""))

template raiseImplementError*(errMsg: string) {.dirty.} =
  raise newImplementError(errMsg & " @" & $instantiationInfo())


proc prepareMsg*(userMsg: string): string =
  var msg: string
  if userMsg.len > 0: msg &= " "
  if '\n' in userMsg: msg &= "\n"
  msg &= userMsg

  return msg


template kindToStr*(expr: typed): untyped =
  when expr is enum:
    "\e[32m" & $expr & "\e[0m"

  elif expr is string:
    "\e[34m\"" & expr & "\"\e[0m"

  else:
    "\e[32m" & $expr.kind & "\e[0m for type \e[33m" & $typeof(expr) & "\e[0m"


template assertKind*(
    inExpr, inExpected: untyped,
    onFail: string = "") {.dirty.} =
  bind kindToStr, UnexpectedKindError
  block:
    let
      expr = inExpr
      expected = inExpected

    when expr is enum:
       let kind = expr

    else:
      let kind = expr.kind

    var msg: string
    when compiles(kind notin expected):
      var anyOf: string
      when kind is set:
        if len(kind) > 1:
          anyOf = "any of "

      if kind notin expected:
        let expected = $expected
        if 80 < len(expected):
          msg = "Unexpected kind. Got " & kindToStr(expr) &
            " - " & onFail &
            ". Expected kinds - " & astToStr(inExpected)

        else:
          msg = "Unexpected kind. Got " & kindToStr(expr) &
            ", but expected " & anyOf & $expected & onFail

    elif expected is enum:
      if kind != expected:
        msg = "Unexpected kind. Got " & kindToStr(expr) &
          ", but expected " & $expected & onFail

    if msg.len > 0:
      {.line: instantiationInfo(fullPaths = true).}:
        var res: ref UnexpectedKindError
        new(res)
        res.msg = msg
        raise res


template assertRef*(expr: typed, onFail: string = ""): untyped {.dirty.} =
  bind NilArgumentError
  mixin isNil
  when not defined(release):
    if isNil(expr):
      let value =
        when (expr is proc):
          " ."

        else:
          " (" & $cast[int](expr) & ")."



      let msg = "expression '" & astToStr(expr) & "' of type '" &
        $typeof(expr) & "' is nil" & value & onFail
      {.line: instantiationInfo(fullPaths = true)}:
        raise (ref NilArgumentError)(msg: msg)

template assertOption*(expr: untyped, onFail: string = ""): untyped {.dirty.} =
  bind isNone, NoneArgumentError
  when not defined(release):
    if options.isNone(expr):
      let msg = "expression '" & astToStr(expr) & "' of type '" &
        $typeof(expr) & "' is `NONE`. " & onFail
      {.line: instantiationInfo(fullPaths = true)}:
        raise (ref NoneArgumentError)(msg: msg)

func getOr*[T](opt: Option[T], onFail: string): T =
  if opt.isSome():
    return opt.get()

  else:
    raise (ref NoneArgumentError)(
      msg: "Cannot get value from none - " & onFail)

func getOr*[K, V](table: Table[K, V], key: K, onFail: string): V =
  if key in table:
    return table[key]

  else:
    raise (ref GetterError)(
      msg: "Cannot get value for key" & (
        when compiles($key):  " " & $key else: ""
      ) & " - " & onFail)

template assertRefFields*[T](
    item: T, onFail: string = "string") {.dirty.} =

  for name, field in fieldPairs(
    when item is ref or item is ptr:
      assertRef item
      item[]

    else:
      item
  ):
    {.line: instantiationInfo(fullPaths = true).}:
      when field is ptr or
           field is ref or
           compiles(isNil(field)):
        if isNil(field):
          raise newException(
            NilArgumentError, "Field '" & name &
              "' of object type '" & $typeof(item) & "'. " & onFail)


proc prepareMsg(msgs: varargs[string]): string =
  for idx, msg in pairs(msgs):
    if '\n' in msg:
      result &= "\n"

    else:
      result &= " "

    result &= msg


proc newImplementKindError*[T](
    node: T, msg: varargs[string, `$`]): ref ImplementKindError =
  newException(ImplementKindError,
    "Unhandled entry kind " & kindToStr(node) & prepareMsg(msg))

proc newImplementBaseError*[T](
    obj: T, name: string): ref ImplementBaseError =
  newException(ImplementBaseError, &[
    $T, " does not implement '", name,
    "'. Use one of the derived types instead"])

proc newUnexpectedKindError*[T](
    expr: T, userMsg: varargs[string, `$`]): ref UnexpectedKindError =
  newException(UnexpectedKindError,
    "Unexpected entry kind " & kindToStr(expr) &
      prepareMsg(userMsg.join("")))

proc newUnexpectedKindError*(userMsg: varargs[string, `$`]):
    ref UnexpectedKindError =
  newException(UnexpectedKindError, prepareMsg(userMsg.join("")))

# template raiseUnexpectedKindError*(
#   node: untyped, userMsg: string = "") {.dirty, deprecated.} =
#   raise newUnexpectedKindError(node, userMsg)


template canImport*(x: untyped): untyped =
  compiles:
    import x
