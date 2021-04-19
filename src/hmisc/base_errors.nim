type
  ArgumentError* = object of CatchableError
  LogicError* = object of CatchableError
  UnexpectedKindError* = object of ArgumentError

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


type
  ImplementError* = object of CatchableError
  ImplementKindError* = object of ImplementError

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
