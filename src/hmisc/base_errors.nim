type
  ArgumentError* = ref object of CatchableError

template raiseArgumentError*(errMsg: string) {.dirty.} =
  raise ArgumentError(msg: errMsg)

type
  ImplementError* = ref object of CatchableError
  ImplementKindError* = ref object of ImplementError

template raiseImplementError*(errMsg: string) {.dirty.} =
  raise ImplementError(msg: errMsg & " @" & $instantiationInfo())


template raiseImplementKindError*(
  node: untyped, userMsg: string = "") {.dirty.} =
  raise ImplementError(
    msg: "\nUnhandled entry kind: " &
      astToStr(node) &
      " has kind \e[32m" & $node.kind & "\e[39m" &
      (if userMsg.len > 0: " " & userMsg else: "") &
      " @" & $instantiationInfo() & "\n"
  )


template canImport*(x: untyped): untyped =
  compiles:
    import x
