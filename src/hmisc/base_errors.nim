type
  ArgumentError* = ref object of CatchableError

template raiseArgumentError*(errMsg: string) {.dirty.} =
  raise ArgumentError(msg: errMsg)

type
  ImplementError* = ref object of CatchableError

template raiseImplementError*(errMsg: string) {.dirty.} =
  raise ImplementError(msg: errMsg)
