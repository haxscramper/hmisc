import std/macros

import ../wrappers/wraphelp

type
  ProcBox* = object
    impl: pointer
    env: pointer

  Vtable* = object
    table: PUarray[ProcBox]

func `$`*(box: ProcBox): string =
  result.add "ProcBox(impl: "
  result.add $cast[int](box.impl)
  result.add ", env: "
  result.add $cast[int](box.env)
  result.add ")"

proc newVtable*(procs: openarray[ProcBox]): Vtable =
  result.table = allocPUarray[ProcBox](procs.len)
  for idx, box in procs:
    result.table[idx] = box

template `[]`*(vt: Vtable, idx: int): untyped = vt.table[][idx]

proc isClosure*[T: proc](): bool =
  var it: T
  result = compiles((let tmp = rawProc(it))) and compiles((let tmp = rawEnv(it)))
  # echo result

# static:
#   echo "? " , isClosure[proc(a: int) {.nimcall.}]()

proc toProcBox*[T: proc](pr: T): ProcBox =
  when isClosure[T]():
    result.impl = rawProc(pr)
    result.env = rawEnv(pr)

  else:
    result.impl = cast[pointer](pr)

template callAs*[Impl](box: ProcBox, args: varargs[untyped]): untyped =
  {.line: instantiationInfo(fullPaths = true).}:
    bind isClosure
    when isClosure[Impl]():
      # echo "is closure"
      var tmp: Impl
      let impl = cast[typeof(closureToCdecl(tmp))](box.impl)
      unpackVarargs(impl, args, box.env)

    else:
      var tmp: Impl
      let impl = cast[typeof(nimcallToCdecl(tmp))](box.impl)
      unpackVarargs(impl, args)

when isMainModule:
  import hmisc/core/all
  import hmisc/other/hpprint
  startHax()
  proc main() =
    if false:
      var env = 123
      let box: ProcBox = toProcBox(
        proc(a: int, b: var int, c: float): int =
          result = a + env + b
          b = 222
      )

      var tmp: int
      let res = callAs[proc(a: int, b: var int, c: float): int](box, 0, tmp, 2)
      echo tmp
      echo res

    block:
      type
        PTree = object
          path*: seq[string]

      # let box = toProcBox(proc(): PTree = result = PTree())
      # let t = callAs[proc(): PTree](box)

      block:
        let impl = (proc(): PTree {.closure.} = result = PTree())
        echo isClosure[typeof(impl)]()
        let p = rawProc(impl)
        let e = rawEnv(impl)
        let impl2 = cast[typeof(closureToCdecl(impl))](p)
        let res = impl2(e)
        echo res

      block:
        let impl = (proc(): PTree = result = PTree())
        echo isClosure[typeof(impl)]()
        let p = cast[pointer](impl)
        let impl2 = cast[proc(): PTree {.cdecl.}](p)
        let res = impl2()
        echo res


    block:
      type
        Obj = ref object
          path: seq[int]

      let box = toProcBox(
        proc(): Obj {.closure.} =
          let tmp = Obj()
          result = tmp
      )

      let res = callAs[proc(): Obj](box)
      echo res[]

  main()
