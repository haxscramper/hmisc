import std/macros, pkg/benchy

import ../wrappers/wraphelp_ptr
import ../core/all

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

# proc tableLen*(vt: VTable): int = cast[int](vt.table[0])
proc newVtable*(procs: openarray[ProcBox]): Vtable =
  result.table = allocPUarray[ProcBox](procs.len)
  for idx, box in procs:
    result.table[idx] = box

template `[]`*(vt: Vtable, idx: int): untyped = vt.table[][idx]

macro isClosure*(t: typed): untyped = newLit(false)

proc toProcBox*(pr: proc): ProcBox =
  when isClosure(pr):
    result.impl = rawProc(pr)
    result.env = rawEnv(pr)

  else:
    result.impl = cast[pointer](pr)

template callAs*[A1, R](
    box: ProcBox, sign: typedesc[proc(a1: A1): R],
    arg1: A1
  ): R =

  when isClosure(sign):
    cast[proc(a1: A1, env: pointer): R {.cdecl.}](box.impl)(arg1, box.env)

  else:
    cast[proc(a1: A1): R {.cdecl.}](box.impl)(arg1)

when isMainModule:
  startHax()
  let box = toProcBox(proc(a: int): int = a)

  timeIt "Call with argument":
    for i in 0 ..< 100_000:
      keep box.callAs(proc(a: int): int, 12)

  proc regular(a: int): int = a
  timeIt "Call regular":
    for i in 0 ..< 100_000:
      keep regular(12)

  let vt = newVTable([box])
  timeIt "Call through vtable":
    for i in 0 ..< 100_000:
      keep callAs(vt[0], proc(a: int): int, 12)
