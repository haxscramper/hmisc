import std/[
  macros, tables, intsets, hashes, sequtils,
  sets, enumerate, algorithm, strutils
]

import
  ../core/all,
  ../types/[colorstring, rev_set_table],
  ../other/[oswrap],
  ../algo/hstring_algo,
  ../macros/nim_ast_aux

type
  CovProcId = distinct int
  CovFileId = distinct int
  CovLineId = distinct int

  CovProcName* = object
    name*: string
    argumentTypes*: seq[string]
    returnType*: string

  CovChunkKind* = enum
    cckExecuted
    cckNotExecuted
    cckEmpty

  CovChunk* = object
    line*: int
    case kind*: CovChunkKind
      of cckExecuted, cckNotExecuted:
        text*: string

      else:
        discard

  CovReport* = object
    chunks*: seq[CovChunk]
    procname*: CovProcInfo
    lineRange*: Slice[int]
    file*: AbsFile

  CovProcInfo* = object
    fullname*: CovProcName
    id*: CovProcId
    defFile*: CovFileId


func hash(pid: CovProcId): Hash = hash(pid.int)
func hash(pid: CovFileId): Hash = hash(pid.int)
func hash(pid: CovLineId): Hash = hash(pid.int)
func `==`*(p1, p2: CovProcId): bool = p1.int == p2.int
func `==`*(p1, p2: CovFileId): bool = p1.int == p2.int
func `==`*(p1, p2: CovLineId): bool = p1.int == p2.int

func `<`(l1, l2: CovLineId): bool = l1.int < l2.int
func `-`(l1: CovLineId, i: int): CovLineId = CovLineId(l1.int - i)

func procId(name: CovProcName): CovProcId =
  CovProcId(!$(
    hash(name.name) !&
    hash(name.argumentTypes) !&
    hash(name.returnType)))

var
  covStats: Table[CovFileId, HashSet[CovLineId]]
  fileTable: Table[CovFileId, AbsFile]
  procTable: Table[string, seq[CovProcInfo]]
  covActive: HashSet[CovProcId]
  covVisited: HashSet[CovProcId]
  procRanges: RevSetTable[CovLineId, CovProcId]


proc startPos(node: NimNode): LineInfo =
  case node.kind:
    of AtomicNodes:
      result = node.lineInfoObj()

    else:
      result = node[0].lineInfoObj()

proc finishPos(node: NimNode): LineInfo =
  case node.kind:
    of AtomicNodes:
      result = node.lineInfoObj()
      result.column += len($node) - 1

    else:
      if len(node) > 0:
        var idx = len(node) - 1
        while idx >= 0 and node[idx].kind in {nnkEmpty}:
          dec idx

        if idx >= 0:
          result = node[idx].finishPos()

        else:
          result = node.lineInfoObj()

      else:
        result = node.lineInfoObj()

func addFile(file: static[string]) =
  {.cast(noSideEffect).}:
    const hash = CovFileId(hashes.hash(file))
    if hash notin fileTable:
      fileTable[hash] = AbsFile(file)

func execNode(hash: CovFileId, line: CovLineId, column: int) =
  {.cast(noSideEffect).}:
    if hash notin covStats:
      covStats[hash] = initHashSet[CovLineId]()

    covStats[hash].incl line

func skipNode(hash: CovFileId, lineRange: Slice[CovLineId]) =
  discard


template execExpr(
    hash: CovFileId, line: CovLineId,
    column: int, expr: untyped
  ): untyped =

  execNode(hash, line, column)
  expr

proc hash(procname: CovProcName): Hash = hash(procname.name)

import ./hpprint

func execWithCoverage(
    fileId: int,
    procname: static[CovProcName],
    lineRange: Slice[int]): bool =

  const id = hcoverage.procId(procname)

  {.cast(noSideEffect).}:
    if id notin covVisited:
      covVisited.incl id
      procTable.mgetOrPut(procname.name, @[]).add CovProcInfo(
        fullname: procname,
        id: id,
        defFile: CovFileId(fileId))

      for line in lineRange:
        procRanges[CovLineId(line)] = id

    return true


proc getCoverage*(procname: CovProcName): CovReport =
  if procname.name notin procTable: return

  var
    id: CovFileId
    pid: CovProcId

  for impl in procTable[procname.name]:
    if procname.argumentTypes.len == 0:
      id = impl.defFile
      pid = impl.id
      result.procname = impl
      break

    elif procname.argumentTypes == impl.fullName.argumentTypes:
      id = impl.defFile
      pid = impl.id
      result.procname = impl
      break

  let file = fileTable[id]

  const w = 5
  let other = procRanges.getKeys(pid).sorted()

  result.lineRange = other[0].int .. other[^1].int
  result.file = file

  var lastNoexec = -1
  if id in covStats:
    let lines = readFile(file).split("\n")
    for lineNum in other:
      let line = lines[int(lineNum - 1)]
      if lineNum in covStats[id]:
        result.chunks.add CovChunk(kind: cckExecuted, text: line)

      elif line.allIt(it in {' '}):
        result.chunks.add CovChunk(kind: cckEmpty)

      else:
        result.chunks.add CovChunk(kind: cckNotExecuted, text: line)

      result.chunks[^1].line = lineNum.int

proc fileId(file: string): NimNode =
  hash(file).newLit()

proc execCall(node: NimNode): NimNode =
  let iinfo = node.lineInfoObj()
  return newCall(
    bindSym"execNode",
    newCall(bindSym"CovFileId", fileId(iinfo.filename)),
    newCall(bindSym"CovLineId", newLit(iinfo.line)),
    newLit(iinfo.column))



proc execNode(node: NimNode): NimNode =
  nnkStmtListExpr.newTree(execCall(node), node)

proc execExpr(base, node: NimNode): NimNode =
  let iinfo = base.lineInfoObj()
  return newCall(
    bindSym"execExpr",
    newCall(bindSym"CovFileId", fileId(iinfo.filename)),
    newCall(bindSym"CovLineId", newLit(iinfo.line)),
    newLit(iinfo.column),
    node)

const
  convTable = {
    nnkStmtList, nnkElifBranch, nnkElse, nnkPragmaBlock,
    nnkWhileStmt,
    nnkDiscardStmt, nnkReturnStmt: 0 .. ^1,
    nnkCall, nnkCommand, nnkAsgn: 1 .. ^1,
    nnkForStmt: 2 .. ^1
  }

  convRanges = toMapArray convTable
  convKinds = toKeySet(convTable)

template nohcov*(body: untyped): untyped = body

proc transform(node: NimNode): NimNode =
  case node.kind:
    of AtomicNodes:
      result = node

    of convKinds:
      # echo node.repr()
      # echo node.kind
      if (node.kind == nnkPragmaBlock and node[0][0].eqIdent("nohcov")) or
         (node.kind == nnkCall and node[0].eqIdent("nohcov")):
        return newStmtList(
          newCall(
            bindSym"skipNode",
            newCall(bindSym"CovFileId", newCall(
              bindSym"CovFileId", hash(node.lineInfoObj().filename).newLit())),
            nnkInfix.newTree(
              ident"..",
              newCall(bindSym"CovLineId", node.startPos().line.newLit()),
              newCall(bindSym"CovLineId", node.finishPos().line.newLit()))),
          node)

      result = newTree(node.kind)

      for sub in node[0 ..< convRanges[node.kind].a]:
        result.add transform(sub)

      for sub in node[convRanges[node.kind]]:
        if sub.kind in convKinds:
          result.add execNode(transform(sub))

        elif node.kind in { nnkCall, nnkCommand }:
          result.add execExpr(sub, transform(sub))

        else:
          result.add transform(sub)

      if node.kind in { nnkElse, nnkForStmt, nnkElifBranch, nnkWhileStmt }:
        let exec = execCall(node)
        result[^1].insert(0, exec)

    else:
      result = newTree(node.kind)
      for idx, sub in node:
        result.add transform(sub)


proc procFullname(impl: NimNode): string =
  result.add impl.name().strVal()
  result.add "("
  for arg in impl.params[1..^1]:
    for _ in arg[1 .. ^2]:
      result.add arg[^2].toStrLit().strVal()
      result.add ","

  result.add ")"
  result.add impl.params[0].toStrLit().strVal()

var addCoverage {.compiletime.}: bool

macro hcoverageEnable*() =
  addCoverage = true


proc getName(impl: NimNode): string =
  if impl[0].kind == nnkPostfix:
    impl[0][1].strVal()

  else:
    impl[0].strVal()

proc getProcname(impl: NimNode): CovProcName =
  result.name = impl.getName()
  for argument in impl.params[1..^1]:
    for id in argument[0 .. ^3]:
      if argument[^1].kind != nnkEmpty:
        result.argumentTypes.add argument[^2].repr()

      else:
        result.argumentTypes.add ""

  if impl.params[0].kind != nnkEmpty:
    result.returnType = impl.params[0].strVal()

proc transformImpl(impl: NimNode): NimNode =
  if not addCoverage:
    result = impl

  else:
    result = copyNimTree(impl)
    # echo impl.treeRepr2(lineInfo = true)

    let
      name = getProcname(impl).newLit()
      oldBody = impl.body
      transformed = impl.body.transform()
      execCheck = bindSym"execWithCoverage"
      start = impl.startPos()
      finish = impl.finishPos()
      startLine = newLit(start.line)
      finishLine = newLit(finish.line)
      fileId =  fileId(impl.lineInfoObj().filename)
      addFile = bindSym"addFile"
      procFile = impl.lineInfoObj().filename
      mainExec = execCall(impl)

    result.body = quote do:
      `addFile`(`procFile`)
      `mainExec`
      if `execWithCoverage`(`fileId`, `name`, `startLine` .. `finishLine`):
        `transformed`

      else:
        `oldBody`

    result = quote do:
      discard `execWithCoverage`(
        `fileId`, `name`, `startLine` .. `finishLine`)

      `result`

    writeFile("/tmp/" & impl.name().strVal() & ".nim", result.repr())




macro hcov*(impl: untyped): untyped = transformImpl(impl)
# macro semCov*(impl: typed): untyped = transformImpl(impl)

# proc expr(): int = 12

# proc testAstStmts(cond: bool) {.astCov.} =
#   if cond:
#     discard 2

#   else:
#     discard 2

#   discard expr()

# proc genericCover[T](arg: T) {.astCov.} =
#   when arg is string or arg is cstring or arg is ptr cstring:
#     return

#   else:
#     when arg isnot ptr and arg isnot ref:
#       discard sizeof(arg)

#     else:
#       discard sizeof(arg[])


# static:
#   startHaxComp()


# proc longCondition(arg1, arg2, arg3: bool) {.astCov.} =
#   if arg1 and
#      arg2 and
#      (arg1 xor arg2):
#     discard 2

#   else:
#     discard 1


# testAstStmts(false)
# testAstStmts(true)

# genericCover(12)
# genericCover("wer")

# longCondition(true, true, true)

# pprintCoverage()
