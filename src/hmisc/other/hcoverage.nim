import std/[
  macros, tables, intsets, hashes, os, sequtils,
  sets, enumerate, algorithm, strutils
]

import
  hmisc/core/all,
  hmisc/types/[colorstring, rev_set_table],
  hmisc/algo/hstring_algo,
  hmisc/macros/nim_ast_help

var
  covStats: Table[int, HashSet[int]]
  fileTable: Table[int, string]
  procTable: Table[string, int]
  covActive: IntSet
  procRanges: RevSetTable[int, string]

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
    const hash = hashes.hash(file)
    if hash notin fileTable:
      fileTable[hash] = file

func execNode(hash, line, column: int) =
  {.cast(noSideEffect).}:
    if hash notin covStats:
      covStats[hash] = initHashSet[int]()

    covStats[hash].incl line

func execWithCoverage(
    fileId: static[int],
    procname: static[string],
    procnameFull: static[string],
    lineRange: static[Slice[int]]): bool =

  const hash = hashes.hash(procname)
  {.cast(noSideEffect).}:
    procTable[procname] = fileId
    if lineRange.a notin procRanges:
      for line in lineRange:
        procRanges[line] = procname

    return true


proc pprintCoverage*(procname: string, onlyMissing: bool) =
  if procname notin procTable: return
  let
    id = procTable[procname]
    file = fileTable[id]

  const w = 5
  let other = procRanges.getKeys(procname).sorted()

  echo procname, " defined in ", splitFile(file).name,
      ":", other[0], ":", other[^1]

  var lastNoexec = -1
  if id in covStats:
    let lines = readFile(file).split("\n")
    for lineNum in other:
      let line = lines[lineNum - 1]
      if lineNum in covStats[id]:
        if not onlyMissing:
          echo $lineNum |<< 4, toGreen(line)

      else:
        let empty = line.allIt(it in {' '})
        if (empty and lastNoexec == lineNum - 1) or not empty:
          echo $lineNum |<< 4, toRed(line)

        lastNoexec = lineNum

proc pprintCoverage*() =
  for id, file in fileTable:
    const w = 5
    var shown: IntSet
    if id in covStats:
      let lines = readFile(file).split("\n")
      for idx, _ in lines:
        let lineNum = idx + 1
        if lineNum notin shown and lineNum in procRanges:
          let other = procRanges.otherKeys(lineNum).sorted()
          for lineNum in other:
            shown.incl lineNum
            let line = lines[lineNum - 1]
            if lineNum in covStats[id]:
              echo $lineNum |<< 4, toGreen(line)

            else:
              echo $lineNum |<< 4, toRed(line)

proc fileId(file: string): NimNode =
  hash(file).newLit()

proc execCall(node: NimNode): NimNode =
  let iinfo = node.lineInfoObj()
  return newCall(
    bindSym"execNode",
    fileId(iinfo.filename),
    newLit(iinfo.line),
    newLit(iinfo.column))


proc execNode(node: NimNode): NimNode =
  nnkStmtListExpr.newTree(execCall(node), node)

const
  convTable = {
    nnkStmtList, nnkElifBranch, nnkElse,
    nnkDiscardStmt, nnkReturnStmt: 0 .. ^1,
    nnkCall, nnkCommand, nnkAsgn: 1 .. ^1,
    nnkForStmt: 2 .. ^1
  }

  convRanges = toMapArray convTable
  convKinds = toKeySet convTable


proc transform(node: NimNode): NimNode =
  case node.kind:
    of AtomicNodes:
      result = node

    of convKinds:
      result = newTree(node.kind)

      for sub in node[0 ..< convRanges[node.kind].a]:
        result.add transform(sub)

      for sub in node[convRanges[node.kind]]:
        if sub.kind in convKinds:
          result.add execNode(transform(sub))

        else:
          result.add transform(sub)

      if node.kind in { nnkElse, nnkForStmt, nnkElifBranch }:
        let exec = execCall(node)
        result[^1].add exec

      # if node.kind in { nnkStmtList, nnkIfStmt }:
      #   result = execNode(result)

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

proc transformImpl(impl: NimNode): NimNode =
  if not addCoverage:
    result = impl

  else:
    result = copyNimTree(impl)
    # echo impl.treeRepr2(lineInfo = true)

    let
      name = newLit(impl.name().strVal())
      fullname = newLit(impl.procFullname())
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
      if `execWithCoverage`(
          `fileId`, `name`, `fullname`,
          `startLine` .. `finishLine`):
        `transformed`

      else:
        `oldBody`

    writeFile("/tmp/" & impl.name().strVal() & ".nim", result.repr())




macro astCov*(impl: untyped): untyped = transformImpl(impl)
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
