import std/[macros, os, strutils]

import
  ../algo/clformat,
  ./ast_spec



func lineIInfo*(node: NimNode): NimNode =
  ## Create tuple literal for `{.line: .}` pragma
  let iinfo = node.lineInfoObj()
  newLit((filename: iinfo.filename, line: iinfo.line))

func eqIdent*(node: NimNode, strs: openarray[string]): bool =
  for str in strs:
    if node.eqIdent(str):
      return true


const
  nimAstSpec* = astSpec(NimNode, NimNodeKind):
    nnkInfix:
      0 as "op": nnkIdent
      1 .. 2: _
      ?3: nnkStmtList


    nnkTryStmt:
      1 .. ^1:
        nnkExceptBranch:
          1 .. ^2:
            nnkSym
            nnkIdent
            nnkInfix:
              0: nnkIdent(strVal: "as")
              1: nnkIdent
              2: nnkIdent

generateConstructors(nimAstSpec, {nnkEmpty}, newTree)


proc treeRepr2*(
    pnode: NimNode,
    colored: bool = true,
    pathIndexed: bool = false,
    positionIndexed: bool = true,
    maxdepth: int = 120,
    lineInfo: bool = false
  ): ColoredText =
  coloredResult()
  ## Advanced `treeRepr` version.
  ##
  ## - show symbol kinds and types
  ## - use colored representation for literals and comments
  ## - support max depth limit using @arg{maxdepth}
  ## - optionally show full index path for each entry
  ## - show node position index
  ## - differentiate between `NilLit` and *actually* `nil` nodes

  proc aux(n: NimNode, level: int, idx: seq[int], err: ColoredText) =
    let pref = joinPrefix(level, idx, pathIndexed, positionIndexed)

    add pref
    if isNil(n):
      add toRed("<nil>", colored)
      return

    if level > maxdepth:
      add " ..."
      return

    add hshow(n.kind) # pref & ($n.kind)[3 ..^ 1]
    if err.len > 0:
      add " "
      add err

    if lineInfo:
      let info = n.lineInfoObj()
      add "@"
      add splitFile(info.filename).name + fgBlue
      add "/"
      add $info.line + fgCyan
      add ":"
      add $info.column + fgCyan
      add " "

    case n.kind:
      of nnkStrLit .. nnkTripleStrLit:
        add " \"" & toYellow(n.strVal(), colored) & "\""

      of nnkCharLit .. nnkUInt64Lit :
        add " " & toCyan($n.intVal, colored)

      of nnkFloatLit .. nnkFloat128Lit:
        add " " & toMagenta($n.floatVal, colored)

      of nnkIdent:
        add " " & toCyan(n.strVal(), colored)

      of nnkSym:
        add " "
        add toBlue(($n.symKind())[3..^1], colored)
        add " "
        add toGreen(n.strVal(), colored)

      of nnkCommentStmt:
        let lines = split(n.strVal(), '\n')
        if lines.len > 1:
          add "\n"
          for idx, line in pairs(lines):
            if idx != 0:
              add "\n"

            add pref & toYellow(line)

        else:
          add toYellow(n.strVal())

      else:
        if n.len > 0:
          add "\n"

        for newIdx, subn in n:
          var e = validateAst(nimAstSpec, n, subn, newIdx)
          aux(subn, level + 1, idx & newIdx, e)
          if level + 1 > maxDepth:
            break

          if newIdx < n.len - 1:
            add "\n"



  aux(pnode, 0, @[], "")
  endResult()
