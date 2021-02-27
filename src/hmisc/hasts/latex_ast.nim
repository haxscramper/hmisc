import strutils, strformat, sequtils, ropes, os
import shell
import hmisc/helpers

type
  LatexCompiler* = enum
    lacLuaLatex
    lacPdflatex
    lacLatexMk

  LAstNodeKind* = enum
    lnkMacro
    lnkEnv
    lnkTable
    lnkPlaintext
    lnkStmtList

  LAstGroup* = object
    delims*: (string, string)
    body*: seq[LAstNode]
    sep*: string

  LACellAlign* = enum
    lalLeft
    lalRight
    lalCenter

  LACellSpec* = object
    align*: LACellAlign
    border*: seq[bool] ## Specification for border of the right cell side
    case fixedsise*: bool
      of false:
        nil
      of true:
        sizeCm*: float

  LAstNode* = object
    case kind*: LAstNodeKind
      of lnkMacro:
        macroName*: string
        macroArgs*: seq[LAstGroup]
      of lnkEnv:
        envName*: string
        envBody*: seq[LAstNode]
        envArgs*: seq[LastGroup]
      of lnkStmtList:
        stmtList*: seq[LAstNode]
      of lnkPlaintext:
        plaintextStr*: string
      of lnkTable:
        colSpecs*: seq[LACellSpec]
        leftBorder*: seq[bool] ## Specification for leftmost border of
                               ## the grid
        tableRows*: seq[seq[LAstNode]]

func makeBraceGroup*(body: LAstNode): LAstGroup =
  LAstGroup(delims: ("{", "}"), body: @[body])

func group*(delims: (string, string), body: seq[LAstNode]): LAstGroup =
  LAstGroup(delims: delims, body: body, sep: ",")

func makePlaintext*(text: string): LAstNode =
  LAstNode(kind: lnkPlaintext, plaintextStr: text)


func makeMacroCall*(name: string): LAstNode =
  LastNode(kind: lnkMacro, macroName: name)

func makeMacroCall*(name: string, args: seq[LAstNode]): LAstNode =
  LastNode(
    kind: lnkMacro,
    macroName: name,
    macroArgs: args.mapIt(it.makeBraceGroup())
  )


func makeMacroCall*(name: string, args: seq[LAstGroup]): LAstNode =
  LastNode(kind: lnkMacro, macroName: name, macroArgs: args)

func makeMacroCall*(name: string, params: seq[string],
                    arg: string): LAstNode =
  makeMacroCall(
    name,
    @[
      group(("[", "]"), params.mapIt(makePlaintext(it))),
      group(("{", "}"), @[makePlaintext(arg)])
    ]
  )

func makeEnv*(envname: string, body: seq[LAstNode],
              args: seq[LAstGroup] = @[]): LAstNode =
  LAstNode(kind: lnkEnv,
           envBody: body,
           envArgs: args,
           envname: envname)

func makeStmtList*(nodes: seq[LAstNode]): LAstNode =
  LAstNode(kind: lnkStmtList, stmtList: nodes)

func makeDocument*(nodes: seq[LAstNode]): LAstNode =
  makeStmtList(@[
    makeMacroCall("documentclass", @["a4paper", "12pt"], "article"),
    makeEnv("document", nodes)
  ])

func `&!`(ropes: seq[Rope]): Rope = `&`(ropes)
func concat(ropes: seq[Rope]): Rope = `&`(ropes)
func `&!`(a: Rope; b: Rope): Rope = {.noSideEffect.}: a & b
func `&!`(a: Rope; b: string): Rope = {.noSideEffect.}: a & b
func `&!`(a: string; b: Rope): Rope = {.noSideEffect.}: a & b
func rope(s: string): Rope = {.noSideEffect.}: ropes.rope(s)


func toRope*(node: LAstNode): Rope
func toRope*(group: LAstGroup): Rope =
  group.delims[0] &! rope(group.body.mapIt(it.toRope()).join(group.sep)) &!
  group.delims[1]

func toRope*(node: LAstNode): Rope =
  case node.kind:
    of lnkMacro:
      return rope(&"\\{node.macroName}") &!
        node.macroArgs.mapIt(it.toRope()).concat()
    of lnkPlaintext:
      return rope(node.plaintextStr)
    of lnkEnv:
      return rope(&"\\begin{{{node.envName}}}\n") & node.envBody.mapIt(
        it.toRope() &! "\n"
      ).concat() &
        rope(&"\\end{{{node.envName}}}")
    of lnkStmtList:
      return node.stmtList.mapIt(it.toRope() &! "\n").concat()
    else:
      discard

#=============================  Compilation  =============================#
proc compileToPdf*(document: LAstNode,
                  tmpfile: string = "/tmp/latextmp.tex",
                  compiler: LatexCompiler = lacPdflatex
                  ): tuple[ok: bool, log: string] =
  let (outdir, _, _) = splitFile(tmpfile)
  tmpfile.writeFile($document.toRope())
  let noMsg: set[DebugOutputKind] = {}
  let (output, error, code) = shellVerboseErr noMsg:
    pdflatex "-interaction=nonstopmode" "-output-directory="($outdir) ($tmpfile)

  # echo code
  result.log = output
  result.ok = (code == 0)

when isMainModule:
  discard makeDocument(@[
    makePlaintext("HHHH")
  ]).compileToPdf()
