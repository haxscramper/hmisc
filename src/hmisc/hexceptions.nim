import strformat, strutils, algorithm, sequtils, macros, os, re, terminal
import types/colorstring
import algo/[hseq_mapping, hmath]

type
  ErrorAnnotation = object
    errpos*: LineInfo
    expr*: string
    annotation*: string
    linerange*: int


  CodeError* = ref object of CatchableError
    raisepos*: LineInfo
    errpos*: LineInfo ## Position of original error
    annots*: seq[ErrorAnnotation] ## Additional error annotations


func startpos*(node: NimNode): LineInfo =
  case node.kind:
    of nnkBracketExpr:
      node[0].lineInfoObj()
    else:
      node.lineInfoObj()


proc nthLine(file: string, line: int): string =
  readLines(file, line)[line - 1]

proc lineRange(file: string, linerange: (int, int)): seq[string] =
  # echo file, linerange
  readLines(file, max(linerange[0], linerange[1]))[
    (linerange[0] - 1) .. (linerange[1] - 1)
  ]

func `[]=`(buf: var seq[seq[ColoredRune]],
           row: int, col: int,
           ch: ColoredRune): void =
  for _ in buf.len .. row:
    buf.add @[coloredWhitespaceRune]

  buf[row] &= coloredWhitespaceRune.repeat(max(col - buf[row].len + 1, 0))
  buf[row][col] = ch

proc toColorString*(err: CodeError): string =
  block:
    let (dir, name, ext) = err.raisepos.filename.splitFile()
    result &= &"\n{toRed(name & ext)}:{toRed($err.raisepos.line)}:\n"

  result &= "\n" & err.msg & "\n\n"


  let (dir, name, ext) = err.errpos.filename.splitFile()

  block:
    let annSorted = err.annots.twoPassSortByIt(
      it.errpos.line, -it.errpos.column)

    for lineAnnots in annSorted:
      let
        firstErr = lineAnnots[0]
        position = &"{name}{ext} {firstErr.errpos.line}:{firstErr.errpos.column} "
        filelines = firstErr.errpos.filename.lineRange((
          firstErr.errpos.line + firstErr.linerange, firstErr.errpos.line
        ))

      block:
        for idx, line in filelines[0..^2]:
          let lineidx = $(-(filelines.len - firstErr.errpos.line - idx + 1))
          result &= " " & lineidx & " ".repeat(
            position.len - lineidx.len - 1) & line & "\n"

        result &= position & filelines[^1] & "\n"
      block:
        var spacing = 0
        var buf: seq[seq[ColoredRune]]
        for annot in lineannots:
          let start = (position.len + annot.errpos.column)
          for line in 1 ..+ (spacing + 1):
            buf[line, start] = toColored('|', initPrintStyling(
              fg = fgRed
            ))

          for col in start ..+ annot.expr.len():
            buf[0, col] = toColored('^', initPrintStyling(
              fg = fgRed
            ))

          inc spacing
          for line in annot.annotation.split("\n"):
            for idx, ch in line:
              buf[spacing, start + idx] = toColored(ch)

            inc spacing

        for line in buf:
          result &= $line & "\n"

      result &= "\n" & $firstErr.errpos.filename.toDefault({styleUnderscore})
      result &= "\n\n"

func toCodeError*(node: NimNode, message: string,
                  annotation: string = "",
                  lineRange: int = -2,
                  iinfo: LineInfo = LineInfo()): CodeError =
  new(result)
  {.noSideEffect.}:
    result.msg = toColorString(CodeError(
      msg: message,
      raisepos: iinfo,
      annots: @[
        ErrorAnnotation(
          linerange: linerange,
          errpos: node.startpos(),
          expr: $node.toStrLit,
          annotation: annotation
        )
      ]
    ))


func toCodeError*(nodes: openarray[tuple[node: NimNode, annot: string]],
                  message: string,
                  iinfo: LineInfo = LineInfo()): CodeError =
  new(result)
  {.noSideEffect.}:
    result.msg = toColorString(CodeError(
      msg: message,
      raisepos: iinfo,
      annots: (
        block:
          nodes.mapIt:
            ErrorAnnotation(
              linerange: 0,
              errpos: it.node.startpos(),
              expr: $it.node.toStrLit,
              annotation: it.annot))))

when isMainModule:
  macro test(a: untyped): untyped =
    raise toCodeError({
      a[3] : "Third element in array",
      a[0] : "Array starts here\nMultiline annotations",
      a[5] : "On different line"
    }, "Annotation for array error")

  test([1,2,3,4,
        5,6])

template assertNodeIt*(
  node: NimNode, cond: untyped, msg: string): untyped =
  # IDEA generate assertions for expected node kinds (for untyped
  # macros) and types (for `typed` arguments) using `NType` from
  # initcalls.

  # NOTE this could be used as a basis for type mismatch error
  # improvements implementation. Could be paired with `objdiff`
  # algorithm (tree diffing for `NType`)
  block:
    let it {.inject.} = node
    if not cond:
      raise toCodeError(it, msg)


func toLineInfo*(arg: tuple[
  filename: string, line: int, column: int]): LineInfo =
  LineInfo(
    filename: arg.filename,
    line: arg.line,
    column: arg.column
  )

template raiseCodeError*(node: NimNode, message: string,
                         annotation: string = "",
                         linerange: int = -2): untyped =
  raise toCodeError(node, message, annotation, linerange,
                    instantiationInfo().toLineInfo())



template getCEx*(t: untyped): untyped =
  cast[t](getCurrentException())

proc printSeparator*(msg: string): void =
  let str = center(
    " " & msg & " ",
    width = terminalWidth(),
    fillChar = '='
  )

  echo str.toDefault(style = { styleDim })

proc getFileName*(f: string): string =
  let (_, name, ext) = f.splitFile()
  return name & ext

template pprintErr*(): untyped =
  mixin toGreen, toDefault, toYellow, getFileName, splitFile
  block:
    let e = getCurrentException()
    let choosenim = getHomeDir() & ".choosenim"

    let stackEntries = e.getStackTraceEntries()
    when nimvm:
      discard
    else:
      echo ""
      printSeparator("Exception")
      echo ""

    for tr in stackEntries:
      let filename: string = $tr.filename

      let prefix =
        if not filename.startsWith(choosenim): "(usr) "
        else: $("(sys) ".toGreen())


      let (_, name, ext) = filename.splitFile()
      echo(
        prefix &
        $name.toDefault(style = { styleDim }) &
          ":" &
          $($tr.line).toDefault(style = { styleUnderscore }) &
          " " &
          $($tr.procname).toYellow())

    let idx = e.msg.find('(')
    echo ""
    echo(
      (idx > 0).tern(e.msg[0 ..< idx].getFileName() & " ", "") &
      e.msg[(if idx > 0: idx else: 0)..^1])


# DOC use formatting only on literal nodes, pas non-literal as-is
template optFmt(arg: string{lit}): untyped = &arg
proc optFmt(arg: string): string = arg

func joinLiteral(msg: string): string =
  msg.split("\n")
    .filterIt(it.find(re"[^\s]") != -1)
    .mapIt(it[it.find(re"""[^\s]""") .. ^1])
    .join(" ")

template longAssertionCheck*(expression: untyped, body: untyped): untyped =
  ## Raise `AssertionError` if `expression` evaluates as false. Body
  ## is a string literal which will be passed as a message. It will be
  ## passed to `&` macro - i.e. variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      let variable = 2
      longAssertionCheck(variable == 3):
        """
        Failed to break math while comparing {variable} to `3`
        """
    except AssertionError:
      test = true

    assert test

  static: assert body is string
  assert expression, joinLiteral(optFmt body)


template longAssertionFail*(body: untyped): untyped =
  ## Raise `AssertionError`. Body is a string literal which will be
  ## passed as a message. It will be passed to `&` macro - i.e.
  ## variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      longAssertionFail:
        "Assertion failed"
    except AssertionError:
      test = true

    assert test

  static: assert body is string
  raise newException(AssertionError, joinLiteral(&body))


template longValueCheck*(expression: untyped, body: untyped): untyped =
  ## Raise `ValueError` if `expression` evaluates as false. Body is a
  ## string literal which will be passed as a message. It will be
  ## passed to `&` macro - i.e. variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      let variable = 2
      longValueCheck(variable == 3):
        """
        Failed to break math while comparing {variable} to `3`
        """
    except ValueError:
      test = true

    assert test

  if not (expression):
    raise newException(ValueError, joinLiteral(&body))


template longValueFail*(body: untyped): untyped =
  ## Raise `ValueError`. Body is a string literal which will be
  ## passed as a message. It will be passed to `&` macro - i.e.
  ## variable interpolation is supported.
  runnableExamples:
    var test = false
    try:
      longValueFail:
        "Assertion failed"
    except ValueError:
      test = true

    assert test

  static: assert body is string
  raise newException(ValueError, joinLiteral(&body))
