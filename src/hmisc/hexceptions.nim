import strformat, strutils, algorithm, sequtils, macros, os, re, terminal
import types/colorstring

type
  ErrorAnnotation = object
    errpos*: LineInfo
    expr*: string
    annotation*: string
    linerange*: int


  CodeError* = ref object of CatchableError
    raisepos*: LineInfo
    errpos: LineInfo ## Position of original error
    annots: seq[ErrorAnnotation] ## Additional error annotations


proc nthLine(file: string, line: int): string =
  readLines(file, line)[line - 1]

proc lineRange(file: string, linerange: (int, int)): seq[string] =
  readLines(file, max(linerange[0], linerange[1]))[
    (linerange[0] - 1) .. (linerange[1] - 1)
  ]

proc toColorString*(err: CodeError): string =
  block:
    let (dir, name, ext) = err.raisepos.filename.splitFile()
    result &= &"\n{toRed(name & ext)}:{toRed($err.raisepos.line)}:\n"

  result &= "\n" & err.msg & "\n\n"
  for err in err.annots:
    let (dir, name, ext) = err.errpos.filename.splitFile()

    let position = &"{name}{ext} {err.errpos.line}:{err.errpos.column} "
    let padding = " ".repeat(position.len + err.errpos.column)

    if err.linerange <= 0:
      let filelines = err.errpos.filename.lineRange((
        err.errpos.line + err.linerange, err.errpos.line
      ))

      for line in filelines[0..^2]:
        result &= " ".repeat(position.len) & line & "\n"

      result &= position & filelines[^1] & "\n"
      result &= padding & $("^".repeat(err.expr.len()).toRed()) & "\n"

      for line in err.annotation.split("\n"):
        result &= padding & err.annotation & "\n"
    else:
      let filelines = err.errpos.filename.lineRange((
        err.errpos.line, err.errpos.line + err.linerange
      ))

      for line in err.annotation.split("\n"):
        result &= padding & err.annotation & "\n"
      result &= padding & $("v".repeat(err.expr.len()).toRed()) & "\n"

      result &= position & filelines[0] & "\n"
      for line in filelines[1..^1]:
        result &= " ".repeat(position.len) & line & "\n"


    result &= "\n" & $err.errpos.filename.toDefault({styleUnderscore})

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
          errpos: node.lineInfoObj(),
          expr: $node.toStrLit,
          annotation: annotation
        )
      ]
    ))

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
