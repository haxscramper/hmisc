import strformat, strutils, algorithm, sequtils, macros, os, re, terminal
import types/colorstring

type
  ErrorAnnotation = object
    errpos*: LineInfo
    expr*: string
    annotation*: string

  CodeError* = ref object of CatchableError
    errpos: LineInfo ## Position of original error
    annots: seq[ErrorAnnotation] ## Additional error annotations


proc nthLine(file: string, line: int): string =
  readLines(file, line)[line - 1]


proc toColorString*(err: CodeError): string =
  result &= "\n" & err.msg & "\n"

  for err in err.annots:
    let (dir, name, ext) = err.errpos.filename.splitFile()
    let position = &"{name}{ext} {err.errpos.line}:{err.errpos.column} "
    let padding = " ".repeat(position.len + err.errpos.column)

    result &= position & nthLine(err.errpos.filename, err.errpos.line) & "\n"
    result &= padding & $("^".repeat(err.expr.len()).toRed()) & "\n"
    for line in err.annotation.split("\n"):
      result &= padding & err.annotation & "\n"
    result &= ""

func toCodeError*(node: NimNode, message, annotation: string): CodeError =
  new(result)
  {.noSideEffect.}:
    result.msg = toColorString(CodeError(
      msg: message,
      annots: @[
        ErrorAnnotation(
          errpos: node.lineInfoObj(),
          expr: $node.toStrLit,
          annotation: annotation
        )
      ]
    ))

template getCEx*(t: untyped): untyped =
  cast[t](getCurrentException())

proc printSeparator*(msg: string): void =
  let str = center(
    " " & msg & " ",
    width = terminalWidth(),
    fillChar = '='
  )

  echo str.toDefault(style = { styleDim })


template pprintErr*(body: untyped): untyped =
  template pprintStackTrace(): untyped =
    let e = getCurrentException()
    let choosenim = getHomeDir() & ".choosenim"

    let stackEntries = e.getStackTraceEntries()
    echo ""
    printSeparator("Exception")
    echo ""
    for tr in stackEntries:
      let filename: string = $tr.filename

      let prefix =
        if not filename.startsWith(choosenim): "(usr) "
        else: $("(sys) ".toGreen())


      let (_, name, ext) = filename.splitFile()
      ceUserLog0(
        prefix &
        $name.toDefault(style = { styleDim }) &
          ":" &
          $($tr.line).toDefault(style = { styleUnderscore }) &
          " " &
          $($tr.procname).toYellow())

    let idx = e.msg.find('(')
    echo ""
    ceUserError0(
      (idx > 0).tern(e.msg[0 ..< idx].getFileName() & " ", "") &
      e.msg[(if idx > 0: idx else: 0)..^1])

  try:
    body
  except:
    pprintStackTrace()


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
