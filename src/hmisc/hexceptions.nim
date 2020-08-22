import strformat, strutils, algorithm, sequtils, macros, os, re, terminal
import types/colorstring
import algo/[hseq_mapping, hmath]

type
  ErrorAnnotation* = object
    errpos*: LineInfo
    expr*: string
    annotation*: string
    linerange*: int


  CodeError* = ref object of CatchableError
    raisepos*: LineInfo
    errpos*: LineInfo ## Position of original error
    annots*: seq[ErrorAnnotation] ## Additional error annotations
    postannot*: string


## Exception type and helper functions for generating better errors in
## macro - automatically show **original** source code (not mangled
## macro-generated horror you get when `mapIt` result is passed to
## function call), support colored multiline annotations for
## exceptions, multiple annotations on the same line etc.

func startpos*(node: NimNode): LineInfo =
  case node.kind:
    of nnkBracketExpr, nnkDotExpr, nnkAsgn, nnkCall:
      node[0].lineInfoObj()
    of nnkInfix:
      node[1].lineInfoObj()
    else:
      node.lineInfoObj()


proc nthLine(file: string, line: int): string =
  readLines(file, line)[line - 1]

proc lineRange(file: string, linerange: (int, int)): seq[string] =
  # echo file, linerange
  readLines(file, max(linerange[0], linerange[1]))[
    (linerange[0] - 1) .. (linerange[1] - 1)
  ]

proc toColorString*(err: CodeError): string =
  let docolor = not defined(plainStdout)

  result &= "\n\n" & (if docolor: err.msg else: err.msg.stripSGR()) & "\n\n"


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
        var spacing = 1
        var buf: seq[seq[ColoredRune]]
        for annot in lineannots:
          let start = (position.len + annot.errpos.column)
          for line in 1 ..+ (spacing + 1):
            buf[line, start] = toColored('|', initPrintStyling(
              fg = fgRed
            ), colorize = docolor)

          # TODO IDEA add arrow in the expression 'center' and `~`
          # everywhere else.
          buf[0, start] = toColored(uc"^", initPrintStyling(
            fg = fgRed
          ), colorize = docolor)

          for col in (start + 1) ..+ (annot.expr.len() - 1):
            buf[0, col] = toColored('~', initPrintStyling(
              fg = fgRed
            ), colorize = docolor)

          inc spacing
          for line in annot.annotation.split("\n"):
            let line = if docolor: line else: line.stripSGR()
            for idx, ch in line:
              buf[spacing, start + idx] = toColored(ch)

            inc spacing

        for line in buf:
          result &= $line & "\n"

        result &= "\n"


    block:
      let firstErr = annSorted[0][0]
      let (dir, name, ext) = err.raisepos.filename.splitFile()
      result &= $firstErr.errpos.filename.toDefault(
        {styleUnderscore}, colorize = docolor) & "\n"


      result &= &"\nRaised in {toRed(name & ext, docolor)}:{toRed($err.raisepos.line, docolor)}\n"
      result &= err.postannot & "\n\n"



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
              linerange: -1,
              errpos: it.node.startpos(),
              expr: $it.node.toStrLit,
              annotation: it.annot))))

func toStaticMessage*(
  errpos: LineInfo,
  expr: string,
  message: string,
  annot: string, iinfo: LineInfo = LineInfo()): string =
  {.noSideEffect.}:
    toColorString(CodeError(
        msg: message,
        raisepos: iinfo,
        annots: @[
            ErrorAnnotation(
              linerange: -1,
              errpos: errpos,
              expr: expr,
              annotation: annot)]))


func toStaticMessage*(
  node: NimNode, message: string,
  annot: string, iinfo: LineInfo = LineInfo()): string =
  toStaticMessage(node.startpos(), node.toStrLit().strval(),
                  message, annot, iinfo)



func toCompilesAssert*(
  errpos: LineInfo,
  expr: string,
  compileBody: NimNode,
  annotation: string,
  addBody: bool = true): NimNode =
  let str = toStaticMessage(
    errpos, expr, "Failed to compile",
    annotation).newLit()

  if addBody:
    quote do:
      when not compiles(`compileBody`):
        static: echo `str`
        `compileBody`
  else:
    quote do:
      when not compiles(`compileBody`):
        static: echo `str`

func toCompilesAssert*(
  node, compileBody: NimNode, annotation: string,
  addBody: bool = false): NimNode =
  ## Generate `when not compiles()` assert for `compileBody`. If
  ## compilation fails show position of the `node` with `annotaion`.
  ##
  ## This is useful for various DSL that generate code - in that case
  ## `node` is the original code in DSL, `compileBody` is whatever
  ## generated by macro and `annotation` is additional comment.
  ## Something like `annotation = "Generated
  ## {compileBody.toStrLit()}"` is most likely what you want. NOTE:
  ## multiline annotations are supported, if generated code is
  ## multiline it should not be messed up in output.
  toCompilesAssert(
    node.startpos(),
    node.toStrLit().strval(),
    compileBody,
    annotation,
    addBody = addBody)


when isMainModule:
  when false:
    macro randomDSL(body: untyped): untyped =
      let
        start = body.startpos()
        expr = body.toStrLit().strval()

      result = quote do:
        generatedFunction(`body`)

      let staticAss #[ert]# = toCompilesAssert(
        start, expr, result,
        &"Called {result.toStrLit().strVal().toYellow()}")

      result = quote do:
        `staticAss`
        `result`

    randomDSL(90)

  when true:
    macro expectCompiles(body: untyped): untyped =
      result = toCompilesAssert(
        body, body, "Expression is ", addBody = false)

    expectCompiles(1 + "12")


  when false:
    macro test(a: untyped): untyped =
      raise toCodeError({
        a[2] : "Third element in array",
        a[0] : "Array starts here\nMultiline annotations",
        a[5] : "Annotation for part on the different line"
      }, "Annotation for array error")

    test([1,2,3,4,
          5,6])

template assertNodeIt*(
  node: NimNode, cond: untyped,
  msg: untyped, annot: string = ""): untyped =
  # IDEA generate assertions for expected node kinds (for untyped
  # macros) and types (for `typed` arguments) using `NType` from
  # initcalls.

  # NOTE this could be used as a basis for type mismatch error
  # improvements implementation. Could be paired with `objdiff`
  # algorithm (tree diffing for `NType`)
  block:
    let it {.inject.} = node
    if not cond:
      raise toCodeError(it, msg, annot)

template assertNodeKind*(
  node: NimNode, kindSet: set[NimNodeKind]): untyped =
  node.assertNodeIt(
    node.kind in kindSet,
    (&"Unexpected node kind. Expected one of " &
      $kindSet & " but found " & $node.kind),
    $node.kind)


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
