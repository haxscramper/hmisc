import std/[
  strformat, strutils, algorithm, sequtils,
  macros, os, enumerate, terminal
]

export alignLeft
import ./debug, ./exceptions, ./colored, ./algorithms, ./gold

## Exception type and helper functions for generating better errors in
## macro - automatically show **original** source code (not mangled
## macro-generated horror you get when `mapIt` result is passed to
## function call), support colored multiline annotations for
## exceptions, multiple annotations on the same line etc.

func toLineInfo*(arg: InstantiationInfo): LineInfo =
  LineInfo(
    filename: arg.filename,
    line: arg.line,
    column: arg.column
  )


func startpos*(node: NimNode): LineInfo =
  case node.kind:
    of nnkBracketExpr, nnkDotExpr, nnkAsgn, nnkCall,
       nnkExprColonExpr, nnkObjConstr, nnkCommand,
       nnkArgList:
      node[0].lineInfoObj()

    of nnkInfix:
      node[1].lineInfoObj()

    else:
      node.lineInfoObj()


proc nthLine(file: string, line: int): string =
  readLines(file, line)[line - 1]

proc lineRange(file: string, linerange: (int, int)): seq[string] =
  readLines(file, max(linerange[0], linerange[1]))[
    (linerange[0] - 1) .. (linerange[1] - 1)
  ]

proc getLines(annot: ErrorAnnotation, baseStr: string): seq[string] =
  if annot.fromString:
    if annot.offset > baseStr.len:
      return @[baseStr[max(baseStr.rfind('\n'), 0) .. ^1]]

    else:
      var lineStart = annot.offset
      while baseStr[lineStart] != '\n' and lineStart > 0:
        dec lineStart

      inc lineStart

      for idx, line in enumerate(baseStr[lineStart .. ^1].split('\n')):
        if idx > annot.linerange:
          break

        result.add line

  else:
    result = annot.errpos.filename.lineRange((
      annot.errpos.line + annot.linerange,
      annot.errpos.line
    ))

proc getColumn(annot: ErrorAnnotation, main: CodeError): int =
  if annot.fromString:
    var offset = annot.offset
    while offset > -1 and main.substr.str[offset] != '\n':
      dec offset

    inc offset

    while offset < annot.offset:
      inc result
      inc offset

    dec result

  else:
    result = annot.errpos.column



proc getFile(err: CodeError): string =
  if err.fromString:
    err.substr.filename
  else:
    err.errpos.filename

proc formatPos(annot: ErrorAnnotation, name, ext: string): string =
  if annot.fromString:
    &"{name}{ext} "
  else:
    &"{name}{ext} {annot.errpos.line}:{annot.errpos.column} "


proc toColorString*(err: CodeError): string =
  let docolor = not defined(plainStdout)

  result &= "\n\n" & (if docolor: err.msg else: err.msg.stripSGR()) & "\n\n"


  let (dir, name, ext) = err.getFile().splitFile()

  block:
    var annSorted =
      if err.fromString:
        @[err.annots.sortedByIt(it.offset)]

      else:
        err.annots.twoPassSortByIt(it.errpos.line, -it.errpos.column)

    for lineAnnots in annSorted:
      let
        firstErr = lineAnnots[0]
        position = firstErr.formatPos(name, ext)
        filelines = firstErr.getLines(
          tern(err.fromString, err.substr.str, ""))

      block:
        for idx, line in filelines[0..^2]:
          if firstErr.fromString:
            result &= line

          else:
            let lineidx = $(-(filelines.len - firstErr.errpos.line - idx + 1))

            result &= " " & lineidx & " ".repeat(
              position.len - lineidx.len - 1) & line & "\n"

        result &= position & filelines[^1] & "\n"

      block:
        var spacing = 1
        var buf: seq[seq[string]]
        let lastline = position & filelines[^1]
        for annot in lineannots:
          let start = (position.len + annot.getColumn(err))
          for line in 1 ..< 1 + (spacing + 1):
            buf.setPart(line, start, toRedStr("|", docolor))

          # TODO IDEA add arrow in the expression 'center' and `~`
          # everywhere else.
          buf.setPart(0, start, toRedStr("^", docolor))

          var foundEnd = false
          for col in countdown(
            start + 1 + min(60, annot.expr.len() - 1),
            (start + 1)
          ):
            if col < lastline.len and lastline[col] != ' ':
              foundEnd = true

            if foundEnd:
              buf.setPart(0, col, toRedStr("~", doColor))

          if annot.annotation.len > 0:
            inc spacing
            for line in annot.annotation.split("\n"):
              let line = if docolor: line else: line.stripSGR()
              for idx, ch in line:
                buf.setPart(spacing, start + idx, $ch)

              inc spacing

        result.add joinBuf(buf)
        result &= "\n"


    block:
      let firstErr = annSorted[0][0]
      let (dir, name, ext) = err.raisepos.filename.splitFile()
      result &= $err.getFile() & "\n"
      result &= &"\nRaised in {toRedStr(name & ext, docolor)}:{toRedStr($err.raisepos.line, docolor)}\n"
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
      fromString: false,
      annots: @[
        ErrorAnnotation(
          linerange: linerange,
          fromString: false,
          errpos: node.startpos(),
          expr: $node.toStrLit,
          annotation: annotation
        )
      ]
    ))

func toCodeError*(
    str: string,
    offset, exprLen: int,
    message, annotation: string
  ): CodeError =
  result = CodeError(fromString: true)

  {.noSideEffect.}:
    result.msg = toColorString(CodeError(
      msg: message,
      fromString: true,
      substr: GlobalSubstring(str: str),
      annots: @[
        ErrorAnnotation(
          expr: tern(offset > str.high,
                     "",
                     str[offset ..< min(str.len, offset + exprLen)]),

          fromString: true,
          annotation: annotation,
          linerange: 1,
          offset: offset
        )
      ]
    ))



func toCodeError*(nodes: openarray[tuple[node: NimNode, annot: string]],
                  message: string,
                  iinfo: LineInfo = instantiationInfo().toLineInfo()
                 ): CodeError =
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
              fromString: false,
              errpos: it.node.startpos(),
              expr: $it.node.toStrLit,
              annotation: it.annot))))

func toStaticMessage*(
  errpos: LineInfo,
  expr: string,
  message: string,
  annot: string,
  iinfo: LineInfo = instantiationInfo().toLineInfo()): string =
  {.noSideEffect.}:
    toColorString(CodeError(
        msg: message,
        raisepos: iinfo,
        fromString: false,
        annots: @[
            ErrorAnnotation(
              fromString: false,
              linerange: -1,
              errpos: errpos,
              expr: expr,
              annotation: annot)]))


func toStaticMessage*(
  node: NimNode, message: string,
  annot: string,
  iinfo: LineInfo = instantiationInfo().toLineInfo()): string =
  toStaticMessage(node.startpos(), node.toStrLit().strval(),
                  message, annot, iinfo)

func toStaticMessage*(
    errpos: InstantiationInfo,
    expr: string,
    message: string,
    annot: string
  ): string =

  {.cast(noSideEffect).}:
    toColorString(CodeError(
      msg: message,
      raisepos: errpos.toLineInfo(),
      fromString: false,
      annots: @[
        ErrorAnnotation(
          fromString: false,
          linerange: -1,
          errpos: errpos.toLineInfo(),
          expr: expr,
          annotation: annot)]))

template hxInfo*(): untyped {.dirty.} =
  instantiationInfo(fullpaths = true)

template staticAssert*(
    assrt: untyped, message, annotation: string,
    iinfo: InstantiationInfo
  ): untyped =

  static:
    assert assrt, toStaticMessage(
      iinfo,
      astToStr(assrt),
      message,
      annotation
    )


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


when isMainModule and not defined(nimdoc):
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
  msg: untyped, annot: string = "",
  iinfo: LineInfo = LineInfo()): untyped =
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

template assertNodeKind*(node: NimNode, kindSet: set[NimNodeKind]): untyped =
  # iinfo = instantiationInfo(fullpaths = true)): void =
  ## assert node kind is in the set. Provide higlighted error with
  ## list of expected types and kind of given node
  if node.kind notin kindSet:
    let iinfo = instantiationInfo(fullPaths = true)
    {.line: instantiationInfo(fullPaths = true).}:
      raise toCodeError(node,
        (&"Unexpected node kind. Expected one of " &
          $kindSet & " but found " & $node.kind),
        $node.kind,
        iinfo = iinfo.toLineInfo())


proc assertNodeKindNot*(
  node: NimNode, kindSet: set[NimNodeKind],
  iinfo: LineInfo = instantiationInfo().toLineInfo()): void =
  ## assert node kind is in the set. Provide higlighted error with
  ## list of expected types and kind of given node
  if node.kind in kindSet:
    raise node.toCodeError(
      (&"Unexpected node kind - not allowed node kinds: " &
        $kindSet & " but found " & $node.kind),
      $node.kind, iinfo = iinfo
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

  echo str.toWhiteStr()

proc getFileName*(f: string): string =
  let (_, name, ext) = f.splitFile()
  return name & ext

template pprintStackTrace*(ex: ref Exception = nil): untyped =
  mixin toGreen, toDefault, toYellow, getFileName, splitFile
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      let e = if isNil(ex): getCurrentException() else: ex
      let stackEntries =
        if not isNil(e):
          e.getStackTraceEntries()
        else:
          getStackTraceEntries()



      let choosenim = getHomeDir() & ".choosenim"
      when nimvm:
        discard
      else:
        echo ""
        if not isNil(e):
          printSeparator("Exception")
        else:
          printSeparator("Stacktrace")

        echo ""

      var fileW = 0
      for tr in stackEntries:
        let (_, name, ext) = ($tr.filename).splitFile()
        fileW = max(name.len, fileW)


      var foundErr: bool = false
      for idx, tr in stackEntries:
        let filename: string = $tr.filename

        let prefix =
          if not filename.startsWith(choosenim):
            if startsWith($tr.procname, "expect") or
               startsWith($tr.procname, "assert"):
              "\e[34m(asr) \e[39m"

            else:
              "\e[32m(usr) \e[39m"

          else:
            "(sys) "


        let (_, name, ext) = filename.splitFile()
        var filePref = toLink(
          ($tr.filename, tr.line, 0), strutils.alignLeft($name, fileW))

        if (not foundErr) and idx + 1 < stackEntries.len:
          let next = stackEntries[idx + 1]
          let nextFile = $next.filename
          if startsWith(nextFile, choosenim) or
             startsWith($next.procname, "expect") or
             startsWith($next.procname, "assert"):
            filePref = toRedStr(filePref)
            foundErr = true

        echo(
          prefix & (filePref) & " :" &
            $(strutils.alignLeft($tr.line, 4)) &
            " " &
            toYellowStr($tr.procname))

      # let idx = e.msg.find('(')
      echo ""
      if e != nil:
        echo e.msg


template haxStackTrace*(): untyped =
  when nimvm:
    if haxRunningComp():
      pprintStackTrace()

  else:
    if haxRunning():
      pprintStackTrace()


template pprintErr*(): untyped =
  pprintStackTrace()

    # echo(
    #   (idx > 0).tern(e.msg[0 ..< idx].getFileName() & " ", "") &
    #   e.msg[(if idx > 0: idx else: 0)..^1])


# DOC use formatting only on literal nodes, pas non-literal as-is
template optFmt(arg: string{lit}): untyped = &arg
proc optFmt(arg: string): string = arg
