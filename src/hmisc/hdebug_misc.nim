import strutils, macros
# import times

var doLog {.compiletime.}: bool = false
var doLogRuntime: bool = false

template ifHaxComp*(body: untyped): untyped =
  static:
    if doLog:
      body

template startHaxComp*() =
  static:
    doLog = true

proc startHax*() = doLogRuntime = true
proc stopHax*() = doLogRuntime = false

template haxRunning*(): bool = doLogRuntime


template stopHaxComp*() =
  static:
    doLog = false


template workHax*(doIt: static[bool], body: untyped): untyped =
  when doIt:
    doLogRuntime = true

  try:
    body
  except:
    when not nimvm:
      echo getCurrentExceptionMsg()

    echo instantiationInfo()
    quit 1

  when doIt:
    doLogRuntime = true

template workHaxComp*(doIt: static[bool], body: untyped): untyped =
  when doIt:
    static:
      doLog = true

  body

  when doIt:
    static:
      doLog = false

template haxc*(body: untyped): untyped =
  workHaxComp true:
    block:
      body

template haxThis*(a: untyped): untyped =
  {.noSideEffect.}:
    if doLog:
      echov a

template dieHereComp*(): untyped =
  static:
    if true:
      quit 1


template dieHereMacro*(): untyped =
  if true:
    quit 1

template dieHere*(): untyped =
  if true:
    quit 1


proc colorPrint*(
  node: NimNode,
  tmpfile: string = "/tmp/nimast_tmp.nim",
  doPrint: bool = true): void =
  # TODO convert nim ast into adequately readable form without using
  # `pygmentize`. Maybe even color macros/templates/procs differently.
  tmpfile.writeFile($node.toStrLit())
  discard staticExec("sed -Ei 's/`gensym[0-9]+//g' " & tmpfile)
  discard staticExec("nimpretty --maxLineLen:75 " & tmpfile)
  if doPrint:
    echo staticExec("pygmentize -f terminal " & tmpfile)





func d*(text: varargs[string, `$`]): void =
  debugecho text.join(" ")

template de*(expr: untyped, text: varargs[string, `$`]): void =
  debugecho astToStr(expr), ": ", expr, " ", text.join(" ")

template dev*(expr: untyped): untyped =
  let val = expr
  debugecho astToStr(expr), ": ", val
  val

template printCpuTime*(body: untyped): untyped =
  let iinfo = instantiationInfo()
  let start = cpuTime()
  body
  let finish = cpuTime()
  echo "Execution took [ms] ",
      int((finish - start) * 1_000_000).float / 1_000,
      " (line ", iinfo.line, ")"



template expectType*(op, t: untyped): untyped =
  static: assert op is t

template echoi*(indent: int, message: varargs[string, `$`]): void =
  ## Echo with indentation. `message` is joined using spaces
  block:
    let iinfo {.inject.} = instantiationInfo()
    echo &"{iinfo.line:>4} |", "  ".repeat(indent), message.join(" ")

proc debugechoi*(indent: int, message: string): void =
  for line in message.split("\n"):
    debugecho "  ".repeat(indent), line

template dechofmt*(arg: string): untyped =
  debugecho fmt(arg)

proc echoi*(message: varargs[string, `$`]): void =
  ## Echo with message joined by spaces
  echo message.join(" ")

proc `@`*(p: ptr): string = $cast[int](p)


template echov*(variable: untyped, other: varargs[string, `$`]): untyped =
  # Store previous indentation echo and print next level indented or
  # unindented based on new column value.
  #
  # TODO wrap expression in `try-catch` and print exception messages
  {.noSideEffect.}:
    block:
      let iinfo = instantiationInfo()
      var line = strutils.alignLeft($iinfo.line, 4) & " | "
          # & " ".repeat(max(iinfo.column - 6, 0))

      var vart =
        when variable is NimNode:
          "\e[34m" & ($variable.kind)[3..^1] & "\e[39m " &
            variable.toStrLit().strVal().multiReplace({
              "\\\"" : "\"",
              "\\n" : "\n"
            })
        else:
          $variable

      when variable is string:
        # vart = vart.multiReplace({"\n" : "⮒\n"})
        if vart.split("\n").len > 1:
          # discard
          vart = "\n" & vart

        else:
          vart = "\"" & vart & "\""

      elif (variable is char):
        vart =
          case variable:
            of '\n': "\\n"
            of '\t': "\\t"
            of '\r': "\\r"
            else: vart

        vart = "'" & vart & "'"

      else:
        if vart.split("\n").len > 1:
          if (variable is NimNode) and
             (vart[0] == '"') and
             (vart[^1] == '"'):
            vart = "\n" & vart[1..^2]
          else:
            vart = "\n" & vart

      if vart.split("\n").len > 1 and other.len > 0:
        vart = vart & "\n"

      let pref =
        when variable is static[string]:
          "\e[33m" & vart & "\e[39m:"
        else:
          "\e[32m" & astToStr(variable) & "\e[39m: " & vart


      var text = line & pref & " " & other.join(" ")

      when nimvm:
      # when compiles(doLogRuntime):
        if doLog:
          debugecho text
      else:
        if doLogRuntime:
          debugecho text







template plog*(body: untyped): untyped =
  # Really? what the fuck is this shit
  when defined(haxPrintLogging):
    {.noSideEffect.}:
      body

template echove*(body: untyped): untyped =
  let res = body
  echov body.astToStr(), "=", res
  echov "@", instantiationInfo().line
  res
