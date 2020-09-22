import strutils, macros
import times

var doLog {.compiletime.}: bool = false
var doLogRuntime: bool = false

template ifHaxComp*(body: untyped): untyped =
  static:
    if doLog:
      body

template startHaxComp*() =
  static:
    doLog = true

template startHax*() =
  doLogRuntime = true

template stopHax*() =
  doLogRuntime = false

template workHax*(doIt: static[bool], body: untyped): untyped =
  when doIt:
    doLogRuntime = true

  body

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

template haxThis*(a: untyped): untyped =
  {.noSideEffect.}:
    if doLog:
      echov a

template dieHereComp*(): untyped =
  static:
    if doLog:
      quit 1


template dieHereMacro*(): untyped =
  if doLog:
    quit 1

template dieHere*(): untyped =
  if doLogRuntime:
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


template templAssert*(
  iinfo: tuple[filename: string, line: int, column: int],
  condition: untyped,
  message: string): untyped =
  if not condition:
    raiseAssert(
      message & ". Exception from template instantiation on line " &
        $iinfo.line & ", file: " & iinfo.filename)

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

template echov*(variable: untyped, other: varargs[string, `$`]): untyped =
  # Store previous indentation echo and print next level indented or
  # unindented based on new column value.
  {.noSideEffect.}:
    block:
      let iinfo = instantiationInfo()
      var line = ($iinfo.line).alignLeft(4) & " | "
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
        if vart.split("\n").len > 1:
          vart = "\n\"\"\"\n" & vart & "\n\"\"\"\n"
        else:
          vart = "\"" & vart & "\""
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

      var text = line & "\e[32m" & astToStr(variable) & "\e[39m: " &
        vart & " " & other.join(" ")

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
