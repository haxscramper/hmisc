import strutils, macros
import times

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
  {.noSideEffect.}:
    when variable is string:
      debugecho astToStr(variable), ": \"", variable, "\" ", other.join(" ")
    else:
      debugecho astToStr(variable), ": ", variable, " ", other.join(" ")



template plog*(body: untyped): untyped =
  # Really? what the fuck is this shit
  when defined(haxPrintLogging):
    {.noSideEffect.}:
      body

var doLog {.compiletime.}: bool = false

template startHax*() =
  static:
    doLog = true

template stopHax*() =
  static:
    doLog = false

template workHax*(body: untyped): untyped =
  startHax()

  body

  stopHax()

template haxThis*(a: untyped): untyped =
  {.noSideEffect.}:
    if doLog:
      echov a


