import strutils
import times

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
