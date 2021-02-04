import hmisc/algo/hseq_distance

when defined(js):
  import std/[sequtils, jsffi]

  let res = fuzzyMatch("/tmp", "/tmp", @{{'/'} : 10})
  echo res

  proc nimFuzzyMatch*(pattern, str: cstring): array[3, JsObject] {.exportc.} =
    let res = fuzzyMatch($pattern, $str, @{{'/'} : 10})
    return [toJs(res.ok), toJs(res.score), toJs(str)]
