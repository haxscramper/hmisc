import hmisc/algo/hseq_distance
import math, sequtils, strutils
export math, sequtils, strutils, hseqdistance

template matchTest*(inPatt, inInput: string, expr: untyped): untyped =
  let res = fuzzyMatch(
    inPatt, inInput,
    proc(p, o: string, m: seq[int]): int =
      let patt {.inject.} = p
      let other {.inject.} = o
      let matches {.inject.} = m
      # echo m
      expr
  )

  echo "input: ", inInput, " ", inPatt, " :", res.score
  var buf = " ".repeat(inInput.len)
  for pattIdx, inIdx in res.matches:
    buf[inIdx] = inPatt[pattIdx]

  echo "match: ", buf

when isMainModule:
  matchTest "//hell.txt", "/nice/we/hell.txt":
    if other[matches[0]] == '/':
      1000
    else:
      matches.sum()


  matchTest "nicehell.txt", "/nice/we/hell.txt":
    if other[matches[0]] == '/':
      1000
    else:
      matches.sum()
