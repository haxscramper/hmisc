import std/[parseutils, strutils]
import hmisc/core/all
import pkg/jsony

proc dumpHook*(s: var string, c: cstring) = dumpHook(s, $c)
proc parseHook*(s: string, i: var int, res: var cstring) =
  var tmp: string
  parseHook(s, i, tmp)
  res = cstring(tmp)

proc dumpHook*[I](s: var string, ins: set[I]) =
  s.add "["
  for idx, item in pairs(ins):
    if idx > 0:
      s.add ","

    s.add $item
  s.add "]"

proc parseHook*[I](s: string, i: var int, res: var set[I]) =
  s.eatChar(i, '[')
  while s[i] != ']':
    let start = i
    let pos = skipUntil(s, ',', i)
    res.incl parseEnum[I](s[start .. pos])
    i = pos
    s.eatChar(i, ',')
    s.eatChar(i, ' ')

  s.eatChar(i, ']')
