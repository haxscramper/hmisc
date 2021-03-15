import std/[streams, strscans, strutils, strformat, macros]
import ../base_errors, ../hdebug_misc
import ./halgorithm

type
  PosStr* = object
    ## Helper type for scanning complicated strings
    str*: string
    stream*: Stream
    pos*: int
    ranges*: seq[int]

using str: var PosStr

const
  LowerAsciiLetters* = {'a' .. 'b'}
  HighAsciiLetters* = {'A' .. 'Z'}
  HexDigitsLow* = {'a', 'b', 'c', 'd', 'e', 'f'} + Digits
  HexDigitsHigh* = {'A', 'B', 'C', 'D', 'E', 'F'} + Digits
  HexDigits* = HexDigitsLow + HexDigitsHigh
  PunctOpenChars* = {'(', '[', '{', '<'}
  PunctCloseChars* = {')', ']', '}', '>'}
  PunctSentenceChars* = {',', '.', '?', '!'}
  PunctChars* = PunctOpenChars + PunctCloseChars + PunctSentenceChars


template atom*(input: PosStr; idx: int; c: char): bool =
  input.str[input.pos + idx] == c

template atom*(input: PosStr; idx: int; s: set[char]): bool =
  input.str[input.pos + idx] in s

template hasNxt*(input: PosStr; idx: int): bool =
  input.pos + idx < input.str.len

proc finished*(str: PosStr): bool =
  not str.hasNxt(0) and str.stream.atEnd()


template nxt*(input: var PosStr; idx, step: int = 1) =
  inc(idx, step)
  inc(input.pos, step)


macro scanpTemp*(str: typed, pattern: varargs[untyped]): untyped =
  result = nnkStmtList.newTree()
  let tmp = genSym(nskVar, "tmp")
  result.add newVarStmt(tmp, newLit(0))
  result.add newCall("scanp", str, tmp)
  for patt in pattern:
    result[^1].add patt

proc fillNext*(str; chars: int) =
  ## Read necessary amount of data from stream to make at least `chars`
  ## lookahead available
  let needed = chars - (str.str.len - str.pos - 1)
  if needed > 0:
    str.str &= str.stream.readStr(needed)

proc resetBuffer*(str) =
  ## Reset buffer position
  str.pos = 0
  str.str.setLen(0)


proc `[]`*(str; idx: int = 0): char {.inline.} =
  fillNext(str, idx)
  if not hasNxt(str, idx):
    raiseArgumentError(
      &"Cannot get char at [+{idx}]")

  str.str[str.pos + idx]

proc `[]`*(str; offset: int, patt: char | set[char] | string):
  bool {.inline.} =

  fillNext(str, offset)
  when patt is char:
    result = (str.pos + offset < str.str.len) and str[offset] == patt

  elif patt is set[char]:
    result = (str.pos + offset < str.str.len) and str[offset] in patt

  else:
    result = true
    for idx, ch in patt:
      if not str[offset + idx, ch]:
        return false

proc `[]`*(str; patt: char|set[char]|string): bool {.inline.} =
  return str[0, patt]



proc `@`*(str): seq[char] =
  for ch in str.str[str.pos .. ^1]:
    result.add ch

proc `[]`*(str; slice: Slice[int]): string =
  for i in slice:
    result.add str[i]

proc `[]`*(str; slice: HSlice[int, char]): string =
  var pos = slice.a
  while not str[pos, slice.b] and hasNxt(str, pos):
    result.add str[pos]
    inc pos



proc `$`*(str): string {.inline.} =
  &"[{str.pos}: {str.str[str.pos .. ^1]}]"

proc pushRange*(str: var PosStr) {.inline.} =
  str.ranges.add str.pos

proc popRange*(str; leftShift: int = 0, rightShift: int = 0):
  string {.inline.} =

  let start = str.ranges.pop
  return str.str[(start + leftShift) ..<
                 min(str.pos + rightShift, str.str.len)]

proc advance*(str; step: int = 1) {.inline.} =
  inc(str.pos, step)

proc skipWhile*(str; chars: set[char]) {.inline.} =
  if str[] in chars:
    while str[chars]:
      str.advance()

proc popWhile*(str; chars: set[char]): string {.inline.} =
  str.pushRange()
  str.skipWhile(chars)
  return str.popRange()

proc startsWith*(str; skip: set[char], search: string): bool =
  var pos = 0
  while str[pos, skip]:
    inc pos

  result = str[pos, search]

proc peekStr*(str; chars: int): string =
  for i in 0 ..< chars:
    result.add str[i]

proc pop*(str: var PosStr): char {.inline.} =
  result = str[]
  str.advance()

proc popChar*(str: var PosStr): char {.inline.} =
  result = str[]
  str.advance()

proc popStringLit*(str: var PosStr): string {.inline.} =
  str.pushRange()

  str.advance()
  while not str['"']:
    str.advance()

  str.advance()

  return str.popRange()

proc popDigit*(str: var PosStr): string {.inline.} =
  str.pushRange()
  if str["0x"]:
    str.advance(2)
    str.skipWhile(HexDigits + {'-'})

  else:
    str.skipWhile(Digits + {'-'})

  return str.popRange()

proc popIdent*(str; chars: set[char] = IdentChars):
  string {.inline.} = str.popWhile(chars)

proc readLine*(str; skipNl: bool = true): string =
  while not str['\n']:
    if not str.finished():
      result.add str.pop()

    else:
      return

  if skipNl:
    str.advance()

  else:
    result.add str.pop()

proc skipLine*(str) =
  while not str['\n']: str.advance()
  str.advance()
