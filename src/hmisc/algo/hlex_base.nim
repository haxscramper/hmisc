import std/[streams, strscans, strutils, strformat,
            macros, segfaults]
import ../base_errors, ../hdebug_misc
import ./halgorithm


##[

- TODO :: Tokenize range into runes

]##

type
  PosStrSlice = object
    start*: int
    finish*: int

  LineCol* = tuple[line: int, column: int]

  PosStr* = object
    case isSlice*: bool
      of false:
        str*: string
        stream*: Stream

      of true:
        sliceIdx*: int
        baseStr*: ptr string
        slices*: seq[PosStrSlice]

    pos*: int
    line*: int
    column*: int
    ranges*: seq[int]
    bufferActive*: bool
    sliceBuffer*: seq[seq[PosStrSlice]]


  HLexerError* = ref object of CatchableError
    pos*, line*, column*: int


using str: var PosStr

export strutils

const
  LowerAsciiLetters* = {'a' .. 'b'}
  HighAsciiLetters* = {'A' .. 'Z'}
  IntegerStartChars* = {'0' .. '9', '-', '+'}
  HexDigitsLow* = {'a', 'b', 'c', 'd', 'e', 'f'} + Digits
  HexDigitsHigh* = {'A', 'B', 'C', 'D', 'E', 'F'} + Digits
  HexDigits* = HexDigitsLow + HexDigitsHigh
  PunctOpenChars* = {'(', '[', '{', '<'}
  PunctCloseChars* = {')', ']', '}', '>'}
  PunctSentenceChars* = {',', '.', '?', '!', ';', ':'}
  PunctChars* = PunctOpenChars + PunctCloseChars + PunctSentenceChars
  Newline* = {'\n'}
  AllSpace* = Whitespace
  HorizontalSpace* = AllSpace - Newline
  VeritcalSpace* = Newline

template raiseUnexpectedChar*(str: PosStr) =
  raise HLexerError(
    msg: "Unexpected character encountered while parsing: '" & $str[0] &
      "' at " & $str.line & ":" & $str.column,
    column: str.column, line: str.line, pos: str.pos)

template raiseCannotGetOffset*(str: PosStr, offset: int) =
  {.line: instantiationInfo().}:
    mixin finished
    raiseArgumentError(
      "Cannot get char at +" & $offset & " input string is finished: " &
      $finished(str)
    )


# func getLine*(str: PosStr): int {.inline.} = str.line
# func getColumn*(str: PosStr): int {.inline.} = str.column
func lineCol*(str: PosStr): LineCol {.inline.} =
  (line: str.line, column: str.column)

func len*(slice: PosStrSlice): int = slice.finish - slice.start
func toAbsolute*(slice: PosStrSlice, offset: int): int =
  slice.start + offset

func initPosStr*(str: string): PosStr =
  ## Create new string with full buffer and `nil` input stream
  PosStr(str: str, isSlice: false)

func initPosStr*(stream: Stream): PosStr =
  ## Create new string with empty buffer and non-nil input stream.
  PosStr(stream: stream, isSlice: false)

func initPosStr*(str): PosStr =
  ## Pop one layer of slices from slice buffer and create new sub-string
  ## lexer from it.
  PosStr(isSlice: true, baseStr: addr str.str, slices: str.sliceBuffer.pop)

func initPosStrView*(str): PosStr =
  ## Create substring with `0 .. high(int)` slice range
  PosStr(
    isSlice: true, baseStr: addr str.str,
    slices: @[PosStrSlice(start: 0, finish: high(int))]
  )


func contains*(slice: PosStrSlice, position: int): bool =
  slice.start <= position and position <= slice.finish

template atom*(input: PosStr; idx: int; c: char): bool =
  input.str[input.pos + idx] == c

template atom*(input: PosStr; idx: int; s: set[char]): bool =
  input.str[input.pos + idx] in s

proc hasNxt*(input: PosStr; idx: int): bool =
  let pos = input.pos + idx
  if input.isSlice:
    for slice in input.slices[input.sliceIdx .. ^1]:
      if pos in slice and pos < input.baseStr[].len:
        return true

  else:
    return pos < input.str.len

proc finished*(str: PosStr): bool =
  not str.hasNxt(0) and (
    str.isSlice or
    isNil(str.stream) or
    str.stream.atEnd()
  )

proc `?`*(str: PosStr): bool = not str.finished()


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
  if str.isSlice:
    return

  if isNil(str.stream):
    return

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
    raiseCannotGetOffset(str, idx)
    # raiseArgumentError(&"Cannot get char at [+{idx}]")

  if str.isSlice:
    let idx = str.pos + idx
    var sliceStart = 0
    for slice in str.slices[str.sliceIdx .. ^1]:
      if sliceStart <= idx and
         idx <= sliceStart + slice.len:
        return str.baseStr[][slice.toAbsolute(idx - sliceStart)]

      else:
        sliceStart += slice.len

    raiseArgumentError(&"Cannot get char at [+{idx}]")

  else:
    return str.str[str.pos + idx]

proc `[]`*(str; slice: HSlice[int, BackwardsIndex]): string {.inline.} =
  var next = 0
  while str.hasNxt(next):
    fillNext(str, next)
    inc next

  result = str.str[str.pos + slice.a .. (str.pos + next - slice.b.int)]

proc `[]`*(str; slice: HSlice[int, int]): string {.inline.} =
  fillNext(str, max(slice.a, slice.b))
  if str.str.len == 0:
    result = "<empty>"

  else:
    result = str.str[
      min(str.str.high, str.pos + slice.a) ..
      min(str.str.high, str.pos + slice.b)
    ]

    if slice.b > str.str.high:
      result &= "\\0".repeat(slice.b - str.str.high)

proc `[]`*(str; offset: int, patt: char | set[char] | string):
  bool {.inline.} =

  fillNext(str, offset)
  when patt is char:
    result = str.hasNxt(offset) and str[offset] == patt

  elif patt is set[char]:
    result = str.hasNxt(offset) and str[offset] in patt

  else:
    result = true
    for idx, ch in patt:
      if not str[offset + idx, ch]:
        return false

proc `[]`*(str; patt: char|set[char]|string): bool {.inline.} =
  str[0, patt]

proc `[]`*(str; patt1, patt2: char | set[char] | string): bool {.inline.} =
  str[0, patt1] and str[1, patt2]

proc `[]`*(
    str; patt1, patt2, patt3: char | set[char] | string): bool {.inline.} =
  str[0, patt1] and str[1, patt2] and str[2, patt3]


proc `@`*(str): seq[char] =
  for ch in str.str[str.pos .. ^1]:
    result.add ch

# proc `[]`*(str; slice: Slice[int]): string =
#   for i in slice:
#     result.add str[i]

proc `[]`*(str; slice: HSlice[int, char]): string =
  var pos = slice.a
  while not str[pos, slice.b] and hasNxt(str, pos):
    result.add str[pos]
    inc pos

proc `$`*(slice: PosStrSlice): string = &"{slice.start}..{slice.finish}"
proc `[]`*(str; slice: PosStrSlice): string =
  str.baseStr[][
    max(slice.start, 0) ..< min(slice.finish, str.baseStr[].high)]

proc lineAround*(str; pos: int): tuple[line: string, pos: int] =
  var start = pos
  while start > 0 and str.str[start] notin {'\n'}:
    dec start

  result.pos = pos - start

  while start < str.str.len and str.str[start] notin {'\n'}:
    result.line.add str.str[start]
    inc start

proc `$`*(str): string =
  if str.isSlice:
    result = "["
    for slice in str.slices:
      var text = str[slice]
      if text.len == 0:
        continue

      if text.count('\n') == 1:
        text = "\"" & text.replace("\n", "⮒") & "\""

      result &= &"[{slice}: {text}]"

    result &= "]"

  else:
    result = &"[{str.pos}: {str.str[str.pos .. ^1]}]"

template assertAhead*(str: PosStr, ahead: string) =
  if not str[ahead]:
    raise HLexerError(
      msg: "Lexer error - expected : '" & ahead & "', but found " &
        str[0 .. len(ahead)] & " at " & $str.line & ":" & $str.column,
      column: str.column,
      line: str.line,
      pos: str.pos
    )



proc pushRange*(str) {.inline.} =
  str.ranges.add str.pos

proc startSlice*(str) {.inline.} =
  if str.sliceBuffer.len == 0:
    str.sliceBuffer.add @[]

  str.sliceBuffer[^1].add PosStrSlice(start: str.pos)

proc finishSlice*(str) {.inline.} =
  str.sliceBuffer[^1][^1].finish = str.pos

proc toggleBuffer*(str; activate: bool = not str.bufferActive) {.inline.} =
  str.bufferActive = activate


proc popRange*(str; leftShift: int = 0, rightShift: int = 0):
  string {.inline.} =

  let start = str.ranges.pop + leftShift
  let finish = str.pos +  rightShift

  if str.isSlice:
    var foundStart = false
    for slice in str.slices:
      result &= str.baseStr[][
        max(start, slice.start) ..< min(finish, slice.finish)
      ]




  else:
    return str.str[start ..< min(finish, str.str.len)]

proc advance*(str; step: int = 1) {.inline.} =
  if str['\n']:
    inc str.line
    str.column = 0
  else:
    inc str.column

  inc(str.pos, step)

proc skip*(str; ch: char) {.inline.} =
  assert str[] == ch
  str.advance()

proc skipWhile*(str; chars: set[char]) {.inline.} =
  if str[chars]:
    while str[chars]:
      str.advance()

proc skipUntil*(str; chars: set[char], including: bool = false) {.inline.} =
  var changed = false
  while str[AllChars - chars]:
    str.advance()
    changed = true

  if changed and including:
    str.advance()

proc skipToEOL*(str; including: bool = true) =
  str.skipUntil(Newline, including)

proc skipIndent*(str; maxIndent = high(int)): int =
  while str[HorizontalSpace]:
    inc result
    str.advance()
    if result >= maxIndent:
      break

proc popWhile*(str; chars: set[char]): string {.inline.} =
  str.pushRange()
  str.skipWhile(chars)
  return str.popRange()

proc popUntil*(str; chars: set[char] | char): string {.inline.} =
  str.pushRange()
  when chars is set: str.skipUntil(chars) else: str.skipUntil({chars})
  return str.popRange()

proc startsWith*(str; skip: set[char], search: string): bool =
  var pos = 0
  while str[pos, skip]:
    inc pos

  result = str[pos, search]

proc getIndent*(str): int =
  while str[result, HorizontalSpace]:
    inc result

proc hasIndent*(str; indent: int, exactIndent: bool = false): bool =
  var foundIndent = 0
  while str[foundIndent, HorizontalSpace]:
    inc foundIndent
    if foundIndent >= indent:
      break

  if foundIndent == indent:
    return true

  elif foundIndent <= indent:
    return false

  else:
    return not exactIndent or (str[foundIndent] notin HorizontalSpace)


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

  var found = false
  while not found:
    found = str['"'] and not str[-1, '\\']
    str.advance()

  result = str.popRange()
  # str.advance()


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


proc popNext*(str; count: int): string {.inline.} =
  str.pushRange()
  str.advance(count)
  return str.popRange()

proc popBacktickIdent*(str): string {.inline.} =
  if str[] == '`':
    str.advance()

  str.popUntil({'`'})

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
  while not str['\n'] and not str.finished(): str.advance()
  str.advance()

import std/re
import ../other/rx


proc matchLen*(inStr: PosStr, regex: Regex): int =
  matchLen(inStr.str, regex, inStr.pos)

proc matchLen*(
    inStr: PosStr, regex: Regex, matches: var openarray[string]): int =

  matchLen(inStr.str, regex, matches, inStr.pos)

template tildeImpl(inStr: typed, regex: Regex): untyped {.dirty.} =
  var matches {.inject.}: array[20, string]
  let matchLen {.inject.} = matchLen(inStr, regex, matches)
  matchLen != -1


template `=~`*(inStr: var PosStr, regex: Regex, advance: int = 128): untyped =
  fillNext(advance)
  tildeImpl(inStr, regex)

template `=~`*(inStr: PosStr, regex: Regex): untyped =
  tildeImpl(inStr, regex)

template `=~`*(inStr: PosStr, regex: Rx): untyped =
  # const regexPattern = re(toConstStr(regex))
  const str: string = toConstStr(regex)
  let r = re(str)
  tildeImpl(inStr, r)

proc cut*(str; rx: Rx | Regex, fill: int = 128): string =
  fillNext(str, fill)
  let rx = toRegex(rx)
  let match = matchLen(str, rx)
  str.pushRange()
  str.advance(match)
  result = str.popRange()
