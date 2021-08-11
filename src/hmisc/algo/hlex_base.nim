import std/[
  streams, strscans, strutils, strformat, macros, segfaults,
  sequtils
]

import
  ../core/[all, code_errors],
  ./halgorithm,
  ../other/oswrap,
  ../algo/clformat


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
        ranges: seq[int]

      of true:
        sliceIdx*: int
        baseStr*: ptr string
        slices*: seq[PosStrSlice]
        fragmentedRanges: seq[seq[Slice[int]]]

    pos*: int
    line*: int
    column*: int
    bufferActive*: bool
    sliceBuffer*: seq[seq[PosStrSlice]]


  HLexerError* = ref object of ParseError
    pos*, line*, column*: int

  UnexpectedCharError = object of HLexerError


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
  MathChars* = { '+', '/', '%', '*', '='}
  PunctChars* = PunctOpenChars + PunctCloseChars + PunctSentenceChars
  Newline* = {'\n'}
  AllSpace* = Whitespace
  HorizontalSpace* = AllSpace - Newline
  VeritcalSpace* = Newline



# template raiseUnexpectedChar*(str: PosStr) =
#   raise HLexerError(
#     msg: ,
#     column: str.column, line: str.line, pos: str.pos)



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

proc initPosStr*(file: AbsFile): PosStr =
  PosStr(stream: newFileStream(file.getStr()), isSlice: false)

func initPosStr*(str): PosStr =
  ## Pop one layer of slices from slice buffer and create new sub-string
  ## lexer from it.
  result = PosStr(
    isSlice: true, baseStr: addr str.str, slices: str.sliceBuffer.pop)

  result.pos = result.slices[0].start

func initPosStr*(inStr: ptr string, slices: openarray[Slice[int]]): PosStr =
  result = PosStr(isSlice: true, baseStr: inStr)
  for slice in slices:
    result.slices.add PosStrSlice(start: slice.a, finish: slice.b)

  result.pos = result.slices[0].start

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
    result = input.sliceIdx < input.slices.high or (
      input.sliceIdx < input.slices.len and
      pos <= input.slices[input.sliceIdx].finish and
      pos < input.baseStr[].len)

    echov input.baseStr[][pos], input.pos, result
    echov input.sliceIdx


  else:
    return pos < input.str.len

proc finished*(str: PosStr): bool =
  not str.hasNxt(0) and (
    str.isSlice or
    isNil(str.stream) or
    str.stream.atEnd()
  )

proc `?`*(str: PosStr): bool = not str.finished()

# proc newCannotGetOffset*(str: PosStr, offset: int): ref GetterError =



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
    raise newGetterError("Cannot get char at +",
      idx, " input string is finished: ",
      finished(str))
    # raiseArgumentError(&"Cannot get char at [+{idx}]")

  #   let idx = str.pos + idx
  #   var sliceStart = 0
  #   for slice in str.slices[str.sliceIdx .. ^1]:
  #     if sliceStart <= idx and
  #        idx <= sliceStart + slice.len:
  #       return str.baseStr[][slice.toAbsolute(idx - sliceStart)]

  #     else:
  #       sliceStart += slice.len

  #   raise newArgumentError(&"Cannot get char at [+{idx}]")
  if str.isSlice:
    return str.baseStr[][str.pos + idx]

  else:
    return str.str[str.pos + idx]

proc newUnexpectedCharError*(
    str; expected: string = ""): ref UnexpectedCharError =
  result = (ref UnexpectedCharError)(
    column: str.column, line: str.line, pos: str.pos)

  result.msg.add "Unexpected character encoutered during lexing - found '"
  result.msg.add $str[0]

  if expected.len > 0:
    result.msg.add "', but expected - '"
    result.msg.add expected
    result.msg.add "'"

  else:
    result.msg.add "'"

  result.msg.add " at " & $str.line & ":" & $str.column

  # result.msg.add "'"
  #   msg: "Unexpected character encountered while parsing. '" &  &
  #     )


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
    max(slice.start, 0) ..< min(slice.finish, str.baseStr[].len)]

proc sliceStrings*(str): seq[string] =
  for slice in str.slices:
    result.add str[slice]

proc lineAround*(str; pos: int): tuple[line: string, pos: int] =
  var start = pos
  while start > 0 and str.str[start] notin {'\n'}:
    dec start

  result.pos = pos - start

  while start < str.str.len and str.str[start] notin {'\n'}:
    result.line.add str.str[start]
    inc start

proc hshow*(
    slice: PosStrSlice, opts: HDisplayOpts = defaultHDisplay): ColoredText =

  hshow(slice.start) & ".." & hshow(slice.finish)

proc hshow*(str; opts: HDIsplayOpts = defaultHDisplay): ColoredText =
  if str.isSlice:
    result.add "["
    for sliceIdx in str.sliceIdx ..< str.slices.len:
      let slice = str.slices[sliceIdx]
      var text =
        if sliceIdx == str.sliceIdx:
          str.baseStr[][str.pos ..< min(slice.finish, str.baseStr[].len)]

        else:
          str[slice]

      # if text.len == 0:
      #   continue

      if sliceIdx == str.sliceIdx:
        result.add toRed(&"{sliceIdx}@{str.pos}")

      result.add &"[{hshow(slice)}: {hshow(text)}]"

    result.add "]"

  else:
    result.add &"[{str.pos}: {hshow(str.str[str.pos .. ^1])}]"


proc `$`*(str): string = $hshow(str)

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
  if str.isSlice:
    str.fragmentedRanges.add @[str.pos .. str.pos]

  else:
    str.ranges.add str.pos

proc startSlice*(str) {.inline.} =
  ## Start new slice in the string slice buffer.
  if str.sliceBuffer.len == 0:
    str.sliceBuffer.add @[]

  str.sliceBuffer[^1].add PosStrSlice(start: str.pos)

proc finishSlice*(str) {.inline.} =
  str.sliceBuffer[^1][^1].finish = str.pos

proc toggleBuffer*(str; activate: bool = not str.bufferActive) {.inline.} =
  str.bufferActive = activate


func posStrSlice*(a, b: int): PosStrSlice {.inline.} =
  PosStrSlice(start: a, finish: b)

func `[]`*(str: string, slice: PosStrSlice): string =
  str[slice.start .. slice.finish]

func `==`*(posSlice: PosStrSlice, slice: Slice[int]): bool =
  posSlice.start == slice.a and posSlice.finish == slice.b

func `==`*(slice: Slice[int], posSlice: PosStrSlice): bool =
  posSlice == slice

func `==`*(posSlice: seq[PosStrSlice], slice: seq[Slice[int]]): bool =
  if posSlice.len == slice.len:
    result = true
    for (lhs, rhs) in zip(posSlice, slice):
      if lhs != rhs:
        return false

func `==`*(slice: seq[Slice[int]], posSlice: seq[PosStrSlice]): bool =
  posSlice == slice

iterator topRangeIndices*(
    str;
    leftShift: int = 0, rightShift: int = -1,
    doPop: bool = true
  ): PosStrSlice =

  if str.isSlice:
    var ranges = tern(
      doPop,
      str.fragmentedRanges.pop(),
      str.fragmentedRanges.last())

    let baseHigh = str.baseStr[].high

    for idx, fragment in ranges:
      let slice = posStrSlice(fragment.a, min(fragment.b, baseHigh))
      yield slice


  else:
    let start = tern(doPop, str.ranges.pop, str.ranges.last()) + leftShift
    let finish = str.pos

    yield posStrSlice(start, min(finish, str.str.len) + rightShift)

proc popRangeIndices*(
    str; leftShift: int = 0, rightShift: int = -1): seq[PosStrSlice] =

  for slice in topRangeIndices(str, leftShift, rightShift):
    result.add slice

proc getRangeIndices*(
    str; leftShift: int = 0, rightShift: int = -1): seq[PosStrSlice] =

  for slice in topRangeIndices(str, leftShift, rightShift, doPop = false):
    result.add slice


proc getRange*(str; leftShift: int = 0, rightShift: int = -1):
  string {.inline.} =

  for slice in topRangeIndices(
      str, leftShift, rightShift, doPop = false):

    if str.isSlice:
      result.add str.baseStr[][slice]

    else:

      result.add str.str[slice]

proc popRange*(str; leftShift: int = 0, rightShift: int = -1):
  string {.inline.} =

  ## Pop current list of ranges and return combined string.
  ##
  ## - @arg{leftShift} :: Offset from the start range position
  ## - @arg{rightShift} :: Offset from the end range position.
  ##   Offset would be /added/ to the current string position.
  ##   Default value is @val{-1} to simplfy common use case -
  ##   skip while some condition is satisfied, then pop resulting
  ##   range. If this is performed using `while cond(str)`, pop
  ##   of the range would occur after string is out of the range,

  for slice in topRangeIndices(str, leftShift, rightShift):
    if str.isSlice:
      result.add str.baseStr[][slice]

    else:

      result.add str.str[slice]


proc advance*(str; step: int = 1) {.inline.} =
  if str['\n']:
    inc str.line
    str.column = 0
  else:
    inc str.column

  if str.isSlice:
    if str.pos < str.slices[str.sliceIdx].finish:
      for fragment in mitems(str.fragmentedRanges):
        fragment.last().b = str.pos

      inc(str.pos, step)

    else:
      # echov str.pos
      # echov str.fragmentedRanges
      let current = str.pos

      inc str.sliceIdx
      if str.sliceIdx < str.slices.len:
        str.pos = str.slices[str.sliceIdx].start

      for fragment in mitems(str.fragmentedRanges):
        if fragment.len > 0:
          fragment.last().b = current
          if str.sliceIdx < str.slices.len:
            fragment.add @[str.pos .. str.pos]

      # echov str.fragmentedRanges


  else:
    inc(str.pos, step)

proc skip*(str; ch: char) {.inline.} =
  if str[] != ch:
   raise newUnexpectedCharError(str, $ch)
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

proc skipSpace*(str) {.inline.} =
  str.skipWhile(HorizontalSpace)

proc space*(str) {.inline.} =
  str.skipWhile(HorizontalSpace)

proc popWhile*(str; chars: set[char]): string {.inline.} =
  str.pushRange()
  str.skipWhile(chars)
  return str.popRange()

proc popUntil*(str; chars: set[char] | char): string {.inline.} =
  str.pushRange()

  when chars is system.set:
    str.skipUntil(chars)
  else:
    str.skipUntil({chars})

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

  if str["0b"]:
    str.advance(2)
    str.skipWhile({'0', '1'})

  else:
    str.skipWhile(Digits + {'-', '.'})

  return str.popRange()

proc popIdent*(str; chars: set[char] = IdentChars):
  string {.inline.} = str.popWhile(chars)


proc popNext*(str; count: int): string {.inline.} =
  str.pushRange()
  str.advance(count)
  return str.popRange()

proc popBacktickIdent*(str): string {.inline.} =
  if ?str and str[] == '`':
    str.advance()
    result = str.popUntil({'`'})
    str.advance()

  else:
    result = str.popIdent()

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
