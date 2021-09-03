import std/[
  streams, strscans, strutils, strformat, macros, segfaults,
  sequtils, unicode, strutils, parseutils
]

import
  ../core/[all, code_errors],
  ./halgorithm,
  ../other/[oswrap, hcoverage],
  ../algo/clformat


##[

This module provides implementation of a lexer base. Note that it does not
strive to provide most /efficient/ implementaiton, but rather one that
allows to deal with extremely annoying syntaxes - barely specified
'pretty-printed' outputs from various shell commands, markup langauges of
all sorts and so on.

Multiple helper procedures are provided to deal with majority of common use
cases, which allows for rapid prototyping.

]##

type
  PosStrSlice* = object
    line*: int
    column*: int
    start*: int
    finish*: int

  LineCol* = tuple[line: int, column: int]

  PosStrPoint* = object
    pos, line, column: int

  PosStr* = object
    ## Input character stream, either based on input stream, or indexing in
    ## parts of already existing buffer.
    baseStr*: ref string ## For non-slice string used as buffer. Might
    ## contain full input data (in case of lexing over existing string).
    ## For slice string contains reference to the original string (is not
    ## modified)

    case isSlice*: bool
      of false:
        stream*: Stream ## Input data stream. Might be `nil`, in which case
                        ## new data won't be read in.
        ranges*: seq[tuple[pos, line, column: int]] ## Sequence of starting
        ## position for ranges. When `popRange()` is called end position of
        ## the string (with offset) is used to determine end point.

      of true:
        sliceIdx*: int ## Currently active slice
        slices*: seq[PosStrSlice] ## List of slices in the base string
        fragmentedRanges*: seq[seq[PosStrSlice]] ## Sequence of fragments for
        ## active ranges. When `popRange()` is called end position is used,
        ## identically to the [[code:.ranges]] case. When position is
        ## advanced in string, new fragments might be added to the ranges.

    pos*: int ## Current absolute position in the base/buffer string.
              ## Always points to the current valid character. Calling
              ## [[code:advance()]] changes this position, potentially by
              ## an unlimited amount in case of fragmented string.
    line*: int ## Current line index. Automatically tracked by
               ## [[code:advance()]]
    column*: int ## Current column number
    bufferActive*: bool
    sliceBuffer*: seq[seq[PosStrSlice]] ## Buffer for new positional
    ## slices. Used by [[code:startSlice()]] and [[code:finishSlice()]] to
    ## automatically collect new string slices

  HLexerError* = object of ParseError
    ## Base type for lexer errors
    pos*, line*, column*: int

  UnexpectedCharError* = object of HLexerError
  UnbalancedWrapError* = object of HLexerError
  MalformedTokenError* = object of HLexerError


using str: var PosStr

export strutils

const
  LowerAsciiLetters*  = {'a' .. 'z'}
  HighAsciiLetters*   = {'A' .. 'Z'}
  AsciiLetters*       = LowerAsciiLetters + HighAsciiLetters
  AnyRegularAscii*    = { '\x00' .. '\x7F' }
  ControlChars*       = { '\x00' .. '\x1F', '\x7F' }
  MaybeLetters*       = AsciiLetters + Utf8Any
  IntegerStartChars*  = {'0' .. '9', '-', '+'}
  HexDigitsLow*       = {'a', 'b', 'c', 'd', 'e', 'f'} + Digits
  HexDigitsHigh*      = {'A', 'B', 'C', 'D', 'E', 'F'} + Digits
  HexDigits*          = HexDigitsLow + HexDigitsHigh
  PunctOpenChars*     = {'(', '[', '{', '<'}
  PunctCloseChars*    = {')', ']', '}', '>'}
  PunctSentenceChars* = {',', '.', '?', '!', ';', ':'}
  MathChars*          = { '+', '/', '%', '*', '='}
  PunctChars*         = PunctOpenChars + PunctCloseChars + PunctSentenceChars
  Newline*            = {'\n'}
  AllSpace*           = Whitespace
  HorizontalSpace*    = AllSpace - Newline
  VeritcalSpace*      = Newline
  TextLineChars*      = AllChars - ControlChars + { '\t' }
                        ## Character found in regular text line. All chars
                        ## excluding special controls (newline, line feed,
                        ## carriage return etc.). This does include
                        ## tabulation, because it is not uncommon in
                        ## regular text.

func lineCol*(str: PosStr): LineCol {.inline.} =
  (line: str.line, column: str.column)

func len*(slice: PosStrSlice): int =
  ## Get number of byte characters between end and start
  slice.finish - slice.start

func toAbsolute*(slice: PosStrSlice, offset: int): int =
  ## Get absolute position of the @arg{offset}
  slice.start + offset

func initPosStr*(str: string): PosStr =
  ## Create new string with full buffer and `nil` input stream
  PosStr(baseStr: asRef(str), isSlice: false, column: 0, line: 0)

template varPosStr*(str: string): PosStr =
  var posStr = initPosStr(str)
  posStr

func initPosStr*(stream: Stream): PosStr =
  ## Create new string with empty buffer and non-nil input stream.
  result = PosStr(stream: stream, isSlice: false, column: 0, line: 0)
  new(result.baseStr)

proc initPosStr*(file: AbsFile): PosStr =
  ## Create positional string using new file stream
  initPosStr(newFileStream(file.getStr()))

func initPosStr*(
    str;
    allSlice: bool = false,
    popSlice: bool = true): PosStr =
  ## Pop one layer of slices from slice buffer and create new sub-string
  ## lexer from it.
  var s: seq[PosStrSlice]
  if allSlice:
    for slice in str.sliceBuffer:
      s.add slice

    if popSlice:
      str.sliceBuffer = @[]


  else:
    s.add tern(popSlice, str.sliceBuffer.pop(), str.sliceBuffer.last())

  result = PosStr(
    isSlice: true,
    baseStr: str.baseStr,
    column: s[0].column,
    line: s[0].line,
    slices: s)

  result.pos = result.slices[0].start

func initPosStr*(
    inStr: string | ref string, slices: openarray[Slice[int]]): PosStr =
  ## Initl slice positional string, using @arg{inStr} as base
  result = PosStr(isSlice: true, baseStr: asRef(inStr), column: 0, line: 0)
  for slice in slices:
    result.slices.add PosStrSlice(start: slice.a, finish: slice.b)

  result.pos = result.slices[0].start

func initPosStrView*(str): PosStr =
  ## Create substring with `0 .. high(int)` slice range
  PosStr(
    isSlice: true, baseStr: str.baseStr,
    column: str.column, line: str.line,
    slices: @[PosStrSlice(start: 0, finish: high(int))])


func contains*(slice: PosStrSlice, position: int): bool =
  ## Absolute position is within @arg{slice} start and end
  slice.start <= position and position <= slice.finish

template atom*(input: PosStr; idx: int; c: char): bool =
  ## Check if character at current index is @arg{c}. Used by
  ## [[code:std/parseutils.scanp()]] macro
  input.str[input.pos + idx] == c

template atom*(input: PosStr; idx: int; s: set[char]): bool =
  ## Check if character at current index is in set @arg{s}. Used by
  ## [[code:std/parseutils.scanp()]] macro
  input.str[input.pos + idx] in s

proc hasNxt*(input: PosStr; idx: int): bool =
  ## Check if input string has at least @arg{idx} more characters left.
  let pos = input.pos + idx
  if input.isSlice:
    result = input.sliceIdx < input.slices.high or (
      input.sliceIdx < input.slices.len and
      pos <= input.slices[input.sliceIdx].finish and
      0 <= pos and
      pos < input.baseStr[].len)

  else:
    return notNil(input.baseStr) and 0 <= pos and pos < input.baseStr[].len

proc finished*(str: PosStr): bool =
  ## Check if string as no more input data
  not str.hasNxt(0) and (
    str.isSlice or
    isNil(str.stream) or
    str.stream.atEnd())

proc atStart*(str: PosStr): bool = str.pos == 0
proc beforeEnd*(str: PosStr): bool =
  str.hasNxt(0) and not str.hasNxt(1)

proc `?`*(str: PosStr): bool =
  ## Shorthand for src_nim{not str.finished()}
  not str.finished()

template nxt*(input: var PosStr; idx, step: int = 1) =
  inc(idx, step)
  inc(input.pos, step)


macro scanpTemp*(str: typed, pattern: varargs[untyped]): untyped =
  result = nnkStmtList.newTree()
  let tmp = genSym(nskVar, "tmp")
  result.add newVarStmt(tmp, newLit(0))
  result.add newCall(bindSym"scanp", str, tmp)
  for patt in pattern:
    result[^1].add patt

proc fillNext*(str; chars: int) =
  ## Read necessary amount of data from stream to make at least `chars`
  ## lookahead available
  if str.isSlice:
    return

  if isNil(str.stream):
    return

  let needed = chars - (str.baseStr[].len - str.pos - 1)
  if needed > 0:
    str.baseStr[] &= str.stream.readStr(needed)

proc resetBuffer*(str) =
  ## Reset buffer position
  str.pos = 0
  str.baseStr[].setLen(0)

proc `[]`*(str; idx: int = 0): char {.inline.} =
  fillNext(str, idx)
  if not hasNxt(str, idx):
    return '\x00'

  else:
    return str.baseStr[][str.pos + idx]

proc runeAt*(str; idx: int = 0): Rune =
  fillNext(str, idx)
  if not hasNxt(str, idx):
    return Rune(0)

  else:
    if str.isSlice:
      let len = str.baseStr[].graphemeLen(str.pos + idx)
      fillNext(str, len)
      var i = str.pos + idx
      fastRuneAt(str.baseStr[], i, result)

    else:
      let len = str.baseStr[].graphemeLen(str.pos + idx)
      fillNext(str, len)

      var i = str.pos + idx
      fastRuneAt(str.baseStr[], i, result)

proc setLineInfo*(error: ref HLexerError, str: PosStr) =
  error.column = str.column
  error.line = str.line
  error.pos = str.pos

proc newUnexpectedCharError*(
    str;
    expected: string = "",
    parsing: string = ""
  ): ref UnexpectedCharError =

  new(result)
  result.setLineInfo(str)
  var e = "Unexpected character encoutered during lexing - found '"
  if ?str:
    if str[0] in AnyRegularAscii:
      e.add describeChar(str[0])

    else:
      e.add describeChar(str.runeAt(0))

  else:
    e.add "EOF (string finished)"

  if expected.len > 0:
    e.add "', but expected - '"
    e.add expected
    e.add "'"

  else:
    e.add "'"

  if parsing.len > 0:
    e.add " while parsing "
    e.add parsing

  e.add " at " & $str.line & ":" & $str.column
  if notNil(str.baseStr) and str.pos < str.baseStr[].len:
    e.add ". Lookahead characters (from pos = "
    e.add $str.pos
    e.add ") ['"
    for ch in str.pos .. min(str.baseStr[].high, str.pos + 10):
      if ch > str.pos:
        e.add "', '"
      e.add describeChar(
        str.baseStr[][ch], hdisplay(verbosity = dvMinimal))

    e.add "'])"

  result.msg = e

proc `[]`*(str; slice: HSlice[int, BackwardsIndex]): string {.inline.} =
  var next = 0
  while str.hasNxt(next):
    fillNext(str, next)
    inc next

  result = str.baseStr[str.pos + slice.a .. (str.pos + next - slice.b.int)]

proc `[]`*(str; slice: HSlice[int, int]): string {.inline.} =
  fillNext(str, max(slice.a, slice.b))
  if str.baseStr[].len == 0:
    result = "<empty>"

  else:
    result = str.baseStr[
      min(str.baseStr[].high, str.pos + slice.a) ..
      min(str.baseStr[].high, str.pos + slice.b)
    ]

    if slice.b > str.baseStr[].high:
      result &= "\\0".repeat(slice.b - str.baseStr[].high)

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
  for ch in str.baseStr[str.pos .. ^1]:
    result.add ch


# proc `[]`*(str; slice: Slice[int]): string =
#   for i in slice:
#     result.add str[i]

proc `[]`*(str; slice: HSlice[int, char]): string =
  var pos = slice.a
  while not str[pos, slice.b] and hasNxt(str, pos):
    result.add str[pos]
    inc pos

proc `@`*(str; slice: Slice[int]): seq[char] =
  var pos = slice.a
  while hasNxt(str, pos) and pos < slice.b:
    result.add str[pos]
    inc pos


proc `$`*(slice: PosStrSlice): string = &"{slice.start}..{slice.finish}"
proc `[]`*(str; slice: PosStrSlice): string =
  str.baseStr[][
    max(slice.start, 0) .. min(slice.finish, str.baseStr[].high)]

proc sliceStrings*(str): seq[string] =
  for slice in str.slices:
    result.add str[slice]

proc lineAround*(str; pos: int): tuple[line: string, pos: int] =
  var start = pos
  while start > 0 and str.baseStr[][start] notin {'\n'}:
    dec start

  if str.baseStr[][start] in {'\n'}:
    inc start

  result.pos = pos - start

  while start < str.baseStr[].len and
        str.baseStr[][start] notin {'\n'}:
    result.line.add str.baseStr[][start]
    inc start

proc hshow*(
    slice: PosStrSlice, opts: HDisplayOpts = defaultHDisplay): ColoredText =

  hshow(slice.start) & ".." & hshow(slice.finish)

proc hshow*(str; opts: HDIsplayOpts = defaultHDisplay): ColoredText =
  assertRef str.baseStr
  if str.isSlice:
    result.add "["
    for sliceIdx in str.sliceIdx ..< str.slices.len:
      let slice = str.slices[sliceIdx]
      var text =
        if sliceIdx == str.sliceIdx:
          str.baseStr[][str.pos .. min(slice.finish, str.baseStr[].high)]

        else:
          str[slice]

      # if text.len == 0:
      #   continue

      if sliceIdx == str.sliceIdx:
        result.add toRed(&"{sliceIdx}@{str.pos}")

      result.add &"[{hshow(slice)}: {hshow(text)}]"

    result.add "]"

  else:
    result.add "["
    result.add $str.pos
    result.add ": ["
    result.add hshow(str.baseStr[][clamp(str.pos, 0, str.baseStr[].high) .. ^1])
    result.add "]]"


proc `$`*(str): string = $hshow(str)

func add*(str: var PosStr, other: PosStr) =
  assertArg str, str.isSlice
  assertArg other, other.isSlice

  for slice in other.slices:
    if str.slices.len == 0 or
       str.slices.last().finish + 1 < slice.start:
      str.slices.add slice

    else:
      str.slices.last().finish = slice.finish


func concat*(strs: seq[PosStr]): PosStr =
  result = PosStr(
    isSlice: true,
    baseStr: strs[0].baseStr,
    column: strs[0].column,
    line: strs[0].line)

  for str in strs:
    if str.isSlice:
      result.add str

    else:
      raise newArgumentError(
        "Input contains a non-slice strings - cannot be correctly concatenated")



template assertAhead*(str: PosStr, ahead: string) =
  if not str[ahead]:
    raise (ref HLexerError)(
      msg: "Lexer error - expected : '" & ahead & "', but found " &
        str[0 .. len(ahead)] & " at " & $str.line & ":" & $str.column,
      column: str.column,
      line: str.line,
      pos: str.pos
    )


func posStrSlice*(
    a, b: int, line: int, column: int): PosStrSlice {.inline.} =
  PosStrSlice(start: a, finish: b, line: line, column: column)

proc pushRange*(str) {.inline.} =
  if str.isSlice:
    str.fragmentedRanges.add @[posStrSlice(
      str.pos, str.pos, str.line, str.column)]

  else:
    str.ranges.add(str.pos, str.line, str.column)

proc startSlice*(str) {.inline.} =
  ## Start new slice in the string slice buffer.
  str.sliceBuffer.add @[
    PosStrSlice(start: str.pos, line: str.line, column: str.column)]

proc finishSlice*(str; rightShift: int = -1) {.inline.} =
  assertHasIdx str.sliceBuffer, 0,
     "No slices started - use `pushSlice()` to initialize slice buffer"

  assertHasIdx str.sliceBuffer[^1], 0,
     "Internal error - last element in the slice buffer is empty, this " &
       "should not happen", LogicError

  str.sliceBuffer[^1][^1].finish = min(
    str.pos + rightShift, str.baseStr[].high)

proc finishAllSlice*(str; rightShift: int = -1) {.inline.} =
  for slice in mitems(str.sliceBuffer):
    slice.last().finish = min(str.pos + rightShift, str.baseStr[].high)

proc pushSlice*(str) = startSlice(str)

proc popSlice*(str; rightShift: int = -1): PosStr =
  finishSlice(str, rightShift)
  return initPosStr(str)

template asSlice*(
    bufStr: PosStr, expr: untyped; rightShift: int = -1): untyped =
  bufStr.startSlice()
  expr
  bufStr.popSlice(rightShift)


proc peekSlice*(str; rightShift: int = -1): PosStr =
  finishSlice(str, rightShift)
  return initPosStr(str, popSlice = false)

proc sliceBetween*(str; start, finish: PosStrPoint): PosStr =
  result = PosStr(
    isSlice: true, line: start.line,
    column: start.column,
    pos: start.pos,
    baseStr: str.baseStr)

  if str.isSlice:
    raise newImplementError()


  else:
    result.slices.add posStrSlice(
      start.pos, finish.pos, start.line, start.column)


proc toggleBuffer*(str; activate: bool = not str.bufferActive) {.inline.} =
  str.bufferActive = activate



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
      let slice =
        if idx == ranges.high:
          posStrSlice(
            fragment.start, fragment.finish + rightShift,
            fragment.line,
            fragment.column)

        else:
          fragment

      yield slice


  else:
    let (start, line, column) = tern(doPop, str.ranges.pop, str.ranges.last())
    let finish = str.pos

    yield posStrSlice(
      start + leftshift,
      min(finish, str.baseStr[].len) + rightShift,
      line,
      column)

proc popRangeIndices*(
    str; leftShift: int = 0, rightShift: int = -1): seq[PosStrSlice] =

  for slice in topRangeIndices(str, leftShift, rightShift):
    result.add slice

proc getRangeIndices*(
    str; leftShift: int = 0, rightShift: int = -1): seq[PosStrSlice] =

  for slice in topRangeIndices(str, leftShift, rightShift, doPop = false):
    result.add slice


proc getAll*(str: PosStr): string =
  assertRef str.baseStr
  if str.isSlice:
    for slice in str.slices:
      result.add str.baseStr[][slice]

  else:
    result = str.baseStr[]

proc strVal*(str: PosStr): string = getAll(str)
proc strValNorm*(str: PosStr): string = getAll(str).normalize()


proc newMalformedTokenError*(
    got: PosStr, expected: string): ref MalformedTokenError =
  new(result)
  result.setLineInfo(got)
  result.msg.add(
    "Malformed token encountered during lexing - found '",
    got.strVal(),
    "', but expected ",
    expected,
    " at ",
    $got.line,
    ":",
    $got.column)



proc getRange*(str; leftShift: int = 0, rightShift: int = -1):
  string {.inline.} =

  for slice in topRangeIndices(
      str, leftShift, rightShift, doPop = false):

    if str.isSlice:
      result.add str.baseStr[][slice]

    else:

      result.add str.baseStr[slice]

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

      result.add str.baseStr[slice]

template asRange*(str: PosStr, expr: untyped): untyped =
  str.pushRange()
  expr
  str.popRange()


proc next*(
    str; step: int = 1, byteAdvance: bool = false) =

  if step < 0:
    for diff in 0 ..< -step:
      var byteCount = 1
      if not byteAdvance:
        var pos = str.pos

        if str.baseStr[][pos] in Utf8Continuations:
          dec byteCount

        elif str.baseStr[][pos] in Utf8Starts:
          # At the start of utf8 rune, need to advance backwards to the
          # previous one
          dec pos

        while str.baseStr[][pos] in Utf8Continuations:
          # Search backwards until start of the utf is not found
          inc byteCount
          dec pos


      if str.baseStr[][str.pos] == '\n':
        dec str.line
        # TODO correct column number (scan backwards until next newline/SOF)

      else:
        dec str.column

      if str.isSlice:
        if (str.pos - byteCount) < str.slices[str.sliceIdx].start:
          dec str.pos, byteCount
          dec str.sliceIdx

        else:
          dec str.pos, byteCount

      else:
        dec str.pos, byteCount



  else:
    for diff in 0 ..< step:
      let byteCount =
        if byteAdvance:
          1

        else:
          graphemeLen(str.baseStr[], str.pos)

      if str['\n']:
        inc str.line
        str.column = 0

      else:
        inc str.column

      if str.isSlice:
        if str.pos < str.slices[str.sliceIdx].finish:
          inc(str.pos, byteCount)
          for fragment in mitems(str.fragmentedRanges):
            fragment.last().finish = str.pos


        else:
          var current = str.pos

          inc str.sliceIdx
          if str.sliceIdx < str.slices.len:
            str.pos = str.slices[str.sliceIdx].start

          else:
            inc current
            inc str.pos

          for fragment in mitems(str.fragmentedRanges):
            if fragment.len > 0:
              fragment.last().finish = current
              if str.sliceIdx < str.slices.len:
                fragment.add posStrSlice(
                  str.pos,
                  str.pos,
                  str.line,
                  str.column)

      else:
        inc(str.pos, byteCount)


proc advance*(str; step: int = 1, byteAdvance: bool = false)
  {.deprecated: "Alias for `next`".} =
  next(str, step, byteAdvance)

proc back*(str; step: int = 1) =
  next(str, step * -1)

proc getPos*(str: PosStr, offset: int = 0): PosStrPoint =
  ## - TODO should return position, line, column as well (just save whole
  ##   positional string state at once to simplify restoration)
  PosStrPoint(
    pos: str.pos + offset,
    line: str.line, column: str.column + offset)

proc setPos*(str; pos: PosStrPoint) =
  ## - TODO backtrack over slice indices, raise exception if values
  ##   is out of range etc.
  str.pos = pos.pos

proc popPointSlice*(
    str;
    expected: set[char] = AllChars,
    advance: int = 1
  ): PosStr =

  if str[] notin expected:
    raise newUnexpectedCharError(str, expected.describeCharset())

  else:
    str.startSlice()
    str.next(advance)
    return str.popSlice()

proc skip*(str; ch: char) {.inline.} =
  if str[] != ch:
   raise newUnexpectedCharError(str, $ch)
  str.next()

proc skip*(str; ch: set[char]) {.inline.} =
  if str[] notin ch:
   raise newUnexpectedCharError(str, $ch)
  str.next()


proc skip*(str; ch1, ch2: set[char]) {.inline.} =
  str.skip(ch1)
  str.skip(ch2)

proc skipBack*(str; ch: set[char]) {.inline.} =
  if str[] notin ch:
   raise newUnexpectedCharError(str, $ch)
  str.next(-1)

proc peek*(str; ch: set[char]) =
  if str[] notin ch:
   raise newUnexpectedCharError(str, describeCharset(ch))

proc trySkip*(str; s: string): bool =
  if str[s]:
    str.next(s.len)
    result = true

  else:
    result = false


proc trySkip*(str; ch: set[char]): bool  =
  if str[] in ch:
    result = false

  else:
    str.next()
    result = true

proc trySkip*(str; ch: char): bool  =
  if str[] != ch:
    result = false

  else:
    str.next()
    result = true

proc skipWhile*(str; chars: set[char]) {.inline.} =
  if str[chars]:
    while str[chars]:
      str.next()

proc skipUntil*(str; chars: set[char], including: bool = false) {.inline.} =
  var changed = false
  while str[AllChars - chars]:
    str.next()
    changed = true

  if changed and including and ?str:
    str.next()

proc skipToEOL*(str; including = true) =
  ## Skip to the end of current line. After parsing cursor is positioned on
  ## the last character in the string, or closes newline.
  str.skipUntil(Newline, including = including)

proc goToEof*(
    str; byteAdvance: bool = false; rightShift: int = 0) =

  if str.isSlice:
    let s = str.slices.last()
    str.pos = s.finish + rightShift
    str.line = s.line
    str.column = s.column + (s.finish -  s.start) + rightShift
    if not byteAdvance:
      while str.baseStr[][str.pos] in Utf8Continuations:
        dec str.pos

  else:
    for rune in runes(str.baseStr[][str.pos .. ^1]):
      if rune == Rune(10):
        inc str.line
        str.column = 0

      else:
        inc str.column

    dec str.column
    str.pos = str.baseStr[].high
    if not byteAdvance:
      while str.baseStr[][str.pos] in Utf8Continuations:
        dec str.pos

proc gotoSof*(str; byteAdvance: bool = false) =
  if str.isSlice:
    let s = str.slices.first()
    str.pos = s.start
    str.line = s.line
    str.column = s.column

  else:
    str.line = 0
    str.column = 0
    str.pos = 0

proc skipToNewline*(str) =
  ## Skip until end of the current line is found. After parsing cursor is
  ## positioned on the last character in the strign, or *before* closest
  ## newline.
  str.skipUntil(Newline, including = false)

proc skipIndent*(str; maxIndent = high(int)): int =
  while str[HorizontalSpace]:
    inc result
    str.next()
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
  str.next()

proc popChar*(str: var PosStr): char {.inline.} =
  result = str[]
  str.next()

proc skipStringLit*(str: var PosStr) =
  var found = false
  str.next()
  while not found:
    found = str['"'] and not str[-1, '\\']
    str.next()


proc popStringLit*(str: var PosStr): string {.inline.} =
  str.pushRange()
  str.skipStringLit()
  result = str.popRange()
  # str.next()


proc popDigit*(str: var PosStr): string {.inline.} =
  str.pushRange()
  if str["0x"]:
    str.next(2)
    str.skipWhile(HexDigits + {'-'})

  if str["0b"]:
    str.next(2)
    str.skipWhile({'0', '1'})

  else:
    str.skipWhile(Digits + {'-', '.'})

  return str.popRange()

proc popIdent*(str; chars: set[char] = IdentChars):
  string {.inline.} = str.popWhile(chars)

proc popWhileSlice*(str; chars: set[char]): PosStr =
  str.startSlice()
  str.skipWhile(chars)
  str.popSlice()

proc popUntilSlice*(str; chars: set[char], including: bool = false): PosStr =
  str.startSlice()
  str.skipUntil(chars, including)
  str.popSlice()

proc popIdentSlice*(str; chars: set[char] = IdentChars): PosStr =
  str.startSlice()
  str.skipWhile(chars)
  str.popSlice()

proc skipBalancedSlice*(
    str; openChars, closeChars: set[char],
    endChars: set[char] = Newline,
    doRaise: bool = true) =

  var
    fullCount = 0
    count: array[char, int]

  template unbalanced() {.dirty.} =
    var err: ref UnbalancedWrapError
    new(err)
    err.setLineInfo(str)
    err.msg.add "Unbalanced wrap"
    raise err

  while ?str:
    if str[] in openChars:
      inc fullCount
      inc count[str.pop()]

    elif str[] in closeChars:
      dec fullCount
      dec count[str.pop()]

      if fullCount == 0:
        return

    elif str[] in endChars:
      if fullCount > 0:
        if doRaise:
          unbalanced()

        else:
          return

      else:
        return

    else:
      str.next()

  if fullcount > 0 and doRaise:
    unbalanced()


proc popBalancedSlice*(
    str; openChars, closeChars: set[char],
    endChars: set[char] = Newline,
    doRaise: bool = true
  ): PosStr {.hcov.} =

  str.startSlice()
  skipBalancedSlice(str, openChars, closeChars, endChars, doRaise)
  return str.popSlice()



proc popNext*(str; count: int): string {.inline.} =
  str.pushRange()
  str.next(count)
  return str.popRange()

proc popBacktickIdent*(str): string {.inline.} =
  if ?str and str[] == '`':
    str.next()
    result = str.popUntil({'`'})
    str.next()

  else:
    result = str.popIdent()

proc readLine*(str; skipNl: bool = true): string =
  while not str['\n']:
    if not str.finished():
      result.add str.pop()

    else:
      return

  if skipNl:
    str.next()

  else:
    result.add str.pop()

proc skipLine*(str) =
  while not str['\n'] and not str.finished(): str.next()
  str.next()

import std/re
import ../other/rx


proc matchLen*(inStr: PosStr, regex: Regex): int =
  matchLen(inStr.baseStr[], regex, inStr.pos)

proc matchLen*(
    inStr: PosStr, regex: Regex, matches: var openarray[string]): int =
  matchLen(inStr.baseStr[], regex, matches, inStr.pos)

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
  str.next(match)
  result = str.popRange()

macro scanSlice*(str; pattern: varargs[untyped]): untyped =
  # - TODO :: `@">>>"` to skip until `>>>` is found
  result = newStmtList()
  let str = copyNimTree(str)

  result.add newCall("pushSlice", str)

  proc splitPattern(part: NimNode): NimNode =
    case part.kind:
      of nnkPrefix:
        case part[0].strVal():
          of r"*\":
            return nnkPrefix.newTree(
              ident"*", nnkPrefix.newTree(ident"\", part[1]))

          of "+", "*", "-", "\\", "?", "@":
            return part

          else:
            raise newImplementKindError(part[0].strVal())


      else:
        return part

  proc toCharGroup(name: string): NimNode =
    case name:
      of "n": bindSym"Newline"
      of "N": nnkInfix.newTree(ident"-", bindSym"AllChars", bindSym"Newline")
      of "Id": bindSym"IdentChars"
      of "Hex": bindSym"HexDigits"
      of "s": bindSym"HorizontalSpace"

      else:
        raise newImplementKindError(name)


  proc isAt(part: NimNode): NimNode =
    var at: NimNode
    case part.kind:
      of nnkCharLit, nnkStrLit, nnkIdent: at = part
      of nnkPrefix:
        case part[0].strVal():
          of "\\": at = toCharGroup(part[1].strVal())
          else: raise newImplementKindError(part[0].strVal())

      of nnkCurly:
        # TODO check for unquoted `\n` special characters
        at = part

      else:
        raise newImplementKindError(
          part, "Cannot generate 'is at' check")

    return nnkBracketExpr.newTree(str, at)

  proc genSkip(part: NimNode, requires: bool = false): NimNode =
    let part = splitPattern(part)
    case part.kind:
      of nnkCharLit, nnkStrLit, nnkCurly, nnkIdent:
        result = newCall(tern(requires, "skip", "trySkip"), str, part)

      of nnkPrefix:
        case part[0].strVal():
          of "*":
            result = nnkWhileStmt.newTree(isAt(part[1]), newCall("next", str))

          of "\\":
            assertNodeKind(part[1], {nnkIdent, nnkSym})
            result = newCall(
              tern(requires, "skip", "trySkip"),
              str, toCharGroup(part[1].strVal()))

          of "@":
            let at = isAt(part[1])
            result = quote do:
              while not `at`:
                next(`str`)

          else:
            raise newImplementKindError(part[0].strVal())

      else:
        raise newImplementKindError(part)


  for part in pattern:
    result.add genSkip(part, true)

  result.add newCall("popSlice", str)

  # echo result.repr()
