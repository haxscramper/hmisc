import std/[sequtils, sugar, math, algorithm, strutils, unicode, options]
import ../types/colorstring
import halgorithm

## https://xxyxyz.org/line-breaking/


# TODO add support for inserting more than once whitespace between
# lines.

# TODO support 'unwrappable' words - things like code, that cannot be
# wrapped or hyphenated in any way.

# TODO wrap sequence of words using hyphenation.

type
  WordKind* = enum
    wkText
    wkNewline
    wkPunctuation
    wkSpace
    wkMarkup

  Word*[Text, Attr] = object
    text*: Text
    attr*: Attr
    forceNl: bool
    isBreaking: bool
    kind*: WordKind

  TermWord* = Word[string, PrintStyling]
  TextWord*[T] = Word[string, T]
  UnicodeWord*[T] = Word[seq[Rune], T]

  MarkStyling = object
    wrap: char


  OrgStyling = object
    mark: char

  MarkWord = Word[string, MarkStyling]
  OrgWord = Word[string, OrgStyling]

func initTextWord*[A](
    text: string, attr: A, kind: WordKind = wkText): TextWord[A] =
  TextWord[A](text: text, attr: attr, kind: kind)

func isMarkup*[T, A](w: Word[T, A]): bool = w.kind == wkMarkup

func splitMark*(str: string): seq[MarkWord] =
  for word in str.split(' '):
    result.add MarkWord(
      text: word, attr: MarkStyling(wrap: word[0]))

func splitText*[T](str: string): seq[TextWord[T]] =
  for text in str.splitTokenize({',', '.', ' ', '?', '!'}):
    case text:
      of ",", ".", "?", "!":
        result.add TextWord[T](text: text, kind: wkPunctuation)
      of " ":
        result.add TextWord[T](text: text, kind: wkSpace)
      else:
        result.add TextWord[T](text: text, kind: wkText)
  # str.split(" ").mapIt(TextWord[T](text: it))

func len*[T, A](w: Word[T, A]): int = len(w.text)
func `**`(a, b: SomeNumber): SomeNumber = a ^ b


func wrapTextImpl[T, A](
  words: seq[Word[T, A]], width: int): seq[seq[Word[T, A]]] =
  let
    words = words.filterIt(it.kind notin {wkNewline})
    count = len(words)
  var offsets = @[0]

  for w in words:
    offsets.add(offsets[^1] + len(w))

  var minima: seq[int] = @[0] & repeat(10 ** 12, count) # repeat(0, count + 1) #  .concat()
  var breaks: seq[int] = repeat(0, count + 1) # @[0].repeat(count + 1).concat()

  proc cost(i, j: int): auto =
    let w = offsets[j] - offsets[i] + j - i - 1
    if w > width:
      return (10 ^ 10) * (w - width)

    return minima[i] + (width - w) ** 2

  proc smawk(rows, columns: seq[int]) =
    var
      stack: seq[int]
      i = 0

    var rows = rows
    var columns = columns

    while i < len(rows):
      if stack.len > 0:
        let c = columns[len(stack) - 1]
        if cost(stack[^1], c) < cost(rows[i], c):
          if len(stack) < len(columns):
            stack.add(rows[i])
          i += 1

        else:
          discard stack.pop()

      else:
        stack.add(rows[i])
        i += 1

    rows = stack

    if len(columns) > 1:
      smawk(rows) do:
        collect(newSeq):
          for idx, it in columns:
            if idx mod 2 == 1:
              it

    i = 0

    var j = 0

    while j < len(columns):
      let finish =
        if j + 1 < len(columns):
          breaks[columns[j + 1]]
        else:
          rows[^1]

      let c = cost(rows[i], columns[j])
      if c < minima[columns[j]]:
        minima[columns[j]] = c
        breaks[columns[j]] = rows[i]
      if rows[i] < finish:
        i += 1
      else:
        j += 2

  var
    n = count + 1
    i = 0
    offset = 0

  while true:
      let
        r = min(n, 2 ** (i + 1))
        edge = 2 ** i + offset

      smawk(toSeq(0 + offset ..< edge), toSeq(edge ..< r + offset))
      let x = minima[r - 1 + offset]

      var hadBreak: bool = false
      for j in (2 ** i) ..< (r - 1):
          let y = cost(j + offset, r - 1 + offset)
          if y <= x:
              n -= j
              i = 0
              offset += j
              hadBreak = true

      if not hadBreak:
          if r == n:
              break
          i = i + 1

  var j = count
  while j > 0:
      i = breaks[j]
      result.add(words[i ..< j])
      j = i

  result.reverse()

func `$`*(mw: MarkWord): string = mw.text
func `$`*(mw: TextWord): string = mw.text
func `$`*(mw: OrgWord): string = mw.text

func prevTextWordIdx*[T, A](
  words: seq[Word[T, A]], idx: int): int =
  var idx = idx
  dec idx
  while idx > 0:
    if words[idx].kind == wkText:
      return idx
    dec idx


func prevTextWord*[T, A](
  words: seq[Word[T, A]], idx: int): Option[Word[T, A]] =
  let idx = prevTextWordIdx(words, idx)
  if idx >= 0:
    return some(words[idx])




func wrapText*[T, A](
  words: seq[Word[T, A]], width: int): seq[seq[Word[T, A]]] =
  wrapTextImpl(words, width)

func joinText*[T, A](
  wrapped: seq[seq[Word[T, A]]],
  toStr: proc(w: Word[T, A]): string =
             (proc(w: Word[T, A]): string = $w)
                   ): string =

  for line in wrapped:
    var lineBuf: string
    lineBuf.add line.mapIt(toStr(it)).join(" ")

    result.add lineBuf & "\n"

func wrapMarkLines*(str: string, width: int): seq[string] =
  let buf = str.splitMark().wrapText(width)
  for line in buf:
    result.add line.mapIt($it).join(" ")

func splitOrg*(str: string): seq[OrgWord] =
  # TODO implement better org-mode split handling
  for word in str.split(' '):
    result.add OrgWord(text: word)

func wrapOrgLines*(str: string, width: int): seq[string] =
  let buf = str.splitOrg().wrapText(width)
  for line in buf:
    result.add line.mapIt($it).join(" ")

include hyphenation_patterns
import tables

type
  PattTree = object
    case isFinal: bool
      of false:
        sub: TableRef[char, PattTree]
      of true:
        points: seq[int]

func insertPattern(tree: var PattTree, patt: string) =
  var final: ptr PattTree = addr tree
  for ch in patt.filterIt(it notin {'0' .. '9'}):
    if ch notin final[].sub:
      final[].sub[ch] = PattTree(
        isFinal: false, sub: newTable[char, PattTree]())

    final = addr final[].sub[ch]
    # tree.sub = tree.sub[ch]

  final[].sub['\0'] = PattTree(
    isFinal: true,
    points: patt.split({'.', 'a' .. 'z', 'A' .. 'Z'}).mapIt(
        if it.len == 0: 0 else: parseInt(it)
    ))

let defaultHyphenPattTree*: PattTree =
  block:
    var res = PattTree(isFinal: false, sub: newTable[char, PattTree]())

    for patt in patternList:
      res.insertPattern(patt)

    res

let defaultHyphenExceptionList*: Table[string, seq[int]] =
  block:
    var res: Table[string, seq[int]]

    for exc in hyphenationExceptions:
      var points = @[0]
      for spl in exc.split({'a' .. 'z', 'A' .. 'Z'}):
        if spl == "-":
          points.add 1
        else:
          points.add 0

      res[exc.replace("-", "")] = points

    res


func hyphenate*[A](
  word: TextWord[A],
  tree: PattTree = defaultHyphenPattTree,
  exceptions: Table[string, seq[int]] = defaultHyphenExceptionList,
                 ): seq[TextWord[A]] =
  if len(word) <= 4:
    return @[word]

  var points: seq[int]
  # If the word is an exception, get the stored points.
  if word.text.toLowerAscii() in exceptions:
    points = exceptions[word.text.toLowerAscii()]
  else:
    let work = "." & word.text.toLowerAscii() & "."
    points = 0.repeat(work.len + 1)
    for i in 0 ..< work.len:
      var t = tree
      for c in work[i .. ^1]:
        if c in t.sub:
          t = t.sub[c]
          if '\0' in t.sub:
            let p = t.sub['\0'].points
            for j in 0 ..< p.len:
              points[i + j] = max(points[i + j], p[j])
        else:
          break

    # No hyphens in the first two chars or the last two.
    points[1] = 0
    points[2] = 0
    points[^2] = 0
    points[^3] = 0


  when A is void:
    result.add(TextWord[A](text: ""))
  else:
    result.add(TextWord[A](text: "", attr: word.attr))

  # Examine the points to build the pieces list.
  for (c, p) in zip(word.text, points[2 .. ^1]):
    result[^1].text &= c
    if p mod 2 != 0:
      when A is void:
        result.add(TextWord[A](text: ""))
      else:
        result.add(TextWord[A](text: "", attr: word.attr))

func hyphenate*(str: string): seq[string] =
  {.cast(noSideEffect).}:
    for t in TextWord[void](text: str).hyphenate():
      result.add t.text

func hyphenate*(strs: openarray[string]): seq[seq[string]] =
  for str in strs:
    result.add str.hyphenate

when isMainModule:
  let text = splitMark("Anything associated with normal " &
    "operation and without any particular importance").wrapTextImpl(40)

  echo text
