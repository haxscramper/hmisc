import sequtils, sugar, math, algorithm
import ../types/colorstring

## https://xxyxyz.org/line-breaking/

type
  WordKind = enum
    wkText
    wkNewline
    wkSpace

  Word[Text, Attr] = object
    text: Text
    attr: Attr
    forceNl: bool
    isBreaking: bool
    kind: WordKind

  TermWord = Word[string, PrintStyling]

  MarkStyling = object
    wrap: char

  MarkWord = Word[string, MarkStyling]

func splitMark*(str: string): seq[MarkWord] =
  @[
    MarkWord(text: "hello", attr: MarkStyling(wrap: '*')),
    MarkWord(text: "world", attr: MarkStyling(wrap: '`'))
  ]

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

  var minima: seq[int] = @[0] & (@[10 ** 12].repeat count).concat()
  var breaks: seq[int] = @[0].repeat(count + 1).concat()

  proc cost(i, j: int): auto =
      let w = offsets[j] - offsets[i] + j - i - 1
      if w > width:
          return 10 ** 10 * (w - width)

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
              if idx mod 2 == 0:
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

func `$`(mw: MarkWord): string = mw.text

when isMainModule:
  let text = "*hello* `world`".splitMark().wrapTextImpl(10)
  echo text
