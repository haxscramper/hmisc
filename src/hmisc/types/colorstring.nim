import terminal, sequtils, strformat, strutils, unicode, strscans, re,
       macros
import hmisc/algo/halgorithm
import hmisc/hdebug_misc
# import hmisc/helpers

type
  PrintStyling* = object
    fg*: ForegroundColor
    bg*: BackgroundColor
    style*: set[Style]

  ColoredString* = object
    str*: string
    styling*: PrintStyling

  ColoredRune* = object
    styling* {.requiresinit.}: PrintStyling
    rune*: Rune

func contains*(ps: PrintStyling, s: Style): bool =
  ps.style.contains(s)

func initPrintStyling*(fg: ForegroundColor = fgDefault,
                       bg: BackgroundColor = bgDefault): PrintStyling =
  PrintStyling(fg: fg, bg: bg)


func uc*(s: static[string]): Rune = runeAt(s, 0)
const coloredWhitespaceRune*: ColoredRune = ColoredRune(
  rune: Rune(' '), styling: initPrintStyling())


func initColoredRune*(rune: Rune,
                      styling: PrintStyling = initPrintStyling()
                     ): ColoredRune =
  ColoredRune(rune: rune, styling: styling)

func toColored*(rune: Rune): ColoredRune = ColoredRune(
  rune: rune, styling: initPrintStyling())

func toColored*(
  ch: char,
  styling: PrintStyling = initPrintStyling()): ColoredRune =
  ColoredRune(rune: Rune(ch), styling: styling)

func initColoredString*(str: string,
                        bg: BackgroundColor = bgDefault,
                        fg: ForegroundColor = fgDefault,
                        style: set[Style] = {}): ColoredString =
  ColoredString(str: str, styling: PrintStyling(
    fg: fg, bg: bg, style: style))

func initColoredString*(str: string, styling: PrintStyling): ColoredString =
  ColoredString(str: str, styling: styling)

# func initColoredString*(str: str)

func fg*(cs: ColoredString): ForegroundColor = cs.styling.fg
func bg*(cs: ColoredString): BackgroundColor = cs.styling.bg
func style*(cs: ColoredString): set[Style] = cs.styling.style

func wrapcode*(str: string, start, finish: int): string =
  fmt("\e[{start}m{str}\e[{finish}m")

func ansiEsc*(code: int): string = fmt("\e[{code}m")
func reEsc*(str: string): string = str.replace("\e", "\\e")

func ansiDiff*(s1, s2: PrintStyling): string =
  if s2.fg != s1.fg:
    result &= ansiEsc(s2.fg.int)

  if s2.bg != s1.bg:
    result &= ansiEsc(s2.bg.int)

func toString*(runes: seq[ColoredRune]): string =
  var prev = initPrintStyling()
  for rune in runes:
    result &= ansiDiff(prev, rune.styling)
    result &= $rune.rune
    prev = rune.styling

  result &= ansiDiff(prev, initPrintStyling())

func toString*(strs: seq[ColoredString]): string =
  var prev = initPrintStyling()
  for str in strs:
    result &= ansiDiff(prev, str.styling)
    result &= str.str
    prev = str.styling

  result &= ansiDiff(prev, initPrintStyling())

func `$`*(colr: seq[ColoredRune]): string = colr.toString()

func `$`*(colored: ColoredString): string =
  result = colored.str

  if colored.fg.int != 0 and colored.fg != fgDefault:
    result = result.wrapcode(int(colored.fg) +
      (if styleBright in colored.style: 60 else: 0), 39)

  if colored.bg.int != 0 and colored.bg != bgDefault:
    result = result.wrapcode(int(colored.bg) +
      (if styleBright in colored.style: 60 else: 0), 49)

  if styleUnderscore in colored.style: result = result.wrapcode(4, 24)
  if styleItalic in colored.style: result = result.wrapcode(3, 23)

func lispRepr*(colstr: ColoredString): string =
  fmt("(\"{colstr.str}\" :bg {colstr.bg} :fg {colstr.fg} :style {colstr.style})")


func toRed*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, style = style, fg = fgRed)

func toGreen*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, style = style, fg = fgGreen)

func toYellow*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, style = style, fg = fgYellow)

func toWhite*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, style = style, fg = fgWhite)

func toCyan*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, style = style, fg = fgCyan)

func toMagenta*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, style = style, fg = fgMagenta)

func toDefault*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, style = style, fg = fgDefault)


func toRed*(str: string, color: bool): string =
  $initColoredString(str, fg = (if color: fgRed else: fgDefault))

func toGreen*(str: string, color: bool): string =
  $initColoredString(str, fg = (if color: fgGreen else: fgDefault))

func toYellow*(str: string, color: bool): string =
  $initColoredString(str, fg = (if color: fgYellow else: fgDefault))

func toWhite*(str: string, color: bool): string =
  $initColoredString(str, fg = (if color: fgWhite else: fgDefault))

func toCyan*(str: string, color: bool): string =
  $initColoredString(str, fg = (if color: fgCyan else: fgDefault))

func toMagenta*(str: string, color: bool): string =
  $initColoredString(str, fg = (if color: fgMagenta else: fgDefault))


func toItalic*(str: string, color: bool): string =
  if color: str.toDefault({styleItalic}) else: str

func toUndescore*(str: string, color: bool): string =
  if color: str.toDefault({styleUnderscore}) else: str

func toItalic*(str: string): string = str.toDefault({styleItalic})
func toUndescore*(str: string): string = str.toDefault({styleUnderscore})

func len*(str: ColoredString): int = str.str.len

func termLen*(str: string): int =
  ## Get length of the string as visible if printed in terminal
  let runelen = str.runeLen()
  var termsyms: int = 0

  block:
    var pos: int = 0
    while scanp(
      str, pos, (*(~ '\e'), ("\e[", +{'0' .. '9'}, 'm') -> inc(termsyms))):
      inc termsyms


  return runeLen - termsyms

func termAlignLeft*(str: string, length: int, padding: char = ' '): string =
  let lendiff = length - str.termLen
  if lendiff > 0:
    str & padding.repeat(lendiff)
  else:
    str

func changeStyle(ps: var PrintStyling, code: int): void =
  # NOTE copy-pasted table from https://en.wikipedia.org/wiki/ANSI_escape_code
  case code:
    of 0:
      # Reset / Normal All attributes off
      discard
    of 1:
      # Bold or increased intensity As with faint, the color change is
      # a PC (SCO / CGA) invention.
      ps.style.incl styleBright
    of 2:
      # Faint or decreased intensity aka Dim (with a saturated color).
      # May be implemented as a light font weight like bold.[34]
      ps.style.incl styleDim
    of 3:
      # Italic Not widely supported. Sometimes treated as inverse or
      # blink.
      ps.style.incl styleItalic
    of 4:
      # Underline Style extensions exist for Kitty, VTE, mintty and
      # iTerm2.
      ps.style.incl styleUnderscore
    of 5:
      # Slow Blink less than 150 per minute
      ps.style.incl styleBlink
    of 6:
      # Rapid Blink MS-DOS ANSI.SYS, 150+ per minute; not widely supported
      ps.style.incl styleBlinkRapid
    of 7:
      # Reverse video swap foreground and background colors, aka
      # invert; inconsistent emulation[37]
      ps.style.incl styleReverse
    of 8:
      # Conceal aka Hide, not widely supported.
      ps.style.incl styleHidden
    of 9:
      # Crossed-out aka Strike, characters legible but marked as if
      # for deletion.
      ps.style.incl styleStrikethrough
    of 10:
      # Primary (default) font
      discard
    of 11 .. 19:
      # Alternative font Select alternative font n − 10
      discard
    of 20:
      # Fraktur Rarely supported
      discard
    of 21:
      # Doubly underline or Bold off Double-underline per ECMA-48
      discard
    of 22:
      # Normal color or intensity Neither bold nor faint
      discard
    of 23:
      # Not italic, not Fraktur
      ps.style.excl styleItalic
    of 24:
      # Underline off Not singly or doubly underlined
      ps.style.excl styleUnderscore
    of 25:
      # Blink off
      ps.style.excl styleBlink
    of 26:
      # Proportional spacing ITU T.61 and T.416, not known to be used
      # on terminals
      discard
    of 27:
      # Reverse/invert off
      ps.style.excl styleReverse
    of 28:
      # Reveal conceal off
      ps.style.excl styleHidden
    of 29:
      # Not crossed out
      ps.style.excl styleStrikethrough
    of 30 .. 37:
      # Set foreground color See color table below
      ps.fg = ForegroundColor(code)
    of 38:
      # Set foreground color Next arguments are 5;n or 2;r;g;b, see
      # below
      discard
    of 39:
      # Default foreground color implementation defined (according to
      # standard)
      ps.fg = fgDefault
      ps.style.excl styleBright
    of 40 .. 47:
      # Set background color See color table below
      ps.bg = BackgroundColor(code)
    of 48:
      # Set background color Next arguments are 5;n or 2;r;g;b, see below
      discard
    of 49:
      # Default background color implementation defined (according to standard)
      ps.bg = bgDefault
      ps.style.excl styleBright
    of 50:
      # Disable proportional spacing T.61 and T.416
      discard
    of 51:
      # Framed
      discard
    of 52:
      # Encircled Implemented as "emoji variation selector" in mintty.[38]
      discard
    of 53:
      # Overlined
      discard
    of 54:
      # Not framed or encircled
      discard
    of 55:
      # Not overlined
      discard
    of 58:
      # Set underline color Kitty, VTE, mintty, and iTerm2. (not in
      # standard)[35][36] Next: arguments are 5;n or 2;r;g;b, see
      # below
      discard
    of 59:
      # Default underline color Kitty, VTE, mintty, and iTerm2. (not
      # in standard)[35][36]
      discard
    of 60:
      # ideogram underline or right side line Rarely supported
      discard
    of 61:
      # ideogram double underline or double: line on the right side
      discard
    of 62:
      # ideogram overline or left side line
      discard
    of 63:
      # ideogram double overline or double: line on the left side
      discard
    of 64:
      # ideogram stress marking
      discard
    of 65:
      # ideogram attributes off reset the effects of all of 60–64
      discard
    of 73:
      # superscript mintty (not in standard)[38]
      discard
    of 74:
      # subscript
      discard
    of 90 .. 97:
      # Set bright foreground color aixterm (not in standard)
      ps.fg = ForegroundColor(code - 60)
      ps.style.incl styleBright
    of 100 .. 107:
      # Set bright background color
      ps.bg = BackgroundColor(code - 60)
      ps.style.incl styleBright
    else:
      discard

func splitSGR*(str: string): seq[ColoredString] =
  var
    style: PrintStyling = PrintStyling(bg: bgDefault, fg: fgDefault)
    prev: int = 0
    pos: int = 0
    sgrbuf: string

  while scanp(str, pos, (
    *(~ '\e'), ("\e[", ({'0' .. '9'}){1,3} -> sgrbuf.add($_), 'm'))
  ):
    let termsym = sgrbuf.len + 2
    let substr = str[prev ..< (pos - termsym - 1)]
    prev = pos
    if substr.len > 0:
      var res = initColoredString(substr)
      res.styling = style
      result.add res

    changeStyle(style, parseInt(sgrbuf))

    sgrbuf = ""

  # if result.len == 0:
  #   result.add initColoredString(str)

  # debugecho pos, " ", prev, " ", str.len
  if prev < pos:
    result.add initColoredString(str[prev ..< pos])

func split*(str: ColoredString, sep: string): seq[ColoredString] =
  for chunk in str.str.split(sep):
    result.add ColoredString(str: chunk, styling: str.styling)

func splitColor*(str: string, sep: string): seq[string] =
  ## Split string on `sep` but retain correct escape wrappers for each
  ## split part.
  result.add ""
  var prev: seq[ColoredString]
  for str in str.splitSGR():
    let chunks = str.split(sep)
    # if result.len == 0:
    #   result.add $chunks[0]
    # else:
    result[^1] &= $chunks[0]

    for chunk in chunks[1..^1]:
      result.add $chunk

func addToLast*[T](sseq: var seq[seq[T]], val: T): void =
  if sseq.len == 0:
    sseq.add @[val]
  else:
    sseq[^1].add val

func traceIfImpl*(head, body: NimNode): NimNode =
  # TODO also trigger `case`
  # TODO use `when` for `head`
  # TODO add indentation for echo output
  case body.kind:
    of nnkIfExpr, nnkIfStmt:
      result = nnkIfExpr.newTree()
      # debugecho "found if"
      for subn in body:
        # debugecho subn.toStrLit().strVal()
        # debugecho subn.kind
        case subn.kind:
          of nnkElifBranch:
            let
              cond = subn[0]
              expr = traceIfImpl(head, subn[1])
              condpos = cond.lineInfoObj().line
              condstr = (&"\e[33m{condpos}\e[39m {cond.toStrLit().strVal()}").newLit

            result.add nnkElifExpr.newTree(
              cond,
              quote do:
                if `head`:
                  debugecho `condstr`
                `expr`
            )

          of nnkElse:
            let
              expr = traceIfImpl(head, subn[0])
              condpos = expr.lineInfoObj().line
              condstr = (&"\e[33m{condpos}\e[39m else").newLit

            result.add nnkElseExpr.newTree(
              quote do:
                if `head`:
                  debugecho `condstr`
                `expr`
            )

          else:
            discard

      # debugecho result.toStrLit().strVal()
    of nnkStmtList:
      # debugecho body.toStrLit().strVal()
      result = newTree(body.kind)
      for subn in body:
        result.add traceIfImpl(head, subn)

      # debugecho result.toStrLit().strVal()
    else:
      return body

macro traceIf*(head, body: untyped): untyped =
  result = traceIfImpl(head, body)
  # quit 0

func splitSGR_sep*(str: string, sep: string = "\n"): seq[seq[ColoredString]] =
  # NOTE There are unit tests for this thing but I actually have very
  # little understanding of /why/ it actually works. I just added
  # conditsion until it passed all tests.
  let splitted = splitSGR(str)
  for idx, cstr in splitted:
    let splitl = cstr.split(sep)

    traceIf false:
      if splitl[0].len == 0 and splitl[^1].len == 0:
        if idx == 0 and splitted.len == 1:
          for line in splitl:
            result.add @[line]
        else:
          if splitted.len > idx + 1:
            result.add splitl[1..^2]
            if (splitted[idx + 1].styling != splitted[idx - 1].styling) and
               (splitl.len > 2):
              if (idx != splitted.len - 1):
                result.add @[]
          else:
            for ch in splitl:
              result.add @[ch]
      elif cstr.str.startsWith(sep):
        if idx == 0:
          result.add splitl[0..^1]
        else:
          result.add splitl[1..^1]
      elif splitl.len > 0:
        # echov cstr
        # echov splitl
        # echov splitted
        result.addToLast splitl[0]
        if cstr.str.endsWith(sep) and (idx != splitted.len - 1):
          result.add @[]

        if splitl.len > 1:
          if splitl[1].str.len > 0:
            result.add @[splitl[1]]

          for chunk in splitl[min(2, splitl.len) .. ^1]:
            result.add @[chunk]
      else:
        result.addToLast splitl[0]

    # echov result


func toRuneGrid*(sseq: seq[seq[ColoredString]]): seq[seq[ColoredRune]] =
  for idx, row in sseq:
    for chunk in row:
      for rune in chunk.str.toRunes():
        result.addToLast initColoredRune(rune, chunk.styling)

    if idx != sseq.len - 1:
      result.add @[]

func toColoredRuneGrid*(str: string): seq[seq[ColoredRune]] =
  str.splitSGR_sep().toRuneGrid()