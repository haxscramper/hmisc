import terminal, sequtils, strformat, strutils

type
  ColoredString* = object
    str*: string
    fg*: ForegroundColor
    bg*: BackgroundColor
    style*: set[Style]


func toRed*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgRed)

func toGreen*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgGreen)

func toYellow*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgYellow)

func toWhite*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgWhite)

func toCyan*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgCyan)

func toMagenta*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgMagenta)

func toDefault*(str: string, style: set[Style] = {}): ColoredString =
  ColoredString(str: str, style: style, fg: fgDefault)

proc debug*(str: ColoredString) =
  echo "str: ", str.str, " fg: ",
     str.fg, " bg: ",
     str.bg, " style:",
     str.style

func `$`*(colored: ColoredString): string =
  let fgCode = if colored.fg.int != 0:
      ansiForegroundColorCode(
        fg = colored.fg,
        bright = styleBright in colored.style)
    else:
      ""

  let bgCode = if colored.bg.int != 0:
      ansiStyleCode(
        colored.bg.int +
        (if styleBright in colored.style: 60 else: 0))
      else:
        ""

  toSeq(colored.style).mapIt(ansiStyleCode(it)).join &
    fgCode &
    bgCode &
    colored.str &
    ansiStyleCode(0)


func len*(str: ColoredString): int = str.str.len
