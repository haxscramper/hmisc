import terminal, sequtils, strformat, strutils, unicode, strscans

type
  ColoredString* = object
    str*: string
    fg*: ForegroundColor
    bg*: BackgroundColor
    style*: set[Style]



proc debug*(str: ColoredString) =
  echo "str: ", str.str, " fg: ",
     str.fg, " bg: ",
     str.bg, " style:",
     str.style

func `$`*(colored: ColoredString): string =
  result = colored.str

  if colored.fg.int != 0:
    result = ansiForegroundColorCode(
      fg = colored.fg,
      bright = styleBright in colored.style) &
        result &
      ansiStyleCode(39)

  if colored.bg.int != 0:
    result = ansiStyleCode(
      int(colored.bg) + (if styleBright in colored.style: 30 else: 0)
    ) & result & ansiStyleCode(39)

  # for style in cor
  # toSeq(colored.style).mapIt(ansiStyleCode(it)).join &
  #   fgCode &
  #   bgCode &
  #   colored.str &
  #   ansiStyleCode(0)

func toRed*(str: string, style: set[Style] = {}): string =
  $ColoredString(str: str, style: style, fg: fgRed)

func toGreen*(str: string, style: set[Style] = {}): string =
  $ColoredString(str: str, style: style, fg: fgGreen)

func toYellow*(str: string, style: set[Style] = {}): string =
  $ColoredString(str: str, style: style, fg: fgYellow)

func toWhite*(str: string, style: set[Style] = {}): string =
  $ColoredString(str: str, style: style, fg: fgWhite)

func toCyan*(str: string, style: set[Style] = {}): string =
  $ColoredString(str: str, style: style, fg: fgCyan)

func toMagenta*(str: string, style: set[Style] = {}): string =
  $ColoredString(str: str, style: style, fg: fgMagenta)

func toDefault*(str: string, style: set[Style] = {}): string =
  $ColoredString(str: str, style: style, fg: fgDefault)



func len*(str: ColoredString): int = str.str.len

func termLen*(str: string): int =
  ## Get length of the string as visible if printed in terminal
  let runelen = str.runeLen()
  var termsyms: int = 0

  block:
    var pos: int = 0
    while scanp(
      str, pos, (*(~ '\e'), ("\e[", +{'1' .. '9'}, 'm') -> inc(termsyms))):
      inc termsyms


  return runeLen - termsyms
