import terminal, sequtils, strformat, strutils, unicode, strscans

type
  PrintStyling* = object
    fg*: ForegroundColor
    bg*: BackgroundColor
    style*: set[Style]

  ColoredString* = object
    str*: string
    styling*: PrintStyling


func fg*(cs: ColoredString): ForegroundColor = cs.styling.fg
func bg*(cs: ColoredString): BackgroundColor = cs.styling.bg
func style*(cs: ColoredString): set[Style] = cs.styling.style

func `$`*(colored: ColoredString): string =
  result = colored.str

  if colored.fg.int != 0 and colored.fg != fgDefault:
    result = ansiForegroundColorCode(
      fg = colored.fg,
      bright = styleBright in colored.style) &
        result &
      ansiStyleCode(39)

  if colored.bg.int != 0 and colored.bg != bgDefault:
    result = ansiStyleCode(
      int(colored.bg) + (if styleBright in colored.style: 30 else: 0)
    ) & result & ansiStyleCode(39)

func initColoredString*(str: string,
                        bg: BackgroundColor = bgDefault,
                        fg: ForegroundColor = fgDefault,
                        style: set[Style] = {}): ColoredString =
  ColoredString(str: str, styling: PrintStyling(
    fg: fg, bg: bg, style: style))


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

# func splitSGR
