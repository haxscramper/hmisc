import std/[
  sequtils, strformat, strutils, unicode, colors,
  lenientops, macros, strscans, algorithm, math, options
]

import ../macros/traceif
import ../hdebug_misc, ../base_errors
import ../algo/[hseq_distance, htemplates]

type
  TermColor8Bit* = enum
    tcBlack             = 0
    tcMaroon            = 1
    tcGreen             = 2
    tcOlive             = 3
    tcNavy              = 4
    tcPurple            = 5
    tcTeal              = 6
    tcSilver            = 7
    tcGrey              = 8
    tcRed               = 9
    tcLime              = 10
    tcYellow            = 11
    tcBlue              = 12
    tcFuchsia           = 13
    tcAqua              = 14
    tcWhite             = 15
    tcGrey0             = 16
    tcNavyBlue          = 17
    tcDarkBlue          = 18
    tcBlue3             = 19
    tcBlue3_1           = 20
    tcBlue1             = 21
    tcDarkGreen         = 22
    tcDeepSkyBlue4      = 23
    tcDeepSkyBlue4_1    = 24
    tcDeepSkyBlue4_2    = 25
    tcDodgerBlue3       = 26
    tcDodgerBlue2       = 27
    tcGreen4            = 28
    tcSpringGreen4      = 29
    tcTurquoise4        = 30
    tcDeepSkyBlue3      = 31
    tcDeepSkyBlue3_1    = 32
    tcDodgerBlue1       = 33
    tcGreen3            = 34
    tcSpringGreen3      = 35
    tcDarkCyan          = 36
    tcLightSeaGreen     = 37
    tcDeepSkyBlue2      = 38
    tcDeepSkyBlue1      = 39
    tcGreen3_1          = 40
    tcSpringGreen3_1    = 41
    tcSpringGreen2      = 42
    tcCyan3             = 43
    tcDarkTurquoise     = 44
    tcTurquoise2        = 45
    tcGreen1            = 46
    tcSpringGreen2_1    = 47
    tcSpringGreen1      = 48
    tcMediumSpringGreen = 49
    tcCyan2             = 50
    tcCyan1             = 51
    tcDarkRed           = 52
    tcDeepPink4         = 53
    tcPurple4           = 54
    tcPurple4_1         = 55
    tcPurple3           = 56
    tcBlueViolet        = 57
    tcOrange4           = 58
    tcGrey37            = 59
    tcMediumPurple4     = 60
    tcSlateBlue3        = 61
    tcSlateBlue3_1      = 62
    tcRoyalBlue1        = 63
    tcChartreuse4       = 64
    tcDarkSeaGreen4     = 65
    tcPaleTurquoise4    = 66
    tcSteelBlue         = 67
    tcSteelBlue3        = 68
    tcCornflowerBlue    = 69
    tcChartreuse3       = 70
    tcDarkSeaGreen4_1   = 71
    tcCadetBlue         = 72
    tcCadetBlue_1       = 73
    tcSkyBlue3          = 74
    tcSteelBlue1        = 75
    tcChartreuse3_1     = 76
    tcPaleGreen3        = 77
    tcSeaGreen3         = 78
    tcAquamarine3       = 79
    tcMediumTurquoise   = 80
    tcSteelBlue1_1      = 81
    tcChartreuse2       = 82
    tcSeaGreen2         = 83
    tcSeaGreen1         = 84
    tcSeaGreen1_1       = 85
    tcAquamarine1       = 86
    tcDarkSlateGray2    = 87
    tcDarkRed_1         = 88
    tcDeepPink4_1       = 89
    tcDarkMagenta       = 90
    tcDarkMagenta_1     = 91
    tcDarkViolet        = 92
    tcPurple_1          = 93
    tcOrange4_1         = 94
    tcLightPink4        = 95
    tcPlum4             = 96
    tcMediumPurple3     = 97
    tcMediumPurple3_1   = 98
    tcSlateBlue1        = 99
    tcYellow4           = 100
    tcWheat4            = 101
    tcGrey53            = 102
    tcLightSlateGrey    = 103
    tcMediumPurple      = 104
    tcLightSlateBlue    = 105
    tcYellow4_1         = 106
    tcDarkOliveGreen3   = 107
    tcDarkSeaGreen      = 108
    tcLightSkyBlue3     = 109
    tcLightSkyBlue3_1   = 110
    tcSkyBlue2          = 111
    tcChartreuse2_1     = 112
    tcDarkOliveGreen3_1 = 113
    tcPaleGreen3_1      = 114
    tcDarkSeaGreen3     = 115
    tcDarkSlateGray3    = 116
    tcSkyBlue1          = 117
    tcChartreuse1       = 118
    tcLightGreen        = 119
    tcLightGreen_1      = 120
    tcPaleGreen1        = 121
    tcAquamarine1_1     = 122
    tcDarkSlateGray1    = 123
    tcRed3              = 124
    tcDeepPink4_2       = 125
    tcMediumVioletRed   = 126
    tcMagenta3          = 127
    tcDarkViolet_1      = 128
    tcPurple_2          = 129
    tcDarkOrange3       = 130
    tcIndianRed         = 131
    tcHotPink3          = 132
    tcMediumOrchid3     = 133
    tcMediumOrchid      = 134
    tcMediumPurple2     = 135
    tcDarkGoldenrod     = 136
    tcLightSalmon3      = 137
    tcRosyBrown         = 138
    tcGrey63            = 139
    tcMediumPurple2_1   = 140
    tcMediumPurple1     = 141
    tcGold3             = 142
    tcDarkKhaki         = 143
    tcNavajoWhite3      = 144
    tcGrey69            = 145
    tcLightSteelBlue3   = 146
    tcLightSteelBlue    = 147
    tcYellow3           = 148
    tcDarkOliveGreen3_2 = 149
    tcDarkSeaGreen3_1   = 150
    tcDarkSeaGreen2     = 151
    tcLightCyan3        = 152
    tcLightSkyBlue1     = 153
    tcGreenYellow       = 154
    tcDarkOliveGreen2   = 155
    tcPaleGreen1_1      = 156
    tcDarkSeaGreen2_1   = 157
    tcDarkSeaGreen1     = 158
    tcPaleTurquoise1    = 159
    tcRed3_1            = 160
    tcDeepPink3         = 161
    tcDeepPink3_1       = 162
    tcMagenta3_1        = 163
    tcMagenta3_2        = 164
    tcMagenta2          = 165
    tcDarkOrange3_1     = 166
    tcIndianRed_1       = 167
    tcHotPink3_1        = 168
    tcHotPink2          = 169
    tcOrchid            = 170
    tcMediumOrchid1     = 171
    tcOrange3           = 172
    tcLightSalmon3_1    = 173
    tcLightPink3        = 174
    tcPink3             = 175
    tcPlum3             = 176
    tcViolet            = 177
    tcGold3_1           = 178
    tcLightGoldenrod3   = 179
    tcTan               = 180
    tcMistyRose3        = 181
    tcThistle3          = 182
    tcPlum2             = 183
    tcYellow3_1         = 184
    tcKhaki3            = 185
    tcLightGoldenrod2   = 186
    tcLightYellow3      = 187
    tcGrey84            = 188
    tcLightSteelBlue1   = 189
    tcYellow2           = 190
    tcDarkOliveGreen1   = 191
    tcDarkOliveGreen1_1 = 192
    tcDarkSeaGreen1_1   = 193
    tcHoneydew2         = 194
    tcLightCyan1        = 195
    tcRed1              = 196
    tcDeepPink2         = 197
    tcDeepPink1         = 198
    tcDeepPink1_1       = 199
    tcMagenta2_1        = 200
    tcMagenta1          = 201
    tcOrangeRed1        = 202
    tcIndianRed1_1      = 203
    tcIndianRed1_2      = 204
    tcHotPink           = 205
    tcHotPink_1         = 206
    tcMediumOrchid1_1   = 207
    tcDarkOrange        = 208
    tcSalmon1           = 209
    tcLightCoral        = 210
    tcPaleVioletRed1    = 211
    tcOrchid2           = 212
    tcOrchid1           = 213
    tcOrange1           = 214
    tcSandyBrown        = 215
    tcLightSalmon1      = 216
    tcLightPink1        = 217
    tcPink1             = 218
    tcPlum1             = 219
    tcGold1             = 220
    tcLightGoldenrod2_1 = 221
    tcLightGoldenrod2_2 = 222
    tcNavajoWhite1      = 223
    tcMistyRose1        = 224
    tcThistle1          = 225
    tcYellow1           = 226
    tcLightGoldenrod1   = 227
    tcKhaki1            = 228
    tcWheat1            = 229
    tcCornsilk1         = 230
    tcGrey100           = 231
    tcGrey3             = 232
    tcGrey7             = 233
    tcGrey11            = 234
    tcGrey15            = 235
    tcGrey19            = 236
    tcGrey23            = 237
    tcGrey27            = 238
    tcGrey30            = 239
    tcGrey35            = 240
    tcGrey39            = 241
    tcGrey42            = 242
    tcGrey46            = 243
    tcGrey50            = 244
    tcGrey54            = 245
    tcGrey58            = 246
    tcGrey62            = 247
    tcGrey66            = 248
    tcGrey70            = 249
    tcGrey74            = 250
    tcGrey78            = 251
    tcGrey82            = 252
    tcGrey85            = 253
    tcGrey89            = 254
    tcGrey93            = 255

const
  tcGreyColors* = { tcGrey3 .. tcGrey93 }


when defined(nimscript):
  type
    Style* = enum        ## different styles for text output
      styleBright = 1,   ## bright text
      styleDim,          ## dim text
      styleItalic,       ## italic (or reverse on terminals not supporting)
      styleUnderscore,   ## underscored text
      styleBlink,        ## blinking/bold text
      styleBlinkRapid,   ## rapid blinking/bold text (not widely supported)
      styleReverse,      ## reverse
      styleHidden,       ## hidden text
      styleStrikethrough ## strikethrough

    ForegroundColor* = enum ## terminal's foreground colors
      fgBlack = 30,         ## black
      fgRed,                ## red
      fgGreen,              ## green
      fgYellow,             ## yellow
      fgBlue,               ## blue
      fgMagenta,            ## magenta
      fgCyan,               ## cyan
      fgWhite,              ## white
      fg8Bit,               ## 256-color (not supported, see ``enableTrueColors`` instead.)
      fgDefault             ## default terminal foreground color

    BackgroundColor* = enum ## terminal's background colors
      bgBlack = 40,         ## black
      bgRed,                ## red
      bgGreen,              ## green
      bgYellow,             ## yellow
      bgBlue,               ## blue
      bgMagenta,            ## magenta
      bgCyan,               ## cyan
      bgWhite,              ## white
      bg8Bit,               ## 256-color (not supported, see ``enableTrueColors`` instead.)
      bgDefault             ## default terminal background color

else:
  import ../algo/halgorithm
  import ../hdebug_misc
  import terminal
  export terminal


type
  PrintStyling* = object
    case use8Bit*: bool
      of true:
        fg8*: TermColor8Bit
        bg8*: TermColor8Bit

      of false:
        fg*: ForegroundColor
        bg*: BackgroundColor

    style*: set[Style]

  ColoredString* = object
    str*: string
    styling*: PrintStyling

  ColoredRune* = object
    styling* {.requiresinit.}: PrintStyling
    rune*: Rune

  ColoredRuneLine* = seq[ColoredRune]
  ColoredRuneGrid* = seq[ColoredRuneLine]

func isValid*(style: PrintStyling): bool =
  style.use8Bit or (style.fg.int != 0 and style.bg.int != 0)

func `==`*(s1, s2: PrintStyling): bool =
  let check =
    if s1.use8Bit:
      s1.fg8 == s2.fg8 and s1.bg8 == s2.bg8

    else:
      s1.fg == s2.fg and s1.bg == s2.bg


  s1.style == s2.style and
  s1.use8Bit == s2.use8Bit and check

func contains*(ps: PrintStyling, s: Style): bool =
  ps.style.contains(s)

func initPrintStyling*(
    fg: ForegroundColor = fgDefault,
    bg: BackgroundColor = bgDefault,
    style: set[Style] = {}
  ): PrintStyling =
  PrintStyling(use8Bit: false, fg: fg, bg: bg, style: style)

func initStyleBg*(term: TermColor8Bit): PrintStyling {.inline.} =
  PrintStyling(use8Bit: true, bg8: term)

func initStyleFg*(term: TermColor8Bit): PrintStyling {.inline.} =
  PrintStyling(use8Bit: true, fg8: term)

func initStyle*(fg, bg: TermColor8Bit): PrintStyling {.inline.} =
  PrintStyling(use8Bit: true, fg8: fg, bg8: bg)

macro initStyle*(args: varargs[typed]): PrintStyling =
  let tmp = genSym(nskVar, "tmp")
  result = newStmtList()
  result.add quote do:
    var `tmp` = initPrintStyling()

  for fld in args:
    result.add quote do:
      when `fld` is ForegroundColor:
        `tmp`.fg = `fld`

      elif `fld` is BackgroundColor:
        `tmp`.bg = `fld`

      elif `fld` is Style:
        `tmp`.style.incl `fld`

      elif `fld` is set[Style]:
        `tmp`.style = `fld`

      elif `fld` is TermColor8Bit:
        let old = `tmp`.style
        `tmp` = PrintStyling(use8bit: true)
        `tmp`.fg8 = `fld`
        `tmp`.style = old

  result.add quote do:
    `tmp`


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
  styling: PrintStyling = initPrintStyling(),
  colorize: bool = true): ColoredRune =
  ColoredRune(
    rune: Rune(ch),
    styling: (if not colorize: initPrintStyling() else: styling))

func toColored*(rune: ColoredRune, styling: PrintStyling): ColoredRune =
  result = rune
  result.styling = styling

func toColored*(
  ch: Rune,
  styling: PrintStyling = initPrintStyling(),
  colorize: bool = true): ColoredRune =
  ColoredRune(
    rune: ch,
    styling: (if not colorize: initPrintStyling() else: styling))

func initColoredString*(str: string,
                        bg: BackgroundColor = bgDefault,
                        fg: ForegroundColor = fgDefault,
                        style: set[Style] = {}): ColoredString =
  ColoredString(str: str, styling: PrintStyling(
    use8Bit: false, fg: fg, bg: bg, style: style))

func initColoredString*(str: string, styling: PrintStyling): ColoredString =
  ColoredString(str: str, styling: styling)

func toColored*(str: string, styling: PrintStyling): ColoredString =
  ColoredString(str: str, styling: styling)

# func initColoredString*(str: str)

func fg*(cs: ColoredString): ForegroundColor = cs.styling.fg
func bg*(cs: ColoredString): BackgroundColor = cs.styling.bg
func style*(cs: ColoredString): set[Style] = cs.styling.style
func style*(cs: var ColoredString): var set[Style] = cs.styling.style
func `fg=`*(cs: var ColoredString, arg: ForegroundColor) =
  cs.styling.fg = arg

func `bg=`*(cs: var ColoredString, arg: BackgroundColor) =
  cs.styling.bg = arg

func `style=`*(cs: var ColoredString, arg: set[Style]) =
  cs.styling.style = arg

func wrapcode*(str: string, start, finish: int): string =
  fmt("\e[{start}m{str}\e[{finish}m")

func ansiEsc*(code: int): string = fmt("\e[{code}m")
func reEsc*(str: string): string = str.replace("\e", "\\e")

func ansiEsc*(bit8: TermColor8Bit, fg: bool): string =
  if fg: fmt("\e[38;5;{bit8.ord}m")
  else: fmt("\e[48;5;{bit8.ord}m")

func ansiDiff*(s1, s2: PrintStyling): string =
  if s1.use8Bit != s2.use8bit:
    if s2.use8bit:
      if s2.fg8.int != 0: result &= ansiEsc(s2.fg8, true)
      if s2.bg8.int != 0: result &= ansiEsc(s2.bg8, false)

    else:
      result &= ansiEsc(s2.fg.int)
      result &= ansiEsc(s2.bg.int)

  else:
    if s1.use8bit:
      if s1.fg8 != s2.fg8: result &= ansiEsc(s2.fg8, true)
      if s1.bg8 != s2.bg8: result &= ansiEsc(s2.bg8, true)

    else:
      if s2.fg != s1.fg: result &= ansiEsc(s2.fg.int)
      if s2.bg != s1.bg: result &= ansiEsc(s2.bg.int)

func toString*(runes: seq[ColoredRune], color: bool = true): string =
  if color:
    var prev = initPrintStyling()
    for rune in runes:
      result &= ansiDiff(prev, rune.styling)
      result &= $rune.rune
      prev = rune.styling

    result &= ansiDiff(prev, initPrintStyling())
  else:
    for rune in runes:
      result &= $rune.rune


func toString*(strs: seq[ColoredString], color: bool = true): string =
  if color:
    var prev = initPrintStyling()
    for str in strs:
      result &= ansiDiff(prev, str.styling)
      result &= str.str
      prev = str.styling

    result &= ansiDiff(prev, initPrintStyling())
  else:
    for str in strs:
      result &= str.str

func `$`*(colored: ColoredString | ColoredRune): string =
  when colored is ColoredString:
    result = colored.str

  else:
    result = $colored.rune

  if colored.styling.use8Bit:
    if colored.styling.fg8.int != 0:
      result = &"\e[38;5;{colored.styling.fg8.ord}m{result}\e[0m"

    if colored.styling.bg8.int != 0:
      result = &"\e[48;5;{colored.styling.bg8.ord}m{result}\e[0m"

  else:
    if colored.styling.fg.int != 0 and
       colored.styling.fg != fgDefault:
      result = result.wrapcode(
        int(colored.styling.fg) + (
          if styleBright in colored.styling.style: 60 else: 0), 39)

    if colored.styling.bg.int != 0 and
       colored.styling.bg != bgDefault:
      result = result.wrapcode(
        int(colored.styling.bg) + (
          if styleBright in colored.styling.style: 60 else: 0), 49)

  if styleUnderscore in colored.styling.style:
    result = result.wrapcode(4, 24)

  if styleItalic in colored.styling.style:
    result = result.wrapcode(3, 23)


func `$`*(colr: seq[ColoredRune]): string = colr.toString()
func `$`*(colr: seq[seq[ColoredRune]]): string =
  for idx, line in colr:
    if idx > 0:
      result &= "\n"

    for rune in line:
      result &= $rune

func lispRepr*(colstr: ColoredString): string =
  fmt("(\"{colstr.str}\" :bg {colstr.bg} :fg {colstr.fg} :style {colstr.style})")

func openLit*(color: ForegroundColor): string = &"\\033[{color.int}m"
func closeLit*(color: ForegroundColor): string = "\\033[0m"
func openLit*(color: BackgroundColor): string = &"\\033[{color.int}m"
func closeLit*(color: BackgroundColor): string = "\\033[0m"


func toStyled*(
    str: string,
    style: PrintStyling,
    colorize: bool = not defined(plainStdout)
  ): string =

  if colorize:
    $ColoredString(str: str, styling: style)
  else:
    str

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

func toDefault*(
  str: string, style: set[Style] = {}, colorize: bool = true): string =
  $initColoredString(
    str, style = if colorize: style else: {}, fg = fgDefault)


func toBlue*(str: string, style: set[Style] = {}): string =
  $initColoredString(str, fg = fgBlue, style = style)

func toBlue*(str: string, color: bool): string =
  $initColoredString(str, fg = (if color: fgBlue else: fgDefault))

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

func to8Bit*(str: string, color: TermColor8Bit): string =
  &"\e[38;5;{color.ord}m{str}\e[0m"

func to8BitBg*(str: string, color: TermColor8Bit): string =
  &"\e[48;5;{color.ord}m{str}\e[0m"

proc to8Bit*(
  str: string, r, g, b: range[0 .. 5], colored: bool = true): string =
  if colored:
    result = &"\e[38;5;{16 + b + g * 6 + (6 * 6) * r}m{str}\e[0m"

  else:
    result = str

proc to8Bit*(
  str: string, gray: range[0 .. 23], colored: bool = true): string =
  if colored:
    result = &"\e[38;5;{232 + gray}m{str}\e[0m"

  else:
    result = str


proc to8BitBg*(
  str: string, r, g, b: range[0 .. 5], colored: bool = true): string =
  if colored:
    result = &"\e[48;5;{16 + b + g * 6 + (6 * 6) * r}m{str}\e[0m"

  else:
    result = str

proc to8BitBg*(
  str: string, gray: range[0 .. 23], colored: bool = true): string =
  if colored:
    result = &"\e[48;5;{232 + gray}m{str}\e[0m"

  else:
    result = str

func term8Bit*(r, g, b: range[0 .. 5]): TermColor8Bit =
  TermColor8Bit(16 + b + g * 6 + (6 * 6) * r)

func term8Bit*(color: Color): TermColor8Bit =
  let
    (r1, g1, b1) = color.extractRGB()
    (r2, g2, b2) = (int(r1 / 42.6), int(g1 / 42.6), int(b1 / 42.6))

  echov (r1, g1, b1, r2, g2, b2)

  return term8Bit(r2, g2, b2)


func term8Bit*(gray: range[0 .. 23]): TermColor8Bit =
  TermColor8Bit(232 + gray)

func toRGB*(color: TermColor8Bit): tuple[r, g, b: range[0 .. 5]] =
  let col = color.int - 16
  result.r = col div 36
  result.g = (col div 6) mod 6
  result.b = col mod 6

type
  TermHSV* = tuple[h: int, s, v: float]
  ColSpec = object
    hsv: TermHSV
    rgb: tuple[r, g, b: range[0..255]]
    id: TermColor8Bit

func round5(f: int): int = (f div 5) * 5
func round5(f: float): float = float(round(f / 5) * 5)
func clampedHsv(hsv: TermHSV): TermHsv =
   result = (
     h: round5(hsv.h.float).int,
     s: round5(hsv.s * 100) / 100.0,
     v: round5(hsv.v * 100) / 100.0
   )


func hexToRgb(hex: string): tuple[r, g, b: 0 .. 255] =
  parseColor(hex).extractRGB()

func rgbToHsv(rgb: tuple[r, g, b: 0 .. 255]): TermHSV =
  let rgb = (r: rgb.r.float, g: rgb.g.float, b: rgb.b.float)
  var max = max([rgb.r, rgb.g, rgb.b])
  var diff = max - min([rgb.r, rgb.g, rgb.b])
  result.s = if (max == 0.0): 0.float else: (100 * diff / max)
  # echov max
  # echov diff
  if (result.s == 0): result.h = 0
  elif (rgb.r == max): result.h = round(60.0 * (rgb.g - rgb.b) / diff).int
  elif (rgb.g == max): result.h = round(60.0 * (rgb.b - rgb.r) / diff + 120.0).int
  elif (rgb.b == max): result.h = round(60.0 * (rgb.r - rgb.g) / diff + 240.0).int
  if (result.h < 0.0): result.h += 360
  result.v = round(max * 100 / 255)
  result.s = round(result.s)

# startHax()
# for hex in ["#ff0000", "#ff0000"]:
#   let rgb = hexToRGB(hex)
#   echov rgb
#   echov rgbToHSV(rgb)
# stopHax()

const colorsHex = [
  (0, Color(0x00_00_00)),
  (1, Color(0x80_00_00)),
  (2, Color(0x00_80_00)),
  (3, Color(0x80_80_00)),
  (4, Color(0x00_00_80)),
  (5, Color(0x80_00_80)),
  (6, Color(0x00_80_80)),
  (7, Color(0xc0_c0_c0)),
  (8, Color(0x80_80_80)),
  (9, Color(0xff_00_00)),
  (10, Color(0x00_ff_00)),
  (11, Color(0xff_ff_00)),
  (12, Color(0x00_00_ff)),
  (13, Color(0xff_00_ff)),
  (14, Color(0x00_ff_ff)),
  (15, Color(0xff_ff_ff)),
  (16, Color(0x00_00_00)),
  (17, Color(0x00_00_5f)),
  (18, Color(0x00_00_87)),
  (19, Color(0x00_00_af)),
  (20, Color(0x00_00_d7)),
  (21, Color(0x00_00_ff)),
  (22, Color(0x00_5f_00)),
  (23, Color(0x00_5f_5f)),
  (24, Color(0x00_5f_87)),
  (25, Color(0x00_5f_af)),
  (26, Color(0x00_5f_d7)),
  (27, Color(0x00_5f_ff)),
  (28, Color(0x00_87_00)),
  (29, Color(0x00_87_5f)),
  (30, Color(0x00_87_87)),
  (31, Color(0x00_87_af)),
  (32, Color(0x00_87_d7)),
  (33, Color(0x00_87_ff)),
  (34, Color(0x00_af_00)),
  (35, Color(0x00_af_5f)),
  (36, Color(0x00_af_87)),
  (37, Color(0x00_af_af)),
  (38, Color(0x00_af_d7)),
  (39, Color(0x00_af_ff)),
  (40, Color(0x00_d7_00)),
  (41, Color(0x00_d7_5f)),
  (42, Color(0x00_d7_87)),
  (43, Color(0x00_d7_af)),
  (44, Color(0x00_d7_d7)),
  (45, Color(0x00_d7_ff)),
  (46, Color(0x00_ff_00)),
  (47, Color(0x00_ff_5f)),
  (48, Color(0x00_ff_87)),
  (49, Color(0x00_ff_af)),
  (50, Color(0x00_ff_d7)),
  (51, Color(0x00_ff_ff)),
  (52, Color(0x5f_00_00)),
  (53, Color(0x5f_00_5f)),
  (54, Color(0x5f_00_87)),
  (55, Color(0x5f_00_af)),
  (56, Color(0x5f_00_d7)),
  (57, Color(0x5f_00_ff)),
  (58, Color(0x5f_5f_00)),
  (59, Color(0x5f_5f_5f)),
  (60, Color(0x5f_5f_87)),
  (61, Color(0x5f_5f_af)),
  (62, Color(0x5f_5f_d7)),
  (63, Color(0x5f_5f_ff)),
  (64, Color(0x5f_87_00)),
  (65, Color(0x5f_87_5f)),
  (66, Color(0x5f_87_87)),
  (67, Color(0x5f_87_af)),
  (68, Color(0x5f_87_d7)),
  (69, Color(0x5f_87_ff)),
  (70, Color(0x5f_af_00)),
  (71, Color(0x5f_af_5f)),
  (72, Color(0x5f_af_87)),
  (73, Color(0x5f_af_af)),
  (74, Color(0x5f_af_d7)),
  (75, Color(0x5f_af_ff)),
  (76, Color(0x5f_d7_00)),
  (77, Color(0x5f_d7_5f)),
  (78, Color(0x5f_d7_87)),
  (79, Color(0x5f_d7_af)),
  (80, Color(0x5f_d7_d7)),
  (81, Color(0x5f_d7_ff)),
  (82, Color(0x5f_ff_00)),
  (83, Color(0x5f_ff_5f)),
  (84, Color(0x5f_ff_87)),
  (85, Color(0x5f_ff_af)),
  (86, Color(0x5f_ff_d7)),
  (87, Color(0x5f_ff_ff)),
  (88, Color(0x87_00_00)),
  (89, Color(0x87_00_5f)),
  (90, Color(0x87_00_87)),
  (91, Color(0x87_00_af)),
  (92, Color(0x87_00_d7)),
  (93, Color(0x87_00_ff)),
  (94, Color(0x87_5f_00)),
  (95, Color(0x87_5f_5f)),
  (96, Color(0x87_5f_87)),
  (97, Color(0x87_5f_af)),
  (98, Color(0x87_5f_d7)),
  (99, Color(0x87_5f_ff)),
  (100, Color(0x87_87_00)),
  (101, Color(0x87_87_5f)),
  (102, Color(0x87_87_87)),
  (103, Color(0x87_87_af)),
  (104, Color(0x87_87_d7)),
  (105, Color(0x87_87_ff)),
  (106, Color(0x87_af_00)),
  (107, Color(0x87_af_5f)),
  (108, Color(0x87_af_87)),
  (109, Color(0x87_af_af)),
  (110, Color(0x87_af_d7)),
  (111, Color(0x87_af_ff)),
  (112, Color(0x87_d7_00)),
  (113, Color(0x87_d7_5f)),
  (114, Color(0x87_d7_87)),
  (115, Color(0x87_d7_af)),
  (116, Color(0x87_d7_d7)),
  (117, Color(0x87_d7_ff)),
  (118, Color(0x87_ff_00)),
  (119, Color(0x87_ff_5f)),
  (120, Color(0x87_ff_87)),
  (121, Color(0x87_ff_af)),
  (122, Color(0x87_ff_d7)),
  (123, Color(0x87_ff_ff)),
  (124, Color(0xaf_00_00)),
  (125, Color(0xaf_00_5f)),
  (126, Color(0xaf_00_87)),
  (127, Color(0xaf_00_af)),
  (128, Color(0xaf_00_d7)),
  (129, Color(0xaf_00_ff)),
  (130, Color(0xaf_5f_00)),
  (131, Color(0xaf_5f_5f)),
  (132, Color(0xaf_5f_87)),
  (133, Color(0xaf_5f_af)),
  (134, Color(0xaf_5f_d7)),
  (135, Color(0xaf_5f_ff)),
  (136, Color(0xaf_87_00)),
  (137, Color(0xaf_87_5f)),
  (138, Color(0xaf_87_87)),
  (139, Color(0xaf_87_af)),
  (140, Color(0xaf_87_d7)),
  (141, Color(0xaf_87_ff)),
  (142, Color(0xaf_af_00)),
  (143, Color(0xaf_af_5f)),
  (144, Color(0xaf_af_87)),
  (145, Color(0xaf_af_af)),
  (146, Color(0xaf_af_d7)),
  (147, Color(0xaf_af_ff)),
  (148, Color(0xaf_d7_00)),
  (149, Color(0xaf_d7_5f)),
  (150, Color(0xaf_d7_87)),
  (151, Color(0xaf_d7_af)),
  (152, Color(0xaf_d7_d7)),
  (153, Color(0xaf_d7_ff)),
  (154, Color(0xaf_ff_00)),
  (155, Color(0xaf_ff_5f)),
  (156, Color(0xaf_ff_87)),
  (157, Color(0xaf_ff_af)),
  (158, Color(0xaf_ff_d7)),
  (159, Color(0xaf_ff_ff)),
  (160, Color(0xd7_00_00)),
  (161, Color(0xd7_00_5f)),
  (162, Color(0xd7_00_87)),
  (163, Color(0xd7_00_af)),
  (164, Color(0xd7_00_d7)),
  (165, Color(0xd7_00_ff)),
  (166, Color(0xd7_5f_00)),
  (167, Color(0xd7_5f_5f)),
  (168, Color(0xd7_5f_87)),
  (169, Color(0xd7_5f_af)),
  (170, Color(0xd7_5f_d7)),
  (171, Color(0xd7_5f_ff)),
  (172, Color(0xd7_87_00)),
  (173, Color(0xd7_87_5f)),
  (174, Color(0xd7_87_87)),
  (175, Color(0xd7_87_af)),
  (176, Color(0xd7_87_d7)),
  (177, Color(0xd7_87_ff)),
  (178, Color(0xd7_af_00)),
  (179, Color(0xd7_af_5f)),
  (180, Color(0xd7_af_87)),
  (181, Color(0xd7_af_af)),
  (182, Color(0xd7_af_d7)),
  (183, Color(0xd7_af_ff)),
  (184, Color(0xd7_d7_00)),
  (185, Color(0xd7_d7_5f)),
  (186, Color(0xd7_d7_87)),
  (187, Color(0xd7_d7_af)),
  (188, Color(0xd7_d7_d7)),
  (189, Color(0xd7_d7_ff)),
  (190, Color(0xd7_ff_00)),
  (191, Color(0xd7_ff_5f)),
  (192, Color(0xd7_ff_87)),
  (193, Color(0xd7_ff_af)),
  (194, Color(0xd7_ff_d7)),
  (195, Color(0xd7_ff_ff)),
  (196, Color(0xff_00_00)),
  (197, Color(0xff_00_5f)),
  (198, Color(0xff_00_87)),
  (199, Color(0xff_00_af)),
  (200, Color(0xff_00_d7)),
  (201, Color(0xff_00_ff)),
  (202, Color(0xff_5f_00)),
  (203, Color(0xff_5f_5f)),
  (204, Color(0xff_5f_87)),
  (205, Color(0xff_5f_af)),
  (206, Color(0xff_5f_d7)),
  (207, Color(0xff_5f_ff)),
  (208, Color(0xff_87_00)),
  (209, Color(0xff_87_5f)),
  (210, Color(0xff_87_87)),
  (211, Color(0xff_87_af)),
  (212, Color(0xff_87_d7)),
  (213, Color(0xff_87_ff)),
  (214, Color(0xff_af_00)),
  (215, Color(0xff_af_5f)),
  (216, Color(0xff_af_87)),
  (217, Color(0xff_af_af)),
  (218, Color(0xff_af_d7)),
  (219, Color(0xff_af_ff)),
  (220, Color(0xff_d7_00)),
  (221, Color(0xff_d7_5f)),
  (222, Color(0xff_d7_87)),
  (223, Color(0xff_d7_af)),
  (224, Color(0xff_d7_d7)),
  (225, Color(0xff_d7_ff)),
  (226, Color(0xff_ff_00)),
  (227, Color(0xff_ff_5f)),
  (228, Color(0xff_ff_87)),
  (229, Color(0xff_ff_af)),
  (230, Color(0xff_ff_d7)),
  (231, Color(0xff_ff_ff)),
  (232, Color(0x08_08_08)),
  (233, Color(0x12_12_12)),
  (234, Color(0x1c_1c_1c)),
  (235, Color(0x26_26_26)),
  (236, Color(0x30_30_30)),
  (237, Color(0x3a_3a_3a)),
  (238, Color(0x44_44_44)),
  (239, Color(0x4e_4e_4e)),
  (240, Color(0x58_58_58)),
  (241, Color(0x62_62_62)),
  (242, Color(0x66_66_66)),
  (243, Color(0x76_76_76)),
  (244, Color(0x80_80_80)),
  (245, Color(0x8a_8a_8a)),
  (246, Color(0x94_94_94)),
  (247, Color(0x9e_9e_9e)),
  (248, Color(0xa8_a8_a8)),
  (249, Color(0xb2_b2_b2)),
  (250, Color(0xbc_bc_bc)),
  (251, Color(0xc6_c6_c6)),
  (252, Color(0xd0_d0_d0)),
  (253, Color(0xda_da_da)),
  (254, Color(0xe4_e4_e4)),
  (255, Color(0xee_ee_ee)),

]


const (hues, hslMap) =
  block:
    var hues: array[360, seq[ColSpec]]
    var hslMap: array[TermColor8Bit, tuple[hue, idx: int]]

    for (id, hex) in colorsHex:
      let rgb = extractRGB(hex)
      var spec = ColSpec(
        rgb: rgb,
        hsv: rgbToHsv(rgb).clampedHSV(),
        id: TermColor8Bit(id)
      )

      let hue = spec.hsv.h
      hslMap[spec.id] = (hue, hues[hue].len)
      hues[hue].add spec

      assert hues[hue][hues[hue].high].id == spec.id

    for id, (hue, idx) in hslMap:
      if id.int < 16: continue
      assert hues[hue][idx].id == id

    (hues, hslMap)



func `$`(hsv: TermHSV): string = &"<{hsv.h:^3}|{hsv.s:^3}|{hsv.v:^3}>"


func toColor*(color: TermColor8Bit): Color =
  colorsHex[color.int][1]


func toHSV*(color: TermColor8Bit): TermHSV =
  let (hue, idx) = hslMap[color]
  return hues[hue][idx].hsv

func getColor(hsv: TermHSV): Option[TermColor8Bit] =
  var hsv = clampedHsv(hsv)
  if hsv.h notin 0 .. 360: return

  for sv in hues[hsv.h]:
    let ok = sv.hsv.s == hsv.s and sv.hsv.v == hsv.v
    if ok:
      # Can still store colors in range 0-16, but prioritize higher values
      if sv.id.int > 16:
        return some sv.id

      else:
        result = some sv.id


block:
  for col in TermColor8Bit(17) .. high(TermColor8Bit):
    let hsv = toHsv(col)
    let newCol = getColor(hsv)
    if not(col == newCol.get()):
      echo &"{hsv}  {toHsv(newCol.get())} | {col.int:<3} {col} == {newCol.get().int} {newCol.get()}"



func in360(v: auto): auto =
  result = v
  if result >= 360:
    result -= 360

  elif result < 0:
    result += 360

template dist(hsv: TermHSV, spec: ColSpec): float =
  abs(hsv.s - spec.hsv.s) * 5 + abs(hsv.v - spec.hsv.v)

func closestMatchingColor(
    hsv: Option[TermHSV] | array[2, Option[TermHsv]],
    dirUp: Option[bool] = none(bool)): TermColor8Bit =

  var
    colLow: Option[TermHSV]
    colHigh: Option[TermHSV]
    lookUp = dirUp.isNone() or dirUp.get() == true
    lookDown = dirUp.isNone() or dirUp.get() == false

  # echov "INPUT", hsv

  when hsv is array:
    for idx, col in hsv:
      if col.isSome():
        let color = getColor(col.get())
        if color.isSome():
          # echov col.get(), color
          return color.get()

    # echov (lookUp, lookDown)
    if lookUp:
      var h = hsv[1].get().h + 5
      while hues[h].len == 0:
        h += 5
        if h >= 360: h -= 360

      # echov "H:", h

      colHigh = some (h, hsv[1].get().s, hsv[1].get().v)

    if lookDown:
      var h = hsv[0].get().h - 5
      while hues[h].len == 0:
        h -= 5
        if h < 0: h += 360

      colLow = some (h, hsv[0].get().s, hsv[0].get().v)


    return closestMatchingColor([colLow, colHigh], dirUp)

  else:
    if hsv.isSome():
      let color = getColor(hsv.get())
      if color.isSome():
        return color.get()

    if lookUp:
      var h = hsv.get().h + 5
      while hues[h].len == 0:
        h += 5
        if h >= 360: h -= 360

      colHigh = some (h, hsv.get().s, hsv.get().v)

    if lookDown:
      var h = hsv.get().h
      while hues[h].len == 0:
        h -= 5
        if h < 0: h += 360

      colLow = some (h, hsv.get().s, hsv.get().v)

    return closestMatchingColor([colLow, colHigh], dirUp)

func complementary*(col: TermColor8Bit): tuple[main, complement: TermColor8Bit] =
  var hsv = clampedHSV(toHSV(col))
  hsv.h += 180
  if hsv.h >= 360: hsv.h -= 360
  return (col, closestMatchingColor(some hsv))

func splitComplementary*(col: TermColor8Bit):
    tuple[main, compUp, compDown: TermColor8Bit] =

  let hsv = clampedHSV(toHSV(col))

  return (
    col,
    closestMatchingColor(
      some (h: in360(hsv.h + 190), s: hsv.s, v: hsv.v), some true),
    closestMatchingColor(
      some (h: in360(hsv.h + 170), s: hsv.s, v: hsv.v), some false),
  )


func triad*(col: TermColor8Bit): array[3, TermColor8Bit] =
  let hsv = clampedHSV(toHSV(col))
  return [
    col,
    closestMatchingColor(
      some (h: in360(hsv.h + 120), s: hsv.s, v: hsv.v), some true),
    closestMatchingColor(
      some (h: in360(hsv.h + 240), s: hsv.s, v: hsv.v), some false),
  ]

func tetradic*(col: TermColor8Bit): array[4, TermColor8Bit] =
  let hsv = clampedHSV(toHSV(col))
  return [
    col,
    closestMatchingColor(
      some (h: in360(hsv.h + 180), s: hsv.s, v: hsv.v)),
    closestMatchingColor(
      some (h: in360(hsv.h + 40), s: hsv.s, v: hsv.v)),
    closestMatchingColor(
      some (h: in360(hsv.h + 180 + 4), s: hsv.s, v: hsv.v)),
  ]

func square*(col: TermColor8Bit): array[4, TermColor8Bit] =
  let hsv = clampedHSV(toHSV(col))
  return [
    col,
    closestMatchingColor(
      some (h: in360(hsv.h + 90), s: hsv.s, v: hsv.v)),
    closestMatchingColor(
      some (h: in360(hsv.h + 180), s: hsv.s, v: hsv.v)),
    closestMatchingColor(
      some (h: in360(hsv.h + 270), s: hsv.s, v: hsv.v)),
  ]

func analog*(col: TermColor8Bit, shift: int = 5): array[3, TermColor8Bit] =
  let hsv = clampedHSV(toHSV(col))
  return [
    col,
    closestMatchingColor(
      some (h: hsv.h + shift, s: hsv.s, v: hsv.v), some true),
    closestMatchingColor(
      some (h: hsv.h - shift, s: hsv.s, v: hsv.v), some false),
  ]

func inverseSrgbCompanding(c: Color): Color =
  let c = c.extractRGB()
  var (r, g, b) = (c.r / 255, c.g / 255, c.b / 255)
  if (r > 0.04045): r = pow((r + 0.055) / 1.055, 2.4) else: r = r / 12.92
  if (g > 0.04045): g = pow((g + 0.055) / 1.055, 2.4) else: g = g / 12.92
  if (b > 0.04045): b = pow((b + 0.055) / 1.055, 2.4) else: b = b / 12.92

  return rgb(int(r * 255), int(g * 255), int(b * 255))


func srgbCompanding(c: Color): Color =
  let c = c.extractRGB()
  var (r, g, b) = (c.r / 255, c.g / 255, c.b / 255)

  if (r > 0.0031308): r = 1.055 * pow(r, 1 / 2.4) - 0.055 else: r = r * 12.92
  if (g > 0.0031308): g = 1.055 * pow(g, 1 / 2.4) - 0.055 else: g = g * 12.92
  if (b > 0.0031308): b = 1.055 * pow(b, 1 / 2.4) - 0.055 else: b = b * 12.92

  return rgb(int(r * 255), int(g * 255), int(b * 255))

func colorMix*(c1, c2: Color, ratio: float): Color =
  var
    c1 = inverseSrgbCompanding(c1).extractRGB()
    c2 = inverseSrgbCompanding(c2).extractRGB()

  var
    r = c1.r * (1 - ratio) + c2.r * (ratio)
    g = c1.g * (1 - ratio) + c2.g * (ratio)
    b = c1.b * (1 - ratio) + c2.b * (ratio)

  return srgbCompanding(rgb(int(r), int(g), int(b)))

func interpolGradient*(start, finish: TermColor8Bit): seq[TermColor8Bit] =
  let
    start = toColor(start)
    finish = toColor(finish)

  const sep = 5
  for mix in 0 .. sep:
    let col = colorMix(start, finish, 1 / sep * mix)
    result.add term8Bit(col)

  sort(result)
  deduplicate(result)



func complement*(color: TermColor8Bit): TermColor8Bit =
  let (r, g, b) = color.toRGB()
  return term8Bit(5 - r, 5 - g, 5 - b)


func `$`*(col: TermColor8Bit): string =
  $toColored(&"{col.int:<3}", initStyleBg(col))


func initStyleBg*(r, g, b: range[0 .. 5]): PrintStyling {.inline.} =
  PrintStyling(use8Bit: true, bg8: term8Bit(r, g, b))

func initStyleFg*(r, g, b: range[0 .. 5]): PrintStyling {.inline.} =
  PrintStyling(use8Bit: true, fg8: term8Bit(r, g, b))

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

func alignLeft*(str: ColoredString, length: int, padding: char = ' '): string =
  result &= $str
  result &= padding.repeat(clamp(length - str.len, 0, high(int)))

func alignRight*(str: ColoredString, length: int, padding: char = ' '): string =
  result &= padding.repeat(clamp(length - str.len, 0, high(int)))
  result &= $str

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

func stripSGR*(str: string): string =
  var
    style: PrintStyling = initPrintStyling()
    prev: int = 0
    pos: int = 0
    sgrbuf: string

  while scanp(str, pos, (
    *(~ '\e'), ("\e[", ({'0' .. '9'}){1,3} -> sgrbuf.add($_), 'm'))
  ):
    let termsym = sgrbuf.len + 2
    let substr = str[prev ..< (pos - termsym - 1)]
    result &= substr
    prev = pos
    sgrbuf = ""

  if prev < pos:
    result &= str[prev ..< pos]


func splitSGR*(str: string): seq[ColoredString] =
  var
    style: PrintStyling = initPrintStyling()
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

func addToLast[T](sseq: var seq[seq[T]], val: T): void =
  if sseq.len == 0:
    sseq.add @[val]
  else:
    sseq[^1].add val


func splitSGR_sep*(str: string, sep: string = "\n"): seq[seq[ColoredString]] =
  # NOTE There are unit tests for this thing but I actually have very
  # little understanding of /why/ it actually works. I just added
  # conditsion until it passed all tests.
  let splitted = splitSGR(str)
  for idx, cstr in splitted:
    let splitl = cstr.split(sep)

    if true:
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

func `[]=`*[R1, R2: openarray[int] | Slice[int] | int](
    buf: var seq[seq[ColoredRune]],
    rowIdx: R1, colIdx: R2, ch: ColoredRune
  ): void =

  proc aux(row, col: int, buf: var ColoredRuneGrid, ch: ColoredRune) =
    for _ in buf.len .. row:
      buf.add @[coloredWhitespaceRune]

    buf[row] &= coloredWhitespaceRune.repeat(max(col - buf[row].len + 1, 0))
    buf[row][col] = ch


  var rows: seq[int]
  var cols: seq[int]

  when rowIdx is int:
    rows = @[rowIdx]

  elif rowIdx is Slice[int] | openarray[int]:
    for row in rowIdx:
      rows.add row


  when colIdx is int:
    cols = @[colIdx]

  elif colIdx is Slice[int] | openarray[int]:
    for col in colIdx:
      cols.add col

  for row in rows:
    for col in cols:
      aux(row, col, buf, ch)

func `[]=`*(buf: var ColoredRuneGrid, row, col: int, str: ColoredString) =
  var rowIdx = 0
  for line in split(str.str, '\n'):
    for colIdx, ch in line:
      buf[row + rowIdx, col + colIdx] = toColored(ch, str.styling)
    inc rowIdx

func `[]=`*(buf: var ColoredRuneGrid, row, col: int, str: string) =
  let style = initPrintStyling()
  var rowIdx = 0
  for line in split(str, '\n'):
    for colIdx, ch in line:
      buf[row + rowIdx, col + colIdx] = toColored(ch, style)

    inc rowIdx

func `[]`*(buf: ColoredRuneGrid, row, col: int): ColoredRune =
  buf[row][col]

func initRuneGrid*(): ColoredRuneGrid = discard

func getEditVisual*(src, target: seq[char], ops: seq[LevEdit]): string =
  var
    src = src
    currIdx = 0

  for op in ops:
    if currIdx < op.getPos():
      for i in currIdx ..< op.getPos():
        result.add src[i]

      currIdx = op.getPos() + 1

    case op.kind:
      of lekInsert:
        result.add toGreen($op.insertItem)

      of lekDelete:
        result.add toRed($src[op.deletePos])

      of lekReplace:
        result.add toRed($src[op.replacePos]) & toGreen($op.replaceItem)

    src.apply(op)

    if op.kind == lekDelete:
      result.add src[op.deletePos]

  for i in currIdx ..< src.len:
    result.add src[i]
