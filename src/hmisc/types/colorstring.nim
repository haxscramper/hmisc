import std/[
  sequtils, strformat, strutils, unicode, enumerate, colors,
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
  s1.style == s2.style and
  s1.use8Bit == s2.use8Bit and (
    if s1.use8Bit: s1.fg8 == s2.fg8 and s1.bg8 == s2.bg8
    else: s1.fg == s2.fg and s1.bg == s2.bg
  )

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

func ansiDiff*(s1, s2: PrintStyling): string =
  if s2.fg != s1.fg:
    result &= ansiEsc(s2.fg.int)

  if s2.bg != s1.bg:
    result &= ansiEsc(s2.bg.int)

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

func toStyled*(
  str: string,
  style: PrintStyling,
  colorize: bool = not defined(plainStdout)): string =
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
  if (result.s == 0): result.h = 0
  elif (rgb.r == max): result.h = round(60.0 * (rgb.g - rgb.b) / diff).int
  elif (rgb.g == max): result.h = round(60.0 * (rgb.b - rgb.r) / diff + 120.0).int
  elif (rgb.b == max): result.h = round(60.0 * (rgb.r - rgb.g) / diff + 120.0).int
  if (result.h < 0.0): result.h += 360
  result.v = round(max * 100 / 255)
  result.s = round(result.s)

const colorsHex = [
  (0, "#000000"),
  (1, "#800000"),
  (2, "#008000"),
  (3, "#808000"),
  (4, "#000080"),
  (5, "#800080"),
  (6, "#008080"),
  (7, "#c0c0c0"),
  (8, "#808080"),
  (9, "#ff0000"),
  (10, "#00ff00"),
  (11, "#ffff00"),
  (12, "#0000ff"),
  (13, "#ff00ff"),
  (14, "#00ffff"),
  (15, "#ffffff"),
  (16, "#000000"),
  (17, "#00005f"),
  (18, "#000087"),
  (19, "#0000af"),
  (20, "#0000d7"),
  (21, "#0000ff"),
  (22, "#005f00"),
  (23, "#005f5f"),
  (24, "#005f87"),
  (25, "#005faf"),
  (26, "#005fd7"),
  (27, "#005fff"),
  (28, "#008700"),
  (29, "#00875f"),
  (30, "#008787"),
  (31, "#0087af"),
  (32, "#0087d7"),
  (33, "#0087ff"),
  (34, "#00af00"),
  (35, "#00af5f"),
  (36, "#00af87"),
  (37, "#00afaf"),
  (38, "#00afd7"),
  (39, "#00afff"),
  (40, "#00d700"),
  (41, "#00d75f"),
  (42, "#00d787"),
  (43, "#00d7af"),
  (44, "#00d7d7"),
  (45, "#00d7ff"),
  (46, "#00ff00"),
  (47, "#00ff5f"),
  (48, "#00ff87"),
  (49, "#00ffaf"),
  (50, "#00ffd7"),
  (51, "#00ffff"),
  (52, "#5f0000"),
  (53, "#5f005f"),
  (54, "#5f0087"),
  (55, "#5f00af"),
  (56, "#5f00d7"),
  (57, "#5f00ff"),
  (58, "#5f5f00"),
  (59, "#5f5f5f"),
  (60, "#5f5f87"),
  (61, "#5f5faf"),
  (62, "#5f5fd7"),
  (63, "#5f5fff"),
  (64, "#5f8700"),
  (65, "#5f875f"),
  (66, "#5f8787"),
  (67, "#5f87af"),
  (68, "#5f87d7"),
  (69, "#5f87ff"),
  (70, "#5faf00"),
  (71, "#5faf5f"),
  (72, "#5faf87"),
  (73, "#5fafaf"),
  (74, "#5fafd7"),
  (75, "#5fafff"),
  (76, "#5fd700"),
  (77, "#5fd75f"),
  (78, "#5fd787"),
  (79, "#5fd7af"),
  (80, "#5fd7d7"),
  (81, "#5fd7ff"),
  (82, "#5fff00"),
  (83, "#5fff5f"),
  (84, "#5fff87"),
  (85, "#5fffaf"),
  (86, "#5fffd7"),
  (87, "#5fffff"),
  (88, "#870000"),
  (89, "#87005f"),
  (90, "#870087"),
  (91, "#8700af"),
  (92, "#8700d7"),
  (93, "#8700ff"),
  (94, "#875f00"),
  (95, "#875f5f"),
  (96, "#875f87"),
  (97, "#875faf"),
  (98, "#875fd7"),
  (99, "#875fff"),
  (100, "#878700"),
  (101, "#87875f"),
  (102, "#878787"),
  (103, "#8787af"),
  (104, "#8787d7"),
  (105, "#8787ff"),
  (106, "#87af00"),
  (107, "#87af5f"),
  (108, "#87af87"),
  (109, "#87afaf"),
  (110, "#87afd7"),
  (111, "#87afff"),
  (112, "#87d700"),
  (113, "#87d75f"),
  (114, "#87d787"),
  (115, "#87d7af"),
  (116, "#87d7d7"),
  (117, "#87d7ff"),
  (118, "#87ff00"),
  (119, "#87ff5f"),
  (120, "#87ff87"),
  (121, "#87ffaf"),
  (122, "#87ffd7"),
  (123, "#87ffff"),
  (124, "#af0000"),
  (125, "#af005f"),
  (126, "#af0087"),
  (127, "#af00af"),
  (128, "#af00d7"),
  (129, "#af00ff"),
  (130, "#af5f00"),
  (131, "#af5f5f"),
  (132, "#af5f87"),
  (133, "#af5faf"),
  (134, "#af5fd7"),
  (135, "#af5fff"),
  (136, "#af8700"),
  (137, "#af875f"),
  (138, "#af8787"),
  (139, "#af87af"),
  (140, "#af87d7"),
  (141, "#af87ff"),
  (142, "#afaf00"),
  (143, "#afaf5f"),
  (144, "#afaf87"),
  (145, "#afafaf"),
  (146, "#afafd7"),
  (147, "#afafff"),
  (148, "#afd700"),
  (149, "#afd75f"),
  (150, "#afd787"),
  (151, "#afd7af"),
  (152, "#afd7d7"),
  (153, "#afd7ff"),
  (154, "#afff00"),
  (155, "#afff5f"),
  (156, "#afff87"),
  (157, "#afffaf"),
  (158, "#afffd7"),
  (159, "#afffff"),
  (160, "#d70000"),
  (161, "#d7005f"),
  (162, "#d70087"),
  (163, "#d700af"),
  (164, "#d700d7"),
  (165, "#d700ff"),
  (166, "#d75f00"),
  (167, "#d75f5f"),
  (168, "#d75f87"),
  (169, "#d75faf"),
  (170, "#d75fd7"),
  (171, "#d75fff"),
  (172, "#d78700"),
  (173, "#d7875f"),
  (174, "#d78787"),
  (175, "#d787af"),
  (176, "#d787d7"),
  (177, "#d787ff"),
  (178, "#d7af00"),
  (179, "#d7af5f"),
  (180, "#d7af87"),
  (181, "#d7afaf"),
  (182, "#d7afd7"),
  (183, "#d7afff"),
  (184, "#d7d700"),
  (185, "#d7d75f"),
  (186, "#d7d787"),
  (187, "#d7d7af"),
  (188, "#d7d7d7"),
  (189, "#d7d7ff"),
  (190, "#d7ff00"),
  (191, "#d7ff5f"),
  (192, "#d7ff87"),
  (193, "#d7ffaf"),
  (194, "#d7ffd7"),
  (195, "#d7ffff"),
  (196, "#ff0000"),
  (197, "#ff005f"),
  (198, "#ff0087"),
  (199, "#ff00af"),
  (200, "#ff00d7"),
  (201, "#ff00ff"),
  (202, "#ff5f00"),
  (203, "#ff5f5f"),
  (204, "#ff5f87"),
  (205, "#ff5faf"),
  (206, "#ff5fd7"),
  (207, "#ff5fff"),
  (208, "#ff8700"),
  (209, "#ff875f"),
  (210, "#ff8787"),
  (211, "#ff87af"),
  (212, "#ff87d7"),
  (213, "#ff87ff"),
  (214, "#ffaf00"),
  (215, "#ffaf5f"),
  (216, "#ffaf87"),
  (217, "#ffafaf"),
  (218, "#ffafd7"),
  (219, "#ffafff"),
  (220, "#ffd700"),
  (221, "#ffd75f"),
  (222, "#ffd787"),
  (223, "#ffd7af"),
  (224, "#ffd7d7"),
  (225, "#ffd7ff"),
  (226, "#ffff00"),
  (227, "#ffff5f"),
  (228, "#ffff87"),
  (229, "#ffffaf"),
  (230, "#ffffd7"),
  (231, "#ffffff"),
  (232, "#080808"),
  (233, "#121212"),
  (234, "#1c1c1c"),
  (235, "#262626"),
  (236, "#303030"),
  (237, "#3a3a3a"),
  (238, "#444444"),
  (239, "#4e4e4e"),
  (240, "#585858"),
  (241, "#626262"),
  (242, "#666666"),
  (243, "#767676"),
  (244, "#808080"),
  (245, "#8a8a8a"),
  (246, "#949494"),
  (247, "#9e9e9e"),
  (248, "#a8a8a8"),
  (249, "#b2b2b2"),
  (250, "#bcbcbc"),
  (251, "#c6c6c6"),
  (252, "#d0d0d0"),
  (253, "#dadada"),
  (254, "#e4e4e4"),
  (255, "#eeeeee"),
]


const (hues, hslMap) =
  block:
    var hues: array[360, seq[ColSpec]]
    var hslMap: array[TermColor8Bit, tuple[hue, idx: int]]

    for (id, hex) in colorsHex:
      let rgb = hexToRgb(hex)
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

func toHSV*(color: TermColor8Bit): TermHSV =
  let (hue, idx) = hslMap[color]
  return hues[hue][idx].hsv

func getColor(hsv: TermHSV): Option[TermColor8Bit] =
  var hsv = clampedHsv(hsv)
  if hsv.h notin 0 .. 360: return

  for sv in hues[hsv.h]:
    let ok = sv.hsv.s == hsv.s and sv.hsv.v == hsv.v
    if ok:
      return some sv.id


block:
  for col in TermColor8Bit(17) .. high(TermColor8Bit):
    let hsv = toHsv(col)
    let newCol = getColor(hsv)
    if not(col == newCol.get()): echo &"{col.int} == {newCol.get().int}"


func `$`*(col: TermColor8Bit): string =
  to8BitBg($col.int, col)



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

  echov hsv
  var
    colLow: Option[TermHSV]
    colHigh: Option[TermHSV]
    lookUp = dirUp.isNone() or dirUp.get() == true
    lookDown = dirUp.isNone() or dirUp.get() == false

  echov (lookUP, lookDown)

  when hsv is array:
    for idx, col in hsv:
      if col.isSome():
        let color = getColor(col.get())
        if color.isSome():
          echov color.get()
          return color.get()

    if lookUp:
      var h = hsv[1].get().h + 5
      while hues[h].len == 0:
        h += 5
        if h >= 360: h -= 360

      colLow = some (h, hsv[1].get().s, hsv[1].get().v)

    if lookDown:
      var h = hsv[0].get().h - 5
      while hues[h].len == 0:
        h -= 5
        if h < 0: h += 360

      colHigh = some (h, hsv[0].get().s, hsv[0].get().v)


    return closestMatchingColor([colLow, colHigh], dirUp)

  else:
    if hsv.isSome():
      let color = getColor(hsv.get())
      if color.isSome():
        return color.get()

    if lookUp:
      echov hsv.get()
      var h = hsv.get().h
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

    echov colLow
    return closestMatchingColor([colLow, colHigh], dirUp)










func closestMatchingColor*(
    hsv: TermHSV, dirUp: Option[bool] = none(bool)): TermColor8Bit =

  echov hsv
  let start = hsv.h.round5()
  var
    idx1 = 0
    idx2 = 0
    h1 = in360(start + 5)
    h2 = in360(start - 5)

  if dirUp.isNone() or dirUp.get() == true:
    while hues[h1].len == 0:
      h1 += 5

    idx1 = hues[h1].findMinIt:
      dist(hsv, it)

  if dirUp.isNone() or dirUp.get() == false:
    while hues[h2].len == 0:
      h2 -= 5

    idx2 = hues[h2].findMinIt:
      dist(hsv, it)

  if dirUp.isNone():
    if dist(hsv, hues[h2][idx2]) < dist(hsv, hues[h1][idx1]):
      result = hues[h2][idx2].id

    else:
      result = hues[h1][idx1].id

  elif dirUp.get() == false:
    result = hues[h2][idx2].id

  else:
    result = hues[h1][idx1].id


func complementary*(col: TermColor8Bit): tuple[main, complement: TermColor8Bit] =
  var hsv = clampedHSV(toHSV(col))
  hsv.h += 180
  if hsv.h >= 360: hsv.h -= 360
  return (col, closestMatchingColor(hsv))

func splitComplementary*(col: TermColor8Bit):
    tuple[main, compUp, compDown: TermColor8Bit] =

  let hsv = clampedHSV(toHSV(col))

  return (
    col,
    closestMatchingColor((h: in360(hsv.h + 190), s: hsv.s, v: hsv.v), some true),
    closestMatchingColor((h: in360(hsv.h + 170), s: hsv.s, v: hsv.v), some false),
  )


func triad*(col: TermColor8Bit): array[3, TermColor8Bit] =
  let hsv = clampedHSV(toHSV(col))
  return [
    col,
    closestMatchingColor((h: in360(hsv.h + 120), s: hsv.s, v: hsv.v), some true),
    closestMatchingColor((h: in360(hsv.h + 240), s: hsv.s, v: hsv.v), some false),
  ]

func tetradic*(col: TermColor8Bit): array[4, TermColor8Bit] =
  let hsv = clampedHSV(toHSV(col))
  return [
    col,
    closestMatchingColor((h: in360(hsv.h + 180), s: hsv.s, v: hsv.v)),
    closestMatchingColor((h: in360(hsv.h + 40), s: hsv.s, v: hsv.v)),
    closestMatchingColor((h: in360(hsv.h + 180 + 4), s: hsv.s, v: hsv.v)),
  ]

func square*(col: TermColor8Bit): array[4, TermColor8Bit] =
  let hsv = clampedHSV(toHSV(col))
  return [
    col,
    closestMatchingColor((h: in360(hsv.h + 90), s: hsv.s, v: hsv.v)),
    closestMatchingColor((h: in360(hsv.h + 180), s: hsv.s, v: hsv.v)),
    closestMatchingColor((h: in360(hsv.h + 270), s: hsv.s, v: hsv.v)),
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


func complement*(color: TermColor8Bit): TermColor8Bit =
  let (r, g, b) = color.toRGB()
  return term8Bit(5 - r, 5 - g, 5 - b)

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
  for rowIdx, line in enumerate(split(str.str, '\n')):
    for colIdx, ch in line:
      buf[row + rowIdx, col + colIdx] = toColored(ch, str.styling)

func `[]=`*(buf: var ColoredRuneGrid, row, col: int, str: string) =
  let style = initPrintStyling()
  for rowIdx, line in enumerate(split(str, '\n')):
    for colIdx, ch in line:
      buf[row + rowIdx, col + colIdx] = toColored(ch, style)

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

when isMainModule:
  startHax()
  if false:
    for base in 0 .. (ord(high(TermColor8Bit)) -
                      ord(low(TermColor8Bit))) div 4:
      for color in 0 .. 3:
        let color = TermColor8Bit(color + base * 4)
        stdout.write to8BitBg("##", color), " "
        stdout.write to8Bit(strutils.alignLeft($color, 20), color)

      stdout.write "\n"

    echo "done"


  if false:
    for gray in 0 .. 23:
      stdout.write to8BitBg(&"[{gray}]", gray)

    stdout.write("\n")

    for r in 0 .. 5:
      for g in 0 .. 5:
        for b in 0 .. 5:
          stdout.write to8BitBg(&"[{r} {g} {b}]", r, g, b)
        stdout.write("\n")
      stdout.write("\n")

  for r in 0 .. 5:
    for g in 0 .. 5:
      for b in 0 .. 5:
        let col = term8Bit(r, g, b)
        let (r1, g1, b1) = col.toRGB()
        assert r1 == r, &"{r1}, {r}"
        assert g1 == g, &"{g1}, {g}"
        assert b1 == b, &"{b1}, {b}"

  for c in 0 .. 5:
    let
      r = 5 - c
      g = c
      b = 5 - c
      col = term8Bit(r, g, b)
    echo &"{col.int:<3} ", to8Bit(&"{r} {g} {b}", col), " -> ",
          to8Bit("??", col.complement()), "; ", col.toHSV()


  if true:
    # echo term8Bit(5, 0, 5).analog()
    echo term8Bit(4, 0, 4).analog()

  if false:
    echo "analog            square            triad"
    for c in 0 .. 5:
      let
        r = 5 - c
        g = c
        b = 5 - c
        col = term8Bit(r, g, b)

      template w(expr: untyped, col: TermColor8Bit): untyped =
        stdout.write to8BitBg(expr, col), " ",
               strutils.alignLeft($col.int, 3)

      template ws(): untyped = stdout.write(" ")
      template wl(): untyped = stdout.write("\n")


      let a = col.analog()
      w("#", a[0]); ws(); w("1", a[1]); ws(); w("2", a[2]); ws()

      let s = col.square()
      w("1", s[1]); ws(); w("2", s[2]); ws(); w("3", s[3]); ws()

      let t = col.triad()
      w("1", t[1]); ws(); w("2", t[2]); ws()

      wl()
