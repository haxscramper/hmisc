import
  std/[
    strutils, tables, enumerate, strformat,
    sequtils, unicode, parseutils, math
  ]

import
  ./hseq_mapping,
  ./hseq_distance,
  ./htext_algo,
  ../macros/argpass,
  ../core/[all, algorithms],
  ../types/colorstring,
  ../algo/halgorithm

export colorstring


## Implementation of several basic functions from common lisp `format`
## macro.

# https://www.hexstreamsoft.com/articles/common-lisp-format-reference/clhs-summary/

type
  HDisplayVerbosity* = enum

    dvMinimal
    dvNormal
    dvVerbose
    dvDataDump

  HDisplayFlag* = enum
    dfColored
    dfPositionIndexed
    dfPathIndexed
    dfUnicodeNewlines
    dfUnicodePPrint
    dfWithRanges
    dfSpellEmptyStrings

    dfUseCommas
    dfUseQuotes

  HDisplayOpts* = object
    flags*: set[HDisplayFlag]
    # colored*: bool
    indent*: int
    maxDepth*: int
    maxLen*: int
    quoteIdents*: bool ## Add quotes around stings that are valid identifirers
    newlineBeforeMulti*: bool
    verbosity*: HDisplayVerbosity
    dropPrefix*: bool

const defaultHDisplay* = HDisplayOpts(
  flags: {
    dfColored, dfPositionIndexed, dfSpellEmptyStrings,
    dfUseCommas, dfUseQuotes
  },
  dropPrefix: true,
  newlineBeforeMulti: true,
  maxLen: 30,
  maxDepth: 120,
  verbosity: dvNormal,
)

func contains*(opts: HDisplayOpts, flag: HDisplayFlag): bool =
  flag in opts.flags

import std/[macros]

macro hdisplay*(body: varargs[untyped]): untyped =
  result = withFieldAssignsTo(
    ident("defaultHDisplay"), body,
    withTmp = true,
    asExpr = true
  )


func colored*(opts: HDisplayOpts): bool = dfColored in opts.flags
func positionIndexed*(opts: HDisplayOpts): bool =
  dfPositionIndexed in opts.flags

func pathIndexed*(opts: HDisplayOpts): bool =
  dfPathIndexed in opts.flags

func withRanges*(opts: HDisplayOpts): bool =
  dfWithRanges in opts.flags



const romanNumerals = [
  (1000, "M"),
  (900, "CM"),
  (500, "D"),
  (400, "CD"),
  (100, "C"),
  (90, "XC"),
  (50, "L"),
  (40, "XL"),
  (10, "X"),
  (9, "IX"),
  (5, "V"),
  (4, "IV"),
  (1, "I")
]

func toRomanNumeral*(x: int): string =
  ## Generate roman numeral string from number `x`
  var x = x
  for (num, numStr) in romanNumerals:
    result.add(numStr.repeat(x div num))
    x = x mod num

proc toEngNotation*[I: SomeInteger | SomeFloat](val: I): string =
  let power = floor log(float(val), 1000)

  let pref = case power:
    of 8: 'Y'
    of 7: 'Z'
    of 6: 'E'
    of 5: 'P'
    of 4: 'T'
    of 3: 'G'
    of 2: 'M'
    of 1: 'K'
    of -1: 'm'
    of -2: 'u'
    of -3: 'n'
    of -4: 'p'
    of -5: 'f'
    of -6: 'a'
    of -7: 'z'
    of -8: 'y'
    else: ' '

  if power == 0:
    return $val

  else:
    return $I(float(val) / pow(1000.0, power)) & pref

proc toWordNotation*(val: int64): string =
  const lookup = {
    1_000_000_000_000i64: "trillion",
    1_000_000_000i64: "billion",
    1_000_000i64: "million",
    1_000i64: "thousand",
    100i64: "hundred",
    90i64: "ninety",
    80i64: "eighty",
    70i64: "seventy",
    60i64: "sixty",
    50i64: "fifty",
    40i64: "forty",
    30i64: "thirty",
    20i64: "twenty",
    19i64: "nineteen",
    18i64: "eighteen",
    17i64: "seventeen",
    16i64: "sixteen",
    15i64: "fifteen",
    14i64: "fourteen",
    13i64: "thirteen",
    12i64: "twelve",
    11i64: "eleven",
    10i64: "ten",
    9i64: "nine",
    8i64: "eight",
    7i64: "seven",
    6i64: "six",
    5i64: "five",
    4i64: "four",
    3i64: "three",
    2i64: "two",
    1i64: "one",
  }

  if val == 0:
    return "zero"

  if val < 0:
    return "negative " & toWordNotation(-val)

  var val = val
  for (num, name) in lookup:
    var count = int(val div num)
    if (count < 1):
      continue

    if (num >= 100):
      result &= toWordNotation(count) & " "

    result &= name
    val -= count * num
    if (val > 0):
      result &= " "

func toPluralNoun*(
    noun: ColoredText, count: int,
    addNum: bool = true, plural: ColoredText = default(ColoredText)
  ): ColoredText =
  ## Generate correct plural noun from string `noun`.
  ##
  ## NOTE placeholder implementation that just adds 's'
  ##
  ## - TODO Generate either numerical literal, or word representation
  if count == 1:
    result = noun

  elif plural.len > 0:
    result = plural

  else:
    result = noun & "s"

  if addNum:
    result = $count & " " & result

func joinWords*(
    words: seq[ColoredText],
    sepWord: ColoredText,
    quote: char = '\'',
    empty: ColoredText = default(ColoredText)
  ): ColoredText =

  template put(): untyped =
    if quote != '\x00':
      result.add quote

  case words.len:
    of 0: result = empty
    of 1: put(); result &= words[0]; put()
    of 2:
      put(); result.add words[0]; put()
      result.add " "
      result.add sepWord
      result.add " "
      put(); result.add words[1]; put()

    else:
      for idx, word in pairs(words):
        if idx == words.high:
          result &= sepWord & " "
          put()
          result &= word
          put()

        else:
          put()
          result &= word
          put()
          result &= ", "

func joinAnyOf*(
    words: seq[ColoredText],
    quote: char          = '\'',
    prefix: ColoredText  = clt("any of "),
    empty: ColoredText   = clt("no"),
    sepWord: ColoredText = clt("or"),
    suffix: ColoredText  = clt("")
  ): ColoredText =

  case words.len:
    of 0:
      result = empty

    of 1:
      result = words[0]

    else:
      result = prefix & joinWords(words, sepWord, quote) & suffix

func namedItemListing*(
    name: ColoredText,
    words: seq[ColoredText],
    sepWord: ColoredText,
    quote: char = '\x00'
  ): ColoredText =

  if words.len == 0:
    result = toPluralNoun(name, 0).toLower()

  else:
    result = toPluralNoun(name, words.len) &
      ": " & joinWords(words, sepWord, quote)



func toLatinNamedChar*(ch: char): seq[string] =
  ## Convert character `ch` to it's named for punctuation and control
  ## characters, othewise leave intactt. Conversion is (mostly) performed
  ## according to naming in basic latin unicode
  # https://theasciicode.com.ar/
  case ch:
    of '\x00': @[ "null", "character" ]
    of '\x01': @[ "start", "of", "header" ]
    of '\x02': @[ "start", "of", "text" ]
    of '\x03': @[ "end", "of", "text" ]
    of '\x04': @[ "end", "of", "transmission" ]
    of '\x05': @[ "enquiry", ]
    of '\x06': @[ "acknowledgement", ]
    of '\x07': @[ "bell", ]
    of '\x08': @[ "backspace", ]
    of '\x09': @[ "horizontal ,tab" ]
    of '\x0A': @[ "line", "feed" ]
    of '\x0B': @[ "vertical", "tab" ]
    of '\x0C': @[ "form", "feed" ]
    of '\x0D': @[ "carriage", "return" ]
    of '\x0E': @[ "shift", "out" ]
    of '\x0F': @[ "shift", "in" ]
    of '\x10': @[ "data", "link", "escape" ]
    of '\x11': @[ "device", "control", "1" ]
    of '\x12': @[ "device", "control", "2" ]
    of '\x13': @[ "device", "control", "3" ]
    of '\x14': @[ "device", "control", "4" ]
    of '\x15': @[ "negative", "acknowledge" ]
    of '\x16': @[ "synchronous", "idle" ]
    of '\x17': @[ "end", "of" ,"trans.", "block" ]
    of '\x18': @[ "cancel", ]
    of '\x19': @[ "end", "of", "medium" ]
    of '\x1A': @[ "substitute", ]
    of '\x1B': @[ "escape", ]
    of '\x1C': @[ "file", "separator" ]
    of '\x1D': @[ "group", "separator" ]
    of '\x1E': @[ "record", "separator" ]
    of '\x1F': @[ "unit", "separator" ]

    of '\x20': @[ "space"  ]
    of '\x21': @[ "exclamation", "mark"  ]
    of '\x22': @[ "double", "quotation", "mark"  ]
    of '\x23': @[ "number", "sign" ]
    of '\x24': @[ "dollar", "sign"  ]
    of '\x25': @[ "percent", "sign"  ]
    of '\x26': @[ "ampersand"  ]
    of '\x27': @[ "apostrophe" ]
    of '\x28': @[ "left", "parenthesis"  ]
    of '\x29': @[ "right", "parenthesis"  ]
    of '\x2A': @[ "asterisk"  ]
    of '\x2B': @[ "plus", "sign"  ]
    of '\x2C': @[ "comma"  ]
    of '\x2D': @[ "minus", "sign" ]
    of '\x2E': @[ "period" ]
    of '\x2F': @[ "slash" ]
    of '\x30': @[ "digit", "0" ]
    of '\x31': @[ "digit", "1" ]
    of '\x32': @[ "digit", "2" ]
    of '\x33': @[ "digit", "3" ]
    of '\x34': @[ "digit", "4" ]
    of '\x35': @[ "digit", "5" ]
    of '\x36': @[ "digit", "6" ]
    of '\x37': @[ "digit", "7" ]
    of '\x38': @[ "digit", "8" ]
    of '\x39': @[ "digit", "9" ]
    of '\x3A': @[ "colon"  ]
    of '\x3B': @[ "semicolon"  ]
    of '\x3C': @[ "less", "than", "sign"  ]
    of '\x3D': @[ "equal", "sign"  ]
    of '\x3E': @[ "greater", "than", "sign"  ]
    of '\x3F': @[ "question", "mark" ]
    of '\x40': @[ "at", "sign" ]

    of '\x5B': @[ "left", "square", "bracket" ]
    of '\x5C': @[ "backslash" ]
    of '\x5D': @[ "right", "square", "bracket" ]
    of '\x5E': @[ "circumflex" ]
    of '\x5F': @[ "underscore" ]
    of '\x60': @[ "backtick" ]
    of '\x7B': @[ "left", "curly", "bracket" ]
    of '\x7C': @[ "vertical", "bar" ]
    of '\x7D': @[ "right", "curly", "bracket" ]
    of '\x7E': @[ "tilde" ]
    of '\x7F': @[ "delete" ]

    of 'A': @["capital", "A"]
    of 'B': @["capital", "B"]
    of 'C': @["capital", "C"]
    of 'D': @["capital", "D"]
    of 'E': @["capital", "E"]
    of 'F': @["capital", "F"]
    of 'G': @["capital", "G"]
    of 'H': @["capital", "H"]
    of 'I': @["capital", "I"]
    of 'J': @["capital", "J"]
    of 'K': @["capital", "K"]
    of 'L': @["capital", "L"]
    of 'M': @["capital", "M"]
    of 'N': @["capital", "N"]
    of 'O': @["capital", "O"]
    of 'P': @["capital", "P"]
    of 'Q': @["capital", "Q"]
    of 'R': @["capital", "R"]
    of 'S': @["capital", "S"]
    of 'T': @["capital", "T"]
    of 'U': @["capital", "U"]
    of 'V': @["capital", "V"]
    of 'W': @["capital", "W"]
    of 'X': @["capital", "X"]
    of 'Y': @["capital", "Y"]
    of 'Z': @["capital", "Z"]
    of 'a': @["lowercase", "a"]
    of 'b': @["lowercase", "b"]
    of 'c': @["lowercase", "c"]
    of 'd': @["lowercase", "d"]
    of 'e': @["lowercase", "e"]
    of 'f': @["lowercase", "f"]
    of 'g': @["lowercase", "g"]
    of 'h': @["lowercase", "h"]
    of 'i': @["lowercase", "i"]
    of 'j': @["lowercase", "j"]
    of 'k': @["lowercase", "k"]
    of 'l': @["lowercase", "l"]
    of 'm': @["lowercase", "m"]
    of 'n': @["lowercase", "n"]
    of 'o': @["lowercase", "o"]
    of 'p': @["lowercase", "p"]
    of 'q': @["lowercase", "q"]
    of 'r': @["lowercase", "r"]
    of 's': @["lowercase", "s"]
    of 't': @["lowercase", "t"]
    of 'u': @["lowercase", "u"]
    of 'v': @["lowercase", "v"]
    of 'w': @["lowercase", "w"]
    of 'x': @["lowercase", "x"]
    of 'y': @["lowercase", "y"]
    of 'z': @["lowercase", "z"]

    of Utf8Continuations: @["utf8", "continuation"]
    of Utf8Starts2: @["utf8", "two", "byte", "lead"]
    of Utf8Starts3: @["utf8", "three", "byte", "lead"]
    of Utf8Starts4: @["utf8", "four", "byte", "lead"]

    else: @[$ch]

func toLatinAbbrChar*(ch: char): string =
  ## Convert character `ch` to it's abbrefiated name for punctuation
  ## and control characters, othewise leave intactt. Conversion is
  ## (mostly) performed according to naming in basic latin unicode
  case ch:
    of '[': "LBrack"
    of ']': "RBrack"
    of '(': "LPar"
    of ')': "RPar"
    of '{': "LCurly"
    of '}': "RCurly"

    of '#': "Hash"
    of '@': "At"

    of '%': "Percent"
    of '*': "Asterisk"
    of ',': "Comma"
    of '\'': "Apostrophe"
    of '/': "Slash"
    of '+': "Plus"
    of '-': "Minus"
    of '\\': "Backslash"
    of '<': "LessThan"
    of '>': "GreaterThan"
    of '=': "Equal"
    of '^': "Accent"

    of '.': "Dot"
    of '|': "Pipe"
    of '&': "Ampersand"
    of '_': "Underscore"
    of '$': "Dollar"


    of 'a'..'z', 'A'..'Z', '0' .. '9': $ch
    of ' ': "Space"
    of '`': "Backtick"
    of '?': "Question"
    of '!': "Exclamation"
    of '"': "Quote"
    of '~': "Tilde"
    of ';': "Semicolon"
    of ':': "Colon"
    of '\n': "Newline"
    of '\t': "Tab"
    of '\a': "Bell"
    of '\v': "VertTab"
    of '\f': "FormFeed"
    of '\r': "CarriageRet"
    else: $ch

func toDescriptiveIdent*(
    text: string,
    override: array[char, Option[string]] =
      default(array[char, Option[string]]),
    allowed: set[char] = IdentChars
  ): string =

  for ch in text:
    if ch in allowed:
      result.add ch

    elif override[ch].isSome():
      result.add override[ch].get()

    else:
      result.add toLatinAbbrChar(ch)

const subSuperMap: Table[char, (string, string)] = toTable({
                 # subscript superscript
    '0'        : ("₀",        "⁰"      ),
    '1'        : ("₁",        "¹"      ),
    '2'        : ("₂",        "²"      ),
    '3'        : ("₃",        "³"      ),
    '4'        : ("₄",        "⁴"      ),
    '5'        : ("₅",        "⁵"      ),
    '6'        : ("₆",        "⁶"      ),
    '7'        : ("₇",        "⁷"      ),
    '8'        : ("₈",        "⁸"      ),
    '9'        : ("₉",        "⁹"      ),
    'a'        : ("ₐ",        "ᵃ"      ),
    'b'        : ("",        "ᵇ"      ),
    'c'        : ("",        "ᶜ"      ),
    'd'        : ("",        "ᵈ"      ),
    'e'        : ("ₑ",        "ᵉ"      ),
    'f'        : ("",        "ᶠ"      ),
    'g'        : ("",        "ᵍ"      ),
    'h'        : ("ₕ",        "ʰ"      ),
    'i'        : ("ᵢ",        "ⁱ"      ),
    'j'        : ("ⱼ",        "ʲ"      ),
    'k'        : ("ₖ",        "ᵏ"      ),
    'l'        : ("ₗ",        "ˡ"      ),
    'm'        : ("ₘ",        "ᵐ"      ),
    'n'        : ("ₙ",        "ⁿ"      ),
    'o'        : ("ₒ",        "ᵒ"      ),
    'p'        : ("ₚ",        "ᵖ"      ),
    'q'        : ("",         ""      ),
    'r'        : ("ᵣ",        "ʳ"      ),
    's'        : ("ₛ",        "ˢ"      ),
    't'        : ("ₜ",        "ᵗ"      ),
    'u'        : ("ᵤ",        "ᵘ"      ),
    'v'        : ("ᵥ",        "ᵛ"      ),
    'w'        : ("",        "ʷ"      ),
    'x'        : ("ₓ",        "ˣ"      ),
    'y'        : ("",        "ʸ"      ),
    'z'        : ("",         "ᶻ"      ),
    'A'        : ("",        "ᴬ"      ),
    'B'        : ("",        "ᴮ"      ),
    'C'        : ("",         ""      ),
    'D'        : ("",        "ᴰ"      ),
    'E'        : ("",        "ᴱ"      ),
    'F'        : ("",         ""      ),
    'G'        : ("",        "ᴳ"      ),
    'H'        : ("",        "ᴴ"      ),
    'I'        : ("",        "ᴵ"      ),
    'J'        : ("",        "ᴶ"      ),
    'K'        : ("",        "ᴷ"      ),
    'L'        : ("",        "ᴸ"      ),
    'M'        : ("",        "ᴹ"      ),
    'N'        : ("",        "ᴺ"      ),
    'O'        : ("",        "ᴼ"      ),
    'P'        : ("",        "ᴾ"      ),
    'Q'        : ("",         ""      ),
    'R'        : ("",        "ᴿ"      ),
    'S'        : ("",         ""      ),
    'T'        : ("",        "ᵀ"      ),
    'U'        : ("",        "ᵁ"      ),
    'V'        : ("",        "ⱽ"      ),
    'W'        : ("",        "ᵂ"      ),
    'X'        : ("",         ""      ),
    'Y'        : ("",         ""      ),
    'Z'        : ("",         ""      ),
    '+'        : ("₊",        "⁺"      ),
    '-'        : ("₋",        "⁻"      ),
    '='        : ("₌",        "⁼"      ),
    '('        : ("₍",        "⁽"      ),
    ')'        : ("₎",        "⁾"      ),
  })

func toUnicodeSubChar*(c: char): string =
  if c notin subSuperMap or subSuperMap[c][0] == "":
    raise newArgumentError("Unicode does not provide subscript for char '" & $c & "'")

  else:
    return subSuperMap[c][0]


func toUnicodeSupChar*(c: char): string =
  if c notin subSuperMap or subSuperMap[c][1] == "":
    raise newArgumentError("Unicode does not provide superscript for char '" & $c & "'")

  else:
    return subSuperMap[c][1]

const texIdents* = [
  "sqrt",
  "sqrt[3]",
  "sqrt[4]",
  "infty",
  "neq",
  "defeq",
  "subset",
  "subseteq",
  "subsetneq",
  "supset",
  "supseteq",
  "supsetneq",
  "in",
  "notin",
  "int",
  "iint",
  "iiint",
  "iiiint",
  "times",
  "lceil",
  "rceil",
  "lfloor",
  "rfloor",
  "land",
  "lor",
  "exists",
  "forall",
  "sum",
  "prod",
  "coprod",
  "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
  "+", "-", "<", ">", "=", "!", "~", "*", "%", "&", "$", "#",
  "mathbb{N}", "N",
  "mathbb{Z}", "Z",
  "mathbb{Q}", "Q",
  "mathbb{R}", "R",
  "mathbb{C}", "C",
  "mathbb{i}", "i",
  "mathbb{j}", "j",
  "e",
]

const extendedAsciiNames*: array[char, string] = [
  '\x00': "[NUL]",
  '\x01': "[SOH]",
  '\x02': "[STX]",
  '\x03': "[ETX]",
  '\x04': "[EOT]",
  '\x05': "[ENQ]",
  '\x06': "[ACK]",
  '\x07': "[BEL]",
  '\x08': "[BS]",
  '\x09': "[HT]",
  '\x0A': "[LF]",
  '\x0B': "[VT]",
  '\x0C': "[FF]",
  '\x0D': "[CR]",
  '\x0E': "[SO]",
  '\x0F': "[SI]",
  '\x10': "[DLE]",
  '\x11': "[DC1]",
  '\x12': "[DC2]",
  '\x13': "[DC3]",
  '\x14': "[DC4]",
  '\x15': "[NAK]",
  '\x16': "[SYN]",
  '\x17': "[ETB]",
  '\x18': "[CAN]",
  '\x19': "[EM]",
  '\x1A': "[SUB]",
  '\x1B': "[ESC]",
  '\x1C': "[FS]",
  '\x1D': "[GS]",
  '\x1E': "[RS]",
  '\x1F': "[US]",
  '\x20': " " ,
  '\x21': "!" ,
  '\x22': "\"" ,
  '\x23': "#" ,
  '\x24': "$" ,
  '\x25': "%" ,
  '\x26': "&" ,
  '\x27': "'" ,
  '\x28': "(" ,
  '\x29': ")" ,
  '\x2A': "*" ,
  '\x2B': "+" ,
  '\x2C': "," ,
  '\x2D': "-" ,
  '\x2E': "." ,
  '\x2F': "/" ,
  '\x30': "0" ,
  '\x31': "1" ,
  '\x32': "2" ,
  '\x33': "3" ,
  '\x34': "4" ,
  '\x35': "5" ,
  '\x36': "6" ,
  '\x37': "7" ,
  '\x38': "8" ,
  '\x39': "9" ,
  '\x3A': ":" ,
  '\x3B': ";" ,
  '\x3C': "<" ,
  '\x3D': "=" ,
  '\x3E': ">" ,
  '\x3F': "?" ,
  '\x40': "@" ,
  '\x41': "A" ,
  '\x42': "B" ,
  '\x43': "C" ,
  '\x44': "D" ,
  '\x45': "E" ,
  '\x46': "F" ,
  '\x47': "G" ,
  '\x48': "H" ,
  '\x49': "I" ,
  '\x4A': "J" ,
  '\x4B': "K" ,
  '\x4C': "L" ,
  '\x4D': "M" ,
  '\x4E': "N" ,
  '\x4F': "O" ,
  '\x50': "P" ,
  '\x51': "Q" ,
  '\x52': "R" ,
  '\x53': "S" ,
  '\x54': "T" ,
  '\x55': "U" ,
  '\x56': "V" ,
  '\x57': "W" ,
  '\x58': "X" ,
  '\x59': "Y" ,
  '\x5A': "Z" ,
  '\x5B': "[" ,
  '\x5C': "\\" ,
  '\x5D': "]" ,
  '\x5E': "^" ,
  '\x5F': "_" ,
  '\x60': "`" ,
  '\x61': "a" ,
  '\x62': "b" ,
  '\x63': "c" ,
  '\x64': "d" ,
  '\x65': "e" ,
  '\x66': "f" ,
  '\x67': "g" ,
  '\x68': "h" ,
  '\x69': "i" ,
  '\x6A': "j" ,
  '\x6B': "k" ,
  '\x6C': "l" ,
  '\x6D': "m" ,
  '\x6E': "n" ,
  '\x6F': "o" ,
  '\x70': "p" ,
  '\x71': "q" ,
  '\x72': "r" ,
  '\x73': "s" ,
  '\x74': "t" ,
  '\x75': "u" ,
  '\x76': "v" ,
  '\x77': "w" ,
  '\x78': "x" ,
  '\x79': "y" ,
  '\x7A': "z" ,
  '\x7B': "{" ,
  '\x7C': "|" ,
  '\x7D': "}" ,
  '\x7E': "~" ,
  '\x7F': "[DEL]" ,
  '\x80': " " ,
  '\x81': " " ,
  '\x82': "‚" ,
  '\x83': "ƒ" ,
  '\x84': "„" ,
  '\x85': "…" ,
  '\x86': "†" ,
  '\x87': "‡" ,
  '\x88': "ˆ" ,
  '\x89': "‰" ,
  '\x8A': "Š" ,
  '\x8B': "‹" ,
  '\x8C': "Œ" ,
  '\x8D': " " ,
  '\x8E': " " ,
  '\x8F': " " ,
  '\x90': " " ,
  '\x91': "‘" ,
  '\x92': "’" ,
  '\x93': "“" ,
  '\x94': "”" ,
  '\x95': "•" ,
  '\x96': "–" ,
  '\x97': "—" ,
  '\x98': "˜" ,
  '\x99': "™" ,
  '\x9A': "š" ,
  '\x9B': "›" ,
  '\x9C': "œ" ,
  '\x9D': " " ,
  '\x9E': " " ,
  '\x9F': "Ÿ" ,
  '\xA0': " " ,
  '\xA1': "¡" ,
  '\xA2': "¢" ,
  '\xA3': "£" ,
  '\xA4': "¤" ,
  '\xA5': "¥" ,
  '\xA6': "¦" ,
  '\xA7': "§" ,
  '\xA8': "¨" ,
  '\xA9': "©" ,
  '\xAA': "ª" ,
  '\xAB': "«" ,
  '\xAC': "¬" ,
  '\xAD': " " ,
  '\xAE': "®" ,
  '\xAF': "¯" ,
  '\xB0': "°" ,
  '\xB1': "±" ,
  '\xB2': "²" ,
  '\xB3': "³" ,
  '\xB4': "´" ,
  '\xB5': "µ" ,
  '\xB6': "¶" ,
  '\xB7': "·" ,
  '\xB8': "¸" ,
  '\xB9': "¹" ,
  '\xBA': "º" ,
  '\xBB': "»" ,
  '\xBC': "¼" ,
  '\xBD': "½" ,
  '\xBE': "¾" ,
  '\xBF': "¿" ,
  '\xC0': "À" ,
  '\xC1': "Á" ,
  '\xC2': "Â" ,
  '\xC3': "Ã" ,
  '\xC4': "Ä" ,
  '\xC5': "Å" ,
  '\xC6': "Æ" ,
  '\xC7': "Ç" ,
  '\xC8': "È" ,
  '\xC9': "É" ,
  '\xCA': "Ê" ,
  '\xCB': "Ë" ,
  '\xCC': "Ì" ,
  '\xCD': "Í" ,
  '\xCE': "Î" ,
  '\xCF': "Ï" ,
  '\xD0': "Ð" ,
  '\xD1': "Ñ" ,
  '\xD2': "Ò" ,
  '\xD3': "Ó" ,
  '\xD4': "Ô" ,
  '\xD5': "Õ" ,
  '\xD6': "Ö" ,
  '\xD7': "×" ,
  '\xD8': "Ø" ,
  '\xD9': "Ù" ,
  '\xDA': "Ú" ,
  '\xDB': "Û" ,
  '\xDC': "Ü" ,
  '\xDD': "Ý" ,
  '\xDE': "Þ" ,
  '\xDF': "ß" ,
  '\xE0': "à" ,
  '\xE1': "á" ,
  '\xE2': "â" ,
  '\xE3': "ã" ,
  '\xE4': "ä" ,
  '\xE5': "å" ,
  '\xE6': "æ" ,
  '\xE7': "ç" ,
  '\xE8': "è" ,
  '\xE9': "é" ,
  '\xEA': "ê" ,
  '\xEB': "ë" ,
  '\xEC': "ì" ,
  '\xED': "í" ,
  '\xEE': "î" ,
  '\xEF': "ï" ,
  '\xF0': "ð" ,
  '\xF1': "ñ" ,
  '\xF2': "ò" ,
  '\xF3': "ó" ,
  '\xF4': "ô" ,
  '\xF5': "õ" ,
  '\xF6': "ö" ,
  '\xF7': "÷" ,
  '\xF8': "ø" ,
  '\xF9': "ù" ,
  '\xFA': "ú" ,
  '\xFB': "û" ,
  '\xFC': "ü" ,
  '\xFD': "ý" ,
  '\xFE': "þ" ,
  '\xFF': "ÿ"
]

func asciiName*(ch: char, slash: bool = false): string =
  extendedAsciinames[ch]

func unicodeName*(ch: char): string =
  case ch:
    of '\x00': "␀" # "[NUL]",
    of '\x01': "␁" # "[SOH]",
    of '\x02': "␂" # "[STX]",
    of '\x03': "␃" # "[ETX]",
    of '\x04': "␄" # "[EOT]",
    of '\x05': "␅" # "[ENQ]",
    of '\x06': "␆" # "[ACK]",
    of '\x07': "␇" # "[BEL]",
    of '\x08': "␈" # "[BS]",
    of '\x09': "␉" # "[HT]",
    of '\x0A': "␤" # "[LF]",
    of '\x0B': "␋" # "[VT]",
    of '\x0C': "␌" # "[FF]",
    of '\x0D': "␍" # "[CR]",
    of '\x0E': "␎" # "[SO]",
    of '\x0F': "␏" # "[SI]",
    of '\x10': "␐" # "[DLE]",
    of '\x11': "␑" # "[DC1]",
    of '\x12': "␒" # "[DC2]",
    of '\x13': "␓" # "[DC3]",
    of '\x14': "␔" # "[DC4]",
    of '\x15': "␕" # "[NAK]",
    of '\x16': "␖" # "[SYN]",
    of '\x17': "␗" # "[ETB]",
    of '\x18': "␘" # "[CAN]",
    of '\x19': "␙" # "[EM]",
    of '\x1A': "␚" # "[SUB]",
    of '\x1B': "␛" # "[ESC]",
    of '\x1C': "␜" # "[FS]",
    of '\x1D': "␝" # "[GS]",
    of '\x1E': "␞" # "[RS]",
    of '\x1F': "␟" # "[US]",
    of '\x7f': "␡" # "[DEL]"
    of ' ': "␣" # Space
    else: extendedAsciiNames[ch]

func describeChar*(ch: char, opts: HDisplayOpts = defaultHDisplay): string =
  case ch:
    of { '\x00' .. '\x1F' } - { '\n', '\t' } + { '\x80' .. '\xFF' }:
      result.add "\\x"
      result.add toHex(ch.uint8)

    of '\n': result.add "\\n"
    of '\t': result.add "\\t"
    else: result.add $ch

  if dvNormal <= opts.verbosity:
    result.add " ("
    result.add toLatinNamedChar(ch).join(" ")
    result.add ")"



import pkg/unicodedb

func describeChar*(rune: Rune): string =
  result.add $rune
  result.add " ("
  result.add rune.name().toLowerAscii()
  result.add ")"

func describeSet*[S](
    s: set[S], sets: openarray[(set[S], string)]): string =
  var buf: seq[string]
  var left = s

  for (keys, name) in sets:
    if len(keys * left) == len(keys):
      buf.add name
      left.excl keys

  for ch in left:
    buf.add toLatinNamedChar(ch).join(" ")

  return $joinAnyOf(
    words = mapIt(buf, clt(it)),
    empty = clt("no characters"))

func describeCharset*(s: set[char]): string =
  let sets = {
    { 'a' .. 'z', 'A' .. 'Z' }: "lower/upper-case",
    { 'a' .. 'z' }: "lowercase",
    { 'A' .. 'Z' }: "uppercse",
    { '0' .. '9' }: "digit"
  }

  describeSet[char](s, sets)



const AsciiMath* = (
  sqrt: "√",
  times: "×",
  sqrt3: "∛",
  sqrt4: "∜",
  infty: "∞",
  neq: "≔",
  defeq: "≝",
  subset: "⊂",
  subseteq: "⊆",
  subsetneq: "⊈",
  supset: "⊃",
  supseteq: "⊇",
  supsetneq: "⊉",
  setin: "∈",
  setnotin: "∉",
  integral: "∫",
  integral2: "∬",
  integral3: "∭",
  integral4: "⨌",
  lceil:"⌈",
  rceil:"⌉",
  lfloor: "⌊",
  rfloor: "⌋",
  land: "∧",
  lor: "∨",
  exists: "∃",
  forall: "∀",
  sum: "∑",
  prod: "∏",
  coprod: "∐",
  mathN: "ℕ",
  mathZ: "ℤ",
  mathQ: "ℚ",
  mathR: "ℝ",
  mathC: "ℂ",
  mathi: "ⅈ",
  mathj: "ⅉ",
  mathe: "ℯ",
)

func fromTexToUnicodeMath*(tex: string): string =
  let tex = if tex.startsWith("\\"): tex[1..^1] else: tex
  case tex:
    of "sqrt": AsciiMath.sqrt
    of "sqrt[3]": AsciiMath.sqrt3
    of "sqrt[4]": AsciiMath.sqrt4
    of "infty": AsciiMath.infty
    of "neq": AsciiMath.neq
    of "defeq": AsciiMath.defeq
    of "subset": AsciiMath.subset
    of "subseteq": AsciiMath.subseteq
    of "subsetneq": "⊈"
    of "supset": "⊃"
    of "supseteq": "⊇"
    of "supsetneq": "⊉"
    of "in": "∈"
    of "notin": "∉"
    of "int": "∫"
    of "iint": "∬"
    of "iiint": "∭"
    of "iiiint": "⨌"
    of "times": AsciiMath.times
    of "lceil": "⌈"
    of "rceil": "⌉"
    of "lfloor": "⌊"
    of "rfloor": "⌋"
    of "land": "∧"
    of "lor": "∨"
    of "exists": "∃"
    of "forall": "∀"
    of "sum": "∑"
    of "prod": "∏"
    of "coprod": "∐"
    of "0", "1", "2", "3", "4", "5", "6", "7", "8", "9": tex
    of "+", "-", "<", ">", "=", "!", "~", "*", "%", "&", "$", "#": tex
    of "mathbb{N}", "N": "ℕ"
    of "mathbb{Z}", "Z": "ℤ"
    of "mathbb{Q}", "Q": "ℚ"
    of "mathbb{R}", "R": "ℝ"
    of "mathbb{C}", "C": "ℂ"
    of "mathbb{i}", "i": "ⅈ"
    of "mathbb{j}", "j": "ⅉ"
    of "e": "ℯ"
    else:
      raise newArgumentError("Unsupported latex to unicde conversion: '" & tex & "'")

# ⅈ, ⅉ ℯ, ⅇ ℇ ∞ ⧜ ⧝ ⧞
#  ∋  ∌ ⋶ ⋽ ⋲ ⋺ ⋳ ⋻
#    ⊅⊄     ⊊ ⊋ ⫅ ⫆ ⫋ ⫌
# ≠ = ⩵ ⩶
# ≔ ≕ ⩴
# ≝ ≞ ≟ ≎ ≏ ⪮
# ¬ ⫬ ⫭ ⊨ ⊭  ∁  ∄ ∴ ∵ ⊦ ⊬ ⊧ ⊩ ⊮ ⊫ ⊯ ⊪
#   ⊻ ⊼ ⊽ ⋎ ⋏ ⟑ ⟇ ⩑ ⩒ ⩓ ⩔ ⩕ ⩖ ⩗ ⩘ ⩙ ⩚ ⩛ ⩜ ⩝ ⩞ ⩟ ⩠ ⩢ ⩣ ⨇ ⨈
# ⋀ ⋁
#  ✕ ✖ ⨰ ⨱ ⨲ ⨳ ⦻ ⨴ ⨵ ⨶ ⨷
#
#  ⨊ ⨁
# ⨀ ⨂ ∏ ∐ ⨉
#     ⫍ ⫎


# ╓ ╥ ╖
# ╟ ╫ ╢
# ╙ ╨ ╜
# ┍ ┯ ┑
# ┝ ┿ ┥
# ┕ ┷ ┙


# ╆ ╅
# ╄ ╃

# ┲ ┱
# ┺ ┹
# ┢ ╈ ┪
# ╊ ╋ ╉
# ┡ ╇ ┩

const CharBox* = (
  regular: (
    upLeft: "┌", downLeft: "└", downRight: "┘", upRight: "┐", center: "┼",
    vertical: "│", horizontal: "─",
    topCross: "┬", bottomCross: "┴", leftCross: "├", rightCross: "┤"
  ),
  bold: (
    upLeft: "┏", downLeft: "┗", downRight: "┛", upRight: "┓", center: "╋",
    vertical: "┃", horizontal: "━",
    topCross: "┳", bottomCross: "┻", leftCross: "┣", rightCross: "┫"
  ),
  double: (
    upLeft: "╔", downLeft: "╚", downRight: "╝", upRight: "╗", center: "╬",
    vertical: "║", horizontal: "═",
    topCross: "╦", bottomCross: "╩", leftCross: "╠", rightCross: "╣"
  ),
  doubleHoriz: (
    upLeft: "╒", downLeft: "╘", downRight: "┘", upRight: "╕", center: "╪",
    vertical: "│", horizontal: "─",
    topCross: "╤", bottomCross: "╧", leftCross: "╞", rightCross: "╡"
  ),
  doubleVert: (
    upLeft: "┌", downLeft: "└", downRight: "┘", upRight: "┐", center: "┼",
    vertical: "│", horizontal: "─",
    topCross: "┬", bottomCross: "┴", leftCross: "├", rightCross: "┤"
  ),
  wedged: (
    upLeft: "┌", downLeft: "└", downRight: "┘", upRight: "┐", center: "┼",
    vertical: "│", horizontal: "─",
    topCross: "┬", bottomCross: "┴", leftCross: "├", rightCross: "┤"
  ),
  wedgedVert: (
    upLeft: "┌", downLeft: "└", downRight: "┘", upRight: "┐", center: "┼",
    vertical: "│", horizontal: "─",
    topCross: "┬", bottomCross: "┴", leftCross: "├", rightCross: "┤"
  ),
  wedgedHoriz: (
    upLeft: "┌", downLeft: "└", downRight: "┘", upRight: "┐", center: "┼",
    vertical: "│", horizontal: "─",
    topCross: "┬", bottomCross: "┴", leftCross: "├", rightCross: "┤"
  )
)

const CharBrace* = (
  asciiRound: (left: "(", right: ")"),
  asciiSquare: (left: "[", right: "]"),
  asciiCurlty: (left: "{", right: "}"),
  asciiAngle: (left: "<", right: ">"),


  doubleRound: (left: "⦅", right: "⦆"),
  doubleSquare: (left: "⟦", right: "⟧"),
  doubleCurly: (left: "⦃", right: "⦄"),
  doubleAngle: (left: "《", right: "》"),

  ucAngle: (left: "〈", right: "〉"),

  mediumRound: (left: "❨", right: "❩" ),
  mediumRound2: (left: "❪", right: "❫" ),
  mediumCurly: (left: "❴", right: "❵"),
  mediumAngle: (left: "❮", right: "❯" ),
  mediumAngle2: (left: "❬", right: "❭" ),
  mediumAngle3: (left: "❰", right: "❱"),

  # doubleCurly: (left: "⦃", right: "⦄"),
  # doubleCurly: (left: "⦃", right: "⦄"),
  # doubleCurly: (left: "⦃", right: "⦄"),
  # doubleCurly: (left: "⦃", right: "⦄"),
  # doubleCurly: (left: "⦃", right: "⦄"),
  # doubleCurly: (left: "⦃", right: "⦄"),
  # doubleCurly: (left: "⦃", right: "⦄"),
  # doubleCurly: (left: "⦃", right: "⦄"),
)
# White variants

# Western quotation “ ” ‘ ’ ‹ › « »
# unmatched quotation „
# Full width brackets （ ） ［ ］ ｛ ｝ ｟ ｠
# Asian 「 」 〈    【 】 〔 〕 ⦗ ⦘
# Asian white variant 『 』 〖 〗 〘 〙
# Half width variant ｢ ｣
# Math ⟦ ⟧ ⟨ ⟩ ⟪ ⟫ ⟮ ⟯ ⟬ ⟭ ⌈ ⌉ ⌊ ⌋ ⦇ ⦈ ⦉ ⦊

# Decorative ❛ ❜ ❝ ❞ ❨ ❩ ❪ ❫ ❴ ❵ ❬ ❭ ❮ ❯ ❰ ❱ ❲ ❳
# Arabic ornate parenthesis. (You need Arabic font) ﴾ ﴿
# More angle brackets 〈 〉 ⦑ ⦒ ⧼ ⧽
# Small variants ﹙ ﹚ ﹛ ﹜ ﹝ ﹞
# superscript, subscript variants ⁽ ⁾ ₍ ₎
# Square bracket variants ⦋ ⦌ ⦍ ⦎ ⦏ ⦐ ⁅ ⁆
# ⸢ ⸣ ⸤ ⸥
# Misc brackets ⟅ ⟆ ⦓ ⦔ ⦕ ⦖ ⸦ ⸧ ⸨ ⸩ ⧘ ⧙ ⧚ ⧛


type
  AsciiStyle* = enum
    asRegular = 0
    asInverted = 1
    asFraktur = 2
    asBoldFraktur = 3
    asDoubleStruck = 4
    asBold = 5
    asItalic = 6
    asBoldItalic = 7
    asScript = 8
    asBoldScript = 9
    asSansSerif = 10
    asBoldSansSerif = 11
    asItalicSansSerif = 12
    asItalicBoldSansSerif = 13
    asMonospace = 14


const styledAscii =
  block:
    var table: array[char, array[AsciiStyle, string]]

                 # 0    1    2     3     4     5      6     7     8     9     10    11    12
    table['A'] = ["A", "Ɐ", "𝔄",  "𝕬",  "𝔸",  "𝐀",  "𝐴",  "𝑨",  "𝒜",  "𝓐",  "𝖠",  "𝗔",  "𝘈",  "𝘼",  "𝙰"]
    table['B'] = ["B", "B", "𝔅",  "𝕭",  "𝔹",  "𝐁",  "𝐵",  "𝑩",  "ℬ",  "𝓑",  "𝖡",  "𝗕",  "𝘉",  "𝘽",  "𝙱"]
    table['C'] = ["C", "Ɔ", "ℭ",  "𝕮",  "ℂ",  "𝐂",  "𝐶",  "𝑪",  "𝒞",  "𝓒",  "𝖢",  "𝗖",  "𝘊",  "𝘾",  "𝙲"]
    table['D'] = ["D", "D", "𝔇",  "𝕯",  "𝔻",  "𝐃",  "𝐷",  "𝑫",  "𝒟",  "𝓓",  "𝖣",  "𝗗",  "𝘋",  "𝘿",  "𝙳"]
    table['E'] = ["E", "Ǝ", "𝔈",  "𝕰",  "𝔼",  "𝐄",  "𝐸",  "𝑬",  "ℰ",  "𝓔",  "𝖤",  "𝗘",  "𝘌",  "𝙀",  "𝙴"]
    table['F'] = ["F", "Ⅎ", "𝔉",  "𝕱",  "𝔽",  "𝐅",  "𝐹",  "𝑭",  "ℱ",  "𝓕",  "𝖥",  "𝗙",  "𝘍",  "𝙁",  "𝙵"]
    table['G'] = ["G", "⅁", "𝔊",  "𝕲",  "𝔾",  "𝐆",  "𝐺",  "𝑮",  "𝒢",  "𝓖",  "𝖦",  "𝗚",  "𝘎",  "𝙂",  "𝙶"]
    table['H'] = ["H", "H", "ℌ",  "𝕳",  "ℍ",  "𝐇",  "𝐻",  "𝑯",  "ℋ",  "𝓗",  "𝖧",  "𝗛",  "𝘏",  "𝙃",  "𝙷"]
    table['I'] = ["I", "I", "ℑ",  "𝕴",  "𝕀",  "𝐈",  "𝐼",  "𝑰",  "ℐ",  "𝓘",  "𝖨",  "𝗜",  "𝘐",  "𝙄",  "𝙸"]
    table['J'] = ["J", "ſ", "𝔍",  "𝕵",  "𝕁",  "𝐉",  "𝐽",  "𝑱",  "𝒥",  "𝓙",  "𝖩",  "𝗝",  "𝘑",  "𝙅",  "𝙹"]
    table['K'] = ["K", "Ʞ", "𝔎",  "𝕶",  "𝕂",  "𝐊",  "𝐾",  "𝑲",  "𝒦",  "𝓚",  "𝖪",  "𝗞",  "𝘒",  "𝙆",  "𝙺"]
    table['L'] = ["L", "Ꞁ", "𝔏",  "𝕷",  "𝕃",  "𝐋",  "𝐿",  "𝑳",  "ℒ",  "𝓛",  "𝖫",  "𝗟",  "𝘓",  "𝙇",  "𝙻"]
    table['M'] = ["M", "Ɯ", "𝔐",  "𝕸",  "𝕄",  "𝐌",  "𝑀",  "𝑴",  "ℳ",  "𝓜",  "𝖬",  "𝗠",  "𝘔",  "𝙈",  "𝙼"]
    table['N'] = ["N", "N", "𝔑",  "𝕹",  "ℕ",  "𝐍",  "𝑁",  "𝑵",  "𝒩",  "𝓝",  "𝖭",  "𝗡",  "𝘕",  "𝙉",  "𝙽"]
    table['O'] = ["O", "O", "𝔒",  "𝕺",  "𝕆",  "𝐎",  "𝑂",  "𝑶",  "𝒪",  "𝓞",  "𝖮",  "𝗢",  "𝘖",  "𝙊",  "𝙾"]
    table['P'] = ["P", "Ԁ", "𝔓",  "𝕻",  "ℙ",  "𝐏",  "𝑃",  "𝑷",  "𝒫",  "𝓟",  "𝖯",  "𝗣",  "𝘗",  "𝙋",  "𝙿"]
    table['Q'] = ["Q", "Ò", "𝔔",  "𝕼",  "ℚ",  "𝐐",  "𝑄",  "𝑸",  "𝒬",  "𝓠",  "𝖰",  "𝗤",  "𝘘",  "𝙌",  "𝚀"]
    table['R'] = ["R", "ᴚ", "ℜ",  "𝕽",  "ℝ",  "𝐑",  "𝑅",  "𝑹",  "ℛ",  "𝓡",  "𝖱",  "𝗥",  "𝘙",  "𝙍",  "𝚁"]
    table['S'] = ["S", "S", "𝔖",  "𝕾",  "𝕊",  "𝐒",  "𝑆",  "𝑺",  "𝒮",  "𝓢",  "𝖲",  "𝗦",  "𝘚",  "𝙎",  "𝚂"]
    table['T'] = ["T", "Ʇ", "𝔗",  "𝕿",  "𝕋",  "𝐓",  "𝑇",  "𝑻",  "𝒯",  "𝓣",  "𝖳",  "𝗧",  "𝘛",  "𝙏",  "𝚃"]
    table['U'] = ["U", "∩", "𝔘",  "𝖀",  "𝕌",  "𝐔",  "𝑈",  "𝑼",  "𝒰",  "𝓤",  "𝖴",  "𝗨",  "𝘜",  "𝙐",  "𝚄"]
    table['V'] = ["V", "Ʌ", "𝔙",  "𝖁",  "𝕍",  "𝐕",  "𝑉",  "𝑽",  "𝒱",  "𝓥",  "𝖵",  "𝗩",  "𝘝",  "𝙑",  "𝚅"]
    table['W'] = ["W", "ʍ", "𝔚",  "𝖂",  "𝕎",  "𝐖",  "𝑊",  "𝑾",  "𝒲",  "𝓦",  "𝖶",  "𝗪",  "𝘞",  "𝙒",  "𝚆"]
    table['X'] = ["X", "X", "𝔛",  "𝖃",  "𝕏",  "𝐗",  "𝑋",  "𝑿",  "𝒳",  "𝓧",  "𝖷",  "𝗫",  "𝘟",  "𝙓",  "𝚇"]
    table['Y'] = ["Y", "⅄", "𝔜",  "𝖄",  "𝕐",  "𝐘",  "𝑌",  "𝒀",  "𝒴",  "𝓨",  "𝖸",  "𝗬",  "𝘠",  "𝙔",  "𝚈"]
    table['Z'] = ["Z", "Z", "ℨ",  "𝖅",  "ℤ",  "𝐙",  "𝑍",  "𝒁",  "𝒵",  "𝓩",  "𝖹",  "𝗭",  "𝘡",  "𝙕",  "𝚉"]
    table['a'] = ["a", "ɐ",  "𝔞",  "𝖆",  "𝕒",  "𝐚",  "𝑎",  "𝒂",  "𝒶",  "𝓪",  "𝖺",  "𝗮",  "𝘢",  "𝙖",  "𝚊"]
    table['b'] = ["b", "q", "𝔟",  "𝖇",  "𝕓",  "𝐛",  "𝑏",  "𝒃",  "𝒷",  "𝓫",  "𝖻",  "𝗯",  "𝘣",  "𝙗",  "𝚋"]
    table['c'] = ["c", "ɔ",  "𝔠",  "𝖈",  "𝕔",  "𝐜",  "𝑐",  "𝒄",  "𝒸",  "𝓬",  "𝖼",  "𝗰",  "𝘤",  "𝙘",  "𝚌"]
    table['d'] = ["d", "p",  "𝔡",  "𝖉",  "𝕕",  "𝐝",  "𝑑",  "𝒅",  "𝒹",  "𝓭",  "𝖽",  "𝗱",  "𝘥",  "𝙙",  "𝚍"]
    table['e'] = ["e", "ǝ",  "𝔢",  "𝖊",  "𝕖",  "𝐞",  "𝑒",  "𝒆",  "ℯ",  "𝓮",  "𝖾",  "𝗲",  "𝘦",  "𝙚",  "𝚎"]
    table['f'] = ["f", "ɟ",  "𝔣",  "𝖋",  "𝕗",  "𝐟",  "𝑓",  "𝒇",  "𝒻",  "𝓯",  "𝖿",  "𝗳",  "𝘧",  "𝙛",  "𝚏"]
    table['g'] = ["g", "ᵷ",  "𝔤",  "𝖌",  "𝕘",  "𝐠",  "𝑔",  "𝒈",  "ℊ",  "𝓰",  "𝗀",  "𝗴",  "𝘨",  "𝙜",  "𝚐"]
    table['h'] = ["h", "ɥ",  "𝔥",  "𝖍",  "𝕙",  "𝐡",  "ℎ",  "𝒉",  "𝒽",  "𝓱",  "𝗁",  "𝗵",  "𝘩",  "𝙝",  "𝚑"]
    table['i'] = ["i", "ᴉ",  "𝔦",  "𝖎",  "𝕚",  "𝐢",  "𝑖",  "𝒊",  "𝒾",  "𝓲",  "𝗂",  "𝗶",  "𝘪",  "𝙞",  "𝚒"]
    table['j'] = ["j", "f",  "𝔧",  "𝖏",  "𝕛",  "𝐣",  "𝑗",  "𝒋",  "𝒿",  "𝓳",  "𝗃",  "𝗷",  "𝘫",  "𝙟",  "𝚓"]
    table['k'] = ["k", "ʞ",  "𝔨",  "𝖐",  "𝕜",  "𝐤",  "𝑘",  "𝒌",  "𝓀",  "𝓴",  "𝗄",  "𝗸",  "𝘬",  "𝙠",  "𝚔"]
    table['l'] = ["l", "ꞁ",  "𝔩",  "𝖑",  "𝕝",  "𝐥",  "𝑙",  "𝒍",  "𝓁",  "𝓵",  "𝗅",  "𝗹",  "𝘭",  "𝙡",  "𝚕"]
    table['m'] = ["m", "ɯ",  "𝔪",  "𝖒",  "𝕞",  "𝐦",  "𝑚",  "𝒎",  "𝓂",  "𝓶",  "𝗆",  "𝗺",  "𝘮",  "𝙢",  "𝚖"]
    table['n'] = ["n", "u",  "𝔫",  "𝖓",  "𝕟",  "𝐧",  "𝑛",  "𝒏",  "𝓃",  "𝓷",  "𝗇",  "𝗻",  "𝘯",  "𝙣",  "𝚗"]
    table['o'] = ["o", "o",  "𝔬",  "𝖔",  "𝕠",  "𝐨",  "𝑜",  "𝒐",  "ℴ",  "𝓸",  "𝗈",  "𝗼",  "𝘰",  "𝙤",  "𝚘"]
    table['p'] = ["p", "d",  "𝔭",  "𝖕",  "𝕡",  "𝐩",  "𝑝",  "𝒑",  "𝓅",  "𝓹",  "𝗉",  "𝗽",  "𝘱",  "𝙥",  "𝚙"]
    table['q'] = ["q", "b",  "𝔮",  "𝖖",  "𝕢",  "𝐪",  "𝑞",  "𝒒",  "𝓆",  "𝓺",  "𝗊",  "𝗾",  "𝘲",  "𝙦",  "𝚚"]
    table['r'] = ["r", "ɹ",  "𝔯",  "𝖗",  "𝕣",  "𝐫",  "𝑟",  "𝒓",  "𝓇",  "𝓻",  "𝗋",  "𝗿",  "𝘳",  "𝙧",  "𝚛"]
    table['s'] = ["s", "s",  "𝔰",  "𝖘",  "𝕤",  "𝐬",  "𝑠",  "𝒔",  "𝓈",  "𝓼",  "𝗌",  "𝘀",  "𝘴",  "𝙨",  "𝚜"]
    table['t'] = ["t", "ʇ",  "𝔱",  "𝖙",  "𝕥",  "𝐭",  "𝑡",  "𝒕",  "𝓉",  "𝓽",  "𝗍",  "𝘁",  "𝘵",  "𝙩",  "𝚝"]
    table['u'] = ["u", "n",  "𝔲",  "𝖚",  "𝕦",  "𝐮",  "𝑢",  "𝒖",  "𝓊",  "𝓾",  "𝗎",  "𝘂",  "𝘶",  "𝙪",  "𝚞"]
    table['v'] = ["v", "ʌ",  "𝔳",  "𝖛",  "𝕧",  "𝐯",  "𝑣",  "𝒗",  "𝓋",  "𝓿",  "𝗏",  "𝘃",  "𝘷",  "𝙫",  "𝚟"]
    table['w'] = ["w", "ʍ",  "𝔴",  "𝖜",  "𝕨",  "𝐰",  "𝑤",  "𝒘",  "𝓌",  "𝔀",  "𝗐",  "𝘄",  "𝘸",  "𝙬",  "𝚠"]
    table['x'] = ["x", "x",  "𝔵",  "𝖝",  "𝕩",  "𝐱",  "𝑥",  "𝒙",  "𝓍",  "𝔁",  "𝗑",  "𝘅",  "𝘹",  "𝙭",  "𝚡"]
    table['y'] = ["y", "ʎ",  "𝔶",  "𝖞",  "𝕪",  "𝐲",  "𝑦",  "𝒚",  "𝓎",  "𝔂",  "𝗒",  "𝘆",  "𝘺",  "𝙮",  "𝚢"]
    table['z'] = ["z", "z",  "𝔷",  "𝖟",  "𝕫",  "𝐳",  "𝑧",  "𝒛",  "𝓏",  "𝔃",  "𝗓",  "𝘇",  "𝘻",  "𝙯",  "𝚣"]
    table['0'] = ["0", "0",   "0",   "0",  "𝟘",  "𝟎",  "0",   "0",   "0",  "0",   "0",   "𝟢",  "𝟬",  "0",  "𝟶"]
    table['1'] = ["1", "1",   "1",   "1",  "𝟙",  "𝟏",  "1",   "1",   "1",  "1",   "1",   "𝟣",  "𝟭",  "1",  "𝟷"]
    table['2'] = ["2", "2",   "2",   "2",  "𝟚",  "𝟐",  "2",   "2",   "2",  "2",   "2",   "𝟤",  "𝟮",  "2",  "𝟸"]
    table['3'] = ["3", "3",   "3",   "3",  "𝟛",  "𝟑",  "3",   "3",   "3",  "3",   "3",   "𝟥",  "𝟯",  "3",  "𝟹"]
    table['4'] = ["4", "4",   "4",   "4",  "𝟜",  "𝟒",  "4",   "4",   "4",  "4",   "4",   "𝟦",  "𝟰",  "4",  "𝟺"]
    table['5'] = ["5", "5",   "5",   "5",  "𝟝",  "𝟓",  "5",   "5",   "5",  "5",   "5",   "𝟧",  "𝟱",  "5",  "𝟻"]
    table['6'] = ["6", "6",   "6",   "6",  "𝟞",  "𝟔",  "6",   "6",   "6",  "6",   "6",   "𝟨",  "𝟲",  "6",  "𝟼"]
    table['7'] = ["7", "7",   "7",   "7",  "𝟟",  "𝟕",  "7",   "7",   "7",  "7",   "7",   "𝟩",  "𝟳",  "7",  "𝟽"]
    table['8'] = ["8", "8",   "8",   "8",  "𝟠",  "𝟖",  "8",   "8",   "8",  "8",   "8",   "𝟪",  "𝟴",  "8",  "𝟾"]
    table['9'] = ["9", "9",   "9",   "9",  "𝟡",  "𝟗",  "9",   "9",   "9",  "9",   "9",   "𝟫",  "𝟵",  "9",  "𝟿"]


    table

func isTextChar*(c: char): bool = c in {'0' .. '9', 'a' .. 'z', 'A' .. 'Z'}

func toStylizedAscii*(c: char, style: AsciiStyle): string =
  assert c.isTextChar()
  return styledAscii[c][style]


func namedCardinal*(num: int): string =
  ## Generated named cardinal number from integer
  case num:
    of 0: "zero"
    of 1: "one"
    of 2: "two"
    of 3: "three"
    of 4: "four"
    of 5: "five"
    of 6: "six"
    of 7: "seven"
    of 8: "eight"
    of 9: "nine"
    else: "TODO-IMPLEMENT"

func namedNumTuple*(num: int): string =
  ## I have no idea how this thing is named correctly, but you get
  ## `1 -> single`, `2 -> double`, `3 -> triple` etc. TODO implement
  ## for `n > 3`
  case num:
    of 1: "single"
    of 2: "double"
    of 3: "triple"
    else: "TODO"

func toNamedMultichar*(str: string): seq[(string, string, int)] =
  for group in str.mergeUniqByIt(it):
    result.add((
      group.len.namedNumTuple(),
      group[0].toLatinAbbrChar(),
      group.len()
    ))

func toNamedMulticharJoin*(
    str: string, lowerStart: bool = true, singlename: bool = false
  ): string =

  for (name, ch, grLen) in str.toNamedMultichar():
    if ch.len == 1 and ch[0] in IdentChars:
      result.add ch
    else:
      if grLen == 1 and not singlename:
        result.add ch
      else:
        result.add name.capitalizeAscii() & ch

  if lowerStart:
    result[0] = result[0].toLowerAscii()

func fromTexToUnicode*(
  str: string, isSub: bool = false, isSup: bool = false): string =
  for idx, ch in enumerate(split(str, " ")):
    if idx != 0:
      result &= " "

    if '_' in ch:
      for idx, split in enumerate (ch.split("_")):
        result &= fromTexToUnicode(
          split,
          isSub = idx > 0,
          isSup = isSup
        )
    elif '^' in ch:
      for idx, split in enumerate(ch.split("^")):
        result &= fromTexToUnicode(
          split,
          isSup = idx > 0,
          isSub = isSub
        )

    else:
      if isSup:
        for c in ch:
          result &= toUnicodeSupChar(c)

      elif isSub:
        for c in ch:
          result &= toUnicodeSubChar(c)

      else:
        try:
          result &= ch.fromTexToUnicodeMath()

        except:
          result &= ch

func unicodifyIdent*(str: string): string =
  for pref in texIdents:
    if str.startsWith(pref):
      return fromTexToUnicodeMath(pref) & unicodifyIdent(
        str[pref.len .. ^1])

  return fromTexToUnicode(str)

func hFormat*[T](s: openarray[T]): string =
  ## - TODO :: Add formatting options
  result &= "["
  for idx, item in pairs(s):
    if idx > 0:
      result &= " "

    result &= $item

  result &= "]"


func hShow*(ch: char, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  if dfUseQuotes in opts:
    result.add "'" + fgYellow

  result.add unicodeName(ch) + fgYellow

  if dfUseQuotes in opts:
    result.add "'" + fgYellow

func hshow*(b: bool, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  if b: $b + fgGreen else: $b + fgRed

func hShow*(
    ch: SomeInteger, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  $ch + fgCyan

func hshow*(i: BackwardsIndex, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  toCyan("^" & $i.int, opts.colored)

func hshow*(ch: float, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  toMagenta($ch, opts.colored)

func hShow*(ch: Slice[int], opts: HDisplayOpts = defaultHDisplay): ColoredText =
  if ch.a == low(int):
    result.add toCyan("low(int)", opts.colored)

  else:
    result.add toCyan($ch.a, opts.colored)

  result.add ".."

  if ch.b == high(int):
    result.add toCyan("high(int)", opts.colored)

  else:
    result.add toCyan($ch.b, opts.colored)

func hShow*[A, B](
    slice: HSlice[A, B], opts: HDisplayOpts = defaultHDisplay): ColoredText =

  "[" & hshow(slice.a, opts) & ":" & hshow(slice.b, opts) & "]"

func hshowItems*[T](
    expr: T,
    opts: HDisplayOpts = defaultHDisplay): seq[ColoredText] =

  for item in items(expr):
    result.add hshow(item, opts)

func joinBracket*(
    values: seq[ColoredText],
    opts: HDisplayOpts = defaultHDisplay
  ): ColoredText =

  result.add CharBrace.doubleSquare.left
  for idx, item in pairs(values):
    if idx > 0:
      if dfUseCommas in opts.flags:
        result.add ", "

      else:
        result.add " "

    result.add item
  result.add CharBrace.doubleSquare.right




func hshow*[T](s: seq[T], opts: HDisplayOpts = defaultHDisplay): ColoredText =
  hshowItems(s, opts).joinBracket(opts)

func hshow*[R, V](
  s: array[R, V], opts: HDisplayOpts = defaultHDisplay): ColoredText =
  hshowItems(s, opts).joinBracket(opts)

func hshow*[E: enum, V](
    s: array[E, Option[V]],
    opts: HDisplayOpts = defaultHDisplay
  ): ColoredText =

  var buf: seq[ColoredText]
  for key, val in pairs(s):
    if val.isSome():
      buf.add hshow(key, opts) & ": " & hshow(val.get(), opts)

  return joinBracket(buf)


import std/sequtils

func replaceTailNewlines*(
    buf: var ColoredText,
    replaceWith: ColoredRune = uc"⮒" + defaultPrintStyling
  ): int {.discardable.} =
  var nlCount = 0
  while nlCount < buf.len and buf[buf.high - nlCount].isNewline():
    inc nlCount

  let base = buf.len
  buf.runes.setLen(buf.len - nlCount)

  for nl in 0 ..< nlCount:
    buf.add replaceWith

  return nlCount

func replaceNewlines*(
    buf: ColoredText,
    replaceWith: ColoredRune = uc"⮒" + defaultPrintStyling
  ): ColoredText =
  for rune in buf:
    if isNewline(rune):
      result.add replaceWith

    else:
      result.add rune

func addIndent*(
    res: var ColoredText,
    level: int, sep: int = 2,
    prefix: ColoredRune = clr(' ')
  ) =
  if sep == 2 and prefix == clr(' '):
    case level:
      of 0:  res &=  toColoredTExt("")
      of 1:  res &=  toColoredTExt("  ")
      of 2:  res &=  toColoredTExt("    ")
      of 3:  res &=  toColoredTExt("      ")
      of 4:  res &=  toColoredTExt("        ")
      of 5:  res &=  toColoredTExt("          ")
      of 6:  res &=  toColoredTExt("            ")
      of 7:  res &=  toColoredTExt("              ")
      of 8:  res &=  toColoredTExt("                ")
      of 9:  res &=  toColoredTExt("                  ")
      of 10: res &=  toColoredTExt("                    ")
      else: res &= repeat(prefix, level * sep)

  else:
    res &= repeat(prefix, level * sep)


template coloredResult*(): untyped =
  var outPtr: ptr ColoredText = addr result
  template endResult(): untyped =
    when nimvm:
      return outPtr[]

    else:
      discard

  template add(arg: untyped): untyped {.used.} = outPtr[].add arg
  template add(arg1, arg2: untyped): untyped {.used.} =
    outPtr[].add(arg1, arg2)


  template addIndent(level: int, sep: int = 2): untyped {.used.} =
    outPtr[].addIndent(level, sep)

  template addi(ind: int, arg: untyped): untyped {.used.} =
    outPtr[].addIndent(ind, 2)
    outPtr[].add(arg)

func joinPrefix*(
    level: int, idx: seq[int],
    pathIndexed, positionIndexed: bool
  ): ColoredText =

  if pathIndexed:
    result = clt(idx.join("", ("[", "]")) & "  ")

  elif positionIndexed:
    if level > 0:
      result.add "  ".repeat(level - 1)
      result.add to8Bit(align($idx[^1], 2, '#'), 10)
      # result.add to8Bit("/" & alignLeft($level, 2), 20)
      # result.add " "

    # else:
    #   result.add "  "

  else:
    result.addIndent(level)

func joinPrefix*(
    level: int, idx: seq[int], opts: HDisplayOpts): ColoredText =
  joinPrefix(level, idx, opts.pathIndexed(), opts.positionIndexed())

func hShow*(
    str: string, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  if str.len == 0:
    if dfSpellEmptyStrings in opts.flags:
      result = toYellow("''", opts.colored) & " (" &
        toItalic("empty string", opts.colored) & ")"

    else:
      if dfUseQuotes in opts:
        result = toYellow("''")

  else:
    if '\n' in str:
      var str = toYellow(str)
      let onlyTail = str.onlyTailNewline()
      if onlyTail and dfUseQuotes in opts:
        result.add toYellow("\"")

      result.add str
      replaceTailNewlines(result, uc"⮒" + (fgRed + bgDefault))

      if onlyTail and dfUseQuotes in opts:
        result.add toYellow("\"")


    else:
      if dfUseQuotes in opts:
        result = toYellow("\"" & str & "\"", opts.colored)

      else:
        result = toYellow(str, opts.colored)

func hshow*(s: cstring, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  hshow($s, opts)

func hShow*[E: enum](e: E, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  if opts.dropPrefix:
    let nop = dropLowerPrefix($e)
    if nop.len > 0:
      toGreen(nop, opts.colored)

    else:
      toGreen($e, opts.colored)

  else:
    toGreen($e, opts.colored)

func hshow*(
    n: typeof(nil), opts: HDisplayOpts = defaultHDisplay): ColoredText =

  toRed("<nil>")

func hShow*[I](s: set[I], opts: HDisplayOpts = defaultHDisplay): ColoredText =
  result.add toBlue("{")
  for idx, item in pairs(s):
    if idx > 0:
      result.add ", "

    result.add hshow(item, opts)

  result.add toBlue("}")

func describeStrPos*(
    base: string,
    pos: int,
    before: int = 6,
    after: int = 12,
    opts: HDisplayOpts = defaultHDisplay
  ): ColoredText =

  let
    after = base[
      min(pos + 1, base.high) ..< min(base.len, pos + after)]

    before = base[
      max(min(pos - before, base.high), 0) ..< min(pos, base.high)]

  if pos < 0:
    result = clt("positioned before string start - " & $pos)

    result.add " (first part is - "
    if opts.colored():
      result.add hshow(@after, opts)

    else:
      result.add after

    result.add ")"

  elif base.high < pos:
    result = clt("positioned after string end - " & $pos)
    result.add " (last part is - "
    if opts.colored():
      result.add hshow(@before, opts)

    else:
      result.add before

    result.add ")"

  else:
    let at = base[pos]

    result.add $pos
    result.add "/"
    result.add $base.high
    result.add " "

    if opts.colored():
      result.add hshow(@before, opts)
      result.add " "
      result.add hshow(at)
      result.add " "
      result.add hshow(@after, opts)

    else:
      result.add before
      result.add " "
      result.add at
      result.add " "
      result.add after


func formatStringified*(str: string): string =
  if str.len == 0:
    return "'' (empty string)"

  elif str[0] == ' ' or str[^1] == ' ':
    result.add "\'"
    result.add str
    result.add "\'"

  elif str.len == 1 and str[0] in { '\x80' .. '\xFF' }:
    result.add str[0].describeChar()

  elif (str[0] in Utf8Starts2 and str.len == 2) or
       (str[0] in Utf8Starts3 and str.len == 3) or
       (str[0] in Utf8Starts4 and str.len == 4):
    result.add "\'"
    result.add str
    result.add "\' ("
    result.add runeAt(str, 0).name().toLowerAscii()
    result.add ")"


  else:
    return str


func wrap*(text: ColoredText, around: ColorTextConvertible): ColoredText =
  result.add around
  result.add text
  result.add around

func joinc*(text: seq[ColoredText], sep: string): ColoredText =
  for idx, item in pairs(text):
    if 0 < idx:
      result.add sep

    result.add item

func getEditVisual*[T](
    src, target: seq[T],
    ops: seq[LevEdit],
    conv: proc(t: T): string,
    opts: HDisplayOpts = defaultHDisplay
  ): ColoredText =

  coloredResult()

  for group in sweepGroupByIt(ops, it.kind):
    case group[0].kind:
      of lekUnchanged:
        for op in group:
          add conv(src[op.sourcePos])

      of lekNone:
        raise newUnexpectedKindError(group[0])

      of lekInsert:
        for op in group:
          add toGreen(conv target[op.targetPos])

      of lekDelete:
        for op in group:
          add toRed(conv src[op.sourcePos])

      of lekReplace:
        var sourceBuf, targetBuf: ColoredText
        for op in group:
          sourceBuf.add toYellow(conv src[op.sourcePos])
          targetBuf.add toYellow(conv target[op.targetPos])

        add "["
        add sourceBuf
        add "->"
        add targetBuf
        add "]"

func stringEditMessage*(
    source, target: string,
    detailed: bool = true,
    windowSize: int = 4,
    longThreshold: int = 6
  ): ColoredText =
  ## - @arg{windowSize} :: For long strings only show `+/-<window size>`
  ##   characters around the edit. 'long' string should have at least
  ##   @arg{longThreshold} characters before and after the edit area.

  let (source, target) = (source.toSeq(), target.toSeq())

  let (distance, operations) = levenshteinDistance(source, target)

  let edit = getEditVisual(
    source, target, operations,
    proc(c: char): string = tern(detailed, asciiName(c), $c))

  return edit

func stringMismatchMessage*(
    input: string,
    expected: openarray[string],
    colored: bool = true,
    fixSuggestion: bool = true,
    showAll: bool = false,
  ): ColoredText =
  ## - TODO :: Better heuristics for missing/extraneous prefix/suffix

  let expected = deduplicate(expected)

  if expected.len == 0:
    return clt("No matching alternatives")

  var results: seq[tuple[
    edits: tuple[distance: int, operations: seq[LevEdit[char]]],
    target: string
  ]]

  for str in expected:
    if str == input:
      return

    else:
      results.add (
        levenshteinDistance(input.toSeq(), str.toSeq()),
        str
      )

  results = sortedByIt(results, it.edits.distance)

  let best = results[0]

  if best.edits.distance > int(input.len.float * 0.8):
    result = &"No close matches to {toRed(input, colored)}, possible " &
      namedItemListing(
        clt("alternative"),
        results[0 .. min(results.high, 3)].mapIt(
          it.target.toYellow().wrap("''")),
        clt("or")
      )

  else:
    result = clt(&"Did you mean to use '{toYellow(best.target, colored)}'?")

    if fixSuggestion:
      if best.edits.operations.len < min(3, input.len div 2):
        result &= " (" & getEditVisual(
          toSeq(input),
          toSeq(best.target),
          best.edits.operations,
          dollar[char]
        ) & ")"

      else:
        result &= clt(
          &" ({toRed(input, colored)} -> {toGreen(best.target, colored)})")

    if showAll and expected.len > 1:
      result &= " ("
      for idx, alt in results[1 ..^ 1]:
        if idx > 0:
          result &= " "

        result &= (toItalic(alt.target, colored) & "?") + tcGrey63

      result &= ")"

proc colorDollar*[T](arg: T): ColoredText = toColoredText($arg)

func splitKeepSpaces*(str: string): seq[string] =
  # NOTE copy-pasted from `hstring_algo/splitTokenize`. If any bugs found
  # here, edit original implementation and copy-paste things back.
  var prev = 0
  var curr = 0
  while curr < str.len:
    if str[curr] in {' '}:
      if prev != curr:
        result.add str[prev ..< curr]

      prev = curr
      while curr < str.high and str[curr + 1] == str[curr]:
        inc curr

      result.add str[prev .. curr]
      inc curr
      prev = curr

    else:
      inc curr

  if prev < curr:
    result.add str[prev ..< curr]

proc formatDiffed*[T](
    ops: seq[LevEdit[T]],
    oldSeq, newSeq: seq[T],
    maxUnchanged: int = 5
  ): tuple[oldLine, newLine: ColoredText] =

  var unchanged = 0
  for idx, op in ops:
    case op.kind:
      of lekUnchanged:
        if unchanged < maxUnchanged:
          result.oldLine.add oldSeq[op.sourcePos]
          result.newLine.add newSeq[op.targetPos]
          inc unchanged

      of lekDelete:
        result.oldLine.add toRed(oldSeq[op.sourcePos])
        unchanged = 0

      of lekInsert:
        result.newLine.add toGreen(newSeq[op.targetPos])
        unchanged = 0

      of lekReplace:
        result.oldLine.add toYellow(oldSeq[op.sourcePos])
        result.newLine.add toYellow(newSeq[op.targetPos])
        unchanged = 0

      of lekNone:
        raise newUnexpectedKindError(op)



proc formatDiffed*[T](
    shifted: ShiftedDiff,
    oldSeq, newSeq: openarray[T],
    strConv: proc(t: T): string               = dollar[T],
    maxUnchanged: int                         = 5,
    maxUnchangedWords: int                    = high(int),
    showLines: bool                           = false,
    wordSplit: proc(str: string): seq[string] = splitKeepSpaces,
    stackLongLines: int                       = high(int)
  ): ColoredText =

  ## - @arg{stackLongLines} :: If any of two diffed lines are longer than
  ##   threshold, display then one on top of another instead of side by
  ##   side

  var
    oldText, newText: seq[ColoredText]
    lhsMax = 0

  let maxLhsIdx = len($shifted.oldShifted[^1].item)
  let maxRhsIdx = len($shifted.newShifted[^1].item)

  proc editFmt(fmt: DiffShiftKind, idx: int, isLhs: bool): ColoredText =
    if showLines:
      let num =
        if fmt == dskEmpty:
          alignRight(clt(" "), maxLhsIdx)

        elif isLhs:
          alignRight(clt($idx), maxLhsIdx)

        else:
          alignRight(clt($idx), maxRhsIdx)

      case fmt:
        of dskDelete: "- " & num
        of dskInsert: "+ " & num
        of dskKeep: "~ " & num
        of dskEmpty: "? " & num

    else:
      case fmt:
        of dskDelete: clt("- ")
        of dskInsert: clt("+ ")
        of dskKeep: clt("~ ")
        of dskEmpty: clt("? ")


  var unchanged = 0
  for (lhs, rhs) in zip(shifted.oldShifted, shifted.newShifted):
    var add = false
    if lhs.kind == dskKeep and rhs.kind == dskKeep:
      if unchanged < maxUnchanged:
        add = true
        inc unchanged

    else:
      add = true
      unchanged = 0

    if add:
      oldText.add editFmt(lhs.kind, lhs.item, true)
      newText.add editFmt(rhs.kind, rhs.item, false)

    if lhs.kind == dskDelete and rhs.kind == dskInsert:
      let
        oldSplit: seq[string] = wordSplit(strConv(oldSeq[lhs.item]))
        newSplit: seq[string] = wordSplit(strConv(newSeq[rhs.item]))
        (oldLine, newLine) = formatDiffed(
          levenshteinDistance[string](oldSplit, newSplit).operations,
          oldSplit, newSplit, maxUnchanged = maxUnchangedWords
        )

      oldText.last().add oldLine
      newText.last().add newLine


    elif rhs.kind == dskInsert:
      oldText.last().add strConv(oldSeq[lhs.item])
      newText.last().add toGreen(strConv(newSeq[rhs.item]))

    elif lhs.kind == dskDelete:
      oldText.last().add strConv(oldSeq[lhs.item]).toRed()
      newText.last().add strConv(newSeq[rhs.item]).toGreen()

    else:
      if add:
        oldText.last().add strConv(oldSeq[lhs.item])
        newText.last().add strConv(newSeq[rhs.item])

    if add:
      lhsMax = max(oldText[^1].len, lhsMax)

  for (lhs, rhs) in zip(oldtext, newtext):
    if max(len(lhs), len(rhs)) > stackLongLines:
      result.add "@@"
      result.add lhs
      result.add "\n@@"
      result.add rhs
      result.add "\n"

    else:
      result.add alignLeft(lhs, lhsMax + 3)
      result.add rhs
      result.add "\n"




when isMainModule:
  for u in ["w_p", "+infty", "-infty"]:
    echo u, " ", unicodifyIdent(u)
