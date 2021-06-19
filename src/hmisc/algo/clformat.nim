import std/[strutils, tables, enumerate, strformat]
import hseq_mapping, htext_algo
import ../base_errors
import ../types/colorstring
import ../algo/halgorithm



## Implementation of several basic functions from common lisp `format`
## macro.

# https://www.hexstreamsoft.com/articles/common-lisp-format-reference/clhs-summary/

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

func toPluralNoun*(noun: string, count: int, addNum: bool = true): string =
  ## Generate correct plural noun from string `noun`.
  ##
  ## NOTE placeholder implementation that just adds 's'
  ##
  ## - TODO :: implement algorith described here:
  ##   http://users.monash.edu/~damian/papers/HTML/Plurals.html
  ## - TODO Generate either numerical literal, or word representation
  if count == 1:
    result = noun

  else:
    result = noun & "s"

  if addNum:
    result = $count & " " & result

func joinWords*(words: seq[string], sepWord: string): string =
  case words.len:
    of 0: discard
    of 1: result = words[0]
    of 2: result = &"{words[0]} {sepWord} {words[1]}"
    else:
      for idx, word in pairs(words):

        if idx == words.high:
          result &= sepWord & " " & word

        else:
          result &= word & ", "

func namedItemListing*(
    name: string,
    words: seq[string],
    sepWord: string
  ): string =

  if words.len == 0:
    result = &"{toPluralNoun(name, 0).toLowerAscii()}"

  else:
    result = toPluralNoun(name, words.len) &
      ": " & joinWords(words, sepWord)



func toLatinNamedChar*(ch: char): seq[string] =
  ## Convert character `ch` to it's named for punctuation and control
  ## characters, othewise leave intactt. Conversion is (mostly) performed
  ## according to naming in basic latin unicode
  # https://theasciicode.com.ar/
  case ch:
    of '[': @["left", "square", "bracket"]
    of ']': @["right", "square", "bracket"]
    of '\a': @["bell"]
    of '\n': @["newline"]
    of '\v': @["vertical", "tab"]
    # of char(0x1100_0000): @["utf8", "two", "byte", "lead"]
    # of char(0x1110_0000): @["utf8", "three", "byte", "lead"]
    # of char(0x1111_0000): @["utf8", "four", "byte", "lead"]
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
    raiseArgumentError("Unicode does not provide subscript for char '" & $c & "'")

  else:
    return subSuperMap[c][0]


func toUnicodeSupChar*(c: char): string =
  if c notin subSuperMap or subSuperMap[c][1] == "":
    raiseArgumentError("Unicode does not provide superscript for char '" & $c & "'")

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

func fromTexToUnicodeMath*(tex: string): string =
  let tex = if tex.startsWith("\\"): tex[1..^1] else: tex
  case tex:
    of "sqrt": "√"
    of "sqrt[3]": "∛"
    of "sqrt[4]": "∜"
    of "infty": "∞"
    of "neq": "≔"
    of "defeq": "≝"
    of "subset": "⊂"
    of "subseteq": "⊆"
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
    of "times": "×"
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
      raiseArgumentError("Unsupported latex to unicde conversion: '" & tex & "'")
# ∜
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

type
  HDisplayVerbosity* = enum
    dvNormal
    dvMinimal
    dvVerbose
    dvDataDump

  HDisplayOpts* = object
    colored*: bool
    indent*: int
    maxDepth*: int
    quoteIdents*: bool ## Add quotes around stings that are valid identifirers
    newlineBeforeMulti*: bool
    verbosity*: HDisplayVerbosity
    dropPrefix*: bool

const defaultHDisplay* = HDisplayOpts(
  colored: true,
  dropPrefix: true,
  newlineBeforeMulti: true,
  maxDepth: -1,
  verbosity: dvNormal,
)

func hShow*(ch: char, opts: HDisplayOpts = defaultHDisplay): string =
  $ch

func hshow*(b: bool, opts: HDisplayOpts = defaultHDisplay): string =
  if b:
    toGreen($b, opts.colored)

  else:
    toRed($b, opts.colored)

func hShow*(ch: int, opts: HDisplayOpts = defaultHDisplay): string =
  toCyan($ch, opts.colored)

func hShow*[A, B](
    slice: HSlice[A, B], opts: HDisplayOpts = defaultHDisplay): string =

  "[" & hshow(slice.a, opts) & ":" & hshow(slice.b, opts) & "]"

func hShow*(str: string, opts: HDisplayOpts = defaultHDisplay): string =
  if str.len == 0:
    result = toYellow("''", opts.colored) & " (" &
      toItalic("empty string", opts.colored) & ")"

  else:
    let prefix = " ".repeat(opts.indent)
    if '\n' in str:
      for line in str.split('\n'):
        result &= "\n" & prefix & toYellow(line, opts.colored)

    else:
      result = toYellow("\"" & str & "\"", opts.colored)

func hShow*[E: enum](e: E, opts: HDisplayOpts = defaultHDisplay): string =
  if opts.dropPrefix:
    toGreen(dropLowerPrefix($e), opts.colored)

  else:
    toGreen($e, opts.colored)


when isMainModule:
  for u in ["w_p", "+infty", "-infty"]:
    echo u, " ", unicodifyIdent(u)
