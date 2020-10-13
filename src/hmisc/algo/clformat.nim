import strutils
import hseq_mapping

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
  ## TODO implement algorith described here:
  ## http://users.monash.edu/~damian/papers/HTML/Plurals.html
  if count == 1:
    return $count & " " & noun
  else:
    return $count & " " & noun & "s"

func toLatinNamedChar*(ch: char): seq[string] =
  ## Convert character `ch` to it's named for punctuation and control
  ## characters, othewise leave intactt. Conversion is (mostly) performed
  ## according to naming in basic latin unicode
  case ch:
    of '[': @["left", "square", "bracket"]
    of ']': @["right", "square", "bracket"]
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
    else: $ch

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

func toNamedMultichar*(str: string): seq[(string, string)] =
  for group in str.mergeUniqByIt(it):
    result.add((group.len.namedNumTuple(), group[0].toLatinAbbrChar()))

func toNamedMulticharJoin*(str: string, lowerStart: bool = true): string =
  for (name, ch) in str.toNamedMultichar():
    if ch.len == 1 and ch[0] in IdentChars:
      result.add ch
    else:
      result.add name.capitalizeAscii() & ch

  if lowerStart:
    result[0] = result[0].toLowerAscii()




when isMainModule:
  echo toRomanNumeral(12)
  # echo toPluralNoun("item", 0)
