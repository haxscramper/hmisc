import hmisc/types/colorstring
import hmisc/core/all
import hmisc/algo/clformat
import std/[unicode, options, macros, parseutils, strutils]

import std/strformat {.all.}

type
  ClformatFlag* = enum
    clfScientific
    clfEng
    clfRoman
    clfSign
    clfText
    clfCamel
    clfDashed
    clfSnake
    clfPascal
    clfUpper
    clfLower
    clfJoined
    clfCapitalized
    clfNamed
    clfShortNamed
    clfBroadcast
    clfIndent

  ClformatSpec* = object
    flags*: set[ClFormatFlag]
    styling*: Option[PrintStyling]
    padRune*: Rune
    sepRune*: Rune
    word*: Option[(string, string)]
    base*: StandardFormatSpecifier

  ClformatPart* = object
    case isExpr*: bool
      of true:
        expr*: string
        format*: string

      else:
        text*: string

proc newLit*(rune: Rune): NimNode =
  newCall(bindSym"Rune", newLit(rune.int))

proc newLit*[T](opt: Option[T]): NimNode =
  if opt.isSome():
    newCall("some", newLit(opt.get()))

  else:
    newCall("none", newCall("typeof", getTypeInst(T)))

proc newColoredTextOfCap*(cap: int): ColoredText = discard



func parseClformatSpec*(spec: string): ClformatSpec =
  result.base = parseStandardFormatSpecifier(
    spec, ignoreUnknownSuffix = true)

  result.sepRune = uc"_"
  result.padRune = runeAt($result.base.fill, 0)

  for arg in split(spec[result.base.endPosition .. ^1], ","):
    let
      split = strutils.strip(arg).split("=")
      key = strutils.strip(split[0])
      val =
        if len(split) == 2:
          strutils.strip(split[1]).strip(chars = Whitespace + {'\''})
        else:
          ""

    case key:
      of "":
        discard

      of "indent":      result.flags.incl clfIndent
      of "sign":        result.flags.incl clfSign
      of "roman":       result.flags.incl clfRoman
      of "text":        result.flags.incl clfText
      of "dashed":      result.flags.incl clfDashed
      of "snake":       result.flags.incl clfSnake
      of "capitalized": result.flags.incl clfCapitalized
      of "named":       result.flags.incl clfNamed
      of "short-named": result.flags.incl clfShortNamed
      of "broadcast":   result.flags.incl clfBroadcast
      of "scientific":  result.flags.incl clfScientific
      of "eng":         result.flags.incl clfEng
      of "camel":       result.flags.incl clfCamel
      of "pascal":      result.flags.incl clfPascal
      of "upper":       result.flags.incl clfUpper
      of "lower":       result.flags.incl clfLower
      of "joined":      result.flags.incl clfJoined
      of "word":
        let val = val.split("/")
        result.word = some (val[0], val.getOr(1, ""))

      of "pad":
        result.padRune = runeAt(val, 0)

      of "sep":
        result.sepRune = runeAt(val, 0)

      elif key.startsWith("fg"):
        if result.styling.isNone():
          result.styling = some default(PrintStyling)

        result.styling.get().fg = case key[3 .. ^1]:
          of "red": fgRed
          of "blue": fgBlue
          of "green": fgGreen
          of "yellow": fgYellow
          else:
            raise newUnexpectedKindError(key[3 .. ^1])

      elif key.startsWith("bg"):
        if result.styling.isNone():
          result.styling = some default(PrintStyling)

        result.styling.get().bg = case key[3 .. ^1]:
          of "red": bgRed
          of "blue": bgBlue
          of "green": bgGreen
          of "yellow": bgYellow
          else:
            raise newUnexpectedKindError(key[3 .. ^1])

      elif key.startsWith("attr"):
        if result.styling.isNone():
          result.styling = some default(PrintStyling)

        result.styling.get().style.incl do:
          case key[5 .. ^1]:
            of "bold": styleBright
            of "italic": styleItalic
            else:
              raise newUnexpectedKindError(key[3 .. ^1])

      else:
        raise newUnexpectedKindError(key)

proc addAfter(
    target: var ColoredText,
    prefix: string,
    text: ColoredText,
    repeatPrefix: bool = false
  ) =

  var prefixPart = high(prefix)
  while prefixPart < len(prefix) and
        prefix[prefixPart] != '\n' and
        1 < prefixPart:
    dec prefixPart

  if repeatPrefix:
    for line in lines(text):
      target.add prefix[prefixPart .. ^1]
      target.add line
      target.add "\n"

  else:
    target.add indentBody(text, runeLen(prefix[prefixPart .. ^1]) + 1)

proc addAfter(
    target: var ColoredText,
    prefix: string,
    text: string,
    repeatPrefix: bool = false
  ) =

  addAfter(target, prefix, clt(text), repeatPrefix)


func splitClformatParts*(
    str: string,
    openChar, closeChar: char,
    specDelimitChar: char = ':'
  ): seq[ClformatPart] =

  var i = 0
  var strlit = ""
  while i < str.len:
    if str[i] == openChar:
      inc i
      if str[i] == openChar:
        inc i
        strlit.add openChar
      else:
        if strlit.len > 0:
          result.add ClformatPart(isExpr: false, text: strlit)
          strlit = ""

        var
          subexpr = ""
          inParens = 0
          inSingleQuotes = false
          inDoubleQuotes = false

        while i < str.len and str[i] != closeChar and (str[i] != specDelimitChar or inParens != 0):
          case str[i]:
            of '\\':
              if i < str.len-1 and str[i+1] in {openChar,closeChar,specDelimitChar}:
                inc i

            of '\'':
              if not inDoubleQuotes and str[i - 1] != '\\':
                inSingleQuotes = not inSingleQuotes

            of '\"':
              if str[i - 1] != '\\':
                inDoubleQuotes = not inDoubleQuotes

            of '(':
              if not (inSingleQuotes or inDoubleQuotes):
                inc inParens

            of ')':
              if not (inSingleQuotes or inDoubleQuotes):
                dec inParens

            of '=':
              let start = i
              inc i
              i += str.skipWhitespace(i)

              if str[i] == closeChar or str[i] == specDelimitChar:
                result.add ClformatPart(
                  isExpr: false, text: subexpr & str[start ..< i])

              else:
                subexpr.add str[start ..< i]

              continue

            else:
              discard

          subexpr.add str[i]
          inc i

        var expr = ClformatPart(isExpr: true, expr: subexpr)

        if str[i] == specDelimitChar:
          inc i
          while i < str.len and str[i] != closeChar:
            expr.format.add str[i]
            inc i

        if str[i] == closeChar:
          inc i

        else:
          doAssert false, "invalid format string: missing '}'"

        result.add expr

    elif str[i] == closeChar:
      if i < str.len - 1 and str[i + 1] == closeChar:
        strlit.add closeChar
        inc i, 2

      else:
        doAssert false, "invalid format string: '$1' instead of '$1$1'" % $closeChar
        inc i

    else:
      strlit.add str[i]
      inc i

  if strlit.len > 0:
    result.add ClformatPart(isExpr: false, text: strlit)


proc strformatImpl(
    str: string;
    openChar, closeChar: char,
    initCall: NimNode,
    specDelimitChar: char         = ':',
    useAddAfter: bool             = false,
    useAddAfterRepeatPrefix: bool = false
  ): NimNode =
  if openChar == specDelimitChar or closeChar == specDelimitChar:
    error "openChar and closeChar must not be equal to the delimiter character."

  let res = genSym(nskVar, "fmtRes")
  result = newNimNode(nnkStmtListExpr)
  result.add newVarStmt(res, initCall)
  let after = bindSym"addAfter"

  for part in splitClformatParts(str, openChar, closeChar, specDelimitChar):
    if part.isExpr:
      var x: NimNode
      try:
        x = parseExpr(part.expr)
      except ValueError as e:
        error("could not parse `$#` in `$#`.\n$#" % [part.expr, str, e.msg])

      result.add newCall(
        bindSym("formatValue", brOpen),
        res,
        x,
        newLit(parseClformatSpec(part.format)))

    else:
      result.add newCall(bindSym"add", res, newLit(part.text))

  result.add res


proc formatValue*(
    result: var ColoredText,
    value: ColoredText,
    spec: ClformatSpec
  ) =

  var value = value

  assert len(spec.flags * {
    clfCamel, clfPascal, clfLower, clfUpper}) < 2

  if clfDashed in spec.flags:
    value = toDashed(value)

  elif clfSnake in spec.flags:
    for rune in mitems(value.runes):
      if rune.rune in [uc"-", uc"_"]:
        rune.rune = uc"_"

  if clfNamed in spec.flags:
    let tmp = $value
    value = clt("")
    for ch in items(tmp):
      value.add clt(toLatinNamedChar(ch).join(" "))

  elif clfShortNamed in spec.flags:
    value = clt(toNamedMulticharJoin($value))

  if clfLower in spec.flags:
    value = toLower(value)

  elif clfUpper in spec.flags:
    value = toUpper(value)

  elif clfCapitalized in spec.flags:
    if 0 < len(value):
      value.runes[0].rune = toUpper(value[0].rune)

  if spec.styling.canGet(styling):
    for rune in mitems(value.runes):
      rune.styling = styling

  if spec.base.align != '\x00':
    case spec.base.align:
      of '<':
        value = alignLeft(value, spec.base.minimumWidth, clr(spec.base.fill))

      of '>':
        value = alignRight(value, spec.base.minimumWidth, clr(spec.base.fill))

      else:
        raise newUnexpectedKindError(spec.base.align)

  if clfIndent in spec.flags:
    var indent = 0
    while indent < len(result) and
          result[high(result) - indent] != '\n':
      inc indent

    if high(result) - indent < 0:
      indent = high(result)

    if clfBroadcast in spec.flags:
      let prefix = result[(high(result) - indent) .. ^1]
      var first = true
      for line in lines(value):
        if not first:
          result.add prefix
        first = false
        result.add line
        result.newline()

    else:
      result.add indentBody(value, indent)

  else:
    result.add value

proc formatValue*[T](
    result: var ColoredText,
    value: T,
    spec: ClformatSpec
  ) =

  formatValue(result, clt($value), spec)

proc formatInt*(val: int, spec: ClformatSpec): string =
  if clfRoman in spec.flags:
    result = toRomanNumeral(val)

  elif clfEng in spec.flags:
    result = toEngNotation(val)

  elif clfText in spec.flags:
    result = toWordNotation(val)

  else:
    var radix = 10
    case spec.base.typ:
      of 'x', 'X': radix = 16
      of 'd', '\0': discard
      of 'b': radix = 2
      of 'o': radix = 8
      else:
        raise newException(ValueError,
          "invalid type in format string for number, expected one " &
          " of 'x', 'X', 'b', 'd', 'o' but got: " & spec.base.typ)

    result = formatInt(val, radix, spec.base)

  if spec.word.canGet(word):
    result.add $spec.padRune
    result.add $toPluralNoun(
      clt(word[0]),
      count = val,
      addNum = false,
      plural = clt(word[1])
    )

proc formatValue*(
    result: var ColoredText,
    value: int,
    spec: ClformatSpec
  ) =

  formatValue(result, formatInt(value, spec), spec)


macro clfmt*(
    pattern: static[string],
    addAfter: static[bool] = false,
    openChar: static[char] = '{',
    closeChar: static[char] = '}'
  ): untyped =

  let expectedGrowth =
    when defined(useNimRtl):
      0

    else:
      count(pattern, openChar) * 10

  result = strformatImpl(
    pattern,
    openChar,
    closeChar,
    newCall(bindSym"newColoredTextOfCap", newLit(pattern.len + expectedGrowth)),
    useAddAfter = addAfter,
  )
