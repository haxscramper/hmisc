import packages/docutils/highlite
import colorstring
import std/[tables, strutils]

export toString

type StyleMap* = Table[TokenClass, PrintStyling]

func colorize*(
    map: StyleMap,
    tok: tuple[class: TokenClass, text: string]
  ): ColoredString =

  if tok.class in map:
    initColoredString(tok.text, map[tok.class])

  else:
    initColoredString(tok.text, initPrintStyling())

func colorize*(map: StyleMap, text: string, lang: string): seq[ColoredString] =
  var toks: GeneralTokenizer
  initGeneralTokenizer(toks, text)
  let lang = getSourceLanguage(lang)
  var prev = 0
  while true:
    getNextToken(toks, lang)
    if prev + 1 < toks.start:
      result.add initColoredString(
        text[prev ..< toks.start], initPrintStyling())

    prev = toks.start + toks.length - 1

    case toks.kind
      of gtEof:
        break

      else:
        let str = substr(text, toks.start, toks.length + toks.start - 1)
        result.add map.colorize((toks.kind, str))

let cppStyleMap* = {
  gtEof                : initStyle(),
  gtNone               : initStyle(),
  gtWhitespace         : initStyle(),
  gtDecNumber          : initStyle(fgBlue),
  gtBinNumber          : initStyle(fgBlue),
  gtHexNumber          : initStyle(fgBlue),

  gtOctNumber          : initStyle(fgBlue),
  gtFloatNumber        : initStyle(fgBlue),
  gtIdentifier         : initStyle(),
  gtKeyword            : initStyle(fgRed),
  gtStringLit          : initStyle(fgYellow),

  gtLongStringLit      : initStyle(fgYellow),
  gtCharLit            : initStyle(fgYellow),
  gtEscapeSequence     : initStyle(fgYellow),
  gtOperator           : initStyle(),
  gtPunctuation        : initStyle(),

  gtComment            : initStyle(tcGrey62),
  gtLongComment        : initStyle(tcGrey62),
  gtRegularExpression  : initStyle(),
  gtTagStart           : initStyle(),
  gtTagEnd             : initStyle(),
  gtKey                : initStyle(),

  gtValue              : initStyle(),
  gtRawData            : initStyle(),
  gtAssembler          : initStyle(),
  gtPreprocessor       : initStyle(fgGreen),
  gtDirective          : initStyle(),
  gtCommand            : initStyle(),

  gtRule               : initStyle(),
  gtHyperlink          : initStyle(),
  gtLabel              : initStyle(),
  gtReference          : initStyle(),
  gtOther              : initStyle(),
}.toTable()


let nimStyleMap* = {
  gtEof                : initStyle(),
  gtNone               : initStyle(),
  gtWhitespace         : initStyle(),
  gtDecNumber          : initStyle(fgBlue),
  gtBinNumber          : initStyle(fgBlue),
  gtHexNumber          : initStyle(fgBlue),

  gtOctNumber          : initStyle(fgBlue),
  gtFloatNumber        : initStyle(fgBlue),
  gtIdentifier         : initStyle(),
  gtKeyword            : initStyle(fgRed),
  gtStringLit          : initStyle(fgYellow),

  gtLongStringLit      : initStyle(fgYellow),
  gtCharLit            : initStyle(fgYellow),
  gtEscapeSequence     : initStyle(fgYellow),
  gtOperator           : initStyle(),
  gtPunctuation        : initStyle(),

  gtComment            : initStyle(tcGrey62),
  gtLongComment        : initStyle(tcGrey62),
  gtRegularExpression  : initStyle(),
  gtTagStart           : initStyle(),
  gtTagEnd             : initStyle(),
  gtKey                : initStyle(),

  gtValue              : initStyle(),
  gtRawData            : initStyle(),
  gtAssembler          : initStyle(),
  gtPreprocessor       : initStyle(),
  gtDirective          : initStyle(),
  gtCommand            : initStyle(),

  gtRule               : initStyle(),
  gtHyperlink          : initStyle(),
  gtLabel              : initStyle(),
  gtReference          : initStyle(),
  gtOther              : initStyle(),
}.toTable()


func eqIdent(a, b: string): bool =
  a.cmpIgnoreStyle(b) == 0

func eqIdent(str: string, ids: varargs[string]): bool =
  for id in ids:
    if str.eqIdent(id):
      return true

func colorizeToStr*(str: string, map: StyleMap, lang: string): string =
  var lang = lang
  if lang.eqIdent(["cpp", "cxx"]):
    lang = "C++"

  return map.colorize(str, lang).toString()


proc colorizeToStr*(str: string, lang: string): string =
  var lang = lang
  if lang.eqIdent(["cpp", "cxx", "hpp"]):
    lang = "C++"

  var map: StyleMap
  if lang.eqIdent("C++"):
    map = cppStyleMap

  elif lang.eqIdent("nim"):
    map = nimStyleMap

  else:
    return str

  return map.colorize(str, lang).toString()
