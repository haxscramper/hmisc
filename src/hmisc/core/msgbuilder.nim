import std/[strutils, sequtils]
import ./colored



proc madd*(ms: var string, args: varargs[string, `$`]) =
  for arg in args:
    ms.add arg

proc msep*(ms: var string, s: string, expr: bool) =
  if expr:
    ms.add s

func mq*[T](item: T): string = "‘" & $item & "‘"
func mq1*[T](item: T): string = "‘" & toGreenStr($item) & "‘"

func wrapSplit(text, before, after: string): string =
  text.split(" ").mapIt(before & it & after).join(" ")

func mkind*[E: enum](item: E): string = "‘" & toGreenStr($item) & "‘"

func mcode*(str: string): string =
  wrapSplit(str, "\e[4m", "\e[24m")

func mblock*(
    str: string,
    startN, finalN: string = "‘‘‘",
    start1, final1: string = "‘"
  ): string =

  if '\n' in str:
    "\n" & startN & "\n" & str.indent(2) & "\n" & finalN & "\n"

  else:
    start1 & str & final1



func mitem*[T](s: var string, item: T) =
  s.add "'"
  s.add $item
  s.add "'"

template mwrap*(s: var string, wrap: string, body: untyped): untyped =
  s.add wrap
  body
  s.add wrap

func mexpected*[T](s: var string, item: T, expr: bool = true) =
  if expr:
    s.add ", but expected - '"
    s.add $item
    s.add "'"

func mfound*[F, E](s: var string, found: F, expected: E) =
  s.add "found "
  s.add $found
  s.add ", but expected - '"
  s.add $expected
  s.add "'"

func mfound*[F, E](found: F, expected: E): string =
  mfound(result, found, expected)

func mjoin*(args: varargs[string, `$`]): string =
  for arg in items(args):
    result.add arg

func toPluralNoun*(
    noun: string, count: int,
    addNum: bool = true,
    plural: string = ""
  ): string =
  result =
    if count == 1:       noun
    elif plural.len > 0: plural
    else:                noun & "s"

  if addNum:
    result = $count & " " & result

func joinWords*(
    words: seq[string],
    sepWord: string,
    quote: char = '\'',
    empty: string = default(string)
  ): string =

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
    words: seq[string],
    quote: char = '\'',
    prefix: string = "any of ",
    empty: string = "no",
    sepWord: string = "or",
    suffix: string = ""
  ): string =

  case words.len:
    of 0:
      result = empty

    of 1:
      result = words[0]

    else:
      result = prefix & joinWords(words, sepWord, quote) & suffix

func namedItemListing*(
    name: string,
    words: seq[string],
    sepWord: string,
    quote: char = '\x00'
  ): string =

  if words.len == 0:
    result = toPluralNoun(name, 0).toLower()

  else:
    result = toPluralNoun(name, words.len) &
      ": " & joinWords(words, sepWord, quote)

template kindToStr*(expr: typed): untyped =
  when expr is enum:
    "\e[32m" & $expr & "\e[0m"

  elif expr is string:
    "\e[34m\"" & expr & "\"\e[0m"

  else:
    "\e[32m" & $expr.kind & "\e[0m for type \e[33m" & $typeof(expr) & "\e[0m"
