import std/[strutils]
import ./colored



proc madd*(ms: var string, args: varargs[string, `$`]) =
  for arg in args:
    ms.add arg

proc msep*(ms: var string, s: string, expr: bool) =
  if expr:
    ms.add s

func mq*[T](item: T): string = "'" & $item & "'"
func mkind*[E: enum](item: E): string = "'" & toGreenStr($item) & "'"
func mq1*[T](item: T): string = "'" & toGreenStr($item) & "'"

func mcode*(str: string): string = "\e[4m" & str & "\e[24m"

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
