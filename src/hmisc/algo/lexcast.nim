import
  std/[strutils, strformat],
  ./halgorithm,
  ../base_errors

type
  LexcastError* = object of ParseError
  LexcastFlags* = enum
    lfNone

  LexcastOpts* = object
    flags*: set[LexcastFlags]

const defaultLexcastOpts* = LexcastOpts()

proc lcast*(
    s: string,
    t: var SomeInteger,
    opts: LexcastOpts = defaultLexcastOpts
  ) =
  try:
    t = parseInt(s)
  except ValueError:
    raise newException(
      LexcastError,
      &"Cannot parse '{s}' as integer value")

proc lcast*(
    s: string,
    t: var SomeFloat,
    opts: LexcastOpts = defaultLexcastOpts) = t.parseFloat(s)

proc lcast*(
    s: string,
    t: var bool,
    opts: LexcastOpts = defaultLexcastOpts) =

  let norm = normalize(s, snkFullNormalize)
  case norm:
    of "on", "true", "yes", "y", "1", "t":
      t = true

    of "off", "false", "no", "n", "not", "0", "f":
      t = false

    else:
      raise newException(
        LexcastError,
        &"Cannot convert '{s}' (normalized form is '{norm}') to bool")

proc lcast*[T](
    s: string,
    t: var seq[T],
    opts: LexcastOpts = defaultLexcastOpts) =
  let split = s.split(",")
  for item in split:
    var tmp: T
    lcast(item, tmp, opts)
    t.add tmp

proc lcast*[R1, R2](
    s: string,
    slice: var HSlice[R1, R2],
    opts: LexcastOpts = defaultLexcastOpts) =
  let
    r1 = s[s.parseUntil('.')]
    r2 = s[s.len + 1 .. ^1]

  lcast(r1, slice.a)
  lcast(r1, slice.b)


proc lcastImpl[Target, Source](
    source: Source,
    opts: LexcastOpts = defaultLexcastOpts): Target =
  lcast(source, result, opts)

template lexcast*[Target](
    base: untyped,
    opts: LexcastOpts = defaultLexcastOpts): Target =
  lcastImpl[Target, typeof(base)](base, opts)

template lexcast*[T](
    base: T,
    opts: LexcastOpts = defaultLexcastOpts): string =
  discard
