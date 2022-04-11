import
  std/[strutils, strformat, parseutils]

import
  ./halgorithm,
  ../core/all

type
  LexcastError* = object of ParseError
  LexcastFlags* = enum
    lfNone

  LexcastOpts* = object
    flags*: set[LexcastFlags]

const defaultLexcastOpts* = LexcastOpts()

proc setSign(i: var SomeInteger, pos: bool) =
  # echov i, pos
  when i is SomeUnsignedInt:
    if not pos:
      i = (high(typeof(i)) - i) + 1

  else:
    if (pos and i < 0) or (not pos and 0 < i):
      i = -i

    # when (i is uint8) or
    #      (i is uint16) or
    #      (i is uint32) or
    #      (i is uint64):
    # i = -i






proc lcast*[I: SomeInteger](
    s: string,
    t: var I,
    opts: LexcastOpts = defaultLexcastOpts
  ) =
  let offs = (if s[0] in { '-', '+' }: 1 else: 0)
  let neg = s[0] in {'-'}
  if 2 < s.len and s[offs] in {'0'} and s[offs + 1] in {'x', 'X'}:
    let parsed = parseHex(s, t, start = 2 + offs)
    # FIXME raise exception if number of parsed digits does not match with
    # full input length.
    setSign(t, not neg)

  else:
    try:
      t = I(parseInt(s))

    except ValueError:
      raise newException(
        LexcastError,
        &"Cannot parse '{s}' as integer value")

proc lcast*(
    s: string,
    t: var SomeFloat,
    opts: LexcastOpts = defaultLexcastOpts
  ) =
  t = parseFloat(s)

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
