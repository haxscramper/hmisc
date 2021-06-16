import
  std/[strutils, strformat],
  ./halgorithm,
  ../base_errors

type
  LexcastError* = object of ParseError

proc lcast*(s: string, t: var SomeInteger) = t.parseInt(s)
proc lcast*(s: string, t: var SomeFloat) = t.parseFloat(s)
proc lcast*(s: string, t: var bool) =
  let norm = normalize(s, snkFullNormalize)
  case norm:
    of "on", "true", "yes", "y":
      t = true

    of "off", "false", "no", "n", "not":
      t = false

    else:
      raise newException(
        LexcastError,
        &"Cannot convert '{s}' (normalized form is '{norm}') to bool")


proc lcast*[R1, R2](s: string, slice: var HSlice[R1, R2]) =
  let
    r1 = s[s.parseUntil('.')]
    r2 = s[s.len + 1 .. ^1]

  lcast(r1, slice.a)
  lcast(r1, slice.b)



proc lcastImpl[Target, Source](source: Source): Target =
  lcast(source, result)

template lcast*[Target](base: typed): Target =
  lcastImpl[Target, typeof(base)](base)
