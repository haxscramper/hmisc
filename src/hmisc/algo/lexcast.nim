import std/[strutils]

proc lcast*(s: string, t: var SomeInteger) = t.parseInt(s)
proc lcast*(s: string, t: var SomeFloat) = t.parseFloat(s)

proc lcastImpl[Target, Source](source: Source): Target =
  lcast(source, result)

template lcast*[Target](base: typed): Target =
  lcastImpl[Target, typeof(base)](base)
