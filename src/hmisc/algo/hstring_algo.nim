import std/[sequtils, strformat, strutils,
            parseutils, macros, algorithm]

import ../base_errors

type
  StrBackIndex* = distinct string
  CharBackIndex* = distinct char
  StrPartKind* = enum
    spkSet
    spkSubstr


  StrPart* = object
    case kind: StrPartKind
      of spkSet:
        chars*: set[char]
      of spkSubstr:
        strs*: seq[string]

  StrPartTuple* = tuple[lhs, rhs: StrPart]
  StrPartConv* = char | set[char] | string | seq[string] |
    openarray[string]

converter toStrPart*(c: char): StrPart =
  StrPart(kind: spkSet, chars: {c})

converter toStrPart*(s: string): StrPart =
  StrPart(kind: spkSubstr, strs: @[s])

converter toStrPart*(s: openarray[string]): StrPart =
  StrPart(kind: spkSubstr, strs: toSeq(s))

converter toStrPart*(cs: set[char]): StrPart =
  StrPart(kind: spkSet, chars: cs)

converter toStrPartTuple*[A: StrPartConv, B: StrPartConv](
  indata: (A, B)): StrPartTuple =

  (
    lhs: toStrPart(indata[0]),
    rhs: toStrPart(indata[1])
  )


func startsWith*(s: string, part: StrPart): bool =
  case part.kind:
    of spkSet:
      return (s.len > 0) and (s[0] in part.chars)
    else:
      for elem in part.strs:
        if s.startsWith(elem):
          return true


      return false

func endsWith*(s: string, part: Strpart): bool =
  case part.kind:
    of spkSet:
      return (s.len > 0) and (s[^1] in part.chars)
    else:
      for elem in part.strs:
        if s.endsWith(elem):
          return true


      return false

{.push inline.}

func `^`*(s: string): StrBackIndex = StrBackIndex(s)
func `^`*(s: char): CharBackIndex = CharBackIndex(s)

func `[]`*(ins: string, back: StrBackIndex): bool =
  ins.endsWith(back.string)

func `[]`*(ins: string, back: CharBackIndex): bool =
  ins.endsWith(back.char)

func `[]`*(ins: string, forward: string|char): bool =
  ins.startsWith(toStrPart(forward))

func `[]`*(ins: string, strs: openarray[string]): bool =
  ins.startsWith(strs)

func `[]`*(ins: string, beg: StrPart, final: StrPartConv): bool =
  ins.startsWith(beg) and ins.endsWith(toStrPart(final))

func `[]`*(ins: string, beg: StrPart, final: openarray[string]): bool =
  ins[beg, toSeq(final)]


iterator items*(part: StrPart): StrPart =
  case part.kind:
    of spkSet:
      for ch in part.chars:
        yield toStrPart(ch)
    of spkSubstr:
      for s in part.strs:
        yield toStrPart(s)

func len*(part: StrPart): int {.inline.} =
  case part.kind:
    of spkSet:
      if part.chars.len == 0:
        0
      else:
        1
    of spkSubstr:
      if part.strs.len == 1:
        part.strs[0].len
      elif part.strs.len == 0:
        0
      else:
        raiseArgumentError(
          "Cannot get length for string part with more that one substring")


func contains*(str: string, parts: varargs[StrPart, toStrPart]): bool =
  for part in parts:
    case part.kind:
      of spkSet:
        for c in str:
          if c in part.chars:
            return true
      of spkSubstr:
        for sub in part.strs:
          if sub in str:
            return true


func dropPrefix*(str: string, part: StrPart): string =
  for alt in part:
    if str.startsWith(alt):
      return str[min(alt.len, str.len)..^1]

  return str

func dropPrefix*(ss: seq[string], patt: StrPart): seq[string] =
  for s in ss:
    result.add s.dropPrefix(patt)


func dropSuffix*(str: string, part: StrPart): string =
  for alt in part:
    if str.endsWith(alt):
      return str[0 ..^ (alt.len + 1)]

  return str

func addSuffix*(str, suff: string): string =
  ## Add suffix `suff` if not already present
  if str.endsWith(suff):
    return str
  else:
    return str & suff

func addPrefix*(str: var string, pref: string): void =
  ## Add prefix to string if it not starts with `pref`
  if not str.startsWith(pref):
    str = pref & str

func addPrefix*(str, pref: string): string =
  ## Add prefix to string if it not starts with `pref`
  if not str.startsWith(pref):
    pref & str
  else:
    str

func addPrefix*(str: seq[string], pref: string): seq[string] =
  for s in str:
    result.add s.addPrefix(pref)

func commonPrefix*(strs: seq[string]): string =
  ## Find common prefix for list of strings
  # TODO implement without sorting
  if strs.len == 0:
    return ""
  else:
    let strs = strs.sorted()
    for i in 0 ..< min(strs[0].len, strs[^1].len):
      if strs[0][i] == strs[^1][i]:
        result.add strs[0][i]
      else:
        return



func delete*(str: string, chars: set[char]): string =
  for c in str:
    if c notin chars:
      result &= c

func delete*(str: var string, chars: set[char]) =
  var tmp: string
  for c in str:
    if c notin chars:
      tmp &= c

  str = tmp

# func endsWith*(str: string, chars: set[char]): bool =
#   ## True if last character of the strings is in `chars`
#   (str.len > 0) and (str[^1] in chars)

# func startsWith*(str: string, chars: set[char]): bool =
#   ## True if first character of the strings is in `chars`
#   (str.len > 0) and (str[0] in chars)

func startsWith*(str: string; skip: set[char], pref: string): bool =
  ## Return true if string has prefix `<skip*><pref>` - one or more
  ## occurencies of chars in `skip` set, followed by prefix.
  (str.len > 0) and str[str.skipWhile(skip)..^1].startsWith(pref)


func startsWith*(str: string; skip: set[char], pref: set[char]): bool =
  ## Return true if string has prefix `<skip*><pref>` - one or more
  ## occurencies of chars in `skip` set, followed by prefix.
  (str.len > 0) and str[str.skipWhile(skip)..^1].startsWith(pref)

func startsWith*(str: string, pref: varargs[string]): bool =
  ## True if string starts with any of the prefixes
  result = false
  for pr in pref:
    if str.startsWith(pr):
      return true

func endsWith*(str: string, suffixes: varargs[string]): bool =
  ## True if string ends with any of the suffixes
  result = false
  for suff in suffixes:
    if str.endsWith(suff):
      return true

func enclosedIn*(str: string, delim: StrPartTuple): bool =
  ## Check if string starts and ends with strings.
  str.startsWith(delim.lhs) and str.endsWith(delim.rhs)

func enclosedIn*(str: string, delim: StrPart): bool =
  ## Check if string starts and ends with strings.
  return str.startsWith(delim) and str.endsWith(delim)

func filterPrefix*(str: seq[string], pref: StrPart): seq[string] =
  ## Return only strings that have prefix in `pref`
  for s in str:
    if s.startsWith(pref):
      result.add s

func msgjoinImpl*(args: seq[string]): string =
  # FIXME `text __next`
  # FIXME `text /dir/file.tmp`
  var openwrap: bool = false
  let max = args.len - 1
  var idx = 0
  const wraps: set[char] = {'_', '`', '\'', '\"', ' '}
  # debugecho "\e[41m*==========\e[49m  d  \e[41m===========*\e[49m"
  while idx < args.len:
    # debugecho args[idx .. ^1]
    if args[idx].startsWith(wraps):
      if args[idx].allIt(it in wraps):
        result &= args[idx]
        inc idx

      while idx < args.len:
        result &= args[idx]
        inc idx

        if not idx < args.len: break
        if args[idx].endsWith(wraps):
          if idx < args.len - 1: result &= " "
          break

    else:
      if args[idx].endsWith({'[', '(', '\'', '#', '@'} + wraps):
        # Most likely a `"some text[", var, "] else"`
        # debugecho "22_"
        result &= args[idx]
      elif idx < max and args[idx + 1].startsWith({',', ' ', '.'}):
        # Next argument is `".field"`, `" space"` etc.
        # debugecho "122 _as"
        result &= args[idx]
      else:
        # debugecho "else"
        result &= args[idx]
        if idx < max: result &= " "

      inc idx
      # debugecho "==", result, "=="
  # for idx in 0 ..< args.len:
  #   # if idx == args.len - 1:
  #   #   result &= args[idx]
  #   # else:
  #   const wraps: set[char] = {'_', '`', '\'', '\"', ' '}
  #   if args[idx].len >= 3 and args[idx].enclosedIn(wraps):
  #     result &= " " & args[idx] & " "
  #   elif args[idx].endsWith({'[', '(', '\'', '#', '@'} + wraps):
  #     if (args[idx].endsWith wraps) and openwrap:
  #       openwrap = false

  #     result &= args[idx]
  #   elif idx < max and args[idx + 1].startsWith({',', ' ', '.'} + wraps):
  #     # if openwrap:
  #     #   result &= args[idx]
  #     # else:
  #     #   openwrap = true
  #     result &= args[idx]
  #   else:
  #     result &= args[idx]

  #     if idx < max: result &= " "

func toUpperAscii*(strs: seq[string]): seq[string] {.inline.} =
  for str in strs:
    result.add toUpperascii(str)

func msgjoin*(args: varargs[string, `$`]): string =
  ## Concatenate arguments by adding whitespaces when necessary. When
  ## string ends with `_`, `'`, `"` or other similar characters (used
  ## when wrapping things like in `msgjoin("_", text, "_")`).
  ## Whitespace is omitted when strings *ends with* any of `[('#@` +
  ## wrapper characters or next one *starts with* `, .` + wrapper
  ## characters. Wrapper characters are: `_' "`
  msgjoinImpl(toSeq(args))


macro joinLiteral*(body: untyped): untyped =
  if body.kind == nnkStmtList:
    result = newLit(msgjoin body.mapIt(it.strVal())):

  elif body.kind in {nnkStrLit, nnkTripleStrLit}:
    result = body

  else:
    error(
      "Expected either list of string literals or single literal", body)

template fmtJoin*(body: untyped): untyped =
  fmt(joinLiteral(body))

template assertionCheck*(expression: untyped, body: untyped): untyped =
  ## Raise `AssertionError` if `expression` evaluates as false. Body
  ## is a string literal which will be passed as a message. It will be
  ## passed to `&` macro - i.e. variable interpolation is supported.
  static: assert body is string
  assert expression, joinLiteral(optFmt body)


template assertionFail*(body: untyped): untyped =
  ## Raise `AssertionError`. Body is a string literal which will be
  ## passed as a message. It will be passed to `&` macro - i.e.
  ## variable interpolation is supported.
  raise newException(AssertionError, fmt(joinLiteral(body)))

template argumentError*(body: untyped): untyped =
  ## Raise `ArgumentError`. Body is a string literal which will be
  ## passed as a message. It will be passed to `&` macro - i.e.
  ## variable interpolation is supported.
  {.line: instantiationInfo().}:
    raiseArgumentError(fmt(joinLiteral(body)))
