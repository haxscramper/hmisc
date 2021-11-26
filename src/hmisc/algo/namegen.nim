## Identifier and type name generator algorithms

##[

This module provide helper functions to deal with nim's style insensetivity
when generating code (especially when converting from other sources that
are not style-insensetive).

Along with simple tools for fixing up names by removing all non-identifier
symbols 'name cache' is provided for making it easier to keep track of all
identifier renames and avoid name clashes by keeping track of all generated
names.


- NOTE :: All identifiers are genrated in accordance with nep1 style guide.
  Alternative naming styles (snake case) are not currently supported, and
  most likely won't be added in the future either.

]##

import
  std/[tables, strutils, sequtils, sets, enumerate]

import
  ./hstring_algo,
  ./halgorithm,
  ../core/all

type
  StringNameCache* = object
    renames: Table[string, string] ## 'raw' name mapped to renamed
                                   ## counterparts
    idents: Table[string, seq[string]] ## 'renamed' identifier mapped to
    genNormalized: CountTable[string] ## Set of all *normalized* generated
                                      ## identifiers

proc nimNorm*(str: string, normalize: bool = true): string =
  ## Normalize nim identifier name
  if normalize:
    if str.len > 0:
      result = str.replace("_", "")
      let first = result[0]
      result = result.toLowerAscii()
      result[0] = first

  else:
    result = str


func hasExactName*(cache: StringNameCache, name: string): bool =
  name in cache.renames

func newName*(
    cache: var StringNameCache, str: string,
    normalize: bool = true,
    extra: string = "",
    forceRename: bool = false
  ): string =
  ## Create new unique name for `str`
  ##
  ## If *normalized* `str & extra` has never been used as identifier in the
  ## cache return it unmodified, otherwise return new name.
  ##
  ## Rename `str -> result` is registered
  ##
  ## - @arg{extra} :: Disambiguation string that will not be included in
  ##   final name generation, but would participate in collision
  ##   resolution.
  let norm = nimNorm(str & extra, normalize)
  if norm notin cache.idents:
    result = str
    cache.idents[norm] = @[result]

  else:
    result = str & $cache.idents[norm].len
    cache.idents[norm].add result

  if extra.len == 0 or forceRename:
    cache.renames[str] = result

  cache.genNormalized.inc nimNorm(result)

func getName*(cache: var StringNameCache, str: string): string =
  if nimNorm(str) notin cache.idents or str notin cache.renames:
    result = cache.newName(str)

  else:
    result = cache.renames[str]

func newRename*(
    cache: var StringNameCache, baseName, newName: string,
    exact: bool = true
  ) =
  ## Register rename from `baseName` to `newName`

  let norm = nimNorm(baseName, not exact)
  if norm in cache.renames:
    if cache.renames[norm] == newName:
      discard

    else:
      raise newArgumentError(
        "Rename for ", baseName,
        " already exists in cache, but maps to a different name. ",
        "Current mapping is {'", norm, "': '", cache.renames[norm],
        "'}, but new one is {'", norm, "': '", newName)

  else:
    cache.renames[norm] = newName
    cache.idents[newName] = @[baseName]

  cache.genNormalized.inc nimNorm(newName)

func numerateGenerated*(cache: var StringNameCache, newName: string) =
  ## Increment count of the generated normalized of `newName`. This should
  ## be called before any other disambiguation alterations to the `newName`
  cache.genNormalized.inc nimNorm(newName)

func knownRename*(
    cache: StringNameCache, baseName: string, exact: bool = true): bool =
  ## Whether `baseName` has been renamed to something in the `cache`
  if exact:
    baseName in cache.renames

  else:
    nimNorm(baseName) in cache.renames

func generatedCount*(cache: StringNameCache, name: string): int =
  cache.genNormalized[nimNorm(name)]

func knownGenerated*(
    cache: StringNameCache, name: string): bool =
  ## Whether *normalized* form of `name` was created in the cache
  result = nimNorm(name) in cache.genNormalized

func knownName*(cache: StringNameCache, name: string): bool =
  ## Whether `name` has already been generated in the `cache`
  name in cache.idents

func getRename*(
    cache: StringNameCache, baseName: string, exact: bool = true): string =
  ## Get what `baseName` was renamed to in the cache
  cache.renames[nimNorm(baseName, not exact)]


func commonPrefix*[T](seqs: seq[seq[T]]): seq[T] =
  ## Find common prefix for list of strings
  # TODO implement without sorting
  if seqs.len == 0:
    return @[]

  else:
    result = seqs[0]
    for s in seqs:
      var prefix: int = 0
      for i in 0 ..< min(len(s), len(result)):
        if result[i] == s[i]:
          inc prefix

      if prefix == 0:
        return @[]

      else:
        result = result[0 ..< prefix]



func isSharedTypeName*(name: string): bool =
  const sharedNames = toHashSet [
    "void", "int", "float", "bool", "char", # ...
  ]

  return name in sharedNames

func isReservedNimType*(str: string): bool =
  const exactNames = toHashSet [
    "ptr", "lent", "sink", "ref", "var", "pointer",

    "int", "int8", "int16", "int32", "int64",
    "uint", "uint8", "uint16", "uint32", "uint64",
    "float", "float32", "float64",
    "bool", "char", "string",

    # Those types are not built-in, and instead come from C++ interop
    # wraphelp, but I don't want to complicate hcparse implementation, so I
    # add them here.
    "cchar32", "cchar16", "cwchar",

    "seq", "set", "auto", "any", "void"
  ]

  const nameSet = toHashSet [
    "clong",
    "culong",
    "cchar",
    "cschar",
    "cshort",
    "cint",
    "csize",
    "csize_t",
    "clonglong",
    "cfloat",
    "cdouble",
    "clongdouble",
    "cuchar",
    "cushort",
    "cuint",
    "culonglong",

    "cstring", "openarray", "cstringarray",
  ]

  str in exactNames or nimNorm(str) in nameSet

func isReservedNimIdent*(str: string): bool =
  const reserved = toHashSet [

    "addr", "and", "as", "asm", "bind", "block", "break", "case", "cast",
    "concept", "const", "continue", "converter", "defer", "discard",
    "distinct", "div", "do", "elif", "else", "end", "enum", "except",
    "export", "finally", "for", "from", "func", "if", "import", "in",
    "include", "interface", "is", "isnot", "iterator", "let", "macro",
    "method", "mixin", "mod", "nil", "not", "notin", "object", "of", "or",
    "out", "proc", "ptr", "raise", "ref", "return", "shl", "shr", "static",
    "template", "try", "tuple", "type", "using", "var", "when", "while",
    "xor", "yield"

  ]

  return nimNorm(str) in reserved

func isReservedNimWord*(str: string): bool =
  isReservedNimIdent(str) or isReservedNimType(str)

# func isValidNimIdent*(str: string): bool =
#   str.len > 0 and isRevered



func fixIdentName*(
    str: string, prefix: string, toCamel: bool = false): string =
  ## Convert possibly reserved identifier `str` to save identfier by
  ## prepending `prefix`.
  if str.isReservedNimWord():
    assert prefix.len > 0
    while result.isReservedNimWord():
      result = prefix & capitalizeAscii(result)

  else:
    if toCamel:
      result = snakeToCamelCase(str)

    else:
      result = keepNimIdentChars(str)

type
  NameFixStrategy* = enum
    nfsPrependText
    nfsAppendText
    nfsNumerateNew
    nfsDescribeDiff

  NameFixImpl* = proc(str: string, isType: bool): string
  NameFixConf* = object
    fixWith*: NameFixImpl
    strat*: NameFixStrategy
    toCamel*: bool
    prefix*: string
    requirePrefix*: bool
    isType*: bool

proc fixDuplicated*(
    cache: var StringNameCache,
    original, generated: string,
    conf: NameFixConf
  ): string =
  result = generated
  case conf.strat:
    of nfsDescribeDiff:
      raise newImplementError()

    of nfsNumerateNew:
      let count = cache.generatedCount(result)
      cache.numerateGenerated(result)
      result = result & $count

    of nfsPrependText, nfsAppendText:
      if conf.prefix.len == 0:
        raise newArgumentError(
          "'", result, "' ident has already been generated, in order to ",
          "generate new unique result non-empty prefix must be supplied, ",
          "but `prefix` argument is empty")

      var prefix = conf.prefix
      if conf.isType:
        prefix[0] = toUpperAscii(prefix[0])

      else:
        prefix[0] = toLowerAscii(prefix[0])


      while cache.knownGenerated(result):
        if conf.strat == nfsPrependText:
          result = prefix & result

        else:
          result.add prefix


proc fixInitial*(
    str: string, conf: NameFixConf): tuple[res: string, changeFirst: bool] =

  var res: string
  var changeFirst = true
  if (not conf.isType and str.isReservedNimWord()) or
     (str.isReservedNimIdent()):
    if notNil(conf.fixWith):
      res = conf.fixWith(str, conf.isType)

    else:
      if not (0 < conf.prefix.len):
        raise newArgumentError(
          "Reserved identifiers without existing renames must be ",
          "converted using prefix, but empty 'prefix' was supplied. ",
          "identifier was '", str, "'")

      res = conf.prefix & keepNimIdentChars(str).capitalizeAscii()
      while res.isReservedNimWord():
        res = conf.prefix & res

  elif conf.isType and str.isReservedNimType():
    changeFirst = false
    res = str

  elif conf.requirePrefix:
    res = conf.prefix & str
    res[conf.prefix.len] = toUpperAscii(res[conf.prefix.len])

  else:
    if isNil(conf.fixWith):
      res = fixIdentName(str, conf.prefix, conf.toCamel)

    else:
      res = conf.fixWith(str, conf.isType)

  return (res, changeFirst)


proc fixName*(
    cache: var StringNameCache,
    str: string,
    conf: NameFixConf
  ): string =

  if cache.knownRename(str):
    return cache.getRename(str)

  var (res, changeFirst) = fixInitial(str, conf)

  if changeFirst:
    if conf.isType:
      res[0] = toUpperAscii(res[0])

    else:
      res[0] = toLowerAscii(res[0])

  if cache.knownGenerated(res):
    res = cache.fixDuplicated(str, res, conf)

  cache.newRename(str, res)
  return res

proc fixIdentName*(
    c: var StringNameCache,
    str: string,
    fixWith: proc(str: string, isType: bool): string,
    strat: NameFixStrategy = nfsNumerateNew,
    toCamel: bool = true
 ): string =
 c.fixName(str, NameFixConf(
   toCamel: toCamel,
   isType: false,
   strat: strat,
   fixWith: fixWith))

proc fixTypeName*(
    c: var StringNameCache,
    str: string,
    fixWith: proc(str: string, isType: bool): string,
    strat: NameFixStrategy = nfsNumerateNew,
    toCamel: bool = true
 ): string =
 c.fixName(str, NameFixConf(
   toCamel: toCamel,
   isType: true,
   strat: strat,
   fixWith: fixWith))

proc fixIdentName*(
    c: var StringNameCache, str, prefix: string,
    requirePrefix: bool = false,
    toCamel: bool = true
 ): string =
 c.fixName(
   str, NameFixConf(
     prefix: prefix, requirePrefix: requirePrefix,
     strat: nfsPrependText, toCamel: toCamel))

proc fixTypeName*(
    c: var StringNameCache, str, prefix: string,
    requirePrefix: bool = false,
    toCamel: bool = true
 ): string =
 c.fixName(str, NameFixConf(
   prefix: prefix,
   requirePrefix: requirePrefix,
   strat: nfsPrependText, toCamel: toCamel,
   isType: true))

proc fixIdentName*(c: var StringNameCache, str: string,
    toCamel: bool = true): string =
 c.fixName(str, NameFixConf(strat: nfsNumerateNew, toCamel: toCamel))

proc fixTypeName*(c: var StringNameCache, str: string,
    toCamel: bool = true): string =
 c.fixName(str, NameFixConf(strat: nfsNumerateNew, isType: true, toCamel: toCamel))

proc fixNumerateTypeName*(
    c: var StringNameCache, str: string, prefix: string,
    toCamel: bool = true): string =
 c.fixName(
   str, NameFixConf(prefix: prefix, isType: true, strat: nfsNumerateNew, toCamel: toCamel))

proc fixNumerateIdentName*(c: var StringNameCache, str: string, prefix: string,
    toCamel: bool = true): string =
 c.fixName(
   str, NameFixConf(prefix: prefix, isType: false, strat: nfsNumerateNew, toCamel: toCamel))

proc fixNimTypeName*(str: string, useReserved: bool = true): string =
  ## Convert possibly reserved type identifier `str` to string by
  ## capitalizing and replacing all prohibited characters.
  ##
  ## - @arg{useReserved} :: Do not modify type name if it a reserved
  ##   primitive (e.g. `string` won't be changed by default, but with
  ##   `useReserved = false` it will be converted to `String`)
  if str.isReservedNimType() and useReserved:
    return str

  else:
    capitalizeAscii(str)



proc enumPrefixForCamel*(camel: string): string =
  ## Return enum prefix for camel case identifier
  for part in camel.splitCamel():
    result.add toLowerAscii(part[0])

proc enumPrefixForCamel*(camel: string, cache: var StringNameCache): string =
  if cache.knownRename(camel):
    return getRename(cache, camel)

  var split = camel.splitCamel().mapIt((0, it.toLowerAscii()))
  while true:
    result = ""
    for (pref, str) in split:
      result &= str[0 .. pref].toLowerAscii()

    if cache.knownName(result):
      var hadInc = false
      for val in mitems(split):
        if val[0] < val[1].high and
           val[1][val[0] + 1] in {'0' .. '9'}:
          # First try to increment element with integer values (it is very
          # likely that they are already used to disambiguate between
          # different kinds)
          hadInc = true
          inc val[0]
          break

      if not hadInc:
        for val in mitems(split):
          if val[0] < val[1].high:
            inc val[0]
            break

    else:
      break

  cache.newRename(camel, result)


proc kindEnumName*(name, parent: string): string =
  enumPrefixForCamel(parent) & name.capitalizeAscii()

proc kindEnumName*(
    name, parent: string,
    cache: var StringNameCache,
    addPrefix: bool = true
  ): string =

  if cache.knownRename(parent & name):
    return cache.getRename(parent & name)

  else:
    let prefix = enumPrefixForCamel(parent, cache)
    let newName = cache.newName(
      capitalizeAscii(name), extra = parent)
    if addPrefix:
      result = prefix

    result &= newName

    cache.newRename(parent & name, result)
