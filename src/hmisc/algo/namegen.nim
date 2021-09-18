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
  std/[tables, strutils, sequtils, sets]

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
    raise newArgumentError(
      "Rename for ", baseName, " already exists in cache")

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



func isReservedNimType*(str: string): bool =
  const nameSet = toHashSet [
    "ptr", "lent", "sink", "ref", "var", "pointer",

    "int", "int8", "int16", "int32", "int64",
    "uint", "uint8", "uint16", "uint32", "uint64",
    "float", "float32", "float64",
    "bool", "char", "string",

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

    # Those types are not built-in, and instead come from C++ interop
    # wraphelp, but I don't want to complicate hcparse implementation, so I
    # add them here.
    "cchar32", "cchar16", "cwchar",

    "cstring", "openarray", "cstringarray", "seq", "set", "auto", "any",
    "void"
  ]

  nimNorm(str) in nameSet

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

proc keepNimIdentChars*(str: string): string =
  ## Remove all non-identifier characters and collapse multiple
  ## underscrores into single one. Remove all leading underscores.
  result = str[str.find(AllChars - {'_'}) .. ^1]
  result.delete(AllChars - IdentChars)
  while find(result, "__") != -1:
    result = result.replace("__", "_")

proc fixIdentName*(str: string, prefix: string): string =
  ## Convert possibly reserved identifier `str` to save identfier by
  ## prepending `prefix`.
  if str.isReservedNimWord():
    assert prefix.len > 0
    while result.isReservedNimWord():
      result = prefix & capitalizeAscii(result)

  else:
    result = keepNimIdentChars(str)

type
  NameFixStrategy = enum
    nfsPrependText
    nfsAppendText
    nfsNumerateNew
    nfsDescribeDiff

proc fixIdentName*(
    str, prefix: string,
    cache: var StringNameCache,
    requirePrefix: bool = false,
    lower: bool = true,
    strategy: NameFixStrategy = nfsPrependText
  ): string =
  if cache.knownRename(str):
    return cache.getRename(str)

  if str.isReservedNimWord():
    assert prefix.len > 0
    result = prefix & keepNimIdentChars(str).capitalizeAscii()
    while result.isReservedNimWord():
      result = prefix & result

  elif requirePrefix:
    result = prefix & str
    result[prefix.len] = toUpperAscii(result[prefix.len])

  else:
    result = str.fixIdentName(prefix)


  if lower:
    result[0] = toLowerAscii(result[0])

  else:
    result[0] = toUpperAscii(result[0])

  if cache.knownGenerated(result):
    case strategy:
      of nfsDescribeDiff:
        raise newImplementError()

      of nfsNumerateNew:
        let count = cache.generatedCount(result)
        cache.numerateGenerated(result)
        result = result & $count

      of nfsPrependText, nfsAppendText:
        if prefix.len == 0:
          raise newArgumentError(
            "'", result, "' ident has already been generated, in order to ",
            "generate new unique result non-empty prefix must be supplied, ",
            "but `prefix` argument is empty")

        var prefix = prefix
        if lower:
          prefix[0] = toLowerAscii(prefix[0])

        else:
          prefix[0] = toUpperAscii(prefix[0])


        while cache.knownGenerated(result):
          if strategy == nfsPrependText:
            result = prefix & result

          else:
            result.add prefix

  cache.newRename(str, result)


proc fixIdentName*(
    c: var StringNameCache, str, prefix: string,
    requirePrefix: bool = false
 ): string =
 fixIdentName(str, prefix, c, requirePrefix, lower = true, nfsPrependText)

proc fixTypeName*(
    c: var StringNameCache, str, prefix: string,
    requirePrefix: bool = false
 ): string =
 fixIdentName(str, prefix, c, requirePrefix, lower = false, nfsPrependText)

proc fixIdentName*(c: var StringNameCache, str: string): string =
 fixIdentName(str, "", c, false, lower = true, nfsNumerateNew)

proc fixTypeName*(c: var StringNameCache, str: string): string =
 fixIdentName(str, "", c, false, lower = false, nfsNumerateNew)


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
