## Identifier and type name generator algorithms

import std/[tables, strutils, sequtils]
import ./hstring_algo, ./halgorithm

type
  StringNameCache* = object
    renames: Table[string, string]
    idents: Table[string, seq[string]]

proc nimNorm*(str: string): string =
  ## Normalize nim identifier name
  if str.len > 0:
    result = str.replace("_", "").toLowerAscii()
    result[0] = str[0]


func hasExactName*(cache: StringNameCache, name: string): bool =
  name in cache.renames

func newName*(cache: var StringNameCache, str: string): string =
  let norm = nimNorm(str)
  if norm notin cache.idents:
    result = str
    cache.idents[norm] = @[result]

  else:
    result = str & $cache.idents[norm].len
    cache.idents[norm].add result

  cache.renames[str] = result

func getName*(cache: var StringNameCache, str: string): string =
  if nimNorm(str) notin cache.idents or
     str notin cache.renames
    :
    result = cache.newName(str)

  else:
    result = cache.renames[str]


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
  nimNorm(str) in [
    "int", "int8", "int16", "int32", "int64",
    "uint", "uint8", "uint16", "uint32", "uint64",
    "float", "float32", "float64",
    "bool", "char", "string",

    "cstring", "openarray", "cstringarray", "seq", "set", "auto", "any"
  ]

func isReservedNimIdent*(str: string): bool =
  const reserved = [

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

  return nimNorm(str) notin reserved

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
  if not str.isReservedNimIdent():
    assert prefix.len > 0
    result = keepNimIdentChars(str)
    while not result.isReservedNimIdent():
      result = prefix & capitalizeAscii(result)

  else:
    result = str

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

proc kindEnumName*(name, parent: string): string =
  parent.filterIt(it in {'A' .. 'Z', '0' .. '9'}).join().toLowerAscii() &
    name.capitalizeAscii()
