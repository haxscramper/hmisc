import ./oswrap, cligen
import ../algo/hseq_distance
import std/[options, uri]

export cligen

# ~~~~ AbsDir ~~~~ #

proc argHelp*(dfl: GitGlob, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "GitGlob", ""]

proc argParse*(
  dst: var GitGlob, dfl: GitGlob, a: var ArgcvtParams): bool =
  dst = toGitGlob(a.val)
  result = true


# ~~~~ AbsDir ~~~~ #

proc argHelp*(dfl: AbsDir, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "AbsDir", ""]

proc argParse*(
  dst: var AbsDir, dfl: AbsDir, a: var ArgcvtParams): bool =
  dst = toAbsDir(a.val)
  result = true


# ~~~~ AbsFile ~~~~ #

proc argHelp*(dfl: AbsFile, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "AbsFile", ""]

proc argParse*(
  dst: var AbsFile, dfl: AbsFile, a: var ArgcvtParams): bool =
  dst = toAbsFile(a.val)
  result = true

# ~~~~ RelFile ~~~~ #

proc argHelp*(dfl: RelFile, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "RelFile", ""]

proc argParse*(
  dst: var RelFile, dfl: RelFile, a: var ArgcvtParams): bool =
  dst = RelFile(a.val)
  result = true


# ~~~~ FsDir ~~~~ #


proc argHelp*(dfl: FsDir, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "FsDir", ""]

proc argParse*(
  dst: var FsDir, dfl: FsDir, a: var ArgcvtParams): bool =
  dst = parseFsDir(a.val)
  result = true

# ~~~~ FsFile ~~~~ #

proc argHelp*(dfl: FsFile, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "FsFile", ""]

proc argParse*(
  dst: var FsFile, dfl: FsFile, a: var ArgcvtParams): bool =
  dst = parseFsFile(a.val)
  result = true

# ~~~~ ShellExpr ~~~~ #

proc argHelp*(dfl: ShellExpr, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "ShellExpr", ""]

proc argParse*(
  dst: var ShellExpr, dfl: ShellExpr, a: var ArgcvtParams): bool =
  # NOTE potential place for input sanitization
  dst = ShellExpr(a.val)
  result = true

# ~~~~ Option[T] ~~~~ #


proc argHelp*[T](dfl: Option[T], a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, $typeof(T), ""]

proc argParse*[T](
  dst: var Option[T], dfl: Option[T], a: var ArgcvtParams): bool =
  mixin argParse
  var res: T
  if dfl.isSome():
    result = argParse(res, dfl.get(), a)
  else:
    var tmp: T
    result = argParse(res, tmp, a)

  if result:
    dst = some(res)

# ~~~~ Url ~~~~ #

proc argHelp*(dfl: Url, a: var ArgcvtParams): seq[string] =
  @["--" & a.parNm, "Url", ""]

proc argParse*(
  dst: var Url, dfl: Url, a: var ArgcvtParams): bool =
  # NOTE potential place for input sanitization
  dst = Url(a.val)
  result = true


# # ~~~~ Key-value-pair ~~~~ #
# type
#   CliKeyVal*[K, V, sep: static[string]] = object
#     key*: K
#     val*: V

# func initCliKeyVal*[K, V, sep: static[string]](
#   k: K, v: V, sep: string = ":", required: bool = true): CliKeyVal =
#   CliKeyVal[K, V](key: k, val: v, sep: sep, required: required)

# proc argHelp*[K, V](dfl: CliKeyVal[K, V], a: var ArgcvtParams): seq[string] =
#   @[
#     "--" & a.parNm, $typeof(K) & dfl.sep & $typeof(V),
#     if dfl.required: "REQUIRED" else: ""
#   ]

# proc argParse*[K, V](
#   dst: var CliKeyVal[K, V],
#   dfl: CliKeyVal[K, V], a: var ArgcvtParams): bool =

#   let spl = a.val.split(dfl.sep)

#   result = spl.len == 2

#   mixin argParse

#   if result:
#     a.val = spl[0]
#     result = argParse(dst.key, dfl.key, a)

#   if result:
#     a.val = spl[1]
#     result = argParse(dst.val, dfl.val, a)
