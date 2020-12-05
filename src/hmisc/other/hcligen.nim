import oswrap, cligen
import std/options

export cligen

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
  @["--" & a.parNm, $typeof(dfl), ""]

proc argParse*[T](
  dst: var Option[T], dfl: Option[T], a: var ArgcvtParams): bool =
  var res: T
  if dfl.isSome():
    result = argParse(res, dfl.get(), a)
  else:
    var tmp: T
    result = argParse(res, tmp, a)

  if result:
    dst = some(res)
