## `os` module function wrappers than can work with both nimscript and
## regular target.


## - TODO :: add support for error messages on missing files. etc
## - TODO :: add `MaybeFile` and `MaybeDir` typeclasses for things that
##   might represent anything at runtime
## - TODO :: unit test with taint mode on
## - TODO :: parse file paths from URI (`file:///`)
## - TEST :: that compiles without errors.
## - IDEA :: optionally enable colored exception messages.
## - TODO :: convert exceptions to structured output
## - TODO :: Add `/` overload for `static[string]` argument and generate
##   warnings when literal string with `"/"` is used, or path looks like a
##   file (though if `directory.d` naming convention is used it might be
##   a big problematic).
## - TODO :: Check if file can be created or written to (e.g. if parent
##   directory for a target file exists, or the file itself exists).

# {.experimental: "caseStmtMacros".}

import std/[strutils, macros, random, hashes, json, math,
            strformat, sequtils, options, streams]

import ../algo/[hstring_algo, hseq_distance, halgorithm, clformat]
import ../macros/hmacro_utils
import ../types/colorstring
import ../core/[all, code_errors]

from os import nil


export os.PathComponent
export os.FileInfo
export os.FilePermission
export os.ReadDirEffect, os.ReadEnvEffect,
       os.WriteDirEffect, os.WriteEnvEffect,
       os.fileExists, os.dirExists

export os.sleep, os.getCurrentProcessId
export os.unixToNativePath
export os.quoteShellWindows
export os.quoteShellPosix
export os.quoteShell, os.joinPath

# import fusion/matching

const cbackend* = not (defined(nimscript) or defined(js))

when cbackend:
  import times, pathnorm, posix, parseopt


## Os wrapper with more typesafe operations for envrionment variables,
## paths etc. Provides many convenience utilities on top of `oswrap`.
## Tested to work on both nimscript and compiled backend. Proc names were
## taken from `std/nimscript` - `cpFile` instead of `copyFile` and so on.
##
## Main focus of this module is to provide drop-in replacement for `std/os`
## and elmination of the unchecked raw strings when working with OS -
## executing shell code, working with environment variables and filesystem
## paths.
##
## Even though `std/os` is OS-agnostic and designed to work on both
## UNIX-like operating systems and windows, this module provides some
## extensions that are not fully portable (yet), and might rely on
## `std/posix` module.
##
## Wrappers XDG directory `specification
## <https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>`_
## are provided in form of `getUserConfigDir()` and `getAppConfigDir()` -
## as well as all other directories specified in the standard.


type
  ShellVar* = distinct string
  ShellExpr* = distinct string ## Shell expression
  ## Can be evaluated by external shell invokation (bash, zsh etc).
  ## Supported syntax on shell being used
  AbsFile* = distinct string
  AbsDir* = distinct string
  AbsPath* = AbsFile | AbsDir

  RelFile* = distinct string
  RelDir* = distinct string
  RelPath* = RelFile | RelDir

type
  FsFile* = object
    info*: Option[os.FileInfo]
    case isRelative*: bool
      of true:
        relFile*: RelFile

      of false:
        absFile*: AbsFile

  FsDir* = object
    case isRelative*: bool
      of true:
        relDir*: RelDir

      of false:
        absDir*: AbsDir

  FsEntry* = object
    case kind*: os.PathComponent
      of os.pcFile, os.pcLinkToFile:
        file*: FsFile
      of os.pcDir, os.pcLinkToDir:
        dir*: FsDir


  FsTree* = object
    basename*: string
    parent*: seq[string]
    isAbsolute*: bool
    case isDir*: bool
      of true:
        sub*: seq[FsTree]
      of false:
        ext*: string

type
  PathErrorKind* = enum
    pekDefault

    pekExpectedDir
    pekExpectedFile
    pekExpectedAbs
    pekExpectedRel
    pekNoSuchEntry
    pekFileExists
    pekInvalidEntry

  PathError* = ref object of OSError
    entry*: FsEntry
    kind*: PathErrorKind

  FileSearchError = object of OsError
    directories*: seq[AbsDir]
    basename*: string
    extensions*: seq[GitGlob]

  EnvVarError* = ref object of OSError
    varname*: ShellVar

type
  AnyPath* = AbsFile | AbsDir | RelFile | RelDir |
             FsFile | FsDir | FsEntry | FsTree

  AnyDir* = AbsDir | RelDir | FsDir
  AnyFile* = AbsFile | RelFile | FsFile


func newFileSearchError*(
    directories: seq[AbsDir],
    name: string,
    extensions: seq[GitGlob],
    userMsg: string
  ): ref FileSearchError =

  var
    dirs: seq[string] = directories.mapIt(it.string)
    prefix = dirs.commonPrefix()
    suffix = dirs.dropCommonPrefix().mapIt("'" & it & "'")
    dirListing = prefix & "{" & strutils.join(suffix, ",") & "}"

  if prefix == "" or dirs.len == 1:
    dirListing = strutils.join(dirs.mapIt("'" & it & "'"), ", ")

  result = newException(
    FileSearchError,
    &"Could not find file '{name}' and " &
      &"extensions {extensions} in {dirListing}."
  )

  result.directories = directories
  result.basename = name
  result.extensions = extensions

# template newPathError*(
#   entry: FsEntry, kind: PathErrorKind, msg: typed): PathError =
#   newPathError(entry, kind, fmtJoin msg)

template ignorePathErrors*(
  kinds: set[PathErrorKind], body: untyped): untyped =
  try:
    body
  except PathError:
    let err = PathError(getCurrentException())
    if err.kind notin kinds:
      raise err

func `==`*(se1, se2: ShellExpr): bool = se1.string == se2.string


template getStr*(str: string): string = str
func getStr*(path: AnyPath | string): string =
  # `get` prefix is specifically used to indicate that this is an
  # accessor to internal state of the `path`, not just property that
  # you can get/set.
  when path is FsDir:
    if path.isRelative:
      path.relDir.string
    else:
      path.absDir.string
  elif path is FsFile:
    if path.isRelative:
      path.relFile.string
    else:
      path.absFile.string
  elif path is FsEntry:
    case path.kind:
      of os.pcFile, os.pcLinkToFile:
        path.file.getStr()
      else:
        path.dir.getStr()
  elif path is FsTree:
    let tmp = os.joinpath(path.parent & @[path.basename])
    # debugecho path.ext
    let pref = if path.isAbsolute: "/" else: ""
    if not path.isDir and path.ext.len > 0:
      pref & tmp & ("." & path.ext)
    else:
      pref & tmp
  else:
    path.string

# when not cbackend:
#   type
#     ReadEnvEffect* = object of RootEffect

func hash*(path: AnyPath): Hash = hash(path.getStr())
func len*(path: AnyPath): int = path.getStr().len()
func `<`*(a, b: AnyPath): bool = a.getStr() < b.getStr()
func contains*(path: AnyPath, substr: string): bool =
  path.getStr().contains substr

func contains*(dir: AbsDir, path: AbsPath): bool =
  dir.getStr() in path.getStr()

template endsWith*(path: AnyPath, expr: typed): bool =
  path.getStr().endsWith(expr)

template startsWith*(path: AnyPath, expr: typed): bool =
  path.getStr().startsWith(expr.getStr())


const
  CurDir* = RelDir($os.CurDir)
  ParDir* = RelDir(os.ParDir)
  DirSep* = os.DirSep
  AltSep* = os.AltSep
  PathSep* = os.PathSep
  ExeExts* = os.ExeExts

func parseFsFile*(file: string): FsFile =
  # TODO check if path is not directory-only (optionally)
  if os.isAbsolute(file):
    FsFile(isRelative: false, absFile: AbsFile(file))
  else:
    FsFile(isRelative: true, relFile: RelFile(file))

func parseFsDir*(dir: string): FsDir =
  if os.isAbsolute(dir):
    FsDir(isRelative: true, relDir: RelDir(dir))
  else:
    FsDir(isRelative: false, absDir: AbsDir(dir))

func `$`*(path: AnyPath): string = path.getStr()
func `$`*(entry: FsEntry): string = entry.getStr()

func hshow*(file: AnyPath,
            opts: HDisplayOpts = defaultHDisplay): ColoredText =
  toGreen(file.getStr(), opts.colored)


func toJson*(v: ShellVar): JsonNode = newJString(v.string)

# func `==`*(pathA, pathB: AnyPath, str: string): bool = path.string == str

macro osAndNims*(code: untyped): untyped =
  var
    osExpr = newCall newDotExpr(ident "os", code[0])
    nimsExpr = newCall newDotExpr(ident "system", code[0])

  for arg in code[1..^1]:
    osExpr.add arg
    nimsExpr.add arg

  when cbackend:
    result = osExpr
  else:
    result = nimsExpr

  # echo result.toStrLit()

macro osOrNims(osCode, nimsCode: untyped): untyped =
  when cbackend:
    result = osCode

  else:
    result = nimsCode


proc toFsFile*(absFile: AbsFile): FsFile =
  result = FsFile(isRelative: false, absFile: absFile)
  if os.fileExists(absFile.string):
    result.info = some os.getFileInfo(absFile.string)

proc toFsFile*(relFile: RelFile): FsFile =
  result = FsFile(isRelative: true, relFile: relFile)
  if os.fileExists(relFile.string):
    result.info = some os.getFileInfo(relFile.string)

proc toFsFile*(optFile: Option[AnyFile]): Option[FsFile] =
  if optFile.isSome():
    return some(toFsFile(optFile.get()))

proc toFsDir*(relDir: RelDir): FsDir =
  FsDir(isRelative: true, relDir: relDir)

proc toFsDir*(absDir: AbsDir): FsDir =
  FsDir(isRelative: false, absDir: absDir)

proc toFsDir*(dir: string): FsDir =
  if os.isAbsolute(dir):
    FsDir(isRelative: false, absDir: AbsDir(dir))

  else:
    FsDir(isRelative: true, relDir: RelDir(dir))


proc toFsFileOption*(absFile: Option[AbsFile]): Option[FsFile] =
  if absFile.isSome():
    return some FsFile(isRelative: false, absFile: absFile.get())

proc toFsFileOption*(relFile: Option[RelFile]): Option[FsFile] =
  if relFile.isSome():
    return some FsFile(isRelative: true, relFile: relFile.get())

proc toFsDirOption*(absDir: Option[AbsDir]): Option[FsDir] =
  if absDir.isSome():
    return some FsDir(isRelative: false, absDir: absDir.get())

proc toFsDirOption*(relDir: Option[RelDir]): Option[FsDir] =
  if relDir.isSome():
    return some FsDir(isRelative: true, relDir: relDir.get())

proc toFsDirSeq*(drs: seq[AbsDir]): seq[FsDir] =
  for d in drs:
    result.add d.toFsDir()

proc toFsDirSeq*(drs: seq[RelDir]): seq[FsDir] =
  for d in drs:
    result.add d.toFsDir()

proc toFsFileSeq*(fls: seq[AbsFile]): seq[FsFile] =
  for f in fls:
    result.add f.toFsFile()

proc toFsFileSeq*(fls: seq[RelFile]): seq[FsFile] =
  for f in fls:
    result.add f.toFsFile()

proc toFsEntry*(path: AnyPath, isLink: bool): FsEntry =
  when path is FsEntry:
    path

  elif path is FsDir:
    if isLink:
      FsEntry(kind: os.pcLinkToDir, dir: path)

    else:
      FsEntry(kind: os.pcDir, dir: path)

  elif path is FsFile:
    if isLink:
      FsEntry(kind: os.pcLinkToFile, file: path)

    else:
      FsEntry(kind: os.pcFile, file: path)

  elif path is AbsFile or path is RelFile:
    path.toFsFile().toFsEntry(isLink)

  elif path is RelDir or path is AbsDir:
    path.toFsDir().toFsEntry(isLink)

proc toFsEntry*(path: string): FsEntry =
  var
    info = os.getFileInfo(path)
    link = info.kind in { os.pcLinkToDir, os.pcLinkToFile }

  if info.kind in { os.pcDir, os.pcLinkToDir }:
    if os.isAbsolute(path):
      toFsEntry(AbsDir(path), link)

    else:
      toFsEntry(RelDir(path), link)

  else:
    if os.isAbsolute(path):
      toFsEntry(AbsFile(path), link)

    else:
      toFsEntry(RelFile(path), link)

proc toFsEntry*(path: AnyPath): FsEntry =
  toFsEntry(path, false)


proc newPathError*(
    entry: AnyPath,
    kind: PathErrorKind,
    msg: varargs[string, `$`]
  ): PathError =
  PathError(
    msg: join(msg, "") & " (" & $kind & ")",
    kind: kind,
    entry: toFsEntry(entry))

# converter toAbsDir*(str: string): AbsDir =
#   assert os.isAbsolute(str),
#           "Path '" & str & "' is not an absolute directory"

#   AbsDir(str)

proc getCurrentDir*(): AbsDir =
  ## Retrieves the current working directory.
  AbsDir(osAndNims(getCurrentDir()))

proc getCurrentCompilerExe*(): AbsFile =
  AbsFile(os.getCurrentCompilerExe())

proc cwd*(): AbsDir = getCurrentDir()

proc normalizePathEnd*(path: var AnyPath; trailingSep = false): void =
  ## Ensures ``path`` has exactly 0 or 1 trailing `DirSep`, depending on
  ## ``trailingSep``, and taking care of edge cases: it preservers whether
  ## a path is absolute or relative, and makes sure trailing sep is `DirSep`,
  ## not `AltSep`. Trailing `/.` are compressed, see examples.
  os.normalizePath(AnyPath.string)

proc joinPath*(head: AbsDir, tail: RelFile): AbsFile =
  AbsFile(os.joinPath(head.string, tail.string))


proc joinPath*(head: RelDir, tail: RelDir): RelDir =
  RelDir(os.joinPath(head.string, tail.string))

proc joinPath*(head: AbsDir, tail: RelDir): AbsDir =
  AbsDir(os.joinPath(head.string, tail.string))

proc joinPath*(head: RelDir, tail: RelFile): RelFile =
  RelFile(os.joinPath(head.string, tail.string))

proc joinPath*(head: AbsDir, tail: seq[string]): AbsDir =
  AbsDir(os.joinPath(head.string & tail))

# proc joinPath*(args: seq[string]): string =
#   os.joinPath()

proc joinPath*(dir: FsDir, relFile: RelFile): FsFile =
  let file = os.joinPath(dir.getStr(), relFile.string)

  if dir.isRelative:
    toFsFile RelFile(file)

  else:
    toFsFile AbsFile(file)

proc joinPath*(dir: FsDir, relDir: RelDir): FsDir =
  let res = os.joinPath(dir.getStr(), relDir.string)

  if dir.isRelative:
    toFsDir RelDir(res)

  else:
    toFsDir AbsDir(res)


template `/`*(head, tail: AnyPath | string): untyped =
  joinPath(head, RelDir(tail))

template `/`*(head: AbsDir, tail: seq[string]): untyped =
  joinPath(head, tail)

template `/`*(head: AnyDir, tail: RelFile): untyped =
  joinPath(head, tail)

template `/.`*(head: AnyPath, file: RelFile | string): untyped =
  joinPath(head, RelFile(file))

proc splitPath*(path: AbsFile): tuple[head: AbsDir, tail: RelFile] =
  let (head, tail) = os.splitPath(path.string)
  result.head = AbsDir(head)
  result.tail = RelFile(tail)

proc splitDir*(dir: AbsDir): tuple[head: AbsDir, tail: RelDir] =
  let (head, tail) = os.splitPath(dir.getStr())
  result.head = AbsDir(head)
  result.tail = RelDir(tail)

proc dropSuffix*(dir: AbsDir, suffix: string): AbsDir =
  let (head, tail) = os.splitPath(dir.getStr())
  if tail == suffix:
    return AbsDir(head)

  else:
    return AbsDir(os.joinPath(head, tail.dropSuffix(suffix)))

proc splitDir*(dir: RelDir): tuple[head: RelDir, tail: RelDir] =
  let (head, tail) = os.splitPath(dir.getStr())
  result.head = RelDir(head)
  result.tail = RelDir(tail)


template currentSourceDir*(): AbsDir {.dirty.} =
  splitPath(AbsFile(instantiationInfo(fullPaths = true).filename)).head

template currentAbsSourceDir*(): AbsDir {.dirty.} =
  splitPath(AbsFile(instantiationInfo(fullPaths = true).filename)).head


proc getAbsDir*(path: string): AbsDir =
  AbsDir(os.splitPath(path).head)


proc startsWith*(path: AnyPath, prefix: AnyPath): bool =
  path.getStr().startsWith(prefix.getStr())

proc startsWith*(path: AnyPath, str: string): bool =
  path.getStr().startsWith(str)

proc relativePath*(path: AbsDir, base: AbsDir | AbsDir): RelDir =
  RelDir(os.relativePath(path.string, base.string))

proc relativePath*(path: AbsFile, base: AbsFile | AbsDir): RelFile =
  RelFile(os.relativePath(path.string, base.string))

proc relativeUpCount*[P1: AbsPath, P2: AbsPath](
    inCurrent: P1,
    inTarget: P2
  ): int =

  bind dropPrefix

  var (current, target) = (inCurrent.getStr(), inTarget.getStr())

  if current.startsWith(target):
    swap(current, target)

  elif target.startsWith(current):
    discard

  else:
    let common = commonPrefix(@[current, target]).toStrPart()

    if count(dropPrefix(current, common), "/") == 0 and
       count(dropPrefix(target, common), "/") == 0:

      return 0

    else:
      raise PathError(
        msg: "Cannot compute relative up count for unrelated paths " &
          &"(current: '{inCurrent}', target: '{inTarget}')")

  var relative = os.relativePath(current, target)
  when P1 is AnyFile or P2 is AnyFile:
    relative = relative.dropPrefix("..")

  result = relative.count("..")
  # echov result
  # when P1 is AnyFile or P2 is AnyFile:
  #   result += -1 * sgn(result)

  # var diff = current.dropPrefix(target)
  # if inTarget is AnyDir:
  #   diff = diff.dropPrefix("/")

  # echov current
  # echov target
  # echov diff



proc parentDir*(path: AbsPath): AbsDir = AbsDir(os.parentDir(path.getStr()))
proc parentDir*(path: RelPath): RelDir = RelDir(os.parentDir(path.getStr()))

proc parentDir*(path: FsFile): FsDir =
  if path.isRelative:
    parentDir(path.relFile).toFsDir()
  else:
    parentDir(path.absFile).toFsDir()

proc parentDir*(path: FsDir): FsDir =
  if path.isRelative:
    parentDir(path.relDir).toFsDir()
  else:
    parentDir(path.absDir).toFsDir()

proc dir*(file: FsFile): FsDir = parentDir(file)
proc dir*(file: AbsFile): AbsDir = parentDir(file)
proc dir*(file: RelFile): RelDir = parentDir(file)
proc dir*(dir: AbsDir): AbsDir = parentDir(dir)
proc dir*(dir: RelDir): RelDir = parentDir(dir)
proc dir*(dir: FsDir): FsDir = parentDir(dir)

proc tailDir*(path: AnyDir): string = os.tailDir(path.string)

# TODO
# proc isRootDir*(path: string): bool
#   ## Checks whether a given `path` is a root directory.

iterator parentDirs*(
  path: AnyPath,
  fromRoot: bool = false,
  inclusive: bool = path is AnyDir): AbsDir =

  for p in os.parentDirs(path.string, fromRoot, inclusive):
    yield AbsDir(p)

proc `/../`*(head: AbsDir, repeat: int): AbsDir =
  result = head
  for i in 0 ..< repeat:
    result = AbsDir(os.parentDir(result.string))

proc `/../`*(head: AbsDir, tail: RelDir): AbsDir =
  AbsDir(os.parentDir(head.string).AbsDir / tail.RelDir)

proc searchExtPos*(path: AnyFile): int = os.searchExtPos(path.string)

template splitFileImpl(): untyped {.dirty.} =
  if multidot:
    let tmp = (name & ext).split(".")
    result.name = tmp[0]
    result.ext = strutils.join(tmp[1..^1], ".")
  else:
    result.name = name
    result.ext = ext.dropPrefix(".")

proc splitFile*(
    path: AbsFile, multidot: bool = true
  ): tuple[dir: AbsDir, name, ext: string] =

  let (dir, name, ext) = os.splitFile(path.getStr())

  result.dir = AbsDir(dir)

  splitFileImpl()

proc splitFile*(
    path: RelFile, multidot: bool = true
  ): tuple[dir: RelDir, name, ext: string] =

  let (dir, name, ext) = os.splitFile(path.getStr())

  result.dir = RelDir(dir)

  splitFileImpl()

proc splitFile2*(file: RelFile): tuple[dir: RelDir, file: string] =
  ## Split file into two parts - directory and file itself (with extension)
  let (dir, name, ext) = splitFile(file)
  return (dir, name & (if ext.len > 0: "." & ext else: ""))

proc splitFile2*(file: AbsFile): tuple[dir: AbsDir, file: string] =
  let (dir, name, ext) = splitFile(file)
  return (dir, name & (if ext.len > 0: "." & ext else: ""))

proc importSplit*(inCurrent, inTarget: AbsFile):
  tuple[depth: int, path: seq[string]] =

  ## Split relative path between `inCurrent` and `inTarget` into two parts
  ## - depth (number of relative directory changes (`..`)), and `path`
  ## (actual path to the target)
  let (current, target) = (inTarget.getStr(), inCurrent.getStr())
  if current == target:
    result.path = @[splitFile2(inCurrent).file]

  else:
    let rel = os.relativePath(current, target)
    for part in rel.split("/"):
      if part notin ["..", "."]:
        result.path.add part

      else:
        inc result.depth

    dec result.depth


func hasExt*(file: AnyFile, exts: openarray[string] = @[]): bool =
  let (_, _, ext) = file.splitFile()
  if exts.len == 0:
    return ext.len > 0
  else:
    return ext in exts


func hasExt*(file: AnyFile, ext: string): bool =
  let (_, _, fileExt) = file.splitFile()
  return ext == fileExt

func ext*(file: AnyFile, multidot: bool = true): string =
  file.splitFile(multidot).ext

func name*(file: AnyFile, multidot: bool = true): string =
  file.splitFile(multidot).name

func nameExt*(dir: AnyFile): string = splitFile2(dir).file

func name*(dir: AnyDir): string =
  os.splitFile(dir.getStr()).name


proc assertValid*(
    path: AnyPath, msg: string = ""): void =
  if path.getStr().len < 1:
    raise newPathError(
      toFsEntry(path),
      pekInvalidEntry,
      "Path validation failed - empty string is not a valid path" & msg
    )

  when path is RelPath:
    if os.isAbsolute(path.getStr()):
      raise newPathError(toFsEntry(path), pekExpectedRel): fmtJoin:
        "Path '{path.getStr()}' has type {$typeof(path)}, but"
        "contains invalid string - expected relative path. {msg}"

  elif path is AbsPath:
    if not os.isAbsolute(path.getStr()):
      raise newPathError(toFsEntry(path), pekExpectedAbs): fmtJoin:
        "Path '{path.getStr()}' has type {$typeof(path)}, but"
        "contains invalid string - expected absolute path {msg}"

  elif path is FsFile:
    if path.isRelative:
      assertValid(path.relFile)

    else:
      assertValid(path.absFile)

  elif path is FsDir:
    if path.isRelative:
      assertValid(path.relDir)

    else:
      assertValid(path.absDir)

  else:
    static: raiseImplementError("Validation for " & $typeof(path))

proc extractFilename*(path: AnyFile): string =
  os.extractFilename(path.string)

proc lastPathPart*(path: AnyPath): string =
  os.lastPathPart(path.string)

proc changeFileExt*(filename: AbsFile, ext: string): AbsFile =
  AbsFile(os.changeFileExt(filename.string, ext))

proc changeFileExt*(filename: RelFile, ext: string): RelFile =
  RelFile(os.changeFileExt(filename.string, ext))

proc addFileExt*(filename: AbsFile, ext: string): AbsFile =
  AbsFile(os.addFileExt(filename.string, ext))

proc addFileExt*(filename: RelFile, ext: string): RelFile =
  RelFile(os.addFileExt(filename.string, ext))

proc cmpPaths*(pathA, pathB: AnyPath): int =
  os.cmpPaths(pathA.string, pathB.string)

proc getHomeDir*(): AbsDir = AbsDir os.getHomeDir()

proc getConfigDir*(): AbsDir = AbsDir os.getConfigDir()

proc getTempDir*(): AbsDir =
  AbsDir(os.getTempDir())

proc isRelative*(file: AnyPath): bool =
  assertValid file
  when file is FsFile:
    return file.isRelative

  elif file is RelPath:
    return true

  elif file is AbsPath:
    return false



when cbackend:
  proc symlinkExists*(link: AnyPath): bool = os.symlinkExists(link.string)

  proc getLastModificationTime*(file: AnyFile): times.Time =
    os.getLastModificationTime(file.string)

  proc getLastAccessTime*(file: AnyFile): times.Time =
    os.getLastAccessTime(file.string)

  proc getCreationTime*(file: AnyFile): times.Time =
    os.getCreationTime(file.string)

  proc fileNewer*(base, other: AnyFile): bool =
    os.fileNewer(base.string, other.string)

  proc absolute*(dir: RelDir, root: AbsDir = getCurrentDir()): AbsDir =
    AbsDir(os.absolutePath(dir.string, root.string))

  proc isAbsolute*(path: AnyPath): bool = os.isAbsolute(path.getStr())
  proc isAbsolute*(path: string): bool = os.isAbsolute(path)

  proc absolute*(file: RelFile, root: AbsDir = getCurrentDir()): AbsFile =
    AbsFile(os.absolutePath(file.string, root.string))

  proc realpath*(path: string): string =
    var resolved: cstring
    let res = realpath(path.cstring, resolved)
    result = $res

  proc realpath*(path: AbsFile): AbsFile = AbsFile(realpath(path.getStr()))

  proc normalize*(path: AbsDir): AbsDir =
    AbsDir(normalizePath(path.string))

  proc normalize*(path: AbsFile): AbsFile =
    AbsFile(normalizePath(path.string))

  proc sameFile*(file1, file2: AnyFile): bool =
    os.sameFile(file1.string, file2.string)

  proc sameDir*(dir1, dir2: AnyDir): bool =
    os.cmpPaths(dir1.string, dir2.string) == 0

  proc getFilePermissions*(
    filename: AnyFile): set[os.FilePermission] =
    os.getFilePermissions(filename.string)

  proc setFilePermissions*(
    filename: AnyFile, permissions: set[os.FilePermission]) =
    os.setFilePermissions(filename.string, permissions)

proc toAbsFile*(file: AnyFile | string,
                checkExists: bool = false,
                normalize: bool = true,
                root: AbsDir = getCurrentDir()): AbsFile =
  # assert file.getStr().isAbsolute()
  when file is string:
    if os.isAbsolute(file):
      assertValid(AbsFile(file))

    else:
      assertValid(RelFile(file))

  else:
    assertValid(file)

  result = AbsFile(os.absolutePath(file.getStr(), root.getStr()))
  if checkExists:
    result.assertExists()

proc toAbsDir*(
    dir: AnyDir | string,
    checkExists: bool = false,
    normalize: bool = true,
    root: AbsDir = getCurrentDir()
  ): AbsDir =

  when dir is string:
    if os.isAbsolute(dir):
      assertValid(AbsDir(dir))

    else:
      assertValid(RelDir(dir))

  else:
    assertValid(dir)

  result = AbsDir(os.absolutePath(dir.getStr(), root.getStr()))

  if checkExists:
    result.assertExists("")



proc normalizePath*(path: var AnyPath) =
  os.normalizePath(path.string)

proc normalizedPath*(path: AbsDir): AbsDir =
  AbsDir os.normalizedPath(path.string)

proc normalizedPath*(path: RelDir): RelDir =
  RelDir os.normalizedPath(path.string)

proc normalizedPath*(path: AbsFile): AbsFile =
  AbsFile os.normalizedPath(path.string)

proc normalizedPath*(path: RelFile): RelFile =
  RelFile os.normalizedPath(path.string)


# when cbackend:
#   proc `==`*(file1, file2: AnyFile): bool = sameFile(file1, file2)
#   proc `==`*(dir1, dir2: AnyDir): bool = sameDir(dir1, dir2)
# else:
proc `==`*(file1, file2: AnyFile): bool =
  (file1.getStr() == file2.getStr())

proc `<`*(file1, file2: AnyFile): bool =
  (file1.getStr() < file2.getStr())

proc `==`*(dir1, dir2: AnyDir): bool =
  (dir1.getStr() == dir2.getStr())

proc `<`*(dir1, dir2: AnyDir): bool =
  (dir1.getStr() < dir2.getStr())


# proc sameFileContent*(path1, path2: string): bool {.rtl, extern: "nos$1",



# proc tryMoveFSObject(source, dest: string): bool {.noNimScript.} =

# proc exitStatusLikeShell*(status: cint): cint =

# iterator walkPattern*(pattern: string): string {.tags: [ReadDirEffect], noNimScript.} =

# iterator walkFiles*(pattern: string): string {.tags: [ReadDirEffect], noNimScript.} =

# iterator walkDirs*(pattern: string): string {.tags: [ReadDirEffect], noNimScript.} =

# proc expandFilename*(filename: string): string {.rtl, extern: "nos$1",


template walkYieldImpl(): untyped {.dirty.} =
  case comp:
    of os.pcFile, os.pcLinkToFile:
      yield FsEntry(kind: comp, file:
        block:
          if relative:
            RelFile(path).toFsFile()

          else:
            AbsFile(path).toFsFile()
      )

    of os.pcDir, os.pcLinkToDir:
      yield FsEntry(kind: comp, dir:
        block:
          if relative:
            RelDir(path).toFsDir()

          else:
            AbsDir(path).toFsDir()
      )


iterator walkDir*(
    dir: AnyDir;
    relative: bool = false,
    yieldFilter = {os.pcFile},
    globs: seq[GitGlob] = @[]
  ): FsEntry =

  for (comp, path) in os.walkDir(dir.getStr(), relative = relative):
    let comp = comp
    if comp in yieldFilter:
      if globs.len > 0:
        if globs.accept(path, true):
          walkYieldImpl()

      else:
        walkYieldImpl()

iterator walkDirRec*(
    dir: AnyDir,
    yieldFilter = {os.pcFile},
    followFilter = {os.pcDir},
    relative = false,
    checkDir = false,
    globs: seq[GitGlob] = @[]
  ): FsEntry =

  for path in os.walkDirRec(
    dir.getStr(),
    yieldFilter = yieldFilter,
    followFilter = followFilter,
    relative = relative,
    checkDir = checkDir,
  ):
    try:
      let comp = os.getFileInfo(
        if relative:
          os.joinPath(dir.getStr(), path)

        else:
          path
      ).kind

      if globs.len > 0:
        if globs.accept(path, true):
          walkYieldImpl()

      else:
        walkYieldImpl()

    except OsError:
      # TODO implement protection against broken symbolic links
      discard

iterator walkDir*[T: AnyPath](
    dir: AnyDir,
    resType: typedesc[T],
    recurse: bool = false,
    yieldLinks: bool = true,
    exts: seq[string] = @[],
    globs: seq[GitGlob] = @[],
    assertExists: bool = true
  ): T =

  ## Iterate over entries in `dir`, yielding only those that match
  ## `resType`.
  ##
  ## For example, `when resType is RelFile` you will only get entries that
  ## represent file, and path will be relative. This is a higher-level
  ## wrapper that takes advantage of distinct types for files/directories
  ## and eliminates need to filter by entry kind in the calling loop.
  if assertExists:
    assertExists(dir, "Cannot iterate over non-existent directory.")

  when dir is RelDir and resType is AbsPath:
    let dir = toAbsDir(dir)

  template optYield(entry: untyped): untyped =
    if exts.len == 0 or
      (exts.len > 0 and
        (ext(entry, true) in exts or
         ext(entry, false) in exts)
      ):
      yield entry


  template yieldImpl(): untyped {.dirty.} =
    when resType is RelFile:
      optYield entry.file.relFile

    elif resType is AbsFile:
      optYield entry.file.absFile

    elif resType is RelDir:
      yield entry.dir.relDir

    elif resType is AbsDir:
      yield entry.dir.absDir

    elif resType is FsFile:
      optYield entry.file

    elif resType is FsDir:
      yield entry.dir

    elif resType is FsEntry:
      yield entry


  let relative = (resType is RelDir) or (resType is RelFile)

  var resSet: set[os.PathComponent]

  when resType is AbsDir | RelDir | FsDir:
    resSet.incl pcDir
    if yieldLinks:
      resSet.incl os.pcLinkToDir

  elif resType is AbsFile | RelFile | FsFile:
    resSet.incl os.pcFile
    if yieldLinks:
      resSet.incl os.pcLinkToFile

  elif resType is FsEntry:
    resSet.incl {os.pcFile, os.pcDir}
    if yieldLinks:
      resSet.incl {os.pcLinkToFile, os.pcLinkToDir}


  if recurse:
    for entry in walkDirRec(
      dir, relative = relative, yieldFilter = resSet,
      globs = globs):

      yieldImpl()


  else:
    for entry in walkDir(
      dir, relative = relative, yieldFilter = resSet,
      globs = globs):

      yieldImpl()


when cbackend:
  proc existsOrCreateDir*(dir: AnyDir): bool =
    os.existsOrCreateDir(dir.string)

  proc createSymlink*[Src: AnyPath, Dest: AnyPath](
    src: Src, dest: Dest) =
    os.createSymlink(src.string, dest.string)

  proc createHardlink*[Src: AnyPath, Dest: AnyPath](
    src: Src, dest: Dest) =
    os.createHardlink(src.string, dest.string)

  proc copyFileWithPermissions*(source, dest: AnyFile,
                                ignorePermissionErrors = true) =
    os.copyFileWithPermissions(
      source.string, dest.string, ignorePermissionErrors)

  proc copyDirWithPermissions*(source, dest: AnyDir,
      ignorePermissionErrors = true) =

    os.copyDirWithPermissions(
      source.string, dest.string, ignorePermissionErrors)

  proc inclFilePermissions*(filename: AnyFile,
                            permissions: set[os.FilePermission]) =
    os.inclFilePermissions(filename.string, permissions)

  proc exclFilePermissions*(filename: AnyFile,
                            permissions: set[os.FilePermission]) =
    os.exclFilePermissions(filename.string, permissions)

  proc expandSymlink*(path: AbsDir): AbsDir =
    AbsDir os.expandSymlink(path.string)

  proc expandSymlink*(path: AbsFile): AbsFile =
    AbsFile os.expandSymlink(path.string)

  proc getAppFilename*(): AbsFile = AbsFile(os.getAppFilename())
  proc getAppBasename*(
    withoutExt: bool = true, multidot: bool = true): string =
    if withoutExt:
      getAppFilename().splitFile(multidot = multidot).name
    else:
      getAppFilename().splitPath().tail.getStr()


  proc getAppDir*(): AbsDir = AbsDir os.getAppDir()

  proc getFileSize*(file: AnyFile): BiggestInt =
    os.getFileSize(file.string)

  proc getFileInfo*(handle: FileHandle): os.FileInfo =
    os.getFileInfo(handle)

  proc getFileInfo*(file: AnyFile): os.FileInfo =
    os.getFileInfo(file.string)

  proc getFileInfo*(path: AnyFile, followSymlink: bool = true): os.FileInfo =
    os.getFileInfo(path.string, followSymlink)

  proc isHidden*(path: AnyPath): bool =
    os.isHidden(path.string)

  proc setLastModificationTime*(file: AnyFile, t: times.Time) =
    os.setLastModificationTime(file.string, t)

  func isValidFilename*(filename: AbsFile, maxLen = 259.Positive): bool =
    os.isValidFilename(filename.string, maxLen)


proc splitCmdLine*(c: string): seq[string] = os.parseCmdLine(c)




proc paramStr*(i: int, addBin: bool = false): string =
  ## Retrieves the i'th command line parameter.
  ## - @arg{addBin} :: Include binary name in param indexing.
  ##   NOTE: disabled by default (differs from stdlib implementation)

  osAndNims(paramStr(i + (if addBin: 0 else: 1)))

proc paramCount*(): int =
  ## Retrieves the number of command line parameters.
  osAndNims(paramCount())

proc paramStrs*(addBin: bool = false): seq[string] =
  ## Return sequence of command line parameters as strings. If
  ## `addBin` is failse (default) omit first parameter (which is a
  ## name/path of binary being called - most of the time you only need
  ## the parameters itself)
  for i in 0 .. paramCount():
    if i > 0 or addBin:
      result.add paramStr(i, true)

template `$$`*(v: untyped): untyped = ShellVar(astToStr(v))
proc getEnv*(key: ShellVar; default = ""): string =
  ## Retrieves the environment variable of name key.
  osAndNims(getEnv(key.string, default))

proc existsEnv*(key: ShellVar): bool =
  ## Checks for the existence of an environment variable named key.
  osAndNims(existsEnv(key.string))

proc putEnv*(key: ShellVar, val: string): void =
  ## Sets the value of the environment variable named key to val.
  osAndNims(putEnv(key.string, val))

proc setEnv*(key: ShellVar, val: string): void =
  ## Sets the value of the environment variable named key to val.
  putEnv(key, val)

proc delEnv*(key: ShellVar) =
  ## Deletes the environment variable named key.
  osAndNims(delEnv(key.string))

proc get*(v: ShellVar): string = v.getEnv()
proc del*(v: ShellVar) = v.delEnv
proc set*(v: ShellVar, val: string) = v.setEnv(val)

type
  ShellVarType* = string | char | bool | int | float

proc get*[T: ShellVarType](v: ShellVar, TArg: typedesc[T]): auto =
  when TArg is int:
    return parseInt(get(v))

  elif TArg is char:
    return get(v)[0]

  elif TArg is string:
    return get(v)

  else:
    return parseBool(get(v))

proc put*[T: ShellVarType](v: ShellVar, val: T | seq[T]) =
  when val is seq:
    let val = join(val, ",")

  else:
    let val = $val

  v.setEnv(val)

proc exists*(v: ShellVar): bool = v.existsEnv()

proc `==`*(v: ShellVar, val: ShellVarType): bool =
  result = exists(v)
  if result:
    try:
      result = get(v, typeof(val)) == val

    except ValueError:
      result = false

proc toBool*(v: ShellVar, raiseInvalid: bool = true, fallback: bool = false):
  bool =

  const
   trueNames = ["true", "t", "yes", "1", "on"]
   falseNames = ["false", "nil", "no", "none", "off", "1"]

  if not exists(v):
    return false

  var text = v.get().toLowerAscii()

  if text in trueNames:
    return true

  elif text in falseNames:
    return false

  else:
    if raiseInvalid:
      raise EnvVarError(
        msg: &"Environment variable {v.string} cannot be converted to bool, value: {text}",
        varname: v
      )

    else:
      return fallback



proc shellHaxOn*(): bool =
  let v = $$HAX_SHELL
  exists(v) and get(v, bool) == true

proc `~`*(path: string | RelDir): AbsDir = getHomeDir() / path
template `~&`*(path: string): AbsDir =
  getHomeDir() / fmt(path)

template existsEnvTo*(env: ShellVar, varname: untyped): untyped =
  var varname {.inject.}: string = ""
  if exists(env):
    varname = get(env)
    true
  else:
    false

template existsEnvOrDefault*(varname: ShellVar, ifExists, default: untyped): untyped =
  if existsEnvTo(varname, env):
    ifExists
  else:
    default


proc getUserRuntimeDir*(): AbsDir =
  ## Defines the base directory relative to which user-specific
  ## non-essential runtime files and other file objects (such as sockets,
  ## named pipes, ...) should be stored.
  ##
  ## XDG specification *does not* provide default value for this variable.
  ## For systemd systems it is assumed to be equal to
  ## `/run/user/<user-id>`. Right now dummy implementation only accounts
  ## for systemd, but this should be fixed later.
  when cbackend:
    existsEnvOrDefault(
      $$XDG_RUNTIME_DIR, AbsDir(env), ~RelDir($getUID()))

  else:
    raiseImplementErrro("#[ IMPLEMENT ]#")

proc getUserCacheDir*(): AbsDir =
  ## The base directory relative to which user specific non-essential data
  ## files should be stored.
  existsEnvOrDefault(
    $$XDG_CACHE_DIR, AbsDir(env), ~RelDir(".cache"))

proc getUserConfigDir*(): AbsDir =
  ## The base directory relative to which user specific configuration files
  ## should be stored
  existsEnvOrDefault(
    $$XDG_CONFIG_HOME, AbsDir(env), ~RelDir(".config"))

proc getUserDataDir*(): AbsDir =
  ## The base directory relative to which user specific data files should
  ## be stored.
  existsEnvOrDefault(
    $$XDG_DATA_HOME, AbsDir(env), ~RelDir(".local/share"))

when cbackend:
  proc getAppCacheDir*(): AbsDir =
    ## user cache dir + application file basename
    getUserCacheDir() / RelDir(getAppBasename())

  proc getAppConfigDir*(): AbsDir =
    ## user config dir + application file basename
    getUserConfigDir() / RelDir(getAppBasename())

  proc getAppDataDir*(): AbsDir =
    ## user data dir + application file basename
    getUserDataDir() / RelDir(getAppBasename())

  proc getAppRuntimeDir*(): AbsDir =
    ## user runtime dir + application file basename
    getUserRuntimeDir() / RelDir(getAppBasename())

  proc getAppConfRc*(ext: string, hidden: bool = false): AbsFile =
    ## Get application configuration file
    raiseImplementError("#[ IMPLEMENT ]#")


proc fileExists*(filename: AnyFile): bool =
  ## Checks if the file exists.
  osAndNims(fileExists(filename.getStr()))

proc dirExists*(dir: AnyDir): bool =
  ## Checks if the directory dir exists.
  osAndNims(dirExists(dir.getStr()))

proc exists*(path: AnyPath): bool =
  when path is AnyDir:
    dirExists(path)

  else:
    fileExists(path)

proc exists*(fs: FsFile): bool =
  if fs.isRelative:
    fs.relFile.exists()

  else:
    fs.absFile.exists()

proc fsEntryExists*(e: string): bool =
  os.fileExists(e) or os.dirExists(e)

proc exists*(fs: FsDir): bool =
  if fs.isRelative:
    fs.relDir.exists()

  else:
    fs.absDir.exists()

proc exists*(fs: FsEntry): bool =
  if fs.kind in { os.pcDir, os.pcLinkToDir }:
    fs.dir.exists()

  else:
    fs.file.exists()

proc getPermissions*(file: AnyFile): set[os.FilePermission] =
  os.getFilePermissions(file.getStr())

proc getPermissions*(file: FsFile): set[os.FilePermission] =
  file.info.get().permissions

# proc fileExists*(filename: AnyFile): bool =
#   ## An alias for fileExists.
#   osAndNims(file(filename.getStr()))


# proc existsDir*(dir: AnyDir): bool =
#   ## An alias for dirExists.
#   osAndNims(existsDir(dir.string))

proc assertExists*(file: AnyFile, onMissing: string = ""): void =
  let path: AbsFile = file.toAbsFile()

  if dirExists(AbsDir file.getStr()):
    if file.isRelative and file.getStr().len == 0:
      raise newException(
        OSError,
        &"Attempt to assert empty file name in directory {cwd()}")

    else:
      var msg =
          &"{path.string} is expected to be a file but directory was found"

      if file.isRelative:
        msg &= &". Input file {file}, relative to {cwd()}"

      raise newPathError(
        toFsEntry(file), pekExpectedFile, msg)

  if not file.fileExists():
    raise newPathError(
      toFsEntry(file),
      pekNoSuchEntry,
      &"No such file '{path.string}'. {onMissing}")


proc assertExists*(dir: AnyDir, onMissing: string = ""): void =
  let path: AbsDir = dir.toAbsDir()

  if fileExists(AbsFile dir.getStr()):
    if dir.isRelative and dir.getStr().len == 0:
      raise newException(
        OSError,
        &"Attempt to assert empty dir name in directory {cwd()}")

    else:
      var msg =
          &"{path.string} is expected to be a directory but file was found"

      if dir.isRelative:
        msg &= &". Input dir {dir}, relative to {cwd()}"

      raise newPathError(toFsEntry(dir), pekExpectedDir, msg)

  if not dir.dirExists():
    raise newPathError(
      toFsEntry(dir),
      pekNoSuchEntry,
      &"No such directory '{path.string}'. {onMissing}")




proc toExe*(filename: string): string =
  ##On Windows adds ".exe" to filename, else returns filename unmodified.
  (when defined(windows): filename & ".exe" else: filename)

proc toDll*(filename: string): string =
  ## On Windows adds ".dll" to filename, on Posix produces
  ## "lib$filename.so".
  (when defined(windows): filename & ".dll" else: "lib" & filename & ".so")

proc listDirs*(dir: AnyDir): seq[AbsDir] =
  ## Lists all absolute paths for all subdirectories (non-recursively)
  ## in the directory dir.
  when defined(NimScript):
    for d in system.listDirs(dir.getStr()):
      result.add AbsDir(d)

  else:
    for (kind, path) in os.walkDir(dir.getStr()):
      if kind == pcDir:
        result.add AbsDir(path)

proc listFiles*(
    dir: AnyDir,
    exts: seq[string] = @[],
    ignoreNames: seq[string] = @[]
  ): seq[AbsFile] =
  ## Lists all the files (non-recursively) in the directory dir.
  when defined(NimScript):
    return system.listFiles(dir)

  else:
    for (kind, path) in os.walkDir(dir.getStr()):
      if kind == pcFile and
         (exts.len() == 0 or AbsFile(path).ext() in exts) and
         (ignoreNames.len == 0 or AbsFile(path).name() notin ignoreNames):
        result.add AbsFile(path)

proc listAll*(dir: AbsDir): seq[FsEntry] =
  for (kind, path) in os.walkDir(dir.getStr()):
    case kind:
      of os.pcFile: result.add toFsEntry(AbsFile(path))
      of os.pcDir: result.add toFsEntry(AbsDir(path))
      of os.pcLinkToFile: result.add toFsEntry(AbsFile(path), true)
      of os.pcLinkToDir: result.add toFsEntry(AbsDir(path), true)

func flatFiles*(tree: FsTree): seq[FsTree] =
  if tree.isDir:
    for sub in tree.sub:
      result.add sub.flatFiles()

  else:
    result = @[tree]


func withoutPrefix*(file: AbsFile, pref: AbsDir): RelFile =
  # assert startsWith($file, addSuffix($pref, "/"))
  RelFile(dropPrefix($file, addSuffix($pref, "/")))

func withoutRoot*(file: AbsFile, root: AbsDir): RelFile =
  if not startsWith(file, root):
    raise PathError(msg: "Cannot remove root '{root}' from path '{file}'")

  return withoutPrefix(file, root)

func withExt*(f: FsTree, ext: string): FsTree =
  if f.isDir:
    raise newArgumentError(
      "Cannot add extension to 'FsTree' file")

  result = f
  result.ext = ext

func addExt*[F: AbsFile | RelFIle](
    f: var F, newExt: string, replace: bool = true) =
  ## Set file extension by either replacing it (default) or adding suffix
  ## `.<extension-string>`

  if replace:
    let (parent, file, ext) = f.splitFile()
    let exts = ext.split(".")

    var resExt = strutils.join(exts[0..^2], ".")
    if newExt.len > 0:
      resExt &= hstring_algo.addprefix(newExt, ".")

    f = parent /. (file & resExt)

  elif newExt.len > 0:
    f.string &= "."
    f.string &= newExt



func withExt*[F: AbsFile | RelFIle](
    f: F, newExt: string, replace: bool = true): F =
  result = f
  addExt(result, newExt, replace)

func withExt*(fs: sink FsFile, ext: string, replace: bool = true): FsFile =
  result = fs
  if fs.isRelative:
    result.relFile = fs.relFile.withExt(ext, replace)

  else:
    result.absFile = fs.absFile.withExt(ext, replace)

proc `&.`*(file: AbsFile, ext: string): AbsFile = withExt(file, ext)

func withoutExt*[F: AbsFile | RelFile](file: F): F =
  withExt(file, "")

func addBasePrefix*[F: AbsFile | RelFile | FsFile](
  f: var F, prefix: string) =

  when f is FsFile:
    if f.isRelative:
      addBasePrefix(f.relFile, prefix)
      # result = toFsFile(withBasePrefix(f.relFile, prefix))

    else:
      addBasePrefix(f.absFile, prefix)
      # result = toFsFile(withBasePrefix(f.absFile, prefix))

  else:
    let (parent, file, ext) = os.splitFile(f.getStr())
    let res = os.joinpath(parent, prefix & file & ext)

    when F is AbsFile:
      f = AbsFile(res)

    else:
      f = RelFile(res)


func withBasePrefix*[F: AbsFile | RelFile | FsFile](f: F, prefix: string): F =
  ## Return copy of the file path `f` with new basename for a file. E.g.
  ## `/tmp/hello.cpp + prefix_ -> /tmp/prefix_hello.cpp`
  result = f
  addBasePrefix(result, prefix)

func withBase*(f: FsTree, base: string): FsTree {.inline.} =
  result = f
  result.basename = base

func withoutBasePrefix*(f: FsTree, pref: string): FsTree =
  result = f
  if f.basename.startsWith(pref):
    result.basename = f.basename[pref.len .. ^1]

func withParent*(
  f: FsTree, parent: seq[string], isAbs: bool = true): FsTree {.inline.} =
  result = f
  result.parent = parent
  result.isAbsolute = isAbs

func withoutParent*(f: FsTree): FsTree =
  result = f
  result.parent = @[]
  result.isAbsolute = false

func withoutParent*(f: FsTree, pref: seq[string]): FsTree =
  result = f
  var cut = 0
  for idx, dir in f.parent:
    if idx < pref.len and dir == pref[idx]:
      inc cut
    else:
      break

  result.parent = f.parent[cut .. ^1]

func withoutNParents*(f: FsTree, cut: int): FsTree =
  result = f
  result.parent = f.parent[cut .. ^1]




func toFsTree*(str: AbsDir): FsTree =
  let (parent, sub) = os.splitPath(str.string)
  FsTree(
    isDir: true,
    parent: parent.split(DirSep),
    basename: sub
  )


func pathLen*(fst: FsTree): int = fst.parent.len + 1

proc buildFsTree*(
  start: AnyDir = getCurrentDir(),
  allowExts: seq[string] = @[],
  blockExts: seq[string] = @[]): seq[FsTree] =
  proc aux(parent: seq[string]): seq[FsTree] =
    # TODO support links
    # echo "Listing in", parent
    for kind, path in os.walkDir(join(parent, "/")):
      case kind:
        of os.pcFile:
          let (_, name, ext) = os.splitFile(path)
          template allow(ext: string): bool =
            # Either blocked
            (ext notin blockExts) and
            # Or in explicitly allowed list
            (allowExts.len == 0 or ext in allowExts)

          if (ext.len > 0 and allow(ext[1..^1])) or
             (ext.len == 0 and allow("")):
            # echo path, " -- ", ext
            result.add FsTree(
              isDir: false,
              parent: parent,
              isAbsolute: true, # HACK
              ext: (if ext.len > 0: ext[1..^1] else: ""),
              basename: name
            )
        of os.pcDir:
          let (_, name) = os.splitPath(path)
          result.add FsTree(
            isDir: true,
            isAbsolute: true, # HACK
            parent: parent,
            sub: aux(parent & @[name]),
            basename: name
          )
        else:
          discard

  return aux(start.getStr().split(DirSep))

template readFile*(file: AnyFile): untyped =
  readFile(file.getStr())

proc open*(file: AnyFile, mode: FileMode = fmRead): File =
  open(file.getStr(), mode)

template writeFile*(file: AnyFile, text: string): untyped =
  writeFile(file.getStr(), text)

proc appendFile*(file: AnyFile, text: string) =
  var file = open(file.getStr(), fmAppend)
  file.write(text)
  file.close()

proc writeNewFile*(file: AnyFile, text: string) =
  if exists(file):
    raise newPathError(
      toFsEntry(file),
      pekFileExists,
      &"Cannot write new file - '{file}' already exists")

  else:
    writeFile(file, text)

proc rmDir*(dir: AnyDir | string; checkDir = false) =
  ## Removes the directory dir.
  osOrNims(
    os.removeDir(dir.getStr(), checkDir),
    system.rmDir(dir.getStr(), checkDir)
  )

proc rmFile*(file: AnyFile) =
  ## Removes the file.
  osOrNims(
    os.removeFile(file.getStr()),
    system.rmFile(file.getStr())
  )

proc rmFiles*(
    dir: AbsDir,
    exts: seq[string] = @[],
    ignoreNames: seq[string] = @[]
  ) =

  for file in walkDir(dir, AbsFile, exts = exts):
    if file.name() notin ignoreNames:
      rmFile file

proc mkDir*(dir: AnyDir | string) =
  ## Creates the directory dir including all necessary subdirectories.
  ## If the directory already exists, no error is raised.
  osOrNims(os.createDir(dir.getStr()), system.mkDir(dir.getStr()))

proc ensureDir*(file: AbsFile) =
  if not exists(file.dir()):
    mkDir(file.dir())

proc mvFile*(source, dest: AnyFile) =
  ## Moves the file from to to.
  osOrNims(
    os.moveFile(source.getStr(), dest.getStr()),
    system.mvFile(source.getStr(), dest.getStr())
  )

proc mvDir*(source, dest: AnyDir) =
  ## Moves the dir from to to.
  osOrNims(
    os.moveDir(source.getStr(), dest.getStr()),
    system.mvDir(source.getStr(), dest.getStr())
  )

proc cpFile*[F1, F2: AnyFile](source: F1, dest: F2) =
  ## Copies the file from to to.
  assertExists source, "Source file is missing"
  assertExists parentDir(dest), "File target directory does not exist."
  osOrNims(
    os.copyFile(source.getStr(), dest.getStr()),
    system.cpFile(source.getStr(), dest.getStr())
  )

proc cpDir*(source, dest: AnyDir) =
  ## Copies the dir from to to.
  osOrNims(
    os.copyDir(source.getStr(), dest.getStr()),
    system.cpDir(source.getStr(), dest.getStr())
  )

proc cd*(dir: AnyDir | string) =
  ## Changes the current directory.
  osOrNims(
    os.setCurrentDir(dir.getStr()),
    system.cd(dir.getStr())
  )

proc cmkd*(dir: AnyDir) =
  mkDir(dir)
  cd(dir)

proc findExe*(bin: string): AbsFile =
  ## Searches for bin in the current working directory and then in
  ## directories listed in the PATH environment variable. Returns "" if
  ## the exe cannot be found.
  AbsFile(osAndNims(findExe(bin)))

export `**`, `*!`

proc findFile*(
    paths: seq[AbsDir], name: string,
    extensions: seq[GitGlob],
    onError: string = ""
  ): AbsFile =

  for path in paths:
    for file in walkDir(path, AbsFile, recurse = false):
      if file.name() == name and extensions.accept(
        file.ext(), invert = true):
        return file

  raise newFileSearchError(paths, $name, extensions, onError)

proc findFilesWithExt*(dir: AbsDir, ext: seq[string]): seq[AbsFile] =
  for file in walkDir(dir, AbsFile, recurse = false):
    if file.ext() in ext:
      result.add file

proc findFile*(dir: AbsDir, name: GitGlob, onError: string = ""): AbsFile =
  for file in walkDir(dir, AbsFile, recurse = false):
    if name.accept(file.splitFile2.file, true):
      return file

  raise newFileSearchError(@[dir], $name, @[], onError)


func `&&`*(lhs, rhs: string): string =
  if lhs.len == 0:
    rhs

  elif rhs.len == 0:
    lhs

  else:
    # TODO check for `||` ending to prevent incorrect string construction
    if lhs.strip().endsWith("&&") or rhs.strip.startsWith("&&"):
      # if rhs.strip().startsWith("&&"): # IMPLEMENT
      lhs & " " & rhs

    else:
      lhs & " && " & rhs


func `&&`*(ex1, ex2: ShellExpr): ShellExpr =
  ShellExpr(ex1.string && ex2.string)

func `&&`*(ex1: ShellExpr, ex2: string): ShellExpr =
  ShellExpr(ex1.string && ex2)

func `&&=`*(ex: var ShellExpr, ex2: ShellExpr) =
  ex = ex && ex2


template withStreamFile*(inFile: AnyFile, body: untyped) =
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      var file {.inject.} = open(inFile.getStr(), fmReadWrite)
      try:
        body

      finally:
        file.close()


template withNewStreamFile*(inFile: AnyFile, body: untyped) =
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      if not inFile.parentDir().exists():
        mkDir(inFile.parentDir())

      withStreamFile(inFile):
        body

iterator xPatterns(pattern: string, pattChar: char = '?'): string =
  while true:
    var next: string
    for ch in pattern:
      if ch == pattChar:
        next.add sample({'a' .. 'z', 'A' .. 'Z'})

      else:
        next.add ch

    yield next


proc getNewTempDir*(
    dirPatt: string = "????????",
    dir: AbsDir = getTempDir(),
    createDir: bool = true,
    pattChar: char = '?'
  ): AbsDir =

  ## Get name for new temporary directory
  if not anyIt(dirPatt, it == pattChar):
    # To avoid infinite search for non-existent directory in case
    # pattern does not contain necessary placeholders
    result = AbsDir(dir / RelDir(dirPatt))
    if createDir:
      mkDir result

  else:
    for next in xPatterns(dirPatt, pattChar):
      if not dirExists(dir / RelDir(next)):
        result = AbsDir(dir / RelDir(next))
        break

    if createDir:
      mkDir result

proc getTempFile*(
    dir: AbsDir,
    filePatt: string,
    assertExists: bool = true
  ): AbsFile =
  ## Get path to new temporary file in directory `dir`. To set particular
  ## extension just use `XXXXXXX.yourExt` as input `filePatt`.
  ##
  ## - @arg{assertExists} :: check if input directory exists

  if filePatt.count('X') == 0:
    return dir / RelFile(filePatt)

  assertExists(dir)
  for next in xPatterns(filePatt):
    if not fileExists(dir / RelFile(next)):
      return dir / RelFile(next)

when cbackend:
  proc getAppTempDir*(): AbsDir =
    getNewTempDir(getAppBaseName())

  proc getAppTempFile*(filePattern: string): AbsFile =
    getTempFile(getAppTempDir(), filePattern)

proc writeTempFile*(
    text: string, pattern: string = "XXXXXXXXXXX"): AbsFile =
  result = getTempDir().getTempFile(pattern)
  result.writeFile(text)

template withDir*(dir: AnyDir, body: untyped): untyped =
  ## Changes the current directory temporarily.
  var curDir = cwd()
  try:
    cd(dir.string)
    body
  finally:
    cd(curDir)

template withNewDir*(dir: AnyDir, body: untyped): untyped =
  if not exists(dir):
    mkDir(dir)

  withDir(dir, body)

template withTempDir*(clean: bool, setup, body: untyped): untyped =
  ## Create temporary directory (not don't cd into it!), execute
  ## `body` and remove it afterwards if `clean` is true
  let curDir = cwd()
  var tempRoot {.inject.} = getTempDir()
  var tempPatt {.inject.} = "????????"
  setup
  let tempDir {.inject.} = getNewTempDir(tempPatt, tempRoot)

  if dirExists(tempDir):
    rmDir(tempDir)

  mkDir(tempDir)
  cd tempDir

  try:
    body
  finally:
    if clean:
      rmDir(tempDir)

    cd(curDir)

template withTempDir*(clean: bool, body: untyped): untyped =
  withTempDir(clean, (discard nil), body)

template withTempDir*(body: untyped): untyped =
  withTempDir(true, (discard nil), body)


template withCleanDir*(dirname, body: untyped): untyped =
  let curDir = cwd()
  rmDir(dirname)
  mkDir(dirname)
  try:
    cd(dirname)
    body
  finally:
    cd(curDir)

template withTempFile*(
    dirname: untyped, pattern: string, body: untyped): untyped =

  let file {.inject.} = getTempFile(dirname, pattern)
  block:
    body



template withEnv*(
  envs: openarray[(ShellVar, string)], body: untyped): untyped =

  var prevValues: seq[(ShellVar, string)]
  var noValues: seq[ShellVar]
  for (varn, value) in envs:
    if oswrap.existsEnv(varn):
      prevValues.add (varn, oswrap.getEnv(varn))

    else:
      noValues.add varn

    oswrap.putEnv(varn, value)


  body

  for (varn, value) in prevValues:
    oswrap.putEnv(varn, value)

  for varn in noValues:
    oswrap.delEnv(varn)

macro mkDirStructure*(body: untyped): untyped =
  # TODO for haxorg - add `header-args:nim` preable with all necessary
  # imports, push `:dir` for all header-args, so code would execute in the
  # same directory by default.
  ##[

DSL for creating directory structures

#+begin_src nim
  mkDirStructure:
    file &"{name}.nimble":
      "author = haxscramper"

    dir "src":
      file &"{name}.nim"
      dir &"{name}":
        file "make_build.nim"
#+end_src

#+begin_src sh
  ls -R
#+end_src

- @edsl{file <filename> [<text>]} ::
  Create file `<filename>`, and optionally store `<text>` in it.
  - For each file block `writeFile(filename, text)` is generated.
  - If text is omited empty string is written to file insted
  - If `<text>` is a statement list only first element is used, to
    allow for multiline string literals as arguments

    #+begin_src nim
      file "test-file.txt":
        """Multiline string as file
      content"""
    #+end_src
- @edsl{dir <dirname> [<body>]} ::
  Create directory `@dirname`, cd to it and execute optional block `@body`.
- @inject{currentFile: string} :: for file blocks

]##
  let file = ident("file")
  proc aux(node: NimNode): NimNode =
    case node.kind:
      of nnkCommand, nnkCall:
        if node[0].eqIdent("file"):
          result = newStmtList()

          if node.len == 3:
            if node[2].kind != nnkStmtList:
              result.add newCall("write", file, node[2])

            else:
              for sub in node[2]:
                result.add aux(sub)

          else:
            result.add newCall("write", file, newLit(""))

          result = newBlockStmt(
            newStmtList(
              newVarStmt(
                file,
                newCall("open", node[1].blockCall(), ident"fmWrite")),
              result,
              newCall("close", file)))

        elif node[0].eqIdent("dir"):
          result = newStmtList()
          if node.len == 3:
            for subnode in node[2]:
              result.add aux(subnode)

          result = newCall(
            "withNewDir", newCall("RelDir", node[1].blockCall()), result)

        else:
          result = newCall("write", file, node)

      of nnkStmtList:
        result = newStmtList()
        for subnode in node:
          result.add aux(subnode)

      of nnkCharLit .. nnkTripleStrLit, nnkPrefix, nnkIdent, nnkSym,
         nnkCallStrLit, nnkInfix:
        result = newCall("write", file, node)

      else:
        result = node

        case node.kind:
          of nnkForStmt:
            result[2] = aux(node[2])

          of nnkIfStmt, nnkIfExpr:
            for idx, sub in node:
              result[idx] = aux(node[idx])

          of nnkCaseStmt:
            for idx, sub in node[1..^1]:
              result[idx] = aux(sub)

          of nnkElifBranch, nnkOfBranch:
            result[^1] = aux(node[^1])

          else:
            discard

  result = aux(body)
  # echo result.repr

template mkWithDirStructure*(dir: AbsDir, body: untyped): untyped =
  mkDir dir
  withDir dir:
    mkDirStructure(body)

type
  CmdLineKind* = enum
    cmdEnd,
    cmdArgument,
    cmdLongOption,
    cmdShortOption

  OptParser* = object of RootObj
    pos*: int
    inShortState: bool
    allowWhitespaceAfterColon: bool
    shortNoVal: set[char]
    longNoVal: seq[string]
    cmds: seq[string]
    idx: int
    kind*: CmdLineKind
    key*, val*: TaintedString



func parseWord(s: string, i: int, w: var string,
               delim: set[char] = {'\t', ' '}): int =
  result = i
  if result < s.len and s[result] == '\"':
    inc(result)
    while result < s.len:
      if s[result] == '"':
        inc result
        break
      add(w, s[result])
      inc(result)
  else:
    while result < s.len and s[result] notin delim:
      add(w, s[result])
      inc(result)

func initOptParser*(
  cmdline: seq[TaintedString],
  shortNoVal: set[char] = {},
  longNoVal: seq[string] = @[];
  allowWhitespaceAfterColon = true): OptParser =
  # TODO implement splitting based on different hshell configuration
  # options.

  # REVIEW move to hshell

  result.shortNoVal = shortNoVal
  result.longNoVal = longNoVal
  result.allowWhitespaceAfterColon = allowWhitespaceAfterColon

  result.cmds = newSeq[string](cmdline.len)
  for i in 0..<cmdline.len:
    result.cmds[i] = cmdline[i].string

func handleShortOption(p: var OptParser; cmd: string) =
  var i = p.pos
  p.kind = cmdShortOption
  if i < cmd.len:
    add(p.key.string, cmd[i])
    inc(i)
  p.inShortState = true
  while i < cmd.len and cmd[i] in {'\t', ' '}:
    inc(i)
    p.inShortState = false
  if i < cmd.len and (cmd[i] in {':', '='} or
      card(p.shortNoVal) > 0 and p.key.string[0] notin p.shortNoVal):
    if i < cmd.len and cmd[i] in {':', '='}:
      inc(i)
    p.inShortState = false
    while i < cmd.len and cmd[i] in {'\t', ' '}: inc(i)
    p.val = TaintedString substr(cmd, i)
    p.pos = 0
    inc p.idx
  else:
    p.pos = i
  if i >= cmd.len:
    p.inShortState = false
    p.pos = 0
    inc p.idx

func next*(p: var OptParser) =
  if p.idx >= p.cmds.len:
    p.kind = cmdEnd
    return

  var i = p.pos
  while i < p.cmds[p.idx].len and p.cmds[p.idx][i] in {'\t', ' '}: inc(i)
  p.pos = i
  setLen(p.key.string, 0)
  setLen(p.val.string, 0)
  if p.inShortState:
    p.inShortState = false
    if i >= p.cmds[p.idx].len:
      inc(p.idx)
      p.pos = 0
      if p.idx >= p.cmds.len:
        p.kind = cmdEnd
        return
    else:
      handleShortOption(p, p.cmds[p.idx])
      return

  if i < p.cmds[p.idx].len and p.cmds[p.idx][i] == '-':
    inc(i)

    if i < p.cmds[p.idx].len and p.cmds[p.idx][i] == '-':
      p.kind = cmdLongOption
      inc(i)

      i = parseWord(p.cmds[p.idx], i, p.key.string, {' ', '\t', ':', '='})

      while i < p.cmds[p.idx].len and p.cmds[p.idx][i] in {'\t', ' '}:
        inc(i)

      if i < p.cmds[p.idx].len and p.cmds[p.idx][i] in {':', '='}:
        inc(i)
        while i < p.cmds[p.idx].len and p.cmds[p.idx][i] in {'\t', ' '}: inc(i)
        # if we're at the end, use the next command line option:
        if i >= p.cmds[p.idx].len and p.idx < p.cmds.len and
            p.allowWhitespaceAfterColon:
          inc p.idx
          i = 0
        if p.idx < p.cmds.len:
          p.val = TaintedString p.cmds[p.idx].substr(i)

      elif len(p.longNoVal) > 0 and
           p.key.string notin p.longNoVal and
           p.idx+1 < p.cmds.len
        :
        p.val = TaintedString p.cmds[p.idx+1]
        inc p.idx

      else:
        p.val = TaintedString""

      inc p.idx
      p.pos = 0

    else:
      p.pos = i
      handleShortOption(p, p.cmds[p.idx])

  else:
    p.kind = cmdArgument
    p.key = TaintedString p.cmds[p.idx]
    p.val = p.key
    inc p.idx
    p.pos = 0

func cmdLineRest*(p: OptParser): TaintedString =
  result = os.quoteShellCommand(p.cmds[p.idx .. ^1]).TaintedString

func remainingArgs*(p: OptParser): seq[TaintedString] =
  result = @[]
  for i in p.idx..<p.cmds.len: result.add TaintedString(p.cmds[i])

iterator getopt*(p: var OptParser): tuple[kind: CmdLineKind, key,
    val: TaintedString] =
  p.pos = 0
  p.idx = 0
  while true:
    next(p)
    if p.kind == cmdEnd: break
    yield (p.kind, p.key, p.val)

iterator getopt*(
    cmdline: seq[TaintedString] = paramStrs(),
    shortNoVal: set[char] = {},
    longNoVal: seq[string] = @[]
  ): tuple[kind: CmdLineKind, key, val: TaintedString, idx: int] =

  var p = initOptParser(cmdline, shortNoVal = shortNoVal,
      longNoVal = longNoVal)

  var idx = 0

  while true:
    next(p)
    if p.kind == cmdEnd: break
    yield (p.kind, p.key, p.val, idx)
    inc idx

import tables

proc splitCmdLine*(
  cmdline: seq[string],
  shortNoVal: set[char] = {},
  longNoVal: seq[string] = @[],
  allowWhitespaceAfterColon = true
): tuple[args: seq[string], opts: Table[string, seq[string]]] =

  var p = initOptParser(
    cmdline, shortNoVal, longNoVal, allowWhitespaceAfterColon)

  while true:
    p.next()
    case p.kind
    of cmdEnd: break
    of cmdShortOption, cmdLongOption:
      if p.key in result.opts:
        result.opts[p.key].add p.val
      else:
        result.opts[p.key] = @[p.val]
    of cmdArgument:
      result.args.add p.key

proc paramVal*(param: string): seq[string] =
  let (_, table) = splitCmdLine(paramStrs())
  table[param]

proc paramVal*(param: string, default: seq[string]): seq[string] =
  let (_, table) = splitCmdLine(paramStrs())
  table.getOrDefault(param, default)


import std/distros except detectOs
export distros

const
  DistributionGenericOsNames* = {Windows, Posix, Linux}
  DistributionDebianDerivatives* = {
    Ubuntu, Elementary, SteamOs, Knoppix, Debian}

when cbackend:
  import osproc

  proc cmdRelease(cmd: string, cache: var string): string =
    if cache.len == 0:
      when nimvm:
        cache = gorge(cmd)
      else:
        cache = execProcess(cmd)

    result = cache

else:
  proc cmdRelease(cmd: string, cache: var string): string =
    if cache.len == 0:
      cache = gorge(cmd)


    result = cache

proc uname(cache: var string): string =
  cmdRelease("uname -a", cache)

proc osReleaseID(cache: var string): string =
  cmdRelease("cat /etc/os-release | grep ^ID=", cache)

proc release(cache: var string): string =
  cmdRelease("lsb_release -d", cache)

proc hostnamectl(cache: var string): string =
  cmdRelease("hostnamectl", cache)


proc detectOsWithAllCmd(d: Distribution, cache: var array[4, string]): bool =
  let dd = toLowerAscii($d)
  result = dd in toLowerAscii(osReleaseID(cache[0])) or
           dd in toLowerAscii(release(cache[1])) or
           dd in toLowerAscii(uname(cache[2])) or
           ("operating system: " & dd) in toLowerAscii(hostnamectl(cache[3]))


proc detectOs*(d: Distribution, cache: var array[4, string]): bool =
  case d:
    of Distribution.Windows: result = defined(windows)
    of Distribution.Posix: result = defined(posix)
    of Distribution.MacOSX: result = defined(macosx)
    of Distribution.Linux: result = defined(linux)
    of Distribution.BSD: result = defined(bsd)
    else:
      when defined(bsd):
        case d:
          of Distribution.FreeBSD, Distribution.OpenBSD:
            result = $d in uname(cache[2])
          else:
            result = false
      elif defined(linux):
        case d:
          of Distribution.Gentoo:
            result = ("-" & $d & " ") in uname(cache[2])
          of Distribution.Elementary,
             Distribution.Ubuntu,
             Distribution.Debian,
             Distribution.Fedora,
             Distribution.OpenMandriva,
             Distribution.CentOS,
             Distribution.Alpine,
             Distribution.Mageia,
             Distribution.Zorin:
            result = toLowerAscii($d) in osReleaseID(cache[0])
          of Distribution.RedHat:
            result = "rhel" in osReleaseID(cache[0])
          of Distribution.ArchLinux:
            result = "arch" in osReleaseID(cache[0])
          of Distribution.NixOS:
            result = existsEnv($$NIX_BUILD_TOP) or
            existsEnv(ShellVar "__NIXOS_SET_ENVIRONMENT_DONE")
          of Distribution.OpenSUSE:
            result =
              "suse" in toLowerAscii(uname(cache[2])) or
              "suse" in toLowerAscii(release(cache[1]))
          of Distribution.GoboLinux:
            result = "-Gobo " in uname(cache[2])
          of Distribution.Solaris:
            let uname = toLowerAscii(uname(cache[2]))
            result =
              ("sun" in uname and false #[ FIXME ]#) or
              ("solaris" in uname)
          of Distribution.Haiku:
            result = defined(haiku)
          else:
            result = detectOsWithAllCmd(d, cache)
      else:
        result = false

proc getCurrentOs*(): set[Distribution] =
  var res {.global.}: set[Distribution]

  if res.len > 0:
    return res

  var cache: array[4, string]
  if detectOs(Windows, cache):      result.incl Windows
  if detectOs(Posix, cache):        result.incl Posix
  if detectOs(MacOSX, cache):       result.incl MacOSX
  if detectOs(Linux, cache):        result.incl Linux
  if detectOs(Ubuntu, cache):       result.incl Ubuntu
  if detectOs(Debian, cache):       result.incl Debian
  if detectOs(Gentoo, cache):       result.incl Gentoo
  if detectOs(Fedora, cache):       result.incl Fedora
  if detectOs(RedHat, cache):       result.incl RedHat
  if detectOs(OpenSUSE, cache):     result.incl OpenSUSE
  if detectOs(Manjaro, cache):      result.incl Manjaro
  if detectOs(Elementary, cache):   result.incl Elementary
  if detectOs(Zorin, cache):        result.incl Zorin
  if detectOs(CentOS, cache):       result.incl CentOS
  if detectOs(Deepin, cache):       result.incl Deepin
  if detectOs(ArchLinux, cache):    result.incl ArchLinux
  if detectOs(Antergos, cache):     result.incl Antergos
  if detectOs(PCLinuxOS, cache):    result.incl PCLinuxOS
  if detectOs(Mageia, cache):       result.incl Mageia
  if detectos(Lxle, cache):         result.incl Lxle
  if detectOs(Solus, cache):        result.incl Solus
  if detectOs(Lite, cache):         result.incl Lite
  if detectOs(Slackware, cache):    result.incl Slackware
  if detectOs(Androidx86, cache):   result.incl Androidx86
  if detectOs(Puppy, cache):        result.incl Puppy
  if detectOs(Peppermint, cache):   result.incl Peppermint
  if detectOs(Tails, cache):        result.incl Tails
  if detectOs(AntiX, cache):        result.incl AntiX
  if detectOs(Kali, cache):         result.incl Kali
  if detectOs(SparkyLinux, cache):  result.incl SparkyLinux
  if detectOs(Apricity, cache):     result.incl Apricity
  if detectOs(BlackLab, cache):     result.incl BlackLab
  if detectOs(Bodhi, cache):        result.incl Bodhi
  if detectOs(TrueOS, cache):       result.incl TrueOS
  if detectOs(ArchBang, cache):     result.incl ArchBang
  if detectOs(KaOS, cache):         result.incl KaOS
  if detectOs(WattOS, cache):       result.incl WattOS
  if detectOs(Korora, cache):       result.incl Korora
  if detectOs(Simplicity, cache):   result.incl Simplicity
  if detectOs(RemixOS, cache):      result.incl RemixOS
  if detectOs(OpenMandriva, cache): result.incl OpenMandriva
  if detectOs(Netrunner, cache):    result.incl Netrunner
  if detectOs(Alpine, cache):       result.incl Alpine
  if detectOs(BlackArch, cache):    result.incl BlackArch
  if detectOs(Ultimate, cache):     result.incl Ultimate
  if detectOs(Gecko, cache):        result.incl Gecko
  if detectOs(Parrot, cache):       result.incl Parrot
  if detectos(Knoppix, cache):      result.incl Knoppix
  if detectOs(GhostBSD, cache):     result.incl GhostBSD
  if detectOs(Sabayon, cache):      result.incl Sabayon
  if detectOs(Salix, cache):        result.incl Salix
  if detectos(Q4os, cache):         result.incl Q4os
  if detectOs(ClearOS, cache):      result.incl ClearOS
  if detectOs(Container, cache):    result.incl Container
  if detectos(Rosa, cache):         result.incl Rosa
  if detectOs(Zenwalk, cache):      result.incl Zenwalk
  if detectOs(Parabola, cache):     result.incl Parabola
  if detectOs(ChaletOS, cache):     result.incl ChaletOS
  if detectOs(BackBox, cache):      result.incl BackBox
  if detectOs(MXLinux, cache):      result.incl MXLinux
  if detectOs(Vector, cache):       result.incl Vector
  if detectOs(Maui, cache):         result.incl Maui
  if detectOs(Qubes, cache):        result.incl Qubes
  if detectOs(RancherOS, cache):    result.incl RancherOS
  if detectOs(Oracle, cache):       result.incl Oracle
  if detectOs(TinyCore, cache):     result.incl TinyCore
  if detectOs(Robolinux, cache):    result.incl Robolinux
  if detectOs(Trisquel, cache):     result.incl Trisquel
  if detectOs(Voyager, cache):      result.incl Voyager
  if detectOs(Clonezilla, cache):   result.incl Clonezilla
  if detectOs(SteamOS, cache):      result.incl SteamOS
  if detectOs(Absolute, cache):     result.incl Absolute
  if detectOs(NixOS, cache):        result.incl NixOS
  if detectos(Austrumi, cache):     result.incl Austrumi
  if detectOs(Arya, cache):         result.incl Arya
  if detectOs(Porteus, cache):      result.incl Porteus
  if detectOs(AVLinux, cache):      result.incl AVLinux
  if detectOs(Elive, cache):        result.incl Elive
  if detectOs(Bluestar, cache):     result.incl Bluestar
  if detectOs(SliTaz, cache):       result.incl SliTaz
  if detectOs(Solaris, cache):      result.incl Solaris
  if detectOs(Chakra, cache):       result.incl Chakra
  if detectOs(Wifislax, cache):     result.incl Wifislax
  if detectOs(Scientific, cache):   result.incl Scientific
  if detectOs(ExTiX, cache):        result.incl ExTiX
  if detectOs(Rockstor, cache):     result.incl Rockstor
  if detectOs(GoboLinux, cache):    result.incl GoboLinux
  if detectos(BSD, cache):          result.incl BSD
  if detectOs(FreeBSD, cache):      result.incl FreeBSD
  if detectOs(OpenBSD, cache):      result.incl OpenBSD
  if detectOs(DragonFlyBSD, cache): result.incl DragonFlyBSD
  if detectOs(Haiku, cache):        result.incl Haiku

  res = result

proc getOsPackageManagerCmd*(): (string, bool) =
  ## Returns the distro's native command line to install 'foreignPackageName'
  ## and whether it requires root/admin rights.
  let d = getCurrentOs()
  if Windows in d:
    ("Chocolatey install", false)
  elif BSD in d:
    ("ports install", true)
  elif Linux in d:
    if len(d * {Ubuntu, Elementary, SteamOs, Knoppix, Debian}) > 0:
      ("apt-get install", true)
    elif Gentoo in d:                       ("emerge install", true)
    elif Fedora in d:                       ("yum install", true)
    elif RedHat in d:                       ("rpm install", true)
    elif OpenSuse in d:                     ("yast -i", true)
    elif Slackware in d:                    ("installpkg", true)
    elif OpenMandriva in d:                 ("urpmi", true)
    elif ZenWalk in d:                      ("netpkg install", true)
    elif NixOS in d:                        ("nix-env -i", false)
    elif len(d * {Solaris, FreeBSD}) > 0:   ("pkg install", true)
    elif OpenBSD in d:                      ("pkg_add", true)
    elif PCLinuxOS in d:                    ("rpm -ivh", true)
    elif len(d * {ArchLinux, Manjaro}) > 0: ("pacman -S", true)
    else:
      ("<your package manager here> install", true)
  elif Haiku in d:
    ("pkgman install", true)
  else:
    ("brew install", false)

proc getInstallCmd*(package: string): string =
  let (cmd, sudo) = getOsPackageManagerCmd()
  result = cmd & " " & package
  if sudo:
    result = "sudo " & result


proc getInstalledPackagesCmd*(): string =
  let d = getCurrentOs()
  if len(d * {ArchLinux, Manjaro}) > 0:
    result = "pacman -Qq"
  elif len(d * DistributionDebianDerivatives) > 0:
    result =  "dpkg-query -f '${binary:Package}\n' -W"

proc isPackageInstalled*(pack: string): bool =
  let cmd = getInstalledPackagesCmd()
  if cmd.len == 0: raiseAssert("#[ IMPLEMENT ]#")
  var cache: string
  # echo cmd
  let release = cmdRelease(cmd & " | grep ^" & pack & "$ ", cache)
  # echo "release ", release
  return release.len > 0

proc getMissingDependencies*(deplist: openarray[tuple[
    distros: set[Distribution],
    packgs: seq[string]
  ]]): seq[tuple[package, installCmd: string]] =

  let currOs = getCurrentOs()
  # echo currOs
  for (distros, packages) in deplist:
    # echo distros, packages
    if len(distros * currOs) > 0:
      for pack in packages:
        # echo pack
        if not isPackageInstalled(pack):
          result.add (pack, getInstallCmd(pack))

when canImport(compiler/pathutils):
  converter toAbsoluteDir*(dir: AbsDir): AbsoluteDir =
    AbsoluteDir(dir.string)

  converter toAbsoluteFile*(file: AbsFile): AbsoluteFile =
    AbsoluteFile(file.string)

proc parseJson*(file: AnyFile): JsonNode =
  parseJson(readFile(file.getStr()))


proc newFileStream*(
  filename: AbsFile | RelFile;
  mode: FileMode = fmRead; bufSize: int = -1): owned FileStream =

  case mode:
    of fmRead:
      assertExists(filename, "Cannot open file for stream reading")

    of fmWrite:
      if not exists(filename):
        assertExists(
          filename.dir(),
          &["Cannot write to non-existent file - parent directory does not ",
            "exist either."])

    else:
      discard


  return newFileStream(filename.string, mode, bufSize)

proc newWriteStream*(file: AbsFile|RelFile): FileStream =
  newFileStream(file, fmWrite)

type
  OutStringStream* = ref object of StreamObj
    str: ptr string

proc newOutStringStream*(target: var string): OutStringStream =
  result = OutStringStream(str: addr target,)
  result.writeDataImpl = proc(
    s: OutStringStream; buffer: pointer; bufLen: int) =
    let len = s.str[].len
    s.str[].setLen(s.str[].len + bufLen)
    copyMem(addr(s.str[len]), buffer, bufLen)
