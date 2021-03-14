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

# {.experimental: "caseStmtMacros".}

import std/[strutils, macros, random, hashes, json,
            strformat, sequtils, options, streams]

import ../algo/hstring_algo
import ../base_errors

from os import nil


export os.PathComponent
export os.FileInfo
export os.FilePermission
export os.ReadDirEffect, os.ReadEnvEffect,
       os.WriteDirEffect, os.WriteEnvEffect

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

type
  AnyPath* = AbsFile | AbsDir | RelFile | RelDir |
             FsFile | FsDir | FsEntry | FsTree

  AnyDir* = AbsDir | RelDir | FsDir
  AnyFile* = AbsFile | RelFile | FsFile

func newPathError*(
  entry: FsEntry, kind: PathErrorKind, msg: string): PathError =
  PathError(msg: msg & " (" & $kind & ")", kind: kind, entry: entry)

template newPathError*(
  entry: FsEntry, kind: PathErrorKind, msg: typed): PathError =
  newPathError(entry, kind, fmtJoin msg)

template ignorePathErrors*(
  kinds: set[PathErrorKind], body: untyped): untyped =
  try:
    body
  except PathError:
    let err = PathError(getCurrentException())
    if err.kind notin kinds:
      raise err

func `==`*(se1, se2: ShellExpr): bool = se1.string == se2.string

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
  path.getStr().startsWith(expr)


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


converter toFsFile*(absFile: AbsFile): FsFile =
  FsFile(isRelative: false, absFile: absFile)

converter toFsFile*(relFile: RelFile): FsFile =
  FsFile(isRelative: true, relFile: relFile)

converter toFsFile*(optFile: Option[AnyFile]): Option[FsFile] =
  if optFile.isSome():
    return some(toFsFile(optFile.get()))

converter toFsDir*(relDir: RelDir): FsDir =
  FsDir(isRelative: true, relDir: relDir)

converter toFsDir*(absDir: AbsDir): FsDir =
  FsDir(isRelative: false, absDir: absDir)

converter toFsDirOption*(absDir: Option[AbsDir]): Option[FsDir] =
  if absDir.isSome():
    return some FsDir(isRelative: false, absDir: absDir.get())

converter toFsDirOption*(relDir: Option[RelDir]): Option[FsDir] =
  if relDir.isSome():
    return some FsDir(isRelative: true, relDir: relDir.get())

converter toFsDirSeq*(drs: seq[AbsDir]): seq[FsDir] =
  for d in drs:
    result.add d.toFsDir()

converter toFsDirSeq*(drs: seq[RelDir]): seq[FsDir] =
  for d in drs:
    result.add d.toFsDir()

converter toFsFileSeq*(fls: seq[AbsFile]): seq[FsFile] =
  for f in fls:
    result.add f.toFsFile()

converter toFsFileSeq*(fls: seq[RelFile]): seq[FsFile] =
  for f in fls:
    result.add f.toFsFile()

converter toFsEntry*(path: AnyPath): FsEntry =
  when path is FsEntry:
    path
  elif path is FsDir:
    FsEntry(kind: os.pcDir, dir: path)
  elif path is FsFile:
    FsEntry(kind: os.pcFile, file: path)
  elif path is AbsFile or path is RelFile:
    path.toFsFile().toFsEntry()
  elif path is RelDir or path is AbsDir:
    path.toFsDir().toFsEntry()


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

proc splitDir*(dir: RelDir): tuple[head: RelDir, tail: RelDir] =
  let (head, tail) = os.splitPath(dir.getStr())
  result.head = RelDir(head)
  result.tail = RelDir(tail)


template currentSourceDir*(): AbsDir {.dirty.} =
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

proc dir*(file: AbsFile): AbsDir = parentDir(file)
proc dir*(file: RelFile): RelDir = parentDir(file)
proc dir*(dir: AbsDir): AbsDir = parentDir(dir)
proc dir*(dir: RelDir): RelDir = parentDir(dir)

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
    result.ext = tmp[1..^1].join(".")
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

func hasExt*(file: AnyFile, exts: openarray[string] = @[]): bool =
  let (_, _, ext) = file.splitFile()
  if exts.len == 0:
    return ext.len > 0
  else:
    return ext in exts


func hasExt*(file: AnyFile, ext: string): bool =
  let (_, _, fileExt) = file.splitFile()
  return ext == fileExt

func ext*(file: AnyFile): string = file.splitFile().ext
func name*(file: AnyFile): string = file.splitFile().name

proc assertValid*(path: AnyPath): void =
  if path.getStr().len < 1:
    raise newPathError(
      path, pekInvalidEntry,
      "Path validation failed - empty string is not a valid path"
    )

  when path is RelPath:
    if os.isAbsolute(path.getStr()):
      raise newPathError(path, pekExpectedRel): fmtJoin:
        "Path '{path.getStr()}' has type {$typeof(path)}, but"
        "contains invalid string - expected relative path"

  elif path is AbsPath:
    if not os.isAbsolute(path.getStr()):
      raise newPathError(path, pekExpectedAbs): fmtJoin:
        "Path '{path.getStr()}' has type {$typeof(path)}, but"
        "contains invalid string - expected absolute path"

  elif path is FsFile:
    if path.isRelative:
      assertValid(path.relFile)

    else:
      assertValid(path.absFile)

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

  proc isAbsolute*(path: AnyPath): bool =
    os.isAbsolute(path.getStr())

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

proc `==`*(dir1, dir2: AnyDir): bool =
  (dir1.getStr() == dir2.getStr())

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
  ): FsEntry =

  for (comp, path) in os.walkDir(dir.getStr(), relative = relative):
    let comp = comp
    if comp in yieldFilter:
      walkYieldImpl()

iterator walkDirRec*(
    dir: AnyDir,
    yieldFilter = {os.pcFile},
    followFilter = {os.pcDir},
    relative = false,
    checkDir = false
  ): FsEntry =

  for path in os.walkDirRec(
    dir.getStr(),
    yieldFilter = yieldFilter,
    followFilter = followFilter,
    relative = relative,
    checkDir = checkDir,
  ):
    let comp = os.getFileInfo(
      if relative:
        os.joinPath(dir.getStr(), path)

      else:
        path
    ).kind

    walkYieldImpl()

iterator walkDir*[T: AnyPath](
    dir: AnyDir,
    resType: typedesc[T],
    recurse: bool = false,
    yieldLinks: bool = true,
    exts: seq[string] = @[],
    assertExists: bool = true
  ): T =

  ## Iterate over entries in `dir`, yielding only those that match
  ## `resType`.
  ##
  ## For example, `when resType is RelFile` you will only get entries that
  ## represent file, and path will be relative. This is a higher-level
  ## wrapper that takes advantage of distinct types for files/directories
  ## and eliminates need to filter by entry kind in the calling loop.
  runnableExamples:
    for dir in walkDir(~".config", RelDir):
      assert dir is RelDir

  if assertExists:
    assertExists(dir, "Cannot iterate over non-existent directory.")

  when dir is RelDir and resType is AbsPath:
    let dir = toAbsDir(dir)

  template optYield(entry: untyped): untyped =
    if exts.len == 0 or
      (exts.len > 0 and ext(entry) in exts):
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
      resSet.incl pcLinkToDir

  elif resType is AbsFile | RelFile | FsFile:
    resSet.incl pcFile
    if yieldLinks:
      resSet.incl pcLinkToFile

  elif resType is FsEntry:
    resSet.incl {pcFile, pcDir}
    if yieldLinks:
      resSet.incl {pcLinkToFile, pcLinkToDir}


  if recurse:
    for entry in walkDirRec(
      dir, relative = relative, yieldFilter = resSet):

      yieldImpl()

  else:
    for entry in walkDir(
      dir, relative = relative, yieldFilter = resSet):

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

proc set*[T: int | char | bool](v: ShellVar, val: T | seq[T]) =
  when val is seq:
    let val = join(val, ",")

  else:
    let val = $val

  v.setEnv(val)

proc get*(
    v: ShellVar,
    TArg: typedesc[int] | typedesc[char] | typedesc[bool]
  ): auto =

  when TArg is int:
    return parseInt(get(v))

  elif TArg is char:
    return get(v)[0]

  else:
    return parseBool(get(v))



proc put*(v: ShellVar, val: string) = v.setEnv(val)
proc exists*(v: ShellVar): bool = v.existsEnv()

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

      raise newPathError(file, pekExpectedFile, msg)

  if not file.fileExists():
    raise newPathError(
      file, pekNoSuchEntry, &"No such file '{path.string}'. {onMissing}")


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

      raise newPathError(dir, pekExpectedDir, msg)

  if not dir.dirExists():
    raise newPathError(
      dir, pekNoSuchEntry, &"No such directory '{path.string}'. {onMissing}")




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

proc listFiles*(dir: AnyDir): seq[AbsFile] =
  ## Lists all the files (non-recursively) in the directory dir.
  when defined(NimScript):
    return system.listFiles(dir)

  else:
    for (kind, path) in os.walkDir(dir.getStr()):
      if kind == pcFile:
        result.add AbsFile(path)

func flatFiles*(tree: FsTree): seq[FsTree] =
  if tree.isDir:
    for sub in tree.sub:
      result.add sub.flatFiles()
  else:
    result = @[tree]


func withoutPrefix*(file: AbsFile, pref: AbsDir): RelFile =
  # assert startsWith($file, addSuffix($pref, "/"))
  RelFile(dropPrefix($file, addSuffix($pref, "/")))

func withExt*(f: FsTree, ext: string): FsTree =
  if f.isDir:
    raiseArgumentError("Cannot add extension to FsTree file")

  result = f
  result.ext = ext

func withExt*[F: AbsFile | RelFIle](
    f: F, newExt: string, replace: bool = true
  ): F =
  ## Set file extension by either replacing it (default) or adding suffix
  ## `.<extension-string>`

  if replace:
    let (parent, file, ext) = f.splitFile()
    let exts = ext.split(".")

    let resExt = join(exts[0..^2] & newExt.dropPrefix("."), ".")
    parent /. (file & (if resExt.len > 0: "." & resExt else: ""))

  else:
    when F is AbsFile:
      AbsFile(f.getStr() & hstring_algo.addPrefix(newExt, "."))

    else:
      RelFile(f.getStr() & hstring_algo.addPrefix(newExt, "."))


func withBasePrefix*[F: AbsFile | RelFile | FsFile](
  f: F, prefix: string): F =
  ## Return copy of the file path `f` with new basename for a file. E.g.
  ## `/tmp/hello.cpp + prefix_ -> /tmp/prefix_hello.cpp`
  when f is FsFile:
    if f.isRelative:
      result = toFsFile(withBasePrefix(f.relFile, prefix))

    else:
      result = toFsFile(withBasePrefix(f.absFile, prefix))

    # result = toFsFile(
    #   withBasePrefix((
    #     if f.isRelative: f.relFile else: f.absFile
    #   ), prefix))

  else:
    let (parent, file, ext) = os.splitFile(f.getStr())
    let res = os.joinpath(parent, prefix & file & ext)

    when F is AbsFile:
      result = AbsFile(res)

    else:
      result = RelFile(res)

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

template writeFile*(file: AnyFile, text: string): untyped =
  writeFile(file.getStr(), text)

proc writeNewFile*(file: AnyFile, text: string) =
  if exists(file):
    raise newPathError(
      file, pekFileExists,
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

proc mkDir*(dir: AnyDir | string) =
  ## Creates the directory dir including all necessary subdirectories.
  ## If the directory already exists, no error is raised.
  osOrNims(os.createDir(dir.getStr()), system.mkDir(dir.getStr()))

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
  block:
    var file {.inject.} = open(inFile.getStr(), fmReadWrite)
    try:
      body

    finally:
      file.close()


template withNewStreamFile*(inFile: AnyFile, body: untyped) =
  block:
    if not inFile.parentDir().exists():
      mkDir(inFile.parentDir())

    withStreamFile(inFile):
      body

iterator xPatterns(pattern: string): string =
  while true:
    var next: string
    for ch in pattern:
      if ch == 'X':
        next.add sample({'a' .. 'z', 'A' .. 'Z'})

      else:
        next.add ch

    yield next


proc getNewTempDir*(
    dir: string | AbsDir = getTempDir(),
    dirPatt: string = "XXXXXXXX",
    createDir: bool = true
  ): AbsDir =

  ## Get name for new temporary directory
  if not anyIt(dirPatt, it == 'X'):
    # To avoid infinite search for non-existent directory in case
    # pattern does not contain necessary placeholders
    return AbsDir(dir / RelDir(dirPatt))

  else:
    for next in xPatterns(dirPatt):
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


  assertExists(dir)
  for next in xPatterns(filePatt):
    if not fileExists(dir / RelFile(next)):
      return dir / RelFile(next)


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
  var tempPatt {.inject.} = "XXXXXXXXX"
  setup
  let tempDir {.inject.} = getNewTempDir(tempRoot, tempPatt)

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



template withEnv*(envs: openarray[(string, string)], body: untyped): untyped =
  var prevValues: seq[(string, string)]
  var noValues: seq[string]
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
- @ndsl{dir <dirname> [<body>]} ::
  Create directory `@dirname`, cd to it and execute optional block `@body`.
- @inject{currentFile: string} :: for file blocks

]##
  proc aux(node: NimNode): NimNode =
    case node.kind:
      of nnkCommand:
        if node[0].eqIdent("file"):
          result = newCall("writeFile", ident "currentFile")

          if node.len == 3:
            if node[2].kind != nnkStmtList:
              result.add node[2]

            else:
              result.add node[2][0]

          else:
            result.add newLit("")

          result = newBlockStmt(
            newStmtList(
              newLetStmt(ident "currentFile", node[1]),
              result
            )
          )

        elif node[0].eqIdent("dir"):
          result = newStmtList()
          if node.len == 3:
            for subnode in node[2]:
              result.add aux(subnode)

          result = newCall("withNewDir", newCall("RelDir", node[1]), result)

      of nnkStmtList:
        result = newStmtList()
        for subnode in node:
          result.add aux(subnode)

      else:
        result = node


  result = aux(body)

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
