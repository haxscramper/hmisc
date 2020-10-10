## `os` module function wrappers than can work with both nimscript and
## regular target.

import strutils, macros, random, hashes
import ../algo/hstring_algo
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
export os.quoteShell


const cbackend* = not (defined(nimscript) or defined(js))

when cbackend:
  import times, pathnorm, posix

## `os` wrapper with more typesafe paths. Mostly taken from compiler
## pathutils and stdlib.

type
  AbsFile* = distinct string
  AbsDir* = distinct string
  AbsPath* = AbsFile | AbsDir

  RelFile* = distinct string
  RelDir* = distinct string
  RelPath* = RelFile | RelDir

type
  FsFile = object
    case isRelative*: bool
      of true:
        relFile*: RelFile
      of false:
        absFile*: AbsFile

  FsDir = object
    case isRelative*: bool
      of true:
        relDir*: RelDir
      of false:
        absDir*: AbsDir

  FsEntry = object
    case kind*: os.PathComponent
      of os.pcFile, os.pcLinkToFile:
        file*: FsFile
      of os.pcDir, os.pcLinkToDir:
        dir*: FsDir

type
  PathErrorKind* = enum
    pekExpectedDir
    pekExpectedFile
    pekExpectedAbs
    pekExpectedRel
    pekNoSuchEntry
    pekInvalidEntry

  PathError* = ref object of OSError
    entry*: FsEntry
    kind*: PathErrorKind

type
  AnyPath* = AbsFile | AbsDir | RelFile | RelDir | FsFile | FsDir | FsEntry
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
  else:
    path.string

# when not cbackend:
#   type
#     ReadEnvEffect* = object of RootEffect

func hash*(path: AnyPath): Hash = hash(path.getStr())
func len*(path: AnyPath): int = path.getStr().len()
func contains*(path: AnyPath, substr: string): bool =
  path.getStr().contains substr

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

func parseFile*(file: string): FsFile =
  # TODO check if path is not directory-only
  if os.isAbsolute(file):
    FsFile(isRelative: false, absFile: AbsFile(file))
  else:
    FsFile(isRelative: true, relFile: RelFile(file))

func `$`*(path: AnyPath): string = path.getStr()
func `$`*(entry: FsEntry): string = entry.getStr()
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


converter toFile*(absFile: AbsFile): FsFile =
  FsFile(isRelative: false, absFile: absFile)

converter toFile*(relFile: RelFile): FsFile =
  FsFile(isRelative: true, relFile: relFile)

converter toDir*(relDir: RelDir): FsDir =
  FsDir(isRelative: true, relDir: relDir)

converter toDir*(absDir: AbsDir): FsDir =
  FsDir(isRelative: false, absDir: absDir)

converter toFsEntry*(path: AnyPath): FsEntry =
  when path is FsEntry:
    path
  elif path is FsDir:
    FsEntry(kind: os.pcDir, dir: path)
  elif path is FsFile:
    FsEntry(kind: os.pcFile, file: path)
  elif path is AbsFile or path is RelFile:
    path.toFile().toFsEntry()
  elif path is RelDir or path is AbsDir:
    path.toDir().toFsEntry()

converter toAbsDir*(str: string): AbsDir =
  assert os.isAbsolute(str),
          "Path '" & str & "' is not an absolute directory"

  AbsDir(str)

proc getCurrentDir*(): AbsDir =
  ## Retrieves the current working directory.
  AbsDir(osAndNims(getCurrentDir()))

proc cwd*(): AbsDir = getCurrentDir()

proc normalizePathEnd*(path: var AnyPath; trailingSep = false): void =
  ## Ensures ``path`` has exactly 0 or 1 trailing `DirSep`, depending on
  ## ``trailingSep``, and taking care of edge cases: it preservers whether
  ## a path is absolute or relative, and makes sure trailing sep is `DirSep`,
  ## not `AltSep`. Trailing `/.` are compressed, see examples.
  os.normalizePath(AnyPath.string)

proc joinPath*(head: AbsDir, tail: RelFile): AbsFile =
  AbsFile(os.joinPath(head.string, tail.string))

proc joinPath*(head: AbsDir, tail: RelDir): AbsDir =
  AbsDir(os.joinPath(head.string, tail.string))

proc joinPath*(head: RelDir, tail: RelFile): RelFile =
  RelFile(os.joinPath(head.string, tail.string))

template `/`*(head, tail: AnyPath | string): untyped =
  joinPath(head, RelDir(tail))

template `/`*(head: AnyDir, tail: RelFile): untyped =
  joinPath(head, tail)

template `/.`*(head: AnyPath, file: RelFile | string): untyped =
  joinPath(head, RelFile(file))

proc splitPath*(path: AbsFile): tuple[head: AbsDir, tail: RelFile] =
  let (head, tail) = os.splitPath(path.string)
  result.head = AbsDir(head)
  result.tail = RelFile(head)

proc startsWith*(path: AnyPath, str: string): bool =
  path.getStr().startsWith(str)

proc relativePath*(path: AbsDir, base: AbsDir): RelDir =
  RelDir(os.relativePath(path.string, base.string))

proc parentDir*(path: AbsPath): AbsDir =
  AbsPath(os.parentDir(path.string))

proc parentDir*(path: RelPath): RelDir =
  RelDir(os.parentDir(path.string))

proc tailDir*(path: AnyDir): string = os.tailDir(path.string)

# TODO
# proc isRootDir*(path: string): bool
#   ## Checks whether a given `path` is a root directory.

iterator parentDirs*(
  path: AnyPath,
  fromRoot: bool = false,
  inclusive: bool = true): AbsPath =

  for p in os.parentDirs(path.string, fromRoot, inclusive):
    yield AbsPath(p)

proc `/../`*(head: AbsDir, repeat: int): AbsDir =
  result = head
  for i in 0 ..< repeat:
    result = AbsDir(os.parentDir(result.string))

proc `/../`*(head: AbsDir, tail: RelDir): AbsDir =
  AbsDir(os.parentDir(head.string).AbsDir / tail.RelDir)

proc searchExtPos*(path: AnyFile): int = os.searchExtPos(path.string)

proc splitFile*(path: AnyFile): tuple[
  dir: AbsDir, name, ext: string] =
  let (dir, name, ext) = os.splitFile(path.getStr())
  result.dir = AbsDir(dir)
  result.name = name
  result.ext = ext

func hasExt*(file: AnyFile, exts: openarray[string] = @[]): bool =
  let (_, _, ext) = file.splitFile()
  if exts.len == 0:
    return ext.len > 0
  else:
    return ext in exts

proc assertValid*(path: AnyPath): void =
  if path.getStr().len < 1:
    raise newPathError(path, pekInvalidEntry, "Empty path string")

  when path is RelPath:
    if os.isAbsolute(path.getStr()):
      raise newPathError(path, pekExpectedRel): fmtJoin:
        "Input path '{path.getStr()}' has type {$typeof(path)}, but"
        "contains invalid string - expected relative path"
  elif path is AbsPath:
    if not os.isAbsolute(path.getStr()):
      raise newPathError(path, pekExpectedAbs): fmtJoin:
        "Input path '{path.getStr()}' has type {$typeof(path)}, but"
        "contains invalid string - expected absolute path"
  else:
    static: raiseAssert("#[ IMPLEMENT ]#")

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

  proc absolute*(file: RelFile, root: AbsDir = getCurrentDir()): AbsFile =
    AbsFile(os.absolutePath(file.string, root.string))

  proc isRelative*(file: AnyPath): bool =
    assertValid file
    when file is FsFile:
      return file.isRelative
    elif file is RelPath:
      return true
    elif file is AbsPath:
      return false

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


iterator walkDir*(
  dir: AnyDir; relative: bool = false, checkDir: bool = false): FsEntry =
  for (comp, path) in os.walkDir(dir.string):
    let comp = comp
    case comp:
      of os.pcFile, os.pcLinkToFile:
        yield FsEntry(kind: comp, file:
          block:
            if relative:
              RelFile(path).toFile()
            else:
              AbsFile(path).toFile()
        )

      of os.pcDir, os.pcLinkToDir:
        yield FsEntry(kind: comp, dir:
          block:
            if relative:
              RelDir(path).toDir()
            else:
              AbsDir(path).toDir()
        )

iterator walkDirRec*(
  dir: AnyDir,
  yieldFilter = {os.pcFile},
  followFilter = {os.pcDir},
  relative = false,
  checkDir = false
         ): FsEntry =

  for dir in os.walkDirRec(
    dir.string,
    yieldFilter,
    followFilter,
    relative,
    checkDir,
  ):
    discard

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

  proc getAppFilename*(): string = os.getAppFilename()

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




proc paramStr*(i: int): string =
  ## Retrieves the i'th command line parameter.
  osAndNims(paramStr(i))

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
      result.add paramStr(i)


proc getEnv*(key: string; default = ""): string =
  ## Retrieves the environment variable of name key.
  osAndNims(getEnv(key, default))

proc existsEnv*(key: string): bool =
  ## Checks for the existence of an environment variable named key.
  osAndNims(existsEnv(key))

proc putEnv*(key, val: string): void =
  ## Sets the value of the environment variable named key to val.
  osAndNims(putEnv(key, val))

proc setEnv*(key, val: string): void =
  ## Sets the value of the environment variable named key to val.
  putEnv(key, val)

proc delEnv*(key: string) =
  ## Deletes the environment variable named key.
  osAndNims(delEnv(key))

proc fileExists*(filename: AnyFile): bool =
  ## Checks if the file exists.
  osAndNims(fileExists(filename.string))

proc dirExists*(dir: AnyDir): bool =
  ## Checks if the directory dir exists.
  osAndNims(dirExists(dir.getStr()))

proc existsFile*(filename: AnyFile): bool =
  ## An alias for fileExists.
  osAndNims(existsFile(filename.getStr()))


proc existsDir*(dir: AnyDir): bool =
  ## An alias for dirExists.
  osAndNims(existsDir(dir.string))

proc assertExists*(file: AnyFile): void =
  let path: AbsFile = file.toAbsFile()

  if existsDir(AbsDir file.getStr()):
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

  if not file.existsFile():
    raise newPathError(
      file, pekNoSuchEntry, &"No such file '{path.string}'")




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
    return system.listDirs()
  else:
    for (kind, path) in os.walkDir(dir):
      if kind == pcDir:
        result.add path

proc listFiles*(dir: AnyDir): seq[AbsFile] =
  ## Lists all the files (non-recursively) in the directory dir.
  when defined(NimScript):
    return system.listFiles(dir)
  else:
    for (kind, path) in os.walkDir(dir):
      if kind == pcFile:
        result.add path

template readFile*(file: AnyFile): untyped =
  readFile(file.getStr())

template writeFile*(file: AnyFile, text: string): untyped =
  writeFile(file.getStr(), text)

proc rmDir*(dir: AnyDir; checkDir = false) =
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

proc mkDir*(dir: AnyDir) =
  ## Creates the directory dir including all necessary subdirectories.
  ## If the directory already exists, no error is raised.
  osOrNims(
    os.createDir(dir.string),
    system.mkDir(dir.string)
  )

proc mvFile*(source, dest: AnyFile) =
  ## Moves the file from to to.
  osOrNims(
    os.moveFile(source.string, dest.string),
    system.mvFile(source.string, dest.string)
  )

proc mvDir*(source, dest: AnyDir) =
  ## Moves the dir from to to.
  osOrNims(
    os.moveDir(source.string, dest.string),
    system.mvDir(source.string, dest.string)
  )

proc cpFile*(source, dest: string | AnyFile) =
  ## Copies the file from to to.
  osOrNims(
    os.copyFile(source.string, dest.string),
    system.cpFile(source.string, dest.string)
  )

proc cpDir*(source, dest: AnyDir) =
  ## Copies the dir from to to.
  osOrNims(
    os.copyDir(source.string, dest.string),
    system.cpDir(source.string, dest.string)
  )

proc cd*(dir: AnyDir) =
  ## Changes the current directory.
  osOrNims(
    os.setCurrentDir(dir.string),
    system.cd(dir.string)
  )

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

proc `~`*(path: string | RelDir): AbsDir = getHomeDir() / path

proc getNewTempDir*(
  dir: string | AbsDir = getTempDir(),
  dirPatt: string = "XXXXXXXX"): AbsDir =
  ## Get name for new temporary directory
  while true:
    var next: string
    for ch in dirPatt:
      if ch == 'X':
        next.add sample({'a' .. 'z', 'A' .. 'Z'})
      else:
        next.add ch

    if not dirExists(dir / RelDir(next)):
      return AbsDir(dir / RelDir(next))

template withDir*(dir: AnyDir, body: untyped): untyped =
  ## Changes the current directory temporarily.
  var curDir = cwd()
  try:
    cd(dir.string)
    body
  finally:
    cd(curDir)

template withTempDir*(clean: bool, body: untyped): untyped =
  ## Create temporary directory (not don't cd into it!), execute
  ## `body` and remove it afterwards if `clean` is true
  let tmpDir {.inject.} = getNewTempDir()
  mkDir(tmpDir)

  try:
    body
  finally:
    if clean:
      rmDir(tmpDir)

template withinTempDir*(clean: bool, body: untyped): untyped =
  withTempDir(clean):
    cd(tmpDir)
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

