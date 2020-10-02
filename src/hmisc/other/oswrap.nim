## `os` module function wrappers than can work with both nimscript and
## regular target.

import strutils, macros, random
from os import nil

# export os.`/`, os.`/../`

const weirdTarget = defined(nimscript) or defined(js)

template osOrNims(osCode, nimsCode: untyped): untyped =
  when not defined(NimScript):
    osCode
  else:
    nimsCode

## `os` wrapper with more typesafe paths. Mostly taken from compiler
## pathutils and stdlib.

type
  AbsFile* = distinct string
  AbsDir* = distinct string
  AbsPath* = AbsFile | AbsDir

  RelFile* = distinct string
  RelDir* = distinct string
  RelPath* = RelFile | RelDir

  AnyPath* = AbsFile | AbsDir | RelFile | RelDir
  AnyDir* = AbsDir | RelDir
  AnyFile* = AbsFile | AbsDir

const
  CurDir* = RelDir($os.CurDir)
  ParDir* = RelDir(os.ParDir)
  DirSep* = os.DirSep
  AltSep* = os.AltSep
  PathSep* = os.PathSep

func `$`*(path: AnyPath): string = path.string

macro osAndNims*(code: untyped): untyped =
  var
    osExpr = newCall newDotExpr(ident "os", code[0])
    nimsExpr = newCall newDotExpr(ident "system", code[0])

  for arg in code[1..^1]:
    osExpr.add arg
    nimsExpr.add arg

  result = quote do:
    when not defined(NimScript):
      `osExpr`
    else:
      `nimsExpr`

proc getCurrentDir*(): AbsDir =
  ## Retrieves the current working directory.
  AbsDir(osAndNims(getCurrentDir()))

import pathnorm


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

template `/`*(head, tail: AnyPath | string): untyped =
  joinPath(head, tail)

proc splitPath*(path: AbsFile): tuple[head: AbsDir, tail: RelFile] =
  let (head, tail) = os.splitPath(path.string)
  result.head = AbsDir(head)
  result.tail = RelFile(head)

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

# iterator parentDirs*(path: AnyPath, fromRoot=false, inclusive=true): AbsPath =

proc `/../`*(head: AbsDir, repeat: int): AbsDir =
  result = head
  for i in 0 ..< repeat:
    result = AbsDir(os.parentDir(result.string))

proc `/../`*(head: AbsDir, tail: string | RelDir): AbsDir =
  AbsDir(os.parentDir(head.string).AbsDir / tail.RelDir)

proc searchExtPos*(path: AnyFile): int = os.searchExtPos(path.string)

proc splitFile*(path: AbsFile): tuple[
  dir: AbsDir, name, ext: string] =
  let (dir, name, ext) = os.splitFile(path.string)
  result.dir = AbsDir(dir)
  result.name = name
  result.ext = ext

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

# proc unixToNativePath*(path: string, drive=""): string {.

proc getHomeDir*(): AbsDir = os.getHomeDir().AbsDir()

# proc getConfigDir*(): string {.rtl, extern: "nos$1",

proc getTempDir*(): AbsDir =
  AbsDir(os.getTempDir())

# proc quoteShellWindows*(s: string): string {.noSideEffect, rtl, extern: "nosp$1".} =

# proc quoteShellPosix*(s: string): string {.noSideEffect, rtl, extern: "nosp$1".} =

# proc symlinkExists*(link: string): bool {.rtl, extern: "nos$1",

# proc findExe*(exe: string, followSymlinks: bool = true;
#               extensions: openArray[string]=ExeExts): string {.

# proc getLastModificationTime*(file: string): times.Time {.rtl, extern: "nos$1", noNimScript.} =

# proc getLastAccessTime*(file: string): times.Time {.rtl, extern: "nos$1", noNimScript.} =

# proc getCreationTime*(file: string): times.Time {.rtl, extern: "nos$1", noNimScript.} =

# proc fileNewer*(a, b: string): bool {.rtl, extern: "nos$1", noNimScript.} =

# proc getCurrentDir*(): string {.rtl, extern: "nos$1", tags: [], noNimScript.} =

# proc setCurrentDir*(newDir: string) {.inline, tags: [], noNimScript.} =

when not weirdTarget:
  # proc absolutePath*(path: string, root = getCurrentDir()): string {.noNimScript.} =
  discard

# proc normalizePath*(path: var string) {.rtl, extern: "nos$1", tags: [].} =

# proc normalizedPath*(path: string): string {.rtl, extern: "nos$1", tags: [].} =

# proc sameFile*(path1, path2: string): bool {.rtl, extern: "nos$1",
# proc sameFileContent*(path1, path2: string): bool {.rtl, extern: "nos$1",

export os.FilePermission

proc getFilePermissions*(
  filename: string | AnyFile): set[os.FilePermission] =
  os.getFilePermissions(filename.string)

proc setFilePermissions*(
  filename: string | AnyFile, permissions: set[os.FilePermission]) =
  os.setFilePermissions(filename.string, permissions)

# proc copyFile*(source, dest: string) {.rtl, extern: "nos$1",

# when not declared(ENOENT) and not defined(Windows):
#   when NoFakeVars:
#     when not defined(haiku):
#       const ENOENT = cint(2) # 2 on most systems including Solaris
#     else:
#       const ENOENT = cint(-2147459069)
#   else:
#     var ENOENT {.importc, header: "<errno.h>".}: cint

# when defined(Windows) and not weirdTarget:
#   when useWinUnicode:
#     template deleteFile(file: untyped): untyped  = deleteFileW(file)
#     template setFileAttributes(file, attrs: untyped): untyped =
#       setFileAttributesW(file, attrs)
#   else:
#     template deleteFile(file: untyped): untyped = deleteFileA(file)
#     template setFileAttributes(file, attrs: untyped): untyped =
#       setFileAttributesA(file, attrs)

# proc tryRemoveFile*(file: string): bool {.rtl, extern: "nos$1", tags: [WriteDirEffect], noNimScript.} =

# proc removeFile*(file: string) {.rtl, extern: "nos$1", tags: [WriteDirEffect], noNimScript.} =

# proc tryMoveFSObject(source, dest: string): bool {.noNimScript.} =

# proc moveFile*(source, dest: string) {.rtl, extern: "nos$1",

# proc exitStatusLikeShell*(status: cint): cint =
#   ## Converts exit code from `c_system` into a shell exit code.
#   when defined(posix) and not weirdTarget:
#     if WIFSIGNALED(status):
#       # like the shell!
#       128 + WTERMSIG(status)
#     else:
#       WEXITSTATUS(status)
#   else:
#     status


# iterator walkPattern*(pattern: string): string {.tags: [ReadDirEffect], noNimScript.} =
#   ## Iterate over all the files and directories that match the `pattern`.
#   ##
#   ## On POSIX this uses the `glob`:idx: call.
#   ## `pattern` is OS dependent, but at least the `"\*.ext"`
#   ## notation is supported.
#   ##
#   ## See also:
#   ## * `walkFiles iterator <#walkFiles.i,string>`_
#   ## * `walkDirs iterator <#walkDirs.i,string>`_
#   ## * `walkDir iterator <#walkDir.i,string>`_
#   ## * `walkDirRec iterator <#walkDirRec.i,string>`_
#   walkCommon(pattern, defaultWalkFilter)

# iterator walkFiles*(pattern: string): string {.tags: [ReadDirEffect], noNimScript.} =
#   ## Iterate over all the files that match the `pattern`.
#   ##
#   ## On POSIX this uses the `glob`:idx: call.
#   ## `pattern` is OS dependent, but at least the `"\*.ext"`
#   ## notation is supported.
#   ##
#   ## See also:
#   ## * `walkPattern iterator <#walkPattern.i,string>`_
#   ## * `walkDirs iterator <#walkDirs.i,string>`_
#   ## * `walkDir iterator <#walkDir.i,string>`_
#   ## * `walkDirRec iterator <#walkDirRec.i,string>`_
#   walkCommon(pattern, isFile)

# iterator walkDirs*(pattern: string): string {.tags: [ReadDirEffect], noNimScript.} =
#   ## Iterate over all the directories that match the `pattern`.
#   ##
#   ## On POSIX this uses the `glob`:idx: call.
#   ## `pattern` is OS dependent, but at least the `"\*.ext"`
#   ## notation is supported.
#   ##
#   ## See also:
#   ## * `walkPattern iterator <#walkPattern.i,string>`_
#   ## * `walkFiles iterator <#walkFiles.i,string>`_
#   ## * `walkDir iterator <#walkDir.i,string>`_
#   ## * `walkDirRec iterator <#walkDirRec.i,string>`_
#   walkCommon(pattern, isDir)

# proc expandFilename*(filename: string): string {.rtl, extern: "nos$1",

export os.PathComponent

type
  File = object
    case isRelative*: bool
      of true:
        relFile*: RelFile
      of false:
        absFile*: AbsFile

  Dir = object
    case isRelative*: bool
      of true:
        relDir*: RelDir
      of false:
        absDir*: AbsDir

  Path = object
    case kind*: os.PathComponent
      of os.pcFile, os.pcLinkToFile:
        file*: File
      of os.pcDir, os.pcLinkToDir:
        dir*: Dir

converter toFile*(absFile: AbsFile): File =
  File(isRelative: false, absFile: absFile)

converter toFile*(relFile: RelFile): File =
  File(isRelative: true, relFile: relFile)


converter toDir*(relDir: RelDir): Dir =
  Dir(isRelative: true, relDir: relDir)

converter toDir*(absDir: AbsDir): Dir =
  Dir(isRelative: false, absDir: absDir)

converter toAbsDir*(str: string): AbsDir =
  assert os.isAbsolute(str)
  AbsDir(str)

iterator walkDir*(dir: AnyDir; relative = false, checkDir = false): Path =
  for (comp, path) in os.walkDir(dir.string):
    let comp = comp
    case comp:
      of os.pcFile, os.pcLinkToFile:
        yield Path(kind: comp, file:
          block:
            if relative:
              RelFile(path).toFile()
            else:
              AbsFile(path).toFile()
        )

      of os.pcDir, os.pcLinkToDir:
        yield Path(kind: comp, dir:
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
         ): Path =

  for dir in os.walkDirRec(
    dir.string,
    yieldFilter,
    followFilter,
    relative,
    checkDir,
  ):
    discard


# proc existsOrCreateDir*(dir: string): bool {.rtl, extern: "nos$1",
#   tags: [WriteDirEffect, ReadDirEffect], noNimScript.} =
#   ## Check if a `directory`:idx: `dir` exists, and create it otherwise.
#   ##
#   ## Does not create parent directories (fails if parent does not exist).
#   ## Returns `true` if the directory already exists, and `false`
#   ## otherwise.
#   ##
#   ## See also:
#   ## * `removeDir proc <#removeDir,string>`_
#   ## * `createDir proc <#createDir,string>`_
#   ## * `copyDir proc <#copyDir,string,string>`_
#   ## * `copyDirWithPermissions proc <#copyDirWithPermissions,string,string>`_
#   ## * `moveDir proc <#moveDir,string,string>`_
#   result = not rawCreateDir(dir)
#   if result:
#     # path already exists - need to check that it is indeed a directory
#     if not existsDir(dir):
#       raise newException(IOError, "Failed to create '" & dir & "'")

# proc createSymlink*(src, dest: string) {.noNimScript.} =

# proc createHardlink*(src, dest: string) {.noNimScript.} =

# proc copyFileWithPermissions*(source, dest: string,
#                               ignorePermissionErrors = true) {.noNimScript.} =

# proc copyDirWithPermissions*(source, dest: string,
#     ignorePermissionErrors = true) {.rtl, extern: "nos$1",
#     tags: [WriteIOEffect, ReadIOEffect], benign, noNimScript.} =
#   ## Copies a directory from `source` to `dest` preserving file permissions.
#   ##
#   ## If this fails, `OSError` is raised. This is a wrapper proc around `copyDir
#   ## <#copyDir,string,string>`_ and `copyFileWithPermissions
#   ## <#copyFileWithPermissions,string,string>`_ procs
#   ## on non-Windows platforms.
#   ##
#   ## On Windows this proc is just a wrapper for `copyDir proc
#   ## <#copyDir,string,string>`_ since that proc already copies attributes.
#   ##
#   ## On non-Windows systems permissions are copied after the file or directory
#   ## itself has been copied, which won't happen atomically and could lead to a
#   ## race condition. If `ignorePermissionErrors` is true (default), errors while
#   ## reading/setting file attributes will be ignored, otherwise will raise
#   ## `OSError`.
#   ##
#   ## See also:
#   ## * `copyDir proc <#copyDir,string,string>`_
#   ## * `copyFile proc <#copyFile,string,string>`_
#   ## * `copyFileWithPermissions proc <#copyFileWithPermissions,string,string>`_
#   ## * `removeDir proc <#removeDir,string>`_
#   ## * `moveDir proc <#moveDir,string,string>`_
#   ## * `existsOrCreateDir proc <#existsOrCreateDir,string>`_
#   ## * `createDir proc <#createDir,string>`_
#   createDir(dest)
#   when not defined(Windows):
#     try:
#       setFilePermissions(dest, getFilePermissions(source))
#     except:
#       if not ignorePermissionErrors:
#         raise
#   for kind, path in walkDir(source):
#     var noSource = splitPath(path).tail
#     case kind
#     of pcFile:
#       copyFileWithPermissions(path, dest / noSource, ignorePermissionErrors)
#     of pcDir:
#       copyDirWithPermissions(path, dest / noSource, ignorePermissionErrors)
#     else: discard

# proc inclFilePermissions*(filename: string,
#                           permissions: set[FilePermission]) {.

# proc exclFilePermissions*(filename: string,
#                           permissions: set[FilePermission]) {.

# proc expandSymlink*(symlinkPath: string): string {.noNimScript.} =

# proc parseCmdLine*(c: string): seq[string] {.


# proc getAppFilename*(): string {.rtl, extern: "nos$1", tags: [ReadIOEffect], noNimScript.} =

# proc getAppDir*(): string {.rtl, extern: "nos$1", tags: [ReadIOEffect], noNimScript.} =

export os.sleep

# proc getFileSize*(file: string): BiggestInt {.rtl, extern: "nos$1",

export os.FileInfo

# proc getFileInfo*(handle: FileHandle): FileInfo {.noNimScript.} =
#   ## Retrieves file information for the file object represented by the given
#   ## handle.
#   ##
#   ## If the information cannot be retrieved, such as when the file handle
#   ## is invalid, `OSError` is raised.
#   ##
#   ## See also:
#   ## * `getFileInfo(file) proc <#getFileInfo,File>`_
#   ## * `getFileInfo(path) proc <#getFileInfo,string>`_

#   # Done: ID, Kind, Size, Permissions, Link Count
#   when defined(Windows):
#     var rawInfo: BY_HANDLE_FILE_INFORMATION
#     # We have to use the super special '_get_osfhandle' call (wrapped above)
#     # To transform the C file descriptor to a native file handle.
#     var realHandle = get_osfhandle(handle)
#     if getFileInformationByHandle(realHandle, addr rawInfo) == 0:
#       raiseOSError(osLastError(), $handle)
#     rawToFormalFileInfo(rawInfo, "", result)
#   else:
#     var rawInfo: Stat
#     if fstat(handle, rawInfo) < 0'i32:
#       raiseOSError(osLastError(), $handle)
#     rawToFormalFileInfo(rawInfo, "", result)

# proc getFileInfo*(file: File): FileInfo {.noNimScript.} =

# proc getFileInfo*(path: string, followSymlink = true): FileInfo {.noNimScript.} =

# proc isHidden*(path: string): bool {.noNimScript.} =

# proc getCurrentProcessId*(): int {.noNimScript.} =

# proc setLastModificationTime*(file: string, t: times.Time) {.noNimScript.} =

func isValidFilename*(filename: AbsFile, maxLen = 259.Positive): bool  =
  os.isValidFilename(filename.string)



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

proc fileExists*(filename: string | AnyFile): bool =
  ## Checks if the file exists.
  osAndNims(fileExists(filename.string))

proc dirExists*(dir: string | AnyDir): bool =
  ## Checks if the directory dir exists.
  osAndNims(dirExists(dir.string))

proc existsFile*(filename: string | AnyFile): bool =
  ## An alias for fileExists.
  osAndNims(existsFile(filename.string))

proc existsDir*(dir: string | AnyDir): bool =
  ## An alias for dirExists.
  osAndNims(existsDir(dir.strign))

proc toExe*(filename: string): string =
  ##On Windows adds ".exe" to filename, else returns filename unmodified.
  (when defined(windows): filename & ".exe" else: filename)

proc toDll*(filename: string): string =
  ## On Windows adds ".dll" to filename, on Posix produces
  ## "lib$filename.so".
  (when defined(windows): filename & ".dll" else: "lib" & filename & ".so")

proc listDirs*(dir: string | AnyDir): seq[AbsDir] =
  ## Lists all absolute paths for all subdirectories (non-recursively)
  ## in the directory dir.
  when defined(NimScript):
    return system.listDirs()
  else:
    for (kind, path) in os.walkDir(dir):
      if kind == pcDir:
        result.add path

proc listFiles*(dir: string | AnyDir): seq[AbsFile] =
  ## Lists all the files (non-recursively) in the directory dir.
  when defined(NimScript):
    return system.listFiles(dir)
  else:
    for (kind, path) in os.walkDir(dir):
      if kind == pcFile:
        result.add path

proc rmDir*(dir: string | AnyDir; checkDir = false) =
  ## Removes the directory dir.
  osOrNims(
    os.removeDir(dir.string, checkDir),
    system.rmDir(dir.string, checkDir)
  )

proc rmFile*(file: string | AnyFile) =
  ## Removes the file.
  osOrNims(
    os.removeFile(file.string),
    system.rmFile(file.string)
  )

proc mkDir*(dir: string | AnyDir) =
  ## Creates the directory dir including all necessary subdirectories.
  ## If the directory already exists, no error is raised.
  osOrNims(
    os.createDir(dir.string),
    system.mkDir(dir.string)
  )

proc mvFile*(source, dest: string | AnyFile) =
  ## Moves the file from to to.
  osOrNims(
    os.moveFile(source.string, dest.string),
    system.mvFile(source.string, dest.string)
  )

proc mvDir*(source, dest: string | AnyDir) =
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

proc cpDir*(source, dest: string | AnyDir) =
  ## Copies the dir from to to.
  osOrNims(
    os.copyDir(source.string, dest.string),
    system.cpDir(source.string, dest.string)
  )

proc cd*(dir: string | AnyDir) =
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

template withDir*(dir: string | AnyDir; body: untyped): untyped =
  ## Changes the current directory temporarily.
  var curDir = getCurrentDir()
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


