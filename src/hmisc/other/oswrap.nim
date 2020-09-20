## `os` module function wrappers than can work with both nimscript and
## regular target.

import os, strutils, macros, random

export os.`/`, os.`/../`

template osOrNims(osCode, nimsCode: untyped): untyped =
  when not defined(NimScript):
    osCode
  else:
    nimsCode

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

proc getCurrentDir*(): string  =
  ## Retrieves the current working directory.
  osAndNims(getCurrentDir())

proc paramStr*(i: int): string =
  ## Retrieves the i'th command line parameter.
  osAndNims(paramStr(i))

proc paramCount*(): int =
  ## Retrieves the number of command line parameters.
  osAndNims(paramCount())

proc getEnv*(key: string; default = ""): string =
  ## Retrieves the environment variable of name key.
  osAndNims(getEnv(key, default))

proc existsEnv*(key: string): bool =
  ## Checks for the existence of an environment variable named key.
  osAndNims(existsEnv(key))

proc putEnv*(key, val: string): void =
  ## Sets the value of the environment variable named key to val.
  osAndNims(putEnv(key, val))

proc delEnv*(key: string) =
  ## Deletes the environment variable named key.
  osAndNims(delEnv(key))

proc fileExists*(filename: string): bool =
  ## Checks if the file exists.
  osAndNims(fileExists(filename))

proc dirExists*(dir: string): bool =
  ## Checks if the directory dir exists.
  osAndNims(dirExists(dir))

proc existsFile*(filename: string): bool =
  ## An alias for fileExists.
  osAndNims(existsFile(filename))

proc existsDir*(dir: string): bool =
  ## An alias for dirExists.
  osAndNims(existsDir(dir))

proc toExe*(filename: string): string =
  ##On Windows adds ".exe" to filename, else returns filename unmodified.
  (when defined(windows): filename & ".exe" else: filename)

proc toDll*(filename: string): string =
  ## On Windows adds ".dll" to filename, on Posix produces
  ## "lib$filename.so".
  (when defined(windows): filename & ".dll" else: "lib" & filename & ".so")

proc listDirs*(dir: string): seq[string] =
  ## Lists all absolute paths for all subdirectories (non-recursively)
  ## in the directory dir.
  when defined(NimScript):
    return system.listDirs()
  else:
    for (kind, path) in os.walkDir(dir):
      if kind == pcDir:
        result.add path

proc listFiles*(dir: string): seq[string] =
  ## Lists all the files (non-recursively) in the directory dir.
  when defined(NimScript):
    return system.listFiles(dir)
  else:
    for (kind, path) in os.walkDir(dir):
      if kind == pcFile:
        result.add path

proc rmDir*(dir: string; checkDir = false) =
  ## Removes the directory dir.
  osOrNims(
    os.removeDir(dir, checkDir),
    system.rmDir(dir, checkDir)
  )

proc rmFile*(file: string) =
  ## Removes the file.
  osOrNims(
    os.removeFile(file),
    system.rmFile(file)
  )

proc mkDir*(dir: string) =
  ## Creates the directory dir including all necessary subdirectories.
  ## If the directory already exists, no error is raised.
  osOrNims(
    os.createDir(dir),
    system.mkDir(dir)
  )

proc mvFile*(source, dest: string) =
  ## Moves the file from to to.
  osOrNims(
    os.moveFile(source, dest),
    system.mvFile(source, dest)
  )

proc mvDir*(source, dest: string) =
  ## Moves the dir from to to.
  osOrNims(
    os.moveDir(source, dest),
    system.mvDir(source, dest)
  )

proc cpFile*(source, dest: string) =
  ## Copies the file from to to.
  osOrNims(
    os.copyFile(source, dest),
    system.cpFile(source, dest)
  )

proc cpDir*(source, dest: string) =
  ## Copies the dir from to to.
  osOrNims(
    os.copyDir(source, dest),
    system.cpDir(source, dest)
  )

proc cd*(dir: string) =
  ## Changes the current directory.
  osOrNims(
    os.setCurrentDir(dir),
    system.cd(dir)
  )

proc findExe(bin: string): string =
  ## Searches for bin in the current working directory and then in
  ## directories listed in the PATH environment variable. Returns "" if
  ## the exe cannot be found.
  osAndNims(findExe(bin))


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

proc `~`*(path: string): string = getHomeDir() / path

proc getNewTempDir*(
  dir: string = "/tmp", patt: string = dir / "XXXXXXXX"): string =
  ## Get name for new temporary directory
  while true:
    var next: string
    for ch in patt:
      if ch == 'X':
        next.add sample({'a' .. 'z', 'A' .. 'Z'})
      else:
        next.add ch

    if not dirExists(next):
      return next

template withDir*(dir: string; body: untyped): untyped =
  ## Changes the current directory temporarily.
  var curDir = getCurrentDir()
  try:
    cd(dir)
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
