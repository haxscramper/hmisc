## `os` module function wrappers than can work with both nimscript and
## regular target.

import os

export os.`/`, os.`/../`

template osOrNims(osCode, nimsCode: untyped): untyped =
  when defined(NimScript):
    system.nimsCode
  else:
    os.osCode


template osAndNims*(code: untyped): untyped =
  when defined(NimScript):
    system.code
  else:
    os.code

proc getCurrentDir*(): string  =
  ## Retrieves the current working directory.
  osNims(getCurrentDir())

proc paramStr*(i: int): string =
  ## Retrieves the i'th command line parameter.
  osAndNims(paramStr(i))

proc paramCount*(): int =
  ## Retrieves the number of command line parameters.
  osAndNims(paramCount())

proc getEnv**(key: string; default = ""): string =
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
    for kind, path in os.walkDirs(dir):
      if kind == pcDir:
        result.add path

proc listFiles*(dir: string): seq[string] =
  ## Lists all the files (non-recursively) in the directory dir.
  when defined(NimScript):
    return system.listFiles(dir)
  else:
    for kind, path in os.walkDirs(dir):
      if kind == pcFile:
        result.add path

proc rmDir*(dir: string; checkDir = false) =
  ## Removes the directory dir.
  osOrNims(removeDir(dir, checkDir), rmDir(dir, checkDir))

proc rmFile*(file: string) =
  ## Removes the file.
  osOrNims(removeFile(file), rmFile(file))

proc mkDir*(dir: string) =
  ## Creates the directory dir including all necessary subdirectories.
  ## If the directory already exists, no error is raised.
  osOrNims(createDir(dir), mkDir(dir))

proc mvFile*(source, dest: string) =
  ## Moves the file from to to.
  osOrNims(moveFile(source, to), mvFile(source, to))

proc mvDir*(source, dest: string) =
  ## Moves the dir from to to.
  osOrNims(moveDir(source, dest), moveDir(source, dest))

proc cpFile*(source, dest: string) =
  ## Copies the file from to to.
  osOrNims(copyFile(source, dest), cpFile(source, dest))

proc cpDir*(source, dest: string) =
  ## Copies the dir from to to.
  osOrNims(copyDir(source, dest), cpDir(source, dest))

proc cd*(dir: string) =
  ## Changes the current directory.
  osOrNims(setCurrentDir(dir), cd(dir))

proc findExe(bin: string): string {...}
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

template withDir*(dir: string; body: untyped): untyped =
  ## Changes the current directory temporarily.
  var curDir = getCurrentDir()
  try:
    cd(dir)
    body
  finally:
    cd(curDir)
