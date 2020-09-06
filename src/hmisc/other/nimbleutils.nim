import strformat, strutils

## Helper utilities for running nimble tasks

func makeSeparator*(msg: string, col: string): string =
  "\e[" & col & "m" & (&"  {msg}  ").center(80, '~') & "\e[39m"

proc info*(msg: string): void = echo makeSeparator(msg, "32")
proc err*(msg: string): void = echo makeSeparator(msg, "31")

proc runDockerTest*(
  projDir, tmpDir, cmd: string, cleanup: bool = true): void =
  ## Copy project directory `projDir` into temporary `tmpDir` and
  ## execute command `cmd` inside new docker container based on
  ## `nim-base` image.
  if tmpDir.dirExists:
    rmDir tmpDir

  cpDir projDir, tmpDir
  let cmd =
      &"docker run -it --rm -v={tmpDir}:/project nim-base sh -c '" &
      &"cd /project && {cmd}" &
      "'"


  echo(cmd)
  exec(cmd)
  if cleanup:
    rmDir tmpDir
