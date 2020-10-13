#!/usr/bin/env nim

import strformat, strutils, sugar, sequtils
import hshell, oswrap

export AbsDir, RelDir, AbsFile, RelFile

import ../algo/htemplates

## Helper utilities for running nimble tasks

func makeSeparator*(msg: string, col: string): string =
  "\e[" & col & "m" & (
    "  " & msg.alignLeft(46, ' ') & "  ").center(80, '~') & "\e[39m"

proc info*(msg: string): void = echo makeSeparator(msg, "34")
proc notice*(msg: string): void = echo makeSeparator(msg, "32")
proc err*(msg: string): void = echo makeSeparator(msg, "31")
proc debug*(msg: string): void = echo makeSeparator(msg, "39")

proc thisAbsDir*(): AbsDir = AbsDir thisDir()

proc runDockerTest*(
  projDir, tmpDir: AbsDir, cmd: string,
  runCb: proc() = (proc() = discard)): void =
  ## Copy project directory `projDir` into temporary `tmpDir` and
  ## execute command `cmd` inside new docker container based on
  ## `nim-base` image.
  mkDir tmpDir

  let mainDir = (tmpDir / "main")
  if mainDir.dirExists:
    rmDir (tmpDir / "main")

  cpDir projDir, (tmpDir / "main")

  echo "copied ", projDir, " to ", mainDir

  withDir tmpDir / "main":
    runCb()

  let dockerCmd = makeGnuCmd("docker").withIt do:
    it.cmd "run"
    it - "i"
    it - "t"
    it - "rm"
    it - ("v", "", $tmpDir & ":/project")
    it.arg "nim-base"
    it.arg "sh"
    it - "c"
    it.raw &"'cd /project/main && {cmd}'"

  # echo dockerCmd.toStr()

  execShell(dockerCmd)


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

proc pkgVersion*(pkg: string): string =
  let (stdout, stderr, code) = runShell("nimble dump " & pkg)
  for line in stdout.split("\n"):
    if line.startsWith("version: "):
      return line["version: \"".len() .. ^2]


proc makeLocalDevel*(testDir: AbsDir, pkgs: seq[string]): string =
  info "Copying local development versions"
  let home = getHomeDir()
  let dirs = collect(newSeq):
    for pkg in pkgs:
      let dir = home / ".nimble/pkgs" / (pkg & "-#head")

      assert existsDir(dir),
          &"Could not find {dir} - run " &
            "`nimble develop` to make it available"

      dir

  mkDir testDir
  for pkg in pkgs:
    let meta = home / &".nimble/pkgs/{pkg}-#head" /
      RelFile(pkg & ".nimble-link")

    for nimble in meta.readFile().split("\n"):
      let nimble = AbsDir nimble
      if nimble.endsWith(&"{pkg}.nimble"): # XXX
        let dir = parentDir(nimble)
        cpDir dir, (testDir / pkg)

  for pkg in pkgs:
    result = result && &"cd /project/{pkg}" && "nimble develop"

proc writeTestConfig*(str: string): void =
  "tests/nim.cfg".writeFile(str.unindent())

proc testAllImpl*(): void =
  try:
    execShell("choosenim stable")
    execShell("nimble test")
    info "Stable test passed"
  except:
    err "Stable test failed"

  try:
    execShell("choosenim devel")
    execShell("nimble test")
    info "Devel test passed"
  except:
    err "Devel test failed"
  finally:
    execShell("choosenim stable")

  try:
    execShell("nimble install")
    info "Installation on stable OK"
  except:
    err "Installation on stable failed"

proc runDockerTestDevel*(
  startDir, testDir: AbsDir, localDevel: seq[string],
  cmd: string, cb: proc()) =
  let develCmd = makeLocalDevel(testDir, localDevel)
  let cmd = develCmd && ("cd " & "/project/main") && cmd

  info "executing docker container"
  debug "command is"
  echo cmd

  runDockerTest(thisAbsDir(), testDir, cmd) do:
    cb()
