import
  ../other/hshell

import std/[
  enumerate, strutils, parseutils, options, strformat, posix]

export enumerate, strutils, parseutils, hshell, options, strformat, posix

const cmdExecFlags* = { poUsePath, poEchoCmd }

type
  CmdExecResult* {.inheritable, pure.} = object
    execResult*: ShellResult

iterator splitRecords*(text: string): seq[string] =
  for line in text.splitLines():
    var buf: seq[string]
    for part in line.split(" "):
      if part.len > 0:
        buf.add part

    if buf.len > 0:
      yield buf

proc addFlags*[F: enum](
    cmd: var ShellCmd, flags: set[F], map: array[F, string]) =

  for flag in flags:
    cmd.flag map[flag]
