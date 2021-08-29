import hmisc/preludes/unittest

testFileStarted()

import std/[sugar, strutils, sequtils, strformat]
import hmisc/other/[hlogger, hshell]

suite "HLogger":
  proc task(log: HLogger) {.logScope(log).} =
    log.info("hello")

  var l = newTermLogger()
  task(l)

  l.waitFor("test")
  l.done("Successfully ran 4 tests")
  l.done("Teardown ok")

  l.pdump [(0 + 90), (3)]
  l.trace "Test"
  l.execShell shellCmd(ls, "/tmp")
  l.execCode echo(12)

testFileEnded()