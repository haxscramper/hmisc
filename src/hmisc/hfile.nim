import os
import re
import sequtils
import hmisc/helpers


proc mktemp*(
  templ: string = "tmp.XXXXXXXXXX",
  dir: string = "/tmp"): string =

  if not existsDir(dir):
    createDir(dir)

  proc get(): string =
    dir.joinPath(
      templ.multiReplace(
        templ.
        findAll(re"X+").
        mapIt((re(it), getRandomBase64(it.len)))
      ))

  var file = get()
  while fileExists(file):
    file = get()

  file.open(fmWrite).close()

  return file
