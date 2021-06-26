import
  ../preludes/cli_app,
  ../other/hjson

import jsony
import std/[sequtils]

type
  RunConf = object
    nimfile: AbsFile
    nimcache: AbsDir

  GCovGroup = object
    gcc_version: string
    files: seq[GCovFile]

  GCovFile = object
    file: string
    functions: seq[GCovFunction]
    lines: seq[GCovLine]

  GCovFunction = object
    blocks: int ## number of blocks that are in the function
    blocks_executed: int ## number of executed blocks of the function
    demangled_name: string ## demangled name of the function
    end_column: int ## column in the source file where the function ends
    end_line: int ## line in the source file where the function ends
    execution_count: int ## number of executions of the function
    name: string ## name of the function
    start_column: int ## column in the source file where the function begins
    start_line: int ## line in the source file where the function begins

  GCovLine = object
    branches: seq[GCovBranch]
    count: int ## number of executions of the line
    line_number: int ## line number
    unexecuted_block: bool ## flag whether the line contains an unexecuted
                           ## block (not all statements on the line are
                           ## executed)
    function_name: string ## a name of a function this line belongs to (for
                          ## a line with an inlined statements can be not
                          ## set)

    lineText: string
    inFile: bool

  GCovBranch = object
    count: int ## number of executions of the branch
    fallthrough: bool ## true when the branch is a fall through branch
    throw: bool ## true when the branch is an exceptional branch


proc loadGcovGroup(file: AbsFile, l: var HLogger): GCovGroup =
  result = file.readFile().fromJson(GCovGroup)

  for file in mitems(result.files):
    if exists(AbsFile(file.file)):
      let text = toSeq(lines file.file)

      for line in mitems(file.lines):
        if line.lineNumber - 1 in 0 .. text.high:
          line.lineText = text[line.lineNumber - 1]
          line.inFile = true

proc mainProc(l: var HLogger, conf: RunConf) =
  l.info "Started"
  let binfile = conf.nimcache /. conf.nimfile.name() &. "bin"

  var cmd = shellCmd(nim).withIt do:
    it.arg "c"
    it.flag "r"
    it.opt "nimcache", $conf.nimcache
    it.opt "passc", "--coverage"
    it.opt "passl", "--coverage"
    it.opt "debugger", "native"
    it.opt "out", binfile
    it.arg conf.nimfile

  l.changeDir conf.nimcache:
    l.warn cwd()
    l.execShell cmd

  var gcov = shellCmd(gcov).withIt do:
    it.flag "json-format"
    it.flag "function-summaries"

    l.debug conf.nimcache
    for file in walkDir(conf.nimcache, AbsFile, exts = @["o"]):
      l.debug file
      it.arg file

  l.changeDir conf.nimcache:
    discard l.runShell gcov

  for file in walkDir(
    conf.nimcache, AbsFile, globs = @[**"*.json.gz"]):

    var cmd = shellCmd(gunzip).withIt do:
      it.flag "keep"
      it.arg file

    let res = file.withoutExt()
    l.info res
    if res.exists():
      rmFile res

    l.execShell cmd

  for file in walkDir(conf.nimcache, AbsFile, globs = @[**"*.json"]):
    let cov = file.loadGCovGroup(l)
    for file in cov.files:
      if AbsFile(file.file).name() == "file":
        for line in file.lines:
          if line.inFile:
            let text = toColored(line.lineText, fgRed + bgDefault ?? line.unexecutedBlock)
            echo &"{line.count:<3} {text}"

when isMainModule:
  let
    cache = getAppTempDir() / "cache"
    file = getAppTempFile("file.nim")

  mkdir cache

  file.writeFile("""
proc a(i: int) =
  case i:
    of 10: echo 10
    of 20: echo 12312
    else:
      echo 900

a(20)
a(10)

""")

  var logger = newTermLogger()

  var conf = RunConf()

  conf.nimcache = cache
  conf.nimfile = file

  mainProc(logger, conf)

# main: file.nim
# 	rm -rf cache
# 	rm -rf html
# 	nim c \
# 		--nimcache:cache							\
# 		--debugger:native							\
# 		--passc:--coverage						\
# 		--passl:--coverage						\
# 		--out:file.bin file.nim

# 	./file.bin
# 	cd cache && gcov --json-format *.o > /dev/null
# 	cd cache && gunzip *.gz
