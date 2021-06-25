import
  ../preludes/cli_app,
  ../other/hjson


type
  RunConf = object
    nimfile: AbsFile
    nimcache: AbsDir

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

  for file in walkDir(
    conf.nimcache, AbsFile, globs = @[**".json"]):

    let json = parseJson(file)

when isMainModule:
  let
    cache = getAppTempDir() / "cache"
    file = getAppTempFile("file.nim")

  mkdir cache

  file.writeFile("""
proc te(a: int, b: string) =
  echo a
  echo b

te(12, "faskdfasd;kf")
te(12, "faskdfasd;kf")
te(12, "faskdfasd;kf")
te(12, "faskdfasd;kf")
te(12, "faskdfasd;kf")
te(12, "faskdfasd;kf")
te(12, "faskdfasd;kf")
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
