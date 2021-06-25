import
  ../other/[hshell, hlogger, hargparse, hpprint, oswrap],
  ../types/[colorstring],
  ../algo/[htemplates, hstring_algo, hlex_base, clformat],
  ../hdebug_misc

import
  std/[strutils, tables, macros, parseutils, strformat]

startHax()

type
  RunConf = object
    nimfile*: AbsFile
    nimcache*: AbsDir

  ProcInfo = object
    filename: AbsFile
    line, column: int
    nimName: string

const NimProcNameChars = IdentChars + {
  '=', '+', '-', '*', '/', '<', '>', '@',
  '$', '~', '&', '%', '|', '!', '?', '^',
  '.', ':', '\\'
}

proc getNdi(dir: AbsDir): Table[string, ProcInfo] =
  for file in walkDir(dir, AbsFile, globs = @[**"*.ndi"]):
    var str = initPosStr(file)
    while ?str:
      var info = ProcInfo()

      info.nimName = str.popUntil(HorizontalSpace)
      str.skipSpace()

      let mangled = str.popIdent()
      str.skipSpace()

      info.filename = str.popUntil(HorizontalSpace).AbsFile()
      str.skipSpace()

      info.line = str.popDigit().parseInt()
      str.skipSpace()

      info.column = str.popDigit().parseInt()
      str.skipSpace()

      str.skipWhile(Whitespace)

      result[mangled] = info


type
  ProfileEntry = object
    time: float
    cumulativeSeconds: float
    selfSeconds: float
    calls: int
    selfTsCall: float
    totalTsCall: float
    name: string

proc splitFlatProfile(str: string): seq[ProfileEntry] =
  var str = initPosStr(str)
  str.skipWhile(Whitespace)
  while ?str:
    let e = ProfileEntry().withIt do:
      it.time = str.popDigit().parseFloat()
      str.space()

      it.cumulativeSeconds = str.popDigit().parseFloat()
      str.space()

      it.selfSeconds = str.popDigit().parseFloat()
      str.space()

      if str[] in Digits:
        it.calls = str.popDigit().parseInt()
        str.space()

        it.selfTsCall = str.popDigit().parseFloat()
        str.space()

        it.totalTsCall = str.popDigit().parseFloat()
        str.space()

      it.name = str.popUntil(Whitespace)

    result.add e
    str.skipWhile(Whitespace)



proc mainProc(l: var HLogger, conf: RunConf) =
  l.info "started"

  let binfile = conf.nimcache /. conf.nimfile.name() &. "bin"

  var cmd = shellCmd(nim).withIt do:
    it.arg "c"
    it.flag "r"

    # it.opt "d", "release"
    # it.opt "stacktrace", "off"

    it.opt "passc", "-pg"
    it.opt "passl", "-pg"
    # it.opt "gc", "orc"
    it.opt "debugger", "native"
    it.opt "nimcache", $conf.nimcache
    it.opt "out", binfile
    it.arg conf.nimfile

  withDir conf.nimcache:
    l.execShell cmd

  let gmonFile = conf.nimcache.findFile(**"gmon.out")

  var gprof = shellCmd(gprof).withIt do:
    it.arg binfile
    it.arg gmonFile
    it - "brief"

  let res = l.runShell gprof

  var
    profileRange = 0 .. 0
    indexRange = 0 .. 0
    graphRange = 0 .. 0

  let
    indexHeader: string = "Index by function name\n"
    graphHeader: string = "Call graph\n"
    graphColumnHeader: string =
      "index % time    self  children    called     name\n"

    profileHeader = "Flat profile:"
    profileColumnHeader = "/call  name"

  let indexStart = res.stdout.find(indexHeader) - 1

  withMutIt indexRange:
    it.a = indexStart + indexHeader.len
    it.b = res.stdout.high

  let graphStart = res.stdout.find(graphHeader) - 1

  withMutIt graphRange:
    it.a = res.stdout.findEnd(
      graphColumnHeader, start = graphStart)

    it.b = indexStart

  withMutIt profileRange:
    it.a = res.stdout.findEnd(profileColumnHeader)
    it.b = graphStart

  assert profileRange.a > 0, res.stdout[0 .. min(200, res.stdout.high)]
  assert profileRange.b > 0, $profileRange

  let
    profile = res.stdout[profileRange].splitFlatProfile()
    ndi = conf.nimcache.getNdi()

  echo "calls".alignLeft(11).toRed(),
    "perc".alignLeft(8).toRed(),
    "time".alignLeft(10).toRed()


  for idx, entry in profile:
    if idx > 20:
      break

    const posAlign = 20
    stdout.write &"{entry.calls:<10} {entry.time:>5.02f}% "
    stdout.write &"{entry.selfSeconds:>5.02f} "
    if entry.name in ndi:
      let i = ndi[entry.name]
      let text = &" @ {i.filename.name()}:{hshow i.line}"
      stdout.write termAlignLeft(text, posAlign)
      echo i.nimName

    else:
      echo " ".repeat(posAlign), entry.name



if isMainModule:
  var app = newCliApp(
    "nim_gprof", (0, 1, 0), "haxscramper",
    "pretty-print gprof data for nim program"
  )

  app.add arg(
    "nimfile",
    "Input nim file",
    check = checkFileReadable()
  )

  app.add opt(
    "nimcache",
    "Nim compilation cache directory",
    default = getStr(getAppTempDir() / "cache"),
    check = orCheck(
      checkDirExists(),
      checkDirCreatable()
    )
  )

  let file = "/tmp/a.nim"

  file.writeFile("""


import hmisc/other/[hargparse, hpprint, blockfmt]

var app = newCliApp(
  "nim_gprof", (0, 1, 0), "haxscramper",
  "pretty-print gprof data for nim program"
)

let (tree, errors) = app.acceptParams(@[])
let bl = ppblock(tree)

"/tmp/out".writeFile(bl.codegenRepr())

proc countBlock(bl: LytBlock): int =
  result = bl.len()
  case bl.kind:
    of bkStack, bkLine, bkChoice:
      for sub in bl.elements:
        result += countBlock(sub)

    else:
      inc result

echo countBlock(bl)

# pprint tree

""")

  let (tree, errors) = app.acceptParams(@[file])

  if errors.len > 0:
    for err in errors:
      echo err.helpStr()

  else:
    echo tree.get("nimfile")
    var obj: RunConf
    obj.fromCli(tree)

    var logger = newTermLogger()

    app.runMain(mainProc, logger, true, obj)
