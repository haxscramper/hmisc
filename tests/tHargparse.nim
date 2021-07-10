import std/[unittest, strformat, sequtils]

import
  hmisc/other/[hargparse, oswrap, hpprint],
  hmisc/algo/htemplates,
  hmisc/hdebug_misc,
  fusion/matching

startHax()

suite "Classify command line arguments":
  test "Flags":
    let conf = CliParseConfig(shortOpts: {'W', 'q'})
    type
      Test = enum
        tNone
        tFirst = "ovewrite"
        tSecond = "overwrite2"

    var res: Test
    let err = cliParse("First", res, conf)
    let es = newSeq[string]()

    let strs = @{
      "--opt.sub[Selector]=value": @[
        (coBracketOpt, @["opt", "sub"], "Selector", "value")],
      "--top[Switch1]:argument": @[
        (coBracketOpt, @["top"], "Switch1", "argument")],

      "--field1:'str-argument'": @[(coOpt, @["field1"], "", "str-argument")],

      "--flag":             @[(coFlag, @["flag"], "", "")],
      "--flag.sub":         @[(coDotFlag, @["flag", "sub"], "", "")],
      "--flag[Bracket]":    @[(coBracketFlag, @["flag"], "Bracket", "")],
      "--opt=value":        @[(coOpt, @["opt"], "", "value")],
      "--opt:123":          @[(coOpt, @["opt"], "", "123")],
      "--opt.sub=val":      @[(coDotOpt, @["opt", "sub"], "", "val")],
      "Sub1":               @[(coArgument, es, "", "Sub1")],
      "--field2:0.3":       @[(coOpt, @["field2"], "", "0.3")],
      "/tmp/test.txt":      @[(coArgument, es, "", "/tmp/test.txt")],
      "-Wnone":             @[(coOpt, @["W"], "", "none")],
      "--dry-run":          @[(coFlag, @["dry-run"], "", "")],
      "-qWnone":            @[
        (coFlag, @["q"], "", ""), (coOpt, @["W"], "", "none")],

      "--": @[(coSpecial, es, "", "")],
      "-": @[(coSpecial, es, "", "")],


    }

    for (arg, it) in strs:
      let parsed = parseCliOpts(@[arg], conf).parsed

      for (got, want) in zip(parsed, it):
        let (kind, path, select, value) = want
        check got.kind == kind
        check got.keyPath == path
        check got.keySelect == select
        check got.valStr == value

  test "Special kind opt kinds":
    for (val, kind) in {
      "--": cskVerbatimNext,
      "-": cskStdinAlias
    }:
      check parseCliOpts(@[val]).parsed[0].specialKind == kind

proc newApp(
    name: string = "a",
    ignore: seq[string] = @["quiet", "dry-run", "help", "verbose",
                            "version", "loglevel", "log-output", "json",
                            "color", "force"]
  ): CliApp =
  newCliApp(
    name, (1, 0, 0), "haxscramper",
    "doc brief",
    noDefault = ignore
  )

proc checkOpts(opts: seq[string], desc: CliDesc):
    (seq[CliError], CliCmdTree) =
  result[1] = parseCliOpts(opts).parsed.structureSplit(desc, result[0])

suite "Argument structuring":
  test "Positional argument":
    let (_, tree) = checkOpts(
      @["zz0"], arg("test", "documentation for test"))

    doAssert tree.kind == coArgument

  test "Switch":
    let (_, tree) = checkOpts(@["--tset"], flag("tset", "Doc"))
    doAssert tree.kind == coFlag

  test "Option":
    let (_, tree) = checkOpts(@["--opt:val"], opt("opt", "Doc"))
    check tree.kind == coOpt
    check tree.name == "opt"
    check tree.strVal() == "val"

  test "Selector option":
    let (_, tree) = checkOpts(
      @["--opt[Sel]:val"],
      opt("opt", "", selector = checkValues({"Sel": "select one"})))

    check tree.kind == coBracketOpt
    check tree.select() == "Sel"
    check tree.strVal() == "val"

  test "Option with repetitions":
    let (err, tree) = checkOpts(
      @["--results", "output", "raw"],
      opt("results", "", maxRepeat = 2))

    echo tree.treeRepr()

  test "Subcommand":
    let (err, tree) = checkOpts(
      @["ip", "--test", "addr"],
      cmd("ip", "", [
        flag("test", ""),
        cmd("addr", "")
    ]))

    tree.assertMatch:
      Command:
        Flag(head: (key: "test"), desc: (name: "test"))
        Command()

  test "Subcommand after flag":
    let (err, tree) = checkOpts(
      @["haxdoc", "--dry-run", "nim", "trail"],
      cmd("haxdoc", "", [
        flag("dry-run", ""),
        cmd("nim", "", [
          cmd("trail", "")
        ])
      ])
    )

    tree.assertMatch:
      Command(desc.name: "haxdoc"):
        Flag(head.key: "dry-run", desc.name: "dry-run")
        Command(desc.name: "nim"):
          Command(desc.name: "trail")



suite "Convet to cli value":
  test "Integer positional":
    var (err, tree) = checkOpts(
      @["12"], arg("i", "", check = cliCheckFor(int)))

    Argument(head: (value: "12")) := tree
    Int(intVal: 12) := tree.toCliValue(err)

  test "Integer or enum positional":
    type Special = enum spec1, spec2

    let arg = arg("i", "", check = checkOr(
      cliCheckFor(int),
      cliCheckFor(Special, toMapArray {
        spec1: "Documentation for enum value 1",
        spec2: "Documentation for enum value 2"
      })
    ))

    block:
      var (err, tree) = checkOpts(@["12"], arg)
      Argument(head: (value: "12")) := tree
      Int(intVal: 12) := tree.toCliValue(err)

    block:
      var (err, tree) = checkOpts(@["spec1"], arg)
      Argument(head: (value: "spec1")) := tree
      String(strVal: "spec1") := tree.toCliValue(err)


suite "Error reporting":
  test "Flag mismatches":
    let (err, _) = checkOpts(@["--za"], flag("aa", "doc"))
    doAssert err.len == 1
    echo err[0].helpStr()

  test "Multiple flag mismatches":
    let (err, _) = checkOpts(@["main", "--zzz"], cmd(
      "main", "doc", [
        flag("zzzq", ""),
        flag("zzze", "")
    ]))

    echo err[0].helpStr()

suite "Default values":
  test "Option":
    var app = newApp()
    app.add opt("test", "", default = cliDefault("false"))
    discard app.acceptArgs(@[])

    let opt = app.getOpt("test")
    doAssert opt.kind == cvkString
    doAssert opt.strVal == "false", opt.strVal

  test "Based on positional":
    var app = newApp()
    app.add arg("file", "", check = cliCheckFor(FsFile).withIt((
      it.fakeCheck = true)))

    app.add opt("outfile", "", default = cliDefaultFromArg(
      "file", "",
      proc(val: CliValue): CliValue =
        val.as(FsFile).withExt("bin").toCliValue()))

    doAssert app.acceptArgs(@["file.nim"])
    let opt = app.getOpt("outfile")
    doAssert opt.kind == cvkFsEntry
    doAssert opt.fsEntryVal.getStr() == "file.bin"

  test "Based on positional subcommand":
    var app = newApp()
    var sub1 = cmd("sub1", "")
    var sub2 = cmd("sub2", "")
    sub2.add arg("file", "", check = cliCheckFor(FsFile).withIt((
      it.fakeCheck = true)))

    sub2.add opt("out", "", default =cliDefaultFromArg(
      "file", "",
      proc(val: CliValue): CliValue =
        val.as(FsFile).withExt("bin").toCliValue()))

    sub1.add sub2
    app.add sub1

    doAssert app.acceptArgs(@["sub1", "sub2", "file.nim"])
    let opt = app.getCmd().getCmd().getOpt("out")
    doAssert opt.kind == cvkFsEntry
    doAssert opt.fsEntryVal.getStr() == "file.bin"



suite "Full app":
  test "Execute with exception":
    proc mainProc(app: CliApp, l: HLogger, arg: int = 2) =
      if arg > 0:
        mainProc(app, l, arg - 1) # Comment
      raise newException(OSError, "123123123")

    startHax()
    var app = newCliApp(
      "test", (1,2,3), "haxscramper", "Brief description")


    app.add arg("main", "Required argumnet for main command")
    var sub = cmd("sub", "Example subcommand", @[], alt = @["s"])
    sub.add arg("index", "Required argument for subcommand")
    app.add sub

    app.raisesAsExit(mainProc, {
      "OSError": (1, "Example os error raise")
    })

    let logger = newTermLogger()

    app.runMain(mainProc, logger, false)

  test "Positional enum arguments":
    type
      En1 = enum en11, en12
      En2 = enum en21, en22

    var app = newApp()
    app.add arg("pos1", "", check = cliCheckFor(En1, toMapArray {
      en11: "Doc for en 1",
      en12: "Doc for en 2"
    }))

    app.add arg("pos2", "", check = cliCheckFor(En2, toMapArray {
      en21: "Doc for en 1",
      en22: "Doc for en 2"
    }))

    echo app.helpStr()

  test "Full app dry-run":
    var app = newApp()
    app.add flag("dry-run", "")
    app.add cmd("nim", "", [cmd("trail", "")])

    if not app.acceptArgs(@["--dry-run", "nim", "trail"]):
      app.showerrors(newtermLogger())
      fail()

    doAssert app.getOpt("dry-run") as bool
