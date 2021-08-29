import hmisc/preludes/unittest

testFileStarted()


import std/[strformat, sequtils, tables, macros]

import
  hmisc/other/[hargparse, oswrap, hpprint],
  hmisc/algo/htemplates,
  hmisc/preludes/unittest


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

    check tree.kind == cctArgument

  test "Switch":
    let (_, tree) = checkOpts(@["--tset"], flag("tset", "Doc"))
    check tree.kind == cctFlag

  test "Option":
    let (_, tree) = checkOpts(@["--opt:val"], opt("opt", "Doc"))
    check tree.kind == cctOpt
    check tree.name == "opt"
    check tree.strVal() == "val"

  test "Selector option":
    let (_, tree) = checkOpts(
      @["--opt[Sel]:val"],
      opt("opt", "", selector = checkValues({"Sel": "select one"})))

    check:
      tree.kind == cctOpt
      tree.select() == "Sel"
      tree.strVal() == "val"

  test "Option with repetitions":
    let (err, tree) = checkOpts(
      @["--results", "output", "raw"],
      opt("results", "", maxRepeat = 2))

    show tree.treeRepr()

  test "Subcommand":
    let (err, tree) = checkOpts(
      @["ip", "--test", "addr"],
      cmd("ip", "", [
        flag("test", ""),
        cmd("addr", "")
    ]))

    show tree.treeRepr()

    check:
      matchdiff tree:
        Command:
          Flag(head: (key: "test"), desc: (name: "test"))
          Command()

  test "Subcommand after flag":
    let (err, tree) = checkOpts(
      @["haxdoc", "--dry-run", "nim", "trail"],
      cmd("haxdoc", "", [
        flag("dry-run", ""),
        cmd("nim", "", [cmd("trail", "")])]))

    show tree.treeRepr()

    show tree[0][0].len
    show tree[0][0].treeRepr()

    check:
      matchdiff tree:
        Command(desc.name: "haxdoc"):
          Command(desc.name: "nim"):
            Command(desc.name: "trail")

          Flag(head.key: "dry-run", desc.name: "dry-run")



suite "Convert to cli value":
  test "Integer positional":
    var (err, tree) = checkOpts(
      @["12"], arg("i", "", check = cliCheckFor(int)))

    check:
      matchdiff tree, Argument(head: (value: "12"))
      matchdiff tree.toCliValue(err), Int(intVal: 12)

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
      check:
        matchdiff tree, Argument(head: (value: "12"))
        matchdiff tree.toCliValue(err), Int(intVal: 12)

    block:
      var (err, tree) = checkOpts(@["spec1"], arg)
      check:
        matchdiff tree, Argument(head: (value: "spec1"))
        matchdiff tree.toCliValue(err), String(strVal: "spec1")

  test "Sequence of optional arguments":
     let arg = cmd("main", "", [
       opt("ignore", "", check = cliCheckFor(seq[string])),
       opt("ranges", "", check = cliCheckFor(seq[int])),
       opt("option", "", check = cliCheckFor(Option[int])),
       opt("opthas", "", check = cliCheckFor(Option[int])),
       opt("tuples", "", check = cliCheckFor((int, string))),
       opt("tupseq", "", check = cliCheckFor(seq[(string, string)])),
       opt("dirpair", "", check = cliCheckFor(seq[(AbsDir, AbsDir)]))
     ])

     var (err, tree) = checkOpts(@[
       "main",
       "--ignore='**/zs_matcher.nim'",
       "--ignore='**/nimble_aux.nim'",
       "--ranges=1,2,3",
       "--opthas=1",
       "--tuples=1:test",
       "--tupseq=mnt:/tmp",
       "--tupseq=/mnt:/tmp",
       "--dirpair=/mnt:/mnt",
       "--dirpair=/tmp:/tmp"
     ], arg)

     let value = tree.toCliValue(err)
     show value.treeRepr()

     if err.len > 0:
       for e in err:
         show helpStr(e)

       fail()

     check:
       matchdiff value:
         Command(
           options: {
             "ignore": Seq(
               seqVal: [
                 String(strVal: "**/zs_matcher.nim"),
                 String(strVal: "**/nimble_aux.nim")]),
             "ranges": Seq(
               seqVal: [
                 Int(intVal: 1),
                 Int(intVal: 2),
                 Int(intVal: 3)])})

     check:
       value.getOpt("ignore") as seq[string] == @[
         "**/zs_matcher.nim", "**/nimble_aux.nim"]

       value.getOpt("ranges") as seq[int] == @[1, 2, 3]
       value.getOpt("option", true) as Option[int] == none(int)
       value.getOpt("opthas") as Option[int] == some(1)

       value.getOpt("tuples") as (int, string) == (1, "test")
       value.getOpt("tupseq") as seq[(string, string)] ==
         @[("mnt", "/tmp"), ("/mnt", "/tmp")]

       value.getOpt("dirpair") as seq[(AbsDir, AbsDir)] ==
         @[(AbsDir("/mnt"), AbsDir("/mnt")), (AbsDir("/tmp"), AbsDir("/tmp"))]

  test "Unjoined options":
    let arg = cmd("main", "", [
      opt("ignore", "", check = cliCheckFor(seq[string]))
    ])

    var (err, tree) = checkOpts(@[
      "main", "--ignore", "ignore-1", "ignore-2"], arg)

    # pprint tree
    pprintObjectTree tree

    let value = tree.toCliValue(err)

    check:
      value.getOpt("ignore") as seq[string] == @[
        "ignore-1", "ignore-2"]

suite "Error reporting":
  test "Flag mismatches":
    let (err, _) = checkOpts(@["--za"], flag("aa", "doc"))
    check err.len == 1
    show err[0].helpStr()

  test "Multiple flag mismatches":
    let (err, _) = checkOpts(@["main", "--zzz"], cmd(
      "main", "doc", [
        flag("zzzq", ""),
        flag("zzze", "")
    ]))

    show err[0].helpStr()

suite "Default values":
  test "Option":
    var app = newApp()
    app.add opt("test", "", default = cliDefault("false"))
    discard app.acceptArgs(@[])

    let opt = app.getOpt("test")
    check opt.kind == cvkString
    check opt.strVal == "false"

  test "Based on positional":
    var app = newApp()
    app.add arg("file", "", check = cliCheckFor(FsFile).withIt((
      it.fakeCheck = true)))

    app.add opt("outfile", "", default = cliDefaultFromArg(
      "file", "",
      proc(val: CliValue): CliValue =
        val.as(FsFile).withExt("bin").toCliValue()))

    check app.acceptArgs(@["file.nim"])
    let opt = app.getOpt("outfile")
    check opt.kind == cvkFsEntry
    check opt.fsEntryVal.getStr() == "file.bin"

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

    check app.acceptArgs(@["sub1", "sub2", "file.nim"])
    let opt = app.getCmd().getCmd().getOpt("out")
    check opt.kind == cvkFsEntry
    check opt.fsEntryVal.getStr() == "file.bin"



suite "Full app":
  test "Execute with exception":
    proc mainProc(app: CliApp, l: HLogger, arg: int = 2) =
      if arg > 0:
        mainProc(app, l, arg - 1) # Comment
      raise newException(OSError, "123123123")

    var app = newCliApp(
      "test", (1,2,3), "haxscramper", "Brief description")


    app.add arg("main", "Required argumnet for main command")
    var sub = cmd("sub", "Example subcommand", @[], alt = @["s"])
    sub.add arg("index", "Required argument for subcommand")
    app.add sub

    app.raisesAsExit(mainProc, {
      "OSError": (1, "Example os error raise")
    })

    let logger = getTestLogger()

    app.runMain(mainProc, logger, false, argpass(app, logger))

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

    show app.helpStr()

  test "Full app dry-run":
    var app = newApp()
    app.add flag("dry-run", "")
    app.add cmd("nim", "", [cmd("trail", "")])

    if not app.acceptArgs(@["--dry-run", "nim", "trail"]):
      app.showerrors(newtermLogger())
      fail()

    check app.getOpt("dry-run") as bool == true


testFileEnded()
