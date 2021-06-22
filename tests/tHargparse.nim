import std/[unittest, strformat, sequtils]

import
  hmisc/other/hargparse,
  hmisc/hdebug_misc

import hpprint

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

    pprint err
    pprint tree



suite "Error reporting":
  test "Flag mismatches":
    let (err, _) = checkOpts(@["--za"], flag("aa", "doc"))
    doAssert err.len == 1
    echo err[0].helpStr()

  test "Multiple flag mismatches":
    let (err, _) = checkOpts(@["--zzz"], cmd(
      "main", "doc", [
        flag("zzzq", ""),
        flag("zzze", "")
    ]))

    echo err[0].helpStr()
