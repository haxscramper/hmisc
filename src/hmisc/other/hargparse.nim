## Command-line argument parsing

import
  ./cliparse,
  ./blockfmt,
  ./oswrap,
  ./hlogger,
  ../hexceptions,
  ../hdebug_misc,
  ../base_errors,
  ../types/colorstring,
  ../algo/[
    lexcast, clformat, htemplates, hseq_mapping, htext_algo,
    hparse_base
  ]

export cliparse, hlogger


import
  std/[
    tables, options, uri, sequtils, strformat,
    strutils, algorithm, effecttraits, macros
  ]

# import hpprint

type
  CliCmdTree* = object
    desc* {.requiresinit.}: CliDesc
    head*: CliOpt
    case kind*: CliOptKind
      of coCommand:
        subnodes*: seq[CliCmdTree]

      of coDashedKinds:
        subPaths*: Table[string, CliOpt]

      else:
        discard

  CliCheckKind = enum
    cckIsFile
    cckIsDirectory
    cckIsWritable
    cckIsReadable
    cckIsCreatable
    cckIsEmpty

    cckIsInRange
    cckIsPositive

    cckIsInSet

    cckUser

    cckAndCheck
    cckOrCheck

    cckAllOf
    cckNoneOf
    cckAnyOf

    cckAcceptAll


  CliErrorKind = enum
    cekUserFailure
    cekMissingEntry
    cekFailedParse
    cekUnexpectedEntry


  CliError* = object of CatchableError
    origin*: CliOrigin
    got*: CliOpt
    desc*: CliDesc
    kind*: CliErrorKind

  CliCheck* = ref object
    case kind*: CliCheckKind
      of cckIsInRange:
        valueRange*: Slice[float64]

      of cckIsInSet:
        values*: seq[CliValue]

      of cckAndCheck, cckOrCheck:
        subChecks*: seq[CliCheck]

      of cckUser:
        userCheck*: proc(value: CliValue): Option[CliError]

      of cckAllOf, cckNoneOf, cckAnyOf:
        allowedRepeat*: Slice[int] ## Allowed range of value repetitions
        itemCheck*: CliCheck

      else:
        discard

  CliOrigin* = object
    ## Where particular CLI value came from (configuration file, default
    ## value or it was passed directled as an argument)

  CliCompletion* = object
    value*: string
    doc*: CliDoc

  CliDoc* = object
    docBrief*: string
    docFull*: string

  CliDocTree* = object
    level*: Table[string, CliDocTree]
    doc*: CliDoc

  CliDesc* = ref object
    name*: string
    altNames*: seq[string]
    defaultValue*: Option[string]
    defaultAsFlag*: Option[string]
    check*: CliCheck

    bracketSelectors: seq[string] ## Valid for `BracketFlag` and `BracketOpt`

    completeFor*: proc(): seq[CliCompletion]

    doc*: CliDoc
    varname*: string

    groupKind*: CliOptKind
    aliasof*: Option[CliOpt]
    case kind*: CliOptKind
      of coCommand:
        options*: seq[CliDesc]
        arguments*: seq[CliDesc]
        subcommands*: seq[CliDesc]

      of coArgument:
        required*: bool

      of coFlag .. coBracketOpt:
        # startKeys*: seq[string]
        subKeys*: seq[CliDesc] ## For `Dot*` fields and options, contains
        ## list of possible continuations - `--cc.exe` and `--cc.ld` will
        ## be represented as `--cc` option with `--exe` and `--ld`
        ## suboptions
        selector*: Option[CliCheck]

      of coSpecial:
        discard

  CliValueKind* = enum
    cvkString
    cvkInt
    cvkFloat
    cvkFsEntry ## Absolute/relative (link) to filesystem file/directory
    cvkBool
    cvkRecord ## For converting values passed via multiple long-path
              ## switches like `--clang.exe=emcc --clang.linkerexe=emcc`

    cvkUnparse ## Peg/regex-based unparsers

    cvkCommand

  CliValue* = ref object
    origin*: CliOrigin
    desc*: CliDesc
    case kind*: CliValueKind
      of cvkCommand:
        name*: string
        positional*: CliValue
        options*: Table[string, CliValue]

      of cvkUnparse:
        matches: seq[string]

      of cvkString:
        strVal*: string

      of cvkInt:
        intVal*: int

      of cvkFloat:
        floatVal*: float

      of cvkBool:
        boolVal*: bool

      of cvkFsEntry:
        fsEntryVal*: FsEntry

      of cvkRecord:
        recValues*: Table[string, CliValue]


  CliExitValueRange = range[0 .. 255]
  CliExitCode = object
    code*: CliExitValueRange
    onException*: Option[string]
    doc*: CliDoc

  CliApp* = object
    author*: string
    name*: string
    url*: Url
    doc*: CliDoc
    configPaths*: seq[FsEntry]

    rootCmd*: CliDesc

    exitCodes*: seq[CliExitCode]

    case isSemVer*: bool
      of true:
        semVersion*: tuple[major, minor, patch: int]

      of false:
        strVersion*: string




proc add*(desc: var CliDesc, other: CliDesc) =
  case desc.kind:
    of coCommand:
      case other.kind:
        of coCommand:
          desc.subcommands.add other

        of coArgument:
          desc.arguments.add other

        else:
          desc.options.add other

    of coArgument:
      raise newUnexpectedKindError(
        desc, "Cannot add sub-entries to argument")

    of coFlag .. coBracketOpt:
      assertKind(
        other, coFlag .. coBracketOpt,
        ". Can only extend option/flag with nested options/flags")

      desc.subKeys.add other

    of coSpecial:
      raise newUnexpectedKindError(desc)

func toOption(str: string): Option[string] =
  if str.len > 0:
    return some str

func add*(app: var CliApp, cmd: CliDesc) =
  app.rootCmd.add cmd

func root*(app: CliApp): CliDesc = app.rootCmd
func root*(app: var CliApp): var CliDesc = app.rootCmd

func cmd*(
    name, docBrief: string,
    subparts: openarray[CliDesc],
    alt: seq[string] = @[],
  ): CliDesc =

  result = CliDesc(
    name: name,
    altNames: @[name] & alt,
    kind: coCommand,
    doc: CliDoc(docBrief: docBrief)
  )

  for part in subparts:
    case part.kind:
      of coDashedKinds:
        result.options.add part

      of coArgument:
        result.arguments.add part

      of coCommand:
        result.subcommands.add part

      else:
        discard

func flag*(
    name, doc: string,
    aliasof: CliOpt = CliOpt()
  ): CliDesc =

  result = CliDesc(
    kind: coFlag,
    name: name,
    altNames: @[name],
    doc: CliDoc(docBrief: doc),
  )

  if aliasof.keyPath.len > 0:
    result.aliasof = some aliasof

proc cliValue*(val: string, doc: string): CliValue =
  CliValue(
    kind: cvkString,
    strVal: val,
    desc: CliDesc(kind: coSpecial, doc: CliDoc(docBrief: doc)))

proc checkValues*(values: openarray[(string, string)]): CliCheck =
  result = CliCheck(kind: cckIsInSet)

  for (name, desc) in values:
    result.values.add cliValue(name, desc)

proc orCheck*(or1, or2: CliCheck, other: varargs[CliCheck]): CLiCheck =
  result = CliCheck(kind: cckOrCheck, subChecks: @[or1, or2])
  for check in other:
    result.subChecks.add check

proc andCheck*(and1, and2: CliCheck, other: varargs[CliCheck]): CLiCheck =
  result = CliCheck(kind: cckAndCheck, subChecks: @[and1, and2])
  for check in other:
    result.subChecks.add check

proc anyOfCheck*(sub: CliCheck, repeated: Slice[int]): CliCheck =
  CliCheck(kind: cckAnyOf, itemCheck: sub, allowedRepeat: repeated)

proc allOfCheck*(sub: CliCheck, repeated: Slice[int]): CliCheck =
  CliCheck(kind: cckAllOf, itemCheck: sub, allowedRepeat: repeated)

proc acceptAllCheck*(): CLiCheck =
  CliCheck(kind: cckAcceptAll)

proc check*(kind: CliCheckKind): CliCheck =
  result = CliCheck(kind: kind)

func cliComplete*(key, doc: string, docFull: string = ""): CliCompletion =
  CliCompletion(value: key, doc: CliDoc(docBrief: doc, docFull: docFull))

proc opt*(
    name,
    doc: string,
    default: string = "",
    values: openarray[(string, string)] = @[],
    docFull: string = "",
    alt: seq[string] = @[],
    defaultAsFlag: string = "",
    groupKind: CliOptKind = coOpt,
    varname: string = name,
    maxRepeat: int = 1,
    aliasof: CliOpt = CliOpt(),
    selector: CliCheck = nil
  ): CliDesc =

  result = CliDesc(
    kind: coOpt,
    name: name,
    altNames: sortedByIt(@[name] & alt, it.len),
    doc: CliDoc(docBrief: doc, docFull: docFull),
    defaultValue: default.toOption(),
    defaultAsFlag: defaultAsFlag.toOption(),
    groupKind: groupKind,
    varname: varname
  )

  if not isNiL(selector):
    result.selector = some selector

  if aliasof.keyPath.len > 0:
    result.aliasof = some aliasof

  if values.len > 0:
    result.check = checkValues(values)
    let completions = values.mapIt(cliComplete(it[0], it[1]))
    result.completeFor = (proc(): seq[CliCompletion] = completions)

  if maxRepeat > 1:
    if isNil(result.check):
      result.check = allOfCheck(acceptAllCheck(), 0 .. maxRepeat)

    else:
      result.check = allOfCheck(result.check, 0 .. maxRepeat)


proc docTree*(
    doc: string,
    sub: openarray[(string, CliDocTree)]): CliDocTree =

  CliDocTree(doc: CliDoc(docBrief: doc), level: toTable(sub))

proc docTree*(doc: string): CliDocTree =
  CliDocTree(doc: CliDoc(docBrief: doc))

proc docFor(docTree: CliDocTree, name: string): CliDoc =
  docTree.level[name].doc

proc docTreeFor(docTree: CliDocTree, name: string): CliDocTree =
  docTree.level[name]

template exprType(e: untyped): untyped =
  proc res(): auto = a
  typeof(res())

proc cliCheckFor*[T](value: typedesc[seq[T]]): CliCheck =
  var tmp: exprType
  result = anyOfCheck(cliCheckFor(
    typeof(tmp[0])), 0 .. high(int))

proc cliCheckFor*(str: typedesc[string]): CliCheck =
  acceptAllCheck()

proc getDefaultCliConfig*(ignored: seq[string] = @[]): seq[CliDesc] =
  if "help" notin ignored:
    result.add opt(
      "help",
      alt = @["h"],
      doc = "Display help messsage",
      default = "off",
      defaultAsFlag = "on",
      groupKind = coFlag,
      values = {
        "off": "Do not show help",
        "on": "Show help using default formatting",
        "json": "Output help in machine-readable json format",
        "verbose": "Show full documentation for each command",
        "cmd": "Display command tree-based help"
      })

  if "version" notin ignored:
    result.add opt(
      "version",
      alt = @["v"],
      doc = "Display version",
      default = "off",
      defaultAsFlag = "on",
      groupKind = coFlag,
      values = {
        "off": "Do not show version",
        "on": "Show version",
        "full": "Show full version information " &
          "(compilation time, author etc.)",
        "json": "Output version informatin in json format"
      }
    )

  if "json" notin ignored:
    result.add flag("json", doc = "Use json output")

  if "color" notin ignored:
    result.add opt(
      "color",
      doc = "When to use color for the output.",
      default = "auto",
      groupKind = coFlag,
      values = {
        "auto": "show colors if the output goes to an interactive console",
        "never": "do not use colorized output",
        "always": "always use colorized output"
      }
    )


  if "quiet" notin ignored:
    if "loglevel" notin ignored:
      result.add flag(
        "quiet",
        doc = "Do not print execution logs",
        aliasof = CliOpt(
          kind: coOpt,
          keyPath: @["loglevel"],
          valStr: "none"
        )
      )

    else:
      result.add flag("quiet", doc = "Do not print execution logs")

  if "dry-run" notin ignored:
    result.add flag("dry-run", "Do not execute irreversible OS actions")

  if "force" notin ignored:
    result.add flag("force", "Force actions")

  if "loglevel" notin ignored:
    result.add opt(
      "loglevel",
      doc = "Configure minimal logging level to be shown.",
      default = "info",
      values = @{
        "all":    "All levels active",
        "debug":  "Debugging information helpful only to developers",
        "info":   "Anything associated with normal " &
          "operation and without any particular importance",
        "notice": "More important information that " &
          "users should be notified about",
        "warn":   "Impending problems that require some attention",
        "error":  "Error conditions that the application can recover from",
        "fatal":  "Fatal errors that prevent the application from continuing",
        "none":   "No levels active; nothing is logged"
      }
    )

  if "log-output" notin ignored:
    var opt = opt(
      "log-output",
      doc = "Configure logging output target",
      default = "/dev/stderr", # Does explicitly writing to `/dev/stderr`
                               # differ from `stderr.write`?
      values = @{
        "/dev/stderr": "Output logs to stderr",
        "/dev/stdout": "Output logs to stdout"
      }
    )

    opt.check =
      orCheck(
        opt.check,
        check(cckIsWritable),
        check(cckIsCreatable)
      )



    result.add opt



proc newCliApp*(
    name: string,
    version: (int, int, int),
    author: string,
    docBrief: string,
    noDefault: seq[string] = @[],
    options: seq[CliDesc] = getDefaultCliConfig(noDefault),
  ): CliApp =

  result = CliApp(
    name: name,
    isSemVer: true,
    semVersion: version,
    author: author,
    rootCmd: cmd(name, name, @[]),
    doc: CliDoc(docBrief: docBrief)
  )

  for opt in options:
    result.add opt

proc exitException*(
    code: CliExitValueRange,
    name: string,
    doc: string,
    docFull: string = ""
  ): CliExitCode =

  CliExitCode(
    code: code,
    doc: CliDoc(docBrief: doc, docFull: docFull),
    onException: some(name)
  )

func add*(app: var CliApp, code: CliExitCode) =
  app.exitCodes.add code


macro raisesAsExit*(
    app: var CLiApp,
    mainProc: typed,
    exceptions: static[openarray[(string, (int, string))]],
  ): untyped =

  result = newStmtList()

  for ex in getRaisesList(mainProc):
    let name = ex.strVal()
    let idx = exceptions.findIt(it[0] == name)
    if idx != -1:
      let it = exceptions[idx]
      result.add newCall("add", app,
        newCall(
          "exitException",
          newLit(it[1][0]), newLit(it[0]), newLit(it[1][1])))

    else:
      warning(
        mainProc.repr() & " can raise " & name &
          ", which does not have corresponding exit code handler.",
          mainProc)

proc exitWith*(app: CliApp, ex: ref Exception, logger: HLogger) =
  for code in app.exitCodes:
    if code.onException.isSome() and code.onException.get() == ex.name:
      ex.log(logger)
      logger.logStackTrace(ex)
      quit code.code

  ex.log(logger)
  logger.logStackTrace(ex)
  quit 1

template runMain*(
    app: CliApp,
    mainProc: typed,
    logger: HLogger
  ): untyped =

  {.line: instantiationInfo(fullPaths = true).}:
    try:
      mainProc()

    except Exception as e:
      app.exitWith(e, logger)

proc arg*(
    name: string,
    doc: string,
    required: bool = true,
    check: CliCheck = nil,
    docFull: string = ""
  ): CliDesc =

  CliDesc(
    doc: CliDoc(docBrief: doc, docFull: docFull),
    name: name,
    kind: coArgument,
    required: required,
    check: check
  )

using errors: var seq[CliError]


proc strVal*(tree: CliCmdTree): string =
  case tree.kind:
    of coOptionKinds, coArgument:
      result = tree.head.valStr

    else:
      raise newUnexpectedKindError(tree, "string value")

proc name*(tree: CliCmdTree): string = tree.desc.name
proc select*(tree: CliCmdTree): string = tree.head.keySelect

proc parseArg(
    lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =
  if lexer[].kind == coArgument:
    result = CliCmdTree(kind: coArgument, head: lexer.pop(), desc: desc)

  else:
    lexer.advance()
    errors.add CliError(
      kind: cekFailedParse, desc: desc, msg: "Failed")

proc parseOptOrFlag(
    lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =

  if lexer[].key in desc.altNames:
    result = CliCmdTree(kind: lexer[].kind, head: lexer.pop(), desc: desc)

  else:
    errors.add CliError(
      kind: cekFailedParse,
      desc: desc,
      got: lexer.pop(),
      msg: "Failed"
    )


proc parseCli(lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =
  var
    argIdx = 0 # Current index of the parsed argument group (for arguments
               # that accept multiple values)
    argParsed: seq[int] # Number of argument group elemens parsed for each
                        # group for `<arg-with two repeats> <arg>`

    optTree: Table[string, CliCmdTree]

  while ?lexer:
    case lexer[].kind:
      of coArgument:
        case desc.kind:
          of coArgument:
            result = parseArg(lexer, desc, errors)

          of coCommand:
            result = parseArg(lexer, desc.arguments[argIdx], errors)

          else:
            raise newImplementError("")

      of coDashedKinds:
        case desc.kind:
          of coDashedKinds:
            result = parseOptOrFlag(lexer, desc, errors)

          of coCommand:
            let subIdx = desc.options.findIt(
              lexer[].key in it.altNames)

            if subIdx == -1:
              errors.add CliError(
                kind: cekUnexpectedEntry,
                desc: desc,
                got: lexer.pop(),
                msg: "Failed"
              )

            else:
              let key = lexer[].key
              result = parseOptOrFlag(lexer, desc.options[subIdx], errors)

          else:
            raise newUnexpectedKindError(desc)

      of coCommand:
        raise newImplementKindError(lexer[])

      of coSpecial:
        raise newImplementKindError(lexer[])





proc structureSplit*(opts: seq[CliOpt], desc: CliDesc, errors): CliCmdTree =
  ## Convert unstructured sequence of CLI commands/options into structured
  ## unchecked tree.

  var lexer = initLexer(opts)
  result = parseCli(lexer, desc, errors)

proc toCliValue*(tree: CliCmdTree, errors): CliValue =
  ## Convert unchecked CLI tree into typed values, without executing
  ## checkers.

initBlockFmtDsl()

proc mergeConfigs*(
    tree: CliCmdTree,
    log: var HLogger,
    configs: seq[CliValue]
  ) =

  discard


let
  section = initStyle(fgYellow)

func version*(app: CliApp): string =
  if app.isSemVer:
    &"v{app.semVersion.major}.{app.semVersion.minor}.{app.semVersion.patch}"

  else:
    app.strVersion


proc postLine(desc: CliDesc): LytBlock =
  result = H[]

  if desc.kind in coOptionKinds:
    if desc.defaultAsFlag.isNone():
      result.add T[&"={initColored(desc.varname, fgCyan)}"]

    else:
      result.add T[&"[={initColored(desc.varname, fgCyan)}]"]

    if desc.defaultValue.isSome():
      result.add T[&", defaults to {toCyan(desc.defaultValue.get())}"]

    if desc.defaultAsFlag.isSome():
      result.add T[&", as flag defaults to {toCyan(desc.defaultAsFlag.get())}"]

  let rep = desc.check.allowedRepeat
  if rep.b > 1:
    result.add T[&", can be repeated {desc.check.allowedRepeat} times"]

  if desc.aliasof.isSome():
    result.add T[&", alias of '{toYellow($desc.aliasof.get())}'"]


proc `$`*(val: CliValue): string =
  case val.kind:
    of cvkFloat: result = $val.floatVal
    of cvkString: result = $val.strVal
    else:
      raise newImplementKindError(val)

proc checkHelp(check: CliCheck, inNested: bool = false): LytBlock =
  result = E[]
  if isNil(check): return
  let prefix = tern(inNested, "- ", "Value must ")

  case check.kind:
    of cckIsInSet:
      result = V[]

      var doc: seq[(string, string)]

      for val in check.values:
        doc.add ($val, val.desc.doc.docBrief)

      let width = maxIt(doc, it[0].len) + 2

      for (val, doc) in doc:
        let doc = T[doc.wrapOrgLines(40, simple = true).join("\n")]
        var item = H[T[initColored(alignLeft(val, width), fgYellow)], V[doc]]
        result.add item

      result = V[T[
        prefix & "be one of the following: "
      ], I[4, result], S[]]

    of cckOrCheck:
      result = V[T[prefix & "meet at least one of the following criteria:"]]
      for sub in check.subChecks:
        result.add I[4, checkHelp(sub, true)]

    of cckIsWritable:
      result = T[prefix & "be a writable file"]

    of cckIsCreatable:
      result = T[prefix & "be a path where can be created"]

    else:
      raise newImplementKindError(check)

proc flagHelp(flag: CliDesc): LytBlock =
  result = V[]
  var flags: seq[string]
  for key in flag.altNames:
    if key.len > 1:
      flags.add toGreen("--" & key)

    else:
      flags.add toGreen("-" & key)

  result.add H[T[flags.join(", ")], postLine(flag)]
  result.add I[4, T[flag.doc.docBrief]]
  result.add I[4, checkHelp(flag.check)]

proc optHelp(opt: CliDesc): LytBlock =
  result = V[]
  var opts: seq[string]
  for key in opt.altNames:
    if key.len > 1:
      opts.add toGreen("--" & key)

    else:
      opts.add toGreen("-" & key)

  result.add H[T[opts.join(", ")], postLine(opt)]
  result.add I[4, T[opt.doc.docBrief]]
  result.add I[4, checkHelp(opt.check)]


proc argHelp(arg: CliDesc): LytBlock =
  result = V[]

  result.add T["<" & toColored(arg.name, fgRed) & ">"]
  result.add I[4, T[arg.doc.docBrief]]
  result.add I[4, checkHelp(arg.check)]

proc help(desc: CliDesc, level: int = 0): LytBlock =
  result = V[]
  var first = S[]
  let indent = level * 4
  if desc.arguments.len > 0:
    var args = V[S[], T[initColored("ARGS:", section)], S[]]

    for arg in desc.arguments:
      args.add I[4, argHelp(arg)]

    result.add I[indent, args]

  if desc.subcommands.len > 0:
    var cmds = V[S[], T[initColored("SUBCOMMANDS:", section)], S[]]

    for cmd in desc.subcommands:
      let names =
        cmd.altNames.mapIt(toColored(it, fgMagenta)).join(toColored("/")) &
          " - " & cmd.doc.docBrief

      cmds.add I[level + 4, T[names]]
      cmds.add I[level + 4, help(cmd, level + 1)]

    result.add I[indent, cmds]

  if desc.options.len > 0 and
     desc.options.anyIt(it.groupKind in coFlagKinds):
    var flags = V[S[], T[toColored("FLAGS:", section)], S[]]

    for flag in desc.options:
      if flag.groupKind in coFlagKinds:
        let help = I[indent + 4, flagHelp(flag)]
        flags.add help


    result.add I[indent, flags]

  if desc.options.len > 0 and
     desc.options.anyIt(it.groupKind in coOptionKinds):
    var opts = V[S[], T[toColored("OPTIONS:", section)], S[]]

    for opt in desc.options:
      if opt.groupKind in coOptionKinds:
        opts.add I[indent + 4, optHelp(opt)]

    result.add I[indent, opts]



proc help(app: CliApp): LytBlock =
  var res = V[
    T[toColored("NAME:", section)],
    S[],
    I[4, T[&"{app.name} - {app.doc.docBrief}"]]
  ]

  if app.exitCodes.len > 0:
    var codes = V[T["EXIT STATUS:".toColored(section)]]

    for exit in app.exitCodes:
      codes.add S[]
      codes.add I[4, T[&"{exit.code} - {exit.doc.docBrief}"]]

    res.add codes

  res.add app.root.help()

  res.add @[
    S[], T["VERSION:".toColored(section)],
    S[], I[4, T[&"{app.version} by {app.author}"]]
  ]

  result = res



proc helpStr*(app: CliApp): string =
  return app.help().toString()

proc help(err: CliError): LytBlock =
  case err.kind:
    of cekFailedParse:
      let split = stringMismatchMessage(
        err.got.key, err.desc.altNames, showAll = true).
        splitSgrSep()

      result = T[split]

    of cekUnexpectedEntry:
      let alts = err.desc.options.mapIt(it.altNames).concat()

      let split = stringMismatchMessage(
        err.got.key, alts, showAll = true).splitSgrSep()

      result = T[split]

    else:
      raise newImplementKindError(err)

proc helpStr*(err: CliError): string =
  return err.help().toString()


if isMainModule:

  proc mainProc(arg: int = 2) =
    if arg > 0:
      mainProc(arg - 1) # Comment
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

  echo app.helpStr()
