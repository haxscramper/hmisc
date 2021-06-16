## Command-line argument parsing

import
  ./cliparse,
  ./oswrap,
  ../algo/lexcast,
  ../base_errors,
  std/[tables, options, uri, sequtils]

type
  CliCmdTree* = object
    desc*: CliDesc
    case kind*: CliOptKind
      of coCommand:
        name*: string
        subnodes*: seq[CliCmdTree]

      else:
        opt*: CliOpt

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

  CliErrorKind = enum
    cekUserFailure

  CliCheckError = object of CatchableError
    case kind*: CliErrorKind:
      of cekUserFailure:
        discard

  CliCheck = ref object
    case kind*: CliCheckKind
      of cckIsInRange:
        valueRange*: Slice[float64]

      of cckIsInSet:
        values*: seq[CliValue]

      of cckAndCheck, cckOrCheck:
        subChecks*: seq[CliCheck]

      of cckUser:
        userCheck*: proc(value: CliValue): Option[CliCheckError]

      else:
        discard

  CliOrigin = object
    ## Where particular CLI value came from (configuration file, default
    ## value or it was passed directled as an argument)

  CliCompletion = object
    value*: string
    doc*: CliDoc

  CliDoc = object
    docBrief*: string
    docFull*: string

  CliDesc = ref object
    name*: string
    defaultValue*: Option[string]
    defaultAsFlag*: Option[string]
    check*: seq[CliCheck]

    bracketSelectors: seq[string] ## Valid for `BracketFlag` and `BracketOpt`

    allowedRepeat*: Slice[int] ## Allowed range of value repetitions
    completeFor*: proc(): seq[CliCompletion]

    doc*: CliDoc

    case kind*: CliOptKind
      of coCommand:
        options*: seq[CliDesc]
        arguments*: seq[CliDesc]
        subcommands*: seq[CliDesc]

      of coArgument:
        required*: bool

      of coFlag .. coBracketOpt:
        startKeys*: seq[string]
        subKeys*: seq[CliDesc] ## For `Dot*` fields and options, contains
        ## list of possible continuations - `--cc.exe` and `--cc.ld` will
        ## be represented as `--cc` option with `--exe` and `--ld`
        ## suboptions

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

  CliApp = object
    author*: string
    name*: string
    url*: Url
    doc*: CliDoc
    configPaths*: seq[FsEntry]

    rootCmd*: CliDesc

    case isSemVer*: bool
      of true:
        semVersion*: tuple[major, minor, patch: int]

      of false:
        strVersion*: string

type
  CliParseLogger = object
    ## Initiall logger configuration




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

proc toOption(str: string): Option[string] =
  if str.len > 0:
    return some str

proc add*(app: var CliApp, cmd: CliDesc) =
  app.rootCmd.add cmd

proc cmd*(name, docBrief: string, subparts: varargs[CliDesc]): CliDesc =
  CliDesc(kind: coCommand, doc: CliDoc(docBrief: docBrief))

proc flag*(name, doc: string): CliDesc =
  CliDesc(
    kind: coFlag, name: name, startKeys: @[name],
    doc: CliDoc(docBrief: doc))

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
    result.subChecks.add result

proc andCheck*(and1, and2: CliCheck, other: varargs[CliCheck]): CLiCheck =
  result = CliCheck(kind: cckAndCheck, subChecks: @[and1, and2])
  for check in other:
    result.subChecks.add result

proc check*(kind: CliCheckKind): CliCheck =
  CliCheck(kind: kind)

func cliComplete*(key, doc: string, docFull: string = ""): CliCompletion =
  CliCompletion(value: key, doc: CliDoc(docBrief: doc, docFull: docFull))

proc opt*(
    name,
    doc: string,
    default: string = "",
    values: openarray[(string, string)] = @[],
    docFull: string = "",
    alt: seq[string] = @[],
    defaultAsFlag: string = ""
  ): CliDesc =

  result = CliDesc(
    kind: coOpt, name: name, startKeys: @[name] & alt,
    doc: CliDoc(docBrief: doc, docFull: docFull),
    defaultValue: default.toOption(),
    defaultAsFlag: defaultAsFlag.toOption()
  )

  if values.len > 0:
    result.check.add checkValues(values)
    let completions = values.mapIt(cliComplete(it[0], it[1]))
    result.completeFor = (proc(): seq[CliCompletion] = completions)


proc getDefaultCliConfig*(ignored: seq[string] = @[]): seq[CliDesc] =
  if "help" notin ignored:
    result.add opt(
      "help",
      alt = @["h", "?"],
      doc = "Display help messsage",
      default = "off",
      defaultAsFlag = "on",
      values = {
        "off": "Do not show help",
        "on": "Show help using default formatting",
        "json": "Output help in machine-readable json format",
        "verbose": "Show full documentation for each command",
      })

  if "version" notin ignored:
    result.add opt(
      "version",
      alt = @["v"],
      doc = "Display version",
      default = "off",
      defaultAsFlag = "on",
      values = {
        "off": "Do not show version",
        "on": "Show version",
        "full": "Show full version information (compilation time, author etc.)",
        "json": "Output version informatin in json format"
      }
    )

  if "json" notin ignored:
    result.add flag("json", doc = "Use json output")

  if "color" notin ignored:
    result.add opt(
      "color",
      doc = "When to use color for the pattern match output.",
      default = "auto",
      values = {
        "auto": "show colors if the output goes to an interactive console",
        "never": "do not use colorized output",
        "always": "always use colorized output"
      }
    )


  if "quiet" notin ignored:
    result.add flag("quiet", doc = "Do not print execution logs")

  if "dry-run" notin ignored:
    result.add flag("dry-run", "Do not execute irreversible OS actions")

  if "force" notin ignored:
    result.add flag("force", "Force actions")

  if "loglevel" notin ignored:
    result.add opt(
      "loglevel",
      doc = "Configure logging level"
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

    opt.check = @[
      orCheck(
        opt.check[0],
        check(cckIsWritable), check(cckIsCreatable))]

    result.add opt



proc newCliApp*(
    name: string,
    version: (int, int, int),
    author: string,
    docBrief: string,
    options: seq[CliDesc] = getDefaultCliConfig()
  ): CliApp =

  result = CliApp(
    name: name,
    isSemVer: true,
    semVersion: version,
    author: author,
    rootCmd: CliDesc(name: name),
    doc: CliDoc(docBrief: docBrief)
  )

  for opt in options:
    result.add opt


proc arg*(
    name: string,
    required: bool = true,
    check: seq[CliCheck] = @[]
  ): CliDesc =

  CliDesc(
    name: name,
    kind: coArgument,
    required: required,
    check: check
  )



proc structureSplit*(
  opts: seq[CliOpt], desc: CliDesc, log: var CliParseLogger): CliCmdTree =
  ## Convert unstructured sequence of CLI commands/options into structured
  ## unchecked tree.

  discard

proc toCliValue*(tree: CliCmdTree, log: var CliParseLogger): CliValue =
  ## Convert unchecked CLI tree into typed values, without executing
  ## checkers.


proc mergeConfigs*(
    tree: CliCmdTree,
    log: var CliParseLogger,
    configs: seq[CliValue]
  ) =

  discard

if isMainModule and false:
  var app = newCliApp(
    "test", (1,2,3), "haxscramper", "Brief description")

  app.add cmd("sub", "Example subcommand")
