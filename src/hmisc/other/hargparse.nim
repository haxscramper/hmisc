## Command-line argument parsing

#[

- TODO :: Use string interpolatio for default values. Add variables like
  - `$appTempDir`
  - `$appName`

- TODO :: Common error redirection handlers - `-?` to `--help`, `--quiet`
  to `--silent` etc.

]#

import
  ./cliparse,
  ./blockfmt,
  ./oswrap,
  ./hlogger,
  ./hpprint,
  ../hexceptions,
  ../hdebug_misc,
  ../base_errors,
  ../helpers,
  ../types/colorstring,
  ../algo/[
    lexcast, clformat, htemplates, hseq_mapping, htext_algo,
    hparse_base, hstring_algo, halgorithm
  ]

export cliparse, hlogger


import
  std/[
    tables, options, uri, sequtils, strformat, sets,
    strutils, algorithm, effecttraits, macros
  ]

# import hpprint

type
  CliCmdTree* = object
    desc* {.requiresinit.}: CliDesc
    head* {.requiresinit.}: CliOpt
    args*: seq[CliOpt]
    mainIdx*: int ## Cmd tree entry index in the main input (to compute
                  ## origin information later)
    case kind*: CliOptKind
      of coCommand:
        subnodes*: seq[CliCmdTree]

      of coDashedKinds:
        subPaths*: Table[string, CliCmdTree]

      else:
        discard

  CliCheckKind = enum
    cckNoCheck
    cckAliasedCheck
    cckRepeatCheck

    cckIsFile
    cckIsDirectory
    cckIsWritable
    cckIsReadable
    cckIsCreatable
    cckIsEmpty

    cckFileExtMatches

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

    cckIsStrValue
    cckIsIntValue
    cckIsFloatValue
    cckIsBoolValue
    cckIsListValue


  CliErrorKind = enum
    cekErrorDescription
    cekUserFailure
    cekMissingEntry
    cekFailedParse
    cekUnexpectedEntry
    cekCheckExecFailure
    cekCheckFailure


  CliError* = object of CatchableError
    location*: typeof(instantiationInfo())
    origin*: CliOrigin
    case isDesc*: bool
      of true:
        desc*: CliDesc

      of false:
        check*: CliCheck

    got*: CliOpt
    kind*: CliErrorKind



  CliCheck* = ref object
    allowedRepeat*: Slice[int] ## Allowed range of value repetitions
    fakeCheck*: bool
    case kind*: CliCheckKind
      of cckIsInRange:
        valueRange*: Slice[float64]

      of cckIsInSet, cckFileExtMatches:
        values*: seq[CliValue]

      of cckAndCheck, cckOrCheck:
        subChecks*: seq[CliCheck]

      of cckUser:
        userCheck*: proc(value: CliValue): Option[CliError]

      of cckAllOf, cckNoneOf, cckAnyOf:
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

  CliDefaultKind = enum
    cdkConstString
    cdkCallback
    cdkCliValue

  CliDefault = ref object
    defaultRepr*: string
    defaultDesc*: string
    case kind*: CliDefaultKind
      of cdkConstString:
        strVal*: string

      of cdkCallback:
        impl*: proc(app: CliApp, path: seq[string]): CliValue

      of cdkCliValue:
        value*: CliValue

  CliDesc* = ref object
    name*: string
    altNames*: seq[string]
    defaultValue*: Option[CliDefault]
    defaultAsFlag*: Option[CliDefault]
    disabledFor*: string
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
    cvkSeq ## Multi-value variable
    cvkRecord ## For converting values passed via multiple long-path
              ## switches like `--clang.exe=emcc --clang.linkerexe=emcc`

    cvkUnparse ## Peg/regex-based unparsers

    cvkCommand
    cvkNotYetDefaulted

  CliValue* = ref object
    origin*: CliOrigin
    desc*: CliDesc
    rawValue*: string
    case kind*: CliValueKind
      of cvkCommand:
        name*: string
        positional*: seq[CliValue]
        options*: Table[string, CliValue]
        subCmd*: Option[CliValue]

      of cvkUnparse:
        matches: seq[string]

      of cvkString:
        strVal*: string

      of cvkInt:
        intVal*: int

      of cvkFloat:
        floatVal*: float

      of cvkSeq:
        seqVal*: seq[CliValue]

      of cvkBool:
        boolVal*: bool

      of cvkFsEntry:
        fsEntryVal*: FsEntry

      of cvkRecord:
        recHead*: CLiValue
        recValues*: Table[string, CliValue]

      of cvkNotYetDefaulted:
        defaultFrom*: CliDefault


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
    rawArgs*: seq[string]

    rootCmd*: CliDesc
    value*: CliValue
    errors*: seq[CliError]

    exitCodes*: seq[CliExitCode]

    case isSemVer*: bool
      of true:
        semVersion*: tuple[major, minor, patch: int]

      of false:
        strVersion*: string

const
  cvkValidatorNames*: array[CliValueKind, seq[string]] = toMapArray {
    cvkFsEntry: @[
      "checkFileReadable",
      "checkDirCratable",
      "cliCheckFor(FsEntry/AbsDir/RelDir/AbsFile/RelFile)"
    ],
    cvkBool: @["cliCheckFor(bool)"],
    cvkInt: @["cliCheckFor(int)"]
  }

const addErrors = true

func addOrRaise(errors: var seq[CliError], err: CliError) =
  when addErrors:
    errors.add err

  else:
    var ex: ref CliError = nil
    new(ex)
    ex[] = err
    raise ex

func treeRepr*(tree: CliCmdTree): string =
  func aux(t: CliCmdTree, res: var string, level: int) =
    res &= getIndent(level) & hshow(t.kind) & " " & toCyan(t.desc.name) &
      " " & hshow($t.head) & "\n"

    for arg in t.args:
      res &= getIndent(level + 1) & hshow($arg) & "\n"

    case t.kind:
      of coCommand:
        for sub in t.subnodes:
          aux(sub, res, level + 1)

      of coDashedKinds:
        for key, path in t.subPaths:
          res &= getIndent(level + 1) & hshow(key) & ": " & $path & "\n"
          # aux(path, res, level + 2)

      else:
        discard



  aux(tree, result, 0)


func treeRepr*(tree: CliValue): string =
  func aux(t: CliValue, res: var string, level: int) =

    res &= getIndent(level) & hshow(t.kind)
    case t.kind:
      of cvkCommand:
        res &= " " & toMagenta(t.desc.name) & "\n"
        let level = level + 1
        for arg in t.positional:
          res &= getIndent(level) & "arg <" & toCyan(arg.desc.name) & ">\n"
          aux(arg, res, level + 1)
          res &= "\n"

        for key, opt in t.options:
          res &= getIndent(level) & "opt <" & toCyan(key) & ">\n"
          aux(opt, res, level + 1)
          res &= "\n"

        if t.subCmd.isSome():
          res &= getIndent(level) & "cmd\n"

          aux(t.subCmd.get(), res, level + 1)
          res &= "\n"


      of cvkString:  res &= " " & hshow(t.strVal)
      of cvkBool:    res &= " " & hshow(t.boolVal)
      of cvkInt:     res &= " " & hshow(t.intVal)
      of cvkFloat:   res &= " " & hshow(t.floatVal)
      of cvkFsEntry: res &= " " & hshow(t.fsEntryVal.getStr())

      of cvkSeq:
        for item in t.seqVal:
          aux(item, res, level + 1)
          res &= "\n"

      else:
        raise newImplementKindError(t)



  aux(tree, result, 0)



func add*(desc: var CliDesc, other: CliDesc) =
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

func toOption[T](r: ref T): Option[ref T] =
  if not isNil(r):
    return some r


func add*(app: var CliApp, cmd: CliDesc) =
  app.rootCmd.add cmd

func root*(app: CliApp): CliDesc = app.rootCmd
func root*(app: var CliApp): var CliDesc = app.rootCmd

proc cliValue*(
    val: string, doc: string, disabled: string = ""): CliValue =
  CliValue(
    kind: cvkString,
    strVal: val,
    rawValue: val,
    desc: CliDesc(
      disabledFor: disabled,
      kind: coSpecial, doc: CliDoc(docBrief: doc)))

proc checkValues*(
    values: openarray[(string, string)], disabled: string = ""): CliCheck =
  result = CliCheck(kind: cckIsInSet)

  for (name, desc) in values:
    result.values.add cliValue(name, desc, disabled = disabled)

func checkExtensions*(exts: openarray[(string, string)]): CliCheck =
  result = CliCheck(kind: cckFileExtMatches)
  for (name, desc) in exts:
    result.values.add cliValue(name, desc)

func checkFileReadable*(): CliCheck =
  CliCheck(kind: cckIsReadable)

func checkDirExists*(): CliCheck =
  CliCheck(kind: cckIsDirectory)

func checkDirCreatable*(): CliCheck =
  CliCheck(kind: cckIsCreatable)

proc checkOr*(or1, or2: CliCheck, other: varargs[CliCheck]): CLiCheck =
  result = CliCheck(kind: cckOrCheck, subChecks: @[or1, or2])
  for check in other:
    result.subChecks.add check

proc checkAliased*(): CliCheck =
  CliCheck(kind: cckAliasedCheck)

proc checkRepeat*(minRepeat, maxRepeat: int): CliCheck =
  CliCheck(kind: cckRepeatCheck, allowedRepeat: minRepeat .. maxRepeat)

proc checkNo*(): CliCheck = CliCheck(kind: cckNoCheck)

proc checkAnd*(and1, and2: CliCheck, other: varargs[CliCheck]): CLiCheck =
  result = CliCheck(kind: cckAndCheck, subChecks: @[and1, and2])
  for check in other:
    result.subChecks.add check

proc checkAnyOf*(sub: CliCheck, repeated: Slice[int]): CliCheck =
  CliCheck(kind: cckAnyOf, itemCheck: sub, allowedRepeat: repeated)

proc checkAllOf*(sub: CliCheck, repeated: Slice[int]): CliCheck =
  CliCheck(kind: cckAllOf, itemCheck: sub, allowedRepeat: repeated)

proc checkAcceptAll*(): CLiCheck =
  CliCheck(kind: cckAcceptAll)

proc check*(kind: CliCheckKind): CliCheck =
  result = CliCheck(kind: kind)

func cliComplete*(key, doc: string, docFull: string = ""): CliCompletion =
  CliCompletion(value: key, doc: CliDoc(docBrief: doc, docFull: docFull))

func setDisabled*(desc: CliDesc, disabled: string) =
  desc.disabledFor = disabled
  proc aux(check: CliCheck) =
    case check.kind:
      of cckIsInSet:
        for val in mitems(desc.check.values):
          val.desc.setDisabled(disabled)

      of cckAndCheck, cckOrCheck:
        for sub in mitems(check.subChecks):
          aux(sub)

      of cckAllOf, cckNoneOf, cckAnyOf:
        aux(check.itemCheck)

      else:
        discard

  if not isNil(desc.check):
    static: {.warning: "Hack, ensure there are no nil checks later".}
    aux(desc.check)


proc opt*(
    name,
    doc: string,
    default:       CliDefault                  = nil,
    values:        openarray[(string, string)] = @[],
    docFull:       string                      = "",
    alt:           seq[string]                 = @[],
    defaultAsFlag: CLiDefault                  = nil,
    groupKind:     CliOptKind                  = coOpt,
    varname:       string                      = name,
    maxRepeat:     int                         = 1,
    aliasof:       CliOpt                      = CliOpt(),
    selector:      CliCheck                    = nil,
    check:         CliCheck                    = nil,
    disabled:      string                      = ""
  ): CliDesc =

  result = CliDesc(
    kind: coOpt,
    name: name,
    altNames: sortedByIt(@[name] & alt, it.len),
    doc: CliDoc(docBrief: doc, docFull: docFull),
    defaultValue: default.toOption(),
    defaultAsFlag: defaultAsFlag.toOption(),
    groupKind: groupKind,
    varname: varname,
    disabledFor: disabled
  )

  if not isNiL(selector):
    result.selector = some selector

  if aliasof.keyPath.len > 0:
    result.aliasof = some aliasof

  if values.len > 0:
    if not isNil(check):
      result.check = checkAnd(check, checkValues(values))

    else:
      result.check = checkValues(values)

    let completions = values.mapIt(cliComplete(it[0], it[1]))
    result.completeFor = (proc(): seq[CliCompletion] = completions)

  elif not isNil(check):
    result.check = check

  if maxRepeat > 1:
    if isNil(result.check):
      result.check = checkRepeat(0, maxRepeat)

    else:
      result.check = checkAnd(
        checkRepeat(0, maxRepeat),
        result.check
      )

  if isNil(result.check):
    result.check = checkNo()

  result.setDisabled(disabled)


func cmd*(
    name, docBrief: string,
    subparts: openarray[CliDesc] = @[],
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
    aliasof: CliOpt = CliOpt(),
    disabled: string = ""
  ): CliDesc =

  result = CliDesc(
    kind: coFlag,
    name: name,
    altNames: @[name],
    doc: CliDoc(docBrief: doc),
    disabledFor: disabled
  )

  if aliasof.keyPath.len > 0:
    result.aliasof = some aliasof

  if isNil(result.check):
    result.check = CliCheck(kind: cckIsBoolValue)



proc docTree*(
    doc: string,
    sub: openarray[(string, CliDocTree)]): CliDocTree =

  CliDocTree(doc: CliDoc(docBrief: doc), level: toTable(sub))

proc docTree*(doc: string): CliDocTree =
  CliDocTree(doc: CliDoc(docBrief: doc))

proc docFor(docTree: CliDocTree, name: string): CliDoc =
  docTree.level[name].doc

proc docBrief(val: CliValue): string =
  if isNil(val): ""
  elif isNil(val.desc): ""
  else: val.desc.doc.docBrief

proc docTreeFor(docTree: CliDocTree, name: string): CliDocTree =
  docTree.level[name]

template exprType(e: untyped): untyped =
  proc res(): auto = a
  typeof(res())

func getCmdName*(val: CliValue): string = val.subCmd.get().desc.name
func getCmd*(val: CliValue): CliValue = val.subCmd.get()
func getArg*(val: CliValue, pos: int = 0): CliValue = val.positional[pos]
func getCmd*(app: CliApp): CliValue = app.value.getCmd()
func getCmdName*(app: CliApp): string = app.value.getCmdName()
func getArg*(app: CliApp, pos: int = 0): CliValue = app.value.getArg(pos)
func getOpt*(val: CliValue, name: string): CliValue =
  assertKind(val, {cvkCommand})
  if name in val.options:
    result = val.options[name]

  else:
    raise newArgumentError(
      &"Could not find option '{name}' for command '{val.desc.name}'.",
      "Available options:",
      val.options.getKeys().joinWords("or")
    )

func getOpt*(app: CliApp, name: string): CliValue = app.value.getOpT(name)

func getArg*(val: CliValue, name: string): CliValue =
  assertKind(val, {cvkCommand})
  for pos in val.positional:
    if pos.desc.name == name:
      return pos

  raise newImplementError(
    &"Could not find argument '{name}' for CLI value '{val.desc.name}'.",
    joinAnyOf(
      val.positional.mapIt(it.desc.name),
      prefix = "Available words - ",
      empty = "Command has no positional arguments")
  )

func getAtPath*(val: CliValue, path: seq[string]): CliValue =
  if path.len == 0:
    result = val

  else:
    assertKind(val, {cvkCommand})
    let head = path[0]
    if head in val.options:
      result = val.options[head].getAtPath(path[1 ..^ 1])

    elif val.subCmd.isSome() and val.subCmd.get().desc.name == head:
      result = val.subCmd.get().getAtPath(path[1 ..^ 1])

    else:
      for pos in val.positional:
        if pos.desc.name == head:
          return pos.getAtPath(path[1 ..^ 1])

      var next: seq[string]
      for pos in val.positional: next.add pos.desc.name
      for key, _ in val.options: next.add key
      if val.subCmd.isSome(): next.add val.subCmd.get().desc.name

      raise newImplementError(
        "Could not find value at path " &
          $path & " " & $next)

func getAtPath*(app: CliApp, path: seq[string]): CliValue =
  app.value.getAtPath(path)

func getArg*(app: CliApp, name: string): CliValue = app.value.getArg(name)

func hasCmd*(val: CliValue): bool =
  val.kind in {cvkCommand} and val.subCmd.isSome()

template assertKind*(val: CliValue, target: set[CliValueKind]): untyped {.dirty.} =
  bind joinAnyOf, assertKind
  var targetNames: seq[string]
  for k in items(target):
    targetNames.add cvkValidatorNames[k]

  assertKind(
    val, target,
    ". In order to convert CLI string to value of this kind use " &
      joinAnyOf(
        targetNames,
        prefix = "one of the converters ",
        empty = "converters for target kind"))

proc `as`*[T](val: CliValue, target: typedesc[T]): T =
  bind assertKind
  when target is AbsFile:
    assertKind(val, {cvkFsEntry})
    assertKind(val.fsEntryVal, {pcFile, pcLinkToFile})
    if val.fsEntryVal.file.isRelative:
      result = val.fsEntryVal.file.relFile.toAbsFile()

    else:
      result = val.fsEntryVal.file.absFile

  elif target is AbsDir:
    assertKind(val, {cvkFsEntry})
    assertKind(val.fsEntryVal, {pcDir, pcLinkToDir})

    if val.fsEntryVal.dir.isRelative:
      result = val.fsEntryVal.dir.relDir.toAbsDir()

    else:
      result = val.fsEntryVal.dir.absDir

  elif target is FsFile:
    case val.kind:
      of cvkFsEntry:
        assertKind(val.fsEntryVal, {pcFile, pcLinkToFile})
        result = val.fsEntryVal.file

      else:
        raise newUnexpectedKindError(val)

  elif target is seq:
    type Item = typeof(result[0])
    case val.kind:
      of cvkSeq:
        for item in val.seqVal:
          result.add item as Item

      else:
        raise newUnexpectedKindError(val)

  elif target is string:
    val.assertKind({cvkString})
    result = val.strVal

  elif target is int:
    val.assertKind({cvkInt})
    result = val.intVal

  elif target is float:
    val.assertKind({cvkFloat})
    result = val.floatVal

  elif target is bool:
    assertKind(val, {cvkBool})
    result = val.boolVal

  else:
    static:
      {.error: "Cannot convert CLI value to " & $typeof(T) &
        " - conversion not implemented yet.".}

func toCliValue*[T](
    cli: T, doc: string = "", desc: CliDesc = nil): CliValue =

  let desc =
    if isNil(desc):
      CliDesc(doc: CliDoc(docBrief: doc))

    else:
      desc

  when cli is FsEntry:
    result = CliValue(kind: cvkFsEntry, fsEntryVal: cli)

  elif cli is FsFile or cli is FsDir:
    result = CliValue(kind: cvkFsEntry, fsEntryVal: cli.toFsEntry())

  elif cli is AbsFile or cli is RelFile:
    result = CliValue(kind: cvkFsEntry,
                      fsEntryVal: cli.toFsFile().toFsEntry())

  elif cli is AbsDir or cli is RelDir:
    result = CliValue(kind: cvkFsEntry,
                      fsEntryVal: cli.toFsDir().toFsEntry())

  elif cli is seq:
    result = CliValue(kind: cvkSeq)
    for item in items(cli):
      result.seqVal.add toCliValue(item)

  elif cli is string:
    result = CliValue(kind: cvkString, strVal: cli)

  elif cli is bool:
    result = CliValue(kind: cvkBool, boolVal: cli)

  else:
    {.error: "Cannot convert " & $typeof(T) &
      " to CLI value - conversion not implemented yet".}

  result.desc =desc
  result.rawValue = $cli


proc cliCheckFor*[T](value: typedesc[seq[T]]): CliCheck =
  var tmp: exprType
  result = checkAnyOf(cliCheckFor(
    typeof(tmp[0])), 0 .. high(int))

proc cliCheckFor*(f: typedesc[FsFile]): CLiCheck =
  checkFileReadable()

proc cliCheckFor*(str: typedesc[string]): CliCheck =
  checkAcceptAll()

proc cliCheckFor*(str: typedesc[int]): CliCheck =
  CliCheck(kind: cckIsIntValue)

export toMapArray

proc cliCheckFor*[En: enum](
    en: typedesc[En], docs: array[En, string]): CliCheck =

  var values: seq[(string, string)]
  for val in { low(en) .. high(en) }:
    values.add ($val, docs[val])

  return checkValues(values)

func cliDefault*(str: string): CliDefault =
  CliDefault(kind: cdkConstString, strVal: str, defaultRepr: str)

func cliDefault*(val: CliValue): CliDefault =
  CliDefault(
    kind: cdkCliValue, value: val,
    defaultRepr: val.rawValue, defaultDesc: val.docBrief())

func cliDefaultFromArg*(
    arg, convertRepr: string,
    convert: proc(val: CliValue): CliValue): CliDefault =

  CliDefault(
    kind: cdkCallback,
    defaultRepr: convertRepr,
    impl: (proc(app: CliApp, path: seq[string]): CliValue =
               convert(app.getAtPath(path & arg))))

type
  CliHelpModes* = enum
    chmOff = "off"
    chmOn = "on"
    chmJson = "json"
    chmCompact = "compact"
    chmVerbose = "verbose"
    chmCmd = "cmd"

proc getDefaultCliConfig*(ignored: seq[string] = @[]): seq[CliDesc] =
  if "help" notin ignored:
    result.add opt(
      "help",
      alt = @["h"],
      doc = "Display help messsage",
      default = cliDefault("off"),
      defaultAsFlag = cliDefault("on"),
      groupKind = coFlag,
      disabled = "off",
      check = cliCheckFor(CliHelpModes, toMapArray {
        chmOff: "Do not show help",
        chmOn: "Show help using default formatting",
        chmJson: "Output help in machine-readable json format",
        chmCompact: "Compact help output",
        chmVerbose: "Show full documentation for each command",
        chmCmd: "Display command tree-based help"
      })
    )

  if "complete-for" notin ignored and
    false: # TODO

    result.add opt(
      "complete-for",
      doc = "List completion options",
      values = {
        "bash": "Bash shell completion",
        "fish": "Fish shell completion",
        "elvish": "Elvish shell completion",
        "zsh": "Z shell completion"
      }
    )

  if "version" notin ignored:
    result.add opt(
      "version",
      alt = @["v"],
      doc = "Display version",
      default = cliDefault("off"),
      defaultAsFlag = cliDefault("on"),
      groupKind = coFlag,
      disabled = "off",
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
      default = cliDefault("auto"),
      groupKind = coFlag,
      disabled = "never",
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
      default = cliDefault("info"),
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
      default = cliDefault("/dev/stderr"),
        # Does explicitly writing to `/dev/stderr`
        # differ from `stderr.write`?
      values = @{
        "/dev/stderr": "Output logs to stderr",
        "/dev/stdout": "Output logs to stdout"
      }
    )

    opt.check =
      checkOr(
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

func add*(tree: var CliCmdTree, other: CliCmdTree, pathLevel: int = 0) =
  case tree.kind:
    of coCommand:
      tree.subnodes.add other

    of coDashedKinds:
      tree.subPaths[other.head.keyPath[pathLevel]] = other

    else:
      raise newUnexpectedKindError(tree)

func get*(
    tree: CliValue, name: string, doRaise: bool = true): CliValue =

  case tree.kind:
    of cvkCommand:
      if name in tree.options:
        return tree.options[name]

      else:
        for pos in tree.positional:
          if name in pos.desc.altNames:
            return pos

        if doRaise:
          raise newImplementError()

        else:
          return nil

    else:
      raise newImplementKindError(tree)

proc fromCli*[T](obj: var T, cli: CliValue) =
  for name, value in fieldPairs(obj):
    let val = cli.get(name, false)

    if isNil(val):
      when value is Option:
        discard

      else:
        raise newArgumentError(name & " was not specified")

    when value is int:
      value = val.intVal

    elif value is string:
      value = val.strVal

    elif value is AbsFile:
      value = val.fsEntryVal.file.toAbsFile()

    elif value is AbsDir:
      value = val.fsEntryVal.dir.toAbsDir()

    else:
      static:
        {.error: "Implement conversion for " & $typeof(value).}



func len*(tree: CliCmdTree): int =
  case tree.kind:
    of coCommand:
      result = tree.subnodes.len

    else:
      discard

iterator items*(tree: CliCmdTree): CliCmdTree =
  if tree.kind == coCommand:
    for sub in tree.subnodes:
      yield sub


iterator items*(desc: CliDesc): CliDesc =
  case desc.kind:
    of coCommand:
      for arg in desc.arguments:
        yield arg

      for opt in desc.options:
        yield opt

      for sub in desc.subcommands:
        yield sub

    of coDashedKinds:
      for sub in desc.subKeys:
        yield sub

    else:
      discard

func isDisabled*(val: CliValue): bool =
  val.rawValue == val.desc.disabledFor

func isEmpty*(val: CliValue): bool =
  case val.kind:
    of cvkCommand:
      val.options.len == 0 and
      val.positional.len == 0 and
      val.subCmd.isNone()

    of cvkRecord:
      val.recValues.len == 0

    else:
      false

func contains*(val: CliValue, key: string): bool =
  case val.kind:
    of cvkCommand:
      if key in val.options:
        result = not val.options[key].isDisabled()

      else:
        for arg in val.positional:
          if key in arg.desc.altNames:
            return not arg.isDisabled()

    of cvkRecord:
      result = key in val.recValues

    else:
      discard


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

proc exitWith*(
    app: CliApp, ex: ref Exception,
    logger: HLogger, doQuit: bool = true
  ) =

  for code in app.exitCodes:
    if code.onException.isSome() and code.onException.get() == ex.name:
      ex.log(logger)
      logger.logStackTrace(ex)
      if doQuit:
        quit code.code

      else:
        return

  ex.log(logger)
  logger.logStackTrace(ex, showError = false)
  if doQuit:
    quit 1

macro runMain*(
    app: CliApp,
    mainProc: typed,
    logger: HLogger,
    doQuit: bool,
    args: varargs[untyped]
  ): untyped =

  let mainCall = newCall(mainProc)
  mainCall.add app
  mainCall.add logger
  for arg in args:
    mainCall.add arg

  let line = app.lineInfoObj()
  let iinfo = newLit((line.filename, line.line, line.column))

  quote do:
    {.line: `iinfo`.}:
      try:
        `mainCall`

      except Exception as e:
        `app`.exitWith(e, `logger`, `doQuit`)

proc arg*(
    name: string,
    doc: string,
    required: bool = true,
    check: CliCheck = checkAcceptAll(),
    docFull: string = ""
  ): CliDesc =

  CliDesc(
    doc: CliDoc(docBrief: doc, docFull: docFull),
    name: name,
    altNames: @[name],
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

template newCliError*(
    inMsg: string, inDesc: CliDesc, inGot: CliOpt,
    inKind: CliErrorKind = cekFailedParse): CliError =

  CliError(
    msg: inMsg,
    location: instantiationInfo(fullpaths = true),
    isDesc: true,
    desc: inDesc,
    got: ingot,
    kind: inKind
  )


template newCliError*(inMsg: string, inKind: CliErrorKind): CliError =

  CliError(
    msg: inMsg,
    location: instantiationInfo(fullpaths = true),
    isDesc: false,
    kind: inKind
  )


proc parseArg(
    lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =
  if lexer[].kind == coArgument:
    result = CliCmdTree(
      kind: coArgument, head: lexer.pop(), desc: desc)

  else:
    errors.addOrRaise newCliError(
      &"Failed to parse '{desc.name}' - expected argument, but found '{lexer[]}'",
      desc, lexer.pop(), cekFailedParse)

proc matches(opt: CliOpt, check: CliCheck): bool = true

proc parseOptOrFlag(
    lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =

  if lexer[].key in desc.altNames:
    result = CliCmdTree(
      kind: lexer[].kind, head: lexer.pop(), desc: desc)

    if ?lexer and result.head.needsValue():
      result.args.add lexer.pop()

  else:
    errors.addOrRaise newCliError(
      &"Failed to parse '{desc.name}' - expected {joinAnyOf(desc.altNames)}, but found '{lexer[]}'",
      desc, lexer.pop(), cekFailedParse)

proc parseCli(
    lexer: var HsLexer[CliOpt],
    desc: CliDesc,
    errors;
    argIdx: var int,
    argParsed: var seq[int]
  ): CliCmdTree


proc parseCmd(lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =
  var
    argIdx: int
    argParsed: seq[int]

  if lexer[].kind in {coArgument, coCommand} and lexer[].key in desc.altNames:
    result = CliCmdTree(
      kind: coCommand, head: lexer.pop(), desc: desc)

    while ?lexer:
      result.add parseCli(lexer, desc, errors, argIdx, argParsed)

  else:
    errors.addOrRaise newCliError(
      &"Failed to parse '{desc.name}' - expected 'argument' or 'command' with " &
        &"names {joinWords(desc.altNames, \"or\")}, but found '{lexer[].key}' of kind '{lexer[].kind}'",
      desc, lexer.pop(), cekFailedParse)


proc parseCli(
    lexer: var HsLexer[CliOpt],
    desc: CliDesc,
    errors;
    argIdx: var int, # Current index of the parsed argument group (for arguments
                     # that accept multiple values)
    argParsed: var seq[int] # Number of argument group elemens parsed for each
                        # group for `<arg-with two repeats> <arg>`
  ): CliCmdTree =

  case lexer[].kind:
    of coArgument:
      case desc.kind:
        of coArgument:
          result = parseArg(lexer, desc, errors)

        of coCommand:
          if lexer[].key in desc.altNames:
            result = parseCmd(lexer, desc, errors)

          else:
            let cmdIdx = desc.subcommands.findIt(lexer[].key in it.altNames)
            if cmdIdx == -1:
              if desc.arguments.high < argIdx:
                errors.addOrRaise newCliError(
                  &"'{desc.name}' does not have matching subcommand " &
                    &"or positional argument, found unexpected '{lexer[]}'",
                  desc, lexer.pop())

              else:
                result = parseArg(lexer, desc.arguments[argIdx], errors)
                # TODO handle repeated arguments
                inc argIdx

            else:
              result = parseCmd(lexer, desc.subcommands[cmdIdx], errors)

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
            errors.addOrRaise newCliError(
              &"No matching option. Got '{lexer[]} ({lexer[].kind})', but expected " &
                desc.options.mapIt(it.altNames).concat().joinWords(
                  "or", '\'', empty = "no options"),

              desc, lexer.pop(),
              cekUnexpectedEntry)

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
  if desc.kind in {coCommand}:
    result = parseCmd(lexer, desc, errors)

  else:
    var
      argIdx: int
      argParsed: seq[int]
    result = parseCli(lexer, desc, errors, argIdx, argParsed)

func `==`*(str: string, val: CliValue): bool =
  case val.kind:
    of cvkInt:
      try:
        result = (parseInt(str) == val.intVal)

      except ValueError:
        result = false

    of cvkString:
      result = str == val.strVal

    else:
      raise newImplementKindError(val)


proc checkedConvert(
    tree: CliCmdTree, check: CliCheck,
    errors; prevValue: CliValue = nil
  ): CliValue =

  assert not isNil(check),
     "Cannot perform checked conversion with `nil` check"

  let str = tree.head.value

  proc newFsEntry(str: string): CliValue =
    CliValue(
      rawValue: str,
      kind: cvkFsEntry,
      desc: tree.desc,
      fsEntryVal: (
        if isAbsolute(str):
          AbsFile(str).toFsEntry(false)

        else:
          RelFile(str).toFsEntry(false)))

  proc newCliError(
      kind: CliErrorKind, msg: varargs[string, `$`]): CliError =
    result = CliError(
      location: currIInfo(),
      isDesc: false,
      check: check,
      kind: kind,
      msg: msg.join(""))

    result.got = tree.head



  case check.kind:
    of cckAcceptAll, cckNoCheck:
      result = CliValue(
        rawValue: str,
        kind: cvkString, strVal: str, desc: tree.desc)

    of cckIsIntValue:
      try:
        let val = lexcast[int](str)
        result = CliValue(
          rawValue: str,
          kind: cvkInt,
          intVal: val,
          desc: tree.desc)

      except LexcastError as ex:
        errors.addOrRaise newCliError(
          cekCheckFailure,
          &"Could not parse '{hshow(str)}' as integer value")

    of cckIsBoolValue:
      if str.len == 0 and tree.kind in coFlagKinds:
        result = CliValue(
          rawValue: str,
          kind: cvkBool,
          boolVal: true,
          desc: tree.desc)

      else:
        try:
          let val = lexcast[bool](str)
          result = CliValue(
            rawValue: str,
            kind: cvkBool,
            boolVal: val,
            desc: tree.desc)

        except LexcastError as ex:
          errors.addOrRaise newCliError(
            cekCheckFailure,
            &"Could not parse '{hshow(str)}' as bool value")

    of cckIsInSet:
      for val in check.values:
        if str == val:
          return val

      raise newImplementError()

    of cckFileExtMatches:
      let
        fExt = RelFile(str).ext()
        exts = check.values.mapIt(it.strVal)

      for ext in check.values:
        if fExt == ext:
          if isNil(prevValue):
            result = newFsEntry(str)

          else:
            result = prevValue

          return

      if exts.len == 0:
        errors.addOrRaise newCliError(
          cekCheckFailure,
          &"Input file has extension {fExt}, but expected filename without it.",
        )

      else:
        errors.addOrRaise newCliError(
          cekCheckFailure,
          "Input file name does not have matching extension - expected any of ",
          exts.joinWords("or"), ", but got ",
          fExt, "."
        )

    of cckIsReadable:
      if check.fakeCheck:
        result = newFsEntry(str)

      elif not fileExists(str):
        errors.addOrRaise newCliError(
          cekCheckExecFailure,
          "Could not validate requirements for ",
          &"<{tree.desc.name}>, input file does not exist ({tree.head.value})"
        )

      else:
        let
          file = str.toFsEntry()
          permissions = file.file.getPermissions()

        if fpUserRead in permissions:
          result = CliValue(
            rawValue: str,
            kind: cvkFsEntry,
            fsEntryVal: file,
            desc: tree.desc)

        else:
          errors.addOrRaise newCliError(
            cekCheckFailure,
            "Input file must be readable, but supplied filename does not have ",
            &"requires permissions (got {permissions})"
          )


    of cckIsDirectory:
      if not str.fsEntryExists():
        errors.addOrRaise newCliError(
          cekCheckExecFailure,
          "Could not validate requirements for ",
          &"<{tree.desc.name}>, input directory does not exist ({tree.head.value})"
        )

      else:
        let dir = str.toFsEntry()
        result = CliValue(
          rawValue: str,
          kind: cvkFsEntry, fsEntryVal: dir, desc: tree.desc)

    of cckAndCheck:
      var errCount = errors.len()
      for sub in check.subChecks:
        result = checkedConvert(tree, sub, errors, result)
        if errors.len() > errCount:
          return nil


    of cckIsCreatable:
      # REVIEW not all files supplied in the command-line must be
      # creatable, but current implementation of the check would inspect
      # all of them
      let dir = tree.head.value.toFsDir()
      if dir.parentDir().exists():
        result = CliValue(
          rawValue: str,
          kind: cvkFsEntry, fsEntryVal: dir.toFsEntry, desc: tree.desc)

      else:
        errors.addOrRaise CliError(
          isDesc: false,
          check: check,
          kind: cekCheckFailure,
          got: tree.head,
          msg: "")

    of cckOrCheck:
      var tmpErrs: seq[CliError]

      for alt in check.subChecks:
        result = checkedConvert(tree, alt, tmpErrs)
        if not isNil(result):
          break

      if isNil(result):
        for err in tmpErrs:
          errors.addOrRaise err


    else:
      raise newImplementKindError(check)

proc toCliValue*(tree: CliCmdTree, errors): CliValue =
  ## Convert unchecked CLI tree into typed values, without executing
  ## checkers.
  case tree.kind:
    of coCommand:
      var converted: HashSet[string]
      result = CliValue(
        rawValue: tree.head.value,
        desc: tree.desc, kind: cvkCommand)

      for sub in tree:
        case sub.kind:
          of coArgument:
            result.positional.add toCliValue(sub, errors)
            converted.incl sub.desc.name

          of coDashedKinds:
            result.options[sub.head.key] = toCliValue(sub, errors)
            converted.incl sub.desc.name

          of coCommand:
            result.subCmd = some toCliValue(sub, errors)
            converted.incl sub.desc.name

          else:
            raise newImplementKindError(sub)

      for sub in tree.desc:
        if sub.name notin converted:
          var outDef: CliValue = nil
          if sub.defaultValue.isSome():
            let def = sub.defaultValue.get()
            case def.kind:
              of cdkConstString:
                var default = CliCmdTree(
                  desc: sub, head: CliOpt(
                    keyPath: @[sub.name], kind: sub.kind,
                    valStr: def.strVal))

                outDef = toCliValue(default, errors)

              of cdkCallback:
                outDef = CliValue(
                  desc: tree.desc, rawValue: "",
                  defaultFrom: def, kind: cvkNotYetDefaulted)

              of cdkCliValue:
                outDef = def.value

          elif sub.kind in coFlagKinds:
            outDef = toCliValue(false)

          if not isNil(outDef):
            case sub.kind:
              of coArgument:    result.positional.add outDef
              of coDashedKinds: result.options[sub.name] = outDef
              else:             raise newUnexpectedKindError(sub)



    of coArgument:
      result = checkedConvert(tree, tree.desc.check, errors)

    of coFlag, coOpt:
      var tree = tree
      if tree.head.canAddValue() and
         tree.desc.defaultAsFlag.isSome():
        let def = tree.desc.defaultAsFlag.get()

        case def.kind:
          of cdkConstString:
            tree.head.valStr = def.strVal
            result = checkedConvert(tree, tree.desc.check, errors)

          of cdkCliValue:
            result = def.value

          of cdkCallback:
            result = CliValue(
              desc: tree.desc,
              rawValue: "",
              defaultFrom: def,
              kind: cvkNotYetDefaulted)

      else:
        result = checkedConvert(tree, tree.desc.check, errors)

    else:
      raise newImplementKindError(tree)

initBlockFmtDsl()

proc mergeConfigs*(
    tree: CliCmdTree,
    configs: seq[CliValue],
    errors
  ) =

  raise newImplementError()

proc finalizeDefaults*(app: var CliApp) =
  proc aux(tree: var CliValue, app: var CliApp, path: seq[string]) =
    case tree.kind:
      of cvkNotYetDefaulted:
        case tree.defaultFrom.kind:
          of cdkCallback:
            tree[] = tree.defaultFrom.impl(app, path)[]

          else:
            raise newImplementKindError(tree.defaultFrom)

      of cvkCommand:
        for pos in mitems(tree.positional):
          aux(pos, app, path)

        for _, opt in mpairs(tree.options):
          aux(opt, app, path)

        if tree.subCmd.isSome():
          aux(tree.subCmd.get(),
              app, path & tree.subCmd.get().desc.name)

      else:
        discard



  aux(app.value, app, @[])

proc acceptArgs*(
    app: var CliApp,
    params: seq[string] = paramStrs()): bool =
  app.rawArgs = params

  var params = @[app.root.name] & params

  let (parsed, failed) = params.parseCliOpts()
  if failed.len > 0:
    app.errors.addOrRaise CliError(
      kind: cekErrorDescription,
      msg: "Could not parse command-line options")

    return false

  let tree = parsed.structureSplit(app.root, app.errors)
  if app.errors.len > 0:
    app.errors.addOrRaise newCliError(
      "Could not structure command-line options",
      cekErrorDescription)

    return false

  app.value = tree.toCliValue(app.errors)
  if app.errors.len > 0:
    app.errors.addOrRaise newCliError(
      "Failure while validating command-line options",
      cekErrorDescription)

    return false

  app.finalizeDefaults()

  if app.errors.len > 0:
    return false

  return true

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
      result.add T[&", defaults to {toCyan(desc.defaultValue.get().defaultRepr)}"]

    if desc.defaultAsFlag.isSome():
      result.add T[&", as flag defaults to {toCyan(desc.defaultAsFlag.get().defaultRepr)}"]

  let rep = desc.check.allowedRepeat
  if rep.b > 1:
    result.add T[&", can be repeated {desc.check.allowedRepeat} times"]

  if desc.aliasof.isSome():
    result.add T[&", alias of '{toYellow($desc.aliasof.get())}'"]


proc `$`*(val: CliValue): string =
  case val.kind:
    of cvkFloat: result = $val.floatVal
    of cvkString: result = $val.strVal
    of cvkFsEntry: result = $val.fsEntryVal
    else:
      raise newImplementKindError(val)

proc checkHelp(check: CliCheck, inNested: bool = false): LytBlock =
  result = E[]
  if isNil(check): return
  let prefix = tern(inNested, "- ", "Value must ")

  case check.kind:
    of cckIsInSet, cckFileExtMatches:
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
        prefix & tern(
          check.kind == cckFileExtMatches,
          "have one of the following extensions",
          "be one of the following: ",
        )
      ], I[4, result], S[]]

    of cckOrCheck:
      result = V[T[prefix & "meet at least one of the following criteria:"]]
      for sub in check.subChecks:
        result.add I[4, checkHelp(sub, true)]

    of cckIsBoolValue:
      result = T[prefix & "must be convertible to bool - `on`/`off`/`true` etc"]

    of cckAndCheck:
      result = V[T[prefix & "meet all of the following criteria:"]]
      for sub in check.subChecks:
        result.add I[4, checkHelp(sub, true)]

    of cckIsWritable:
      result = T[prefix & "be a writable file"]

    of cckIsCreatable:
      result = T[prefix & "be a path where can be created"]

    of cckAllOf:
      result = V[T["Every element must match following criteria"]]
      result.add I[4, checkHelp(check.itemCheck, true)]

    of cckAcceptAll:
      result = T["all values are accepted"]

    of cckNoCheck:
      result = E[]

    of cckIsReadable:
      result = T[prefix & "be readable by the application"]

    of cckIsDirectory:
      result = T[prefix & "be a directory"]

    of cckRepeatCheck:
      result = T[
        prefix & &"be used from {check.allowedRepeat.a} to " &
          tern(
            check.allowedRepeat.b == high(int),
            "unlimited",
            $check.allowedRepeat.b
          ) & " times"
      ]

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

      cmds.add I[4, T[names]]
      cmds.add I[4, help(cmd, level + 1)]

    result.add I[4, cmds]

  if desc.options.len > 0 and
     desc.options.anyIt(it.groupKind in coFlagKinds):
    var flags = V[S[], T[toColored("FLAGS:", section)], S[]]

    for flag in desc.options:
      if flag.groupKind in coFlagKinds:
        let help = I[4, flagHelp(flag)]
        flags.add help


    result.add I[indent, flags]

  if desc.options.len > 0 and
     desc.options.anyIt(it.groupKind in coOptionKinds):
    var opts = V[S[], T[toColored("OPTIONS:", section)], S[]]

    for opt in desc.options:
      if opt.groupKind in coOptionKinds:
        opts.add I[4, optHelp(opt)]

    result.add I[indent, opts]



proc help(app: CliApp): LytBlock =
  var res = V[
    T[toColored("NAME:", section)],
    S[],
    I[4, T[&"{app.name} - {app.doc.docBrief}, " &
           &"{app.version} by {app.author}"]]
  ]

  if app.exitCodes.len > 0:
    var codes = V[T["EXIT STATUS:".toColored(section)]]

    for exit in app.exitCodes:
      codes.add S[]
      codes.add I[4, T[&"{exit.code} - {exit.doc.docBrief}"]]

    res.add codes

  res.add app.root.help()

  # res.add @[
  #   S[], T["VERSION:".toColored(section)],
  #   S[], I[4, T[&"{app.version} by {app.author}"]]
  # ]

  result = res



proc helpStr*(app: CliApp): string =
  return app.help().toString()

proc help(err: CliError): LytBlock =
  let errmsg = err.msg & " in " & toLink(
    err.location,
    AbsFile(err.location[0]).name() & ":" & $err.location[1])

  case err.kind:
    of cekFailedParse:
      let split = stringMismatchMessage(
        err.got.key, err.desc.altNames, showAll = true, colored = false)

      result = V[T[split], T[errmsg]]

    of cekUnexpectedEntry:
      let alts = err.desc.options.mapIt(it.altNames).concat()
      let split = stringMismatchMessage(
        err.got.key, alts, showAll = true, colored = false)

      result = V[T[split], T[errmsg]]

    of cekCheckExecFailure, cekErrorDescription, cekCheckFailure:
      result = T[errmsg]

    else:
      raise newImplementKindError(err)

proc helpStr*(err: CliError): string =
  return err.help().toString()


proc showErrors*(app: CliApp, logger: HLogger) =
  ## Show accumulated errors using `logger`
  for err in app.errors:
    logger.err err.helpStr()

proc builtinActionRequested*(app: CLiApp): bool =
  ## Whether some of the built-in flags such as `--help` or version where
  ## present in toplevel command-line.
  assert not isNil(app.value),
     "Check for builtin request should be performed after `acceptArgs`"

  for builtin in ["help",  "version", "complete-for"]:
    if builtin in app.value:
      return true

  return app.rawArgs.len() == 0

proc showBuiltin*(app: CliApp, logger: HLogger) =
  ## Show output for one (or more) requested built-in entries such as
  ## `--help`
  if "help" in app.value:
    echo app.helpStr()

  if "version" in app.value:
    echo app.version()

  if app.rawArgs.len() == 0:
    echo "Empty args"
