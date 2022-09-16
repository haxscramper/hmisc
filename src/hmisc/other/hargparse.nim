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
  ../core/[all, code_errors],
  ../types/colorstring,
  ../algo/[
    lexcast, clformat, htemplates, hseq_mapping, htext_algo,
    hparse_base, hstring_algo, halgorithm
  ],
  ../macros/argpass

export `$`

export cliparse, hlogger, argpass


import
  std/[
    tables, options, uri, sequtils, strformat, sets,
    strutils, algorithm, effecttraits, macros
  ]

# import hpprint

type
  CliCmdTreeKind* = enum
    cctFlag ## Command-line option flag with no values
    cctOpt ## Option with values
    cctArgument ## Positional argument
    cctCommand ## Subcommand
    cctGrouped ## Repeatedly used flag - `--define:val1 --define:val2`

  CliCmdTree* = object
    desc* {.requiresinit.}: CliDesc ## Command tree structure description
    head* {.requiresinit.}: CliOpt ## Used CLI option
    args*: seq[CliCmdTree] ## Additional arguments to CLI option (for
                           ## separately specified arguments)
    mainIdx*: int ## Cmd tree entry index in the main input (to compute
                  ## origin information later)
    case kind*: CliCmdTreeKind
      of cctCommand:
        subnodes*: seq[CliCmdTree]

      of cctFlag, cctOpt:
        subPaths*: Table[string, CliCmdTree]

      of cctGrouped:
        entries*: seq[CliCmdTree]

      else:
        discard

  CliCheckKind = enum
    cckNoCheck ## Allow any entry kind
    cckAliasedCheck ## Use check for other CLI option

    cckIsFile ## Must be a file
    cckIsDirectory ## Must be a directory
    cckIsWritable ## Writable by current process
    cckIsReadable ## Readable for current process
    cckIsCreatable ## Can be created
    cckIsEmpty

    cckFileExtMatches ## has matching extension

    cckIsInRange ## Within range of values (integer)
    cckIsPositive ## Positive value (integer)

    cckIsInSet ## In set of predefined values

    cckUser

    cckAndCheck ## Match multiple options at once
    cckOrCheck ## Match one or more of the specified options
    cckAndPosCheck ## Multiple conditions. Element at each position must
                   ## match corresponding check. Used for enum checking -
                   ## first element is validated against type in first
                   ## position, second against second and so on.
    cckCheckRepeat ## Repeat seveal times

    cckAllOf
    cckNoneOf
    cckAnyOf

    cckAcceptAll

    cckIsStrValue ## Must be a valid string (basically accept-all)
    cckIsIntValue ## Valid integer
    cckIsFloatValue ## Float
    cckIsBoolValue ## Bool
    cckIsListValue ## ???


  CliErrorKind = enum
    cekErrorDescription
    cekUserFailure
    cekMissingEntry ## Required argument was not specified
    cekFailedParse ## Cannot process input parameters
    cekUnexpectedEntry ## Specified entry was not found in CLI description
    cekCheckExecFailure ## Cannot validate input parameters
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
    fakeCheck*: bool ## Do not execute OS actions.
    case kind*: CliCheckKind
      of cckIsInRange:
        valueRange*: Slice[float64]

      of cckIsInSet, cckFileExtMatches:
        values*: seq[CliValue]

      of cckAndCheck, cckOrCheck, cckAndPosCheck:
        subChecks*: seq[CliCheck]

      of cckCheckRepeat:
        allowedRepeat*: Slice[int] ## Allowed range of value repetitions

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

  CliShowLevel = enum
    cslDefault
    cslHidden
    cslPartial
    cslCommonAlreadyShown

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
        commonArguments*: HashSet[string]

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
    cvkSpecialString

    cvkCommand
    cvkNotYetDefaulted

  CliOptionsTable* = Table[string, CliValue]
  CliValue* = ref object
    origin*: CliOrigin
    desc*: CliDesc
    rawValue*: string
    case kind*: CliValueKind
      of cvkCommand:
        name*: string
        positional*: seq[CliValue]
        options*: CliOptionsTable
        subCmd*: Option[CliValue]

      of cvkUnparse:
        matches: seq[string]

      of cvkString, cvkSpecialString:
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
  cctDashedKinds* = { cctOpt, cctFlag }
  cvkValidatorNames*: array[CliValueKind, seq[string]] = toMapArray {
    cvkFsEntry: @[
      "checkFileReadable",
      "checkDirCratable",
      "cliCheckFor(FsEntry/AbsDir/RelDir/AbsFile/RelFile)"],
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

func treeRepr*(tree: CliCmdTree): ColoredText =
  coloredResult()
  func aux(t: CliCmdTree, level: int) =
    addIndent(level)
    add hshow(t.kind)
    add " " & toCyan(t.desc.name)
    add " " & hshow($t.head) & "\n"

    for arg in t.args:
      addIndent(level + 1)
      add hshow($arg) & "\n"

    case t.kind:
      of cctCommand:
        for sub in t.subnodes:
          aux(sub, level + 1)

      of cctDashedKinds:
        for key, path in t.subPaths:
          addIndent(level + 1)
          add hshow(key) & ": " & $path & "\n"

      of cctGrouped:
        for entry in t.entries:
          aux(entry, level + 1)

      else:
        discard



  aux(tree, 0)


func treeRepr*(tree: CliValue): ColoredText =
  coloredResult()
  func aux(t: CliValue, level: int) =
    addIndent(level)
    if isNil(t):
      add "<nil>" + fgRed
      return

    add hshow(t.kind)
    case t.kind:
      of cvkCommand:
        add " " & toMagenta(t.desc.name) & "\n"
        let level = level + 1
        for arg in t.positional:
          addIndent(level)
          add "arg <" & toCyan(arg.desc.name) & ">\n"
          aux(arg, level + 1)
          add "\n"

        for key, opt in t.options:
          addIndent(level)
          add "opt <" & toCyan(key) & ">\n"
          aux(opt, level + 1)
          add "\n"

        if t.subCmd.isSome():
          add getIndent(level) & "cmd\n"

          aux(t.subCmd.get(), level + 1)
          add "\n"


      of cvkString:  add " " & hshow(t.strVal)
      of cvkBool:    add " " & hshow(t.boolVal)
      of cvkInt:     add " " & hshow(t.intVal)
      of cvkFloat:   add " " & hshow(t.floatVal)
      of cvkFsEntry: add " " & hshow(t.fsEntryVal.getStr())

      of cvkSeq:
        for item in t.seqVal:
          add "\n"
          aux(item, level + 1)

      else:
        raise newImplementKindError(t)

  aux(tree, 0)



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
  CliCheck(kind: cckCheckRepeat, allowedRepeat: minRepeat .. maxRepeat)

proc checkRepeat*(repeat: Slice[int]): CliCheck =
  CliCheck(kind: cckCheckRepeat, allowedRepeat: repeat)

proc checkNo*(): CliCheck = CliCheck(kind: cckNoCheck)

proc checkAnd*(and1, and2: CliCheck, other: varargs[CliCheck]): CLiCheck =
  result = CliCheck(kind: cckAndCheck, subChecks: @[and1, and2])
  for check in other:
    result.subChecks.add check

proc checkAndPos*(checks: varargs[CliCheck]): CliCheck =
  CliCheck(kind: cckAndPosCheck, subChecks: toSeq(checks))

proc checkAnyOf*(sub: CliCheck): CliCheck =
  CliCheck(kind: cckAnyOf, itemCheck: sub)

proc checkAllOf*(sub: CliCheck): CliCheck =
  CliCheck(kind: cckAllOf, itemCheck: sub)

proc checkAcceptAll*(): CLiCheck =
  CliCheck(kind: cckAcceptAll)

proc checkKind*(kind: CliCheckKind): CliCheck =
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
    # HACK Hack, ensure there are no nil checks later
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
    docFull: string = "",
    commonArguments: seq[string] = @[]
  ): CliDesc =

  result = CliDesc(
    name: name,
    altNames: @[name] & alt,
    kind: coCommand,
    doc: CliDoc(docBrief: docBrief, docFull: docFull),
    commonArguments: toHashSet(commonArguments)
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

type
  ProcArg = object
    name: string
    default: Option[NimNode]
    argType: NimNode

func splitProcArgs(node: NimNode): seq[ProcArg] =
  for param in node.params()[1 ..^ 1]:
    var default: Option[NimNode]
    if param[2].kind != nnkEmpty:
      default = some param[2]

    result.add ProcArg(
      name: param[0].strVal(),
      argType: param[1],
      default: default)

type
  ProcConf* = object
    ignore: seq[string]
    help: seq[tuple[name, doc: string]]
    commonArguments: seq[string]
    alt: seq[string]
    positional: seq[string]

func procConf*(
    alt:             seq[string]                         = @[],
    ignore:          seq[string]                         = @[],
    help:            openarray[tuple[name, doc: string]] = @[],
    commonArguments: openarray[string]                   = @[],
    positional:      openarray[string]                   = @[]
  ): ProcConf =

  ## - @arg{ignore} :: List of ignored procedure arguments - they won't be
  ##   added as subcommand options, and must be defaulted or supplied for
  ##   [[code:runDispatched]]

  result = ProcConf(
    ignore: ignore,
    alt: alt,
    help: toSeq(help),
    commonArguments: toSeq(commonArguments),
    positional: toSeq(positional)
  )

func getHelpStr*(conf: ProcConf, arg: string): string =
  for (name, doc) in conf.help:
    if name.eqIdent(arg):
      return doc

func toValue*[T](arg: T): T = arg

func getDocComment(node: NimNode): string =
  proc aux(buf: var string, n: NimNode, depth: int = 0) =
    if n.len > 1:
      for sub in n:
        aux(buf, sub, depth + 1)

    else:
      if n.kind == nnkCommentStmt and depth < 4:
        if n.strVal.len != 0:
          buf.add(" ")
          buf.add(n.strVal)

  aux(result, node, 0)


func regenType(node: NimNode): NimNode =
  case node.kind:
    of nnkSym, nnkIdent:
      result = ident(node.strVal())

    of nnkEmpty:
      result = newEmptyNode()

    else:
      result = newTree(node.kind)
      for sub in node:
        result.add regenType(sub)


macro cmdImpl*(
    procSym: typed,
    argpass: untyped,
    conf: static[ProcConf]
  ): untyped =
  let
    impl = procsym.getImpl()
    doc = impl.body().getDocComment()
    docBrief = doc.split("\n\n")[0].strip()

  result = newCall("cmd")
  result.addArg "name", newLit($procsym)
  result.addArg "docBrief", newLit(docBrief)

  result = newCall("argpass", result)
  if argpass.kind == nnkCall:
    result.add argpass[1 ..^ 1]

  if conf.commonArguments.len > 0:
    result.addArg "commonArguments", newLit(conf.commonArguments)

  if conf.alt.len > 0:
    result.addArg "alt", newLit(conf.alt)

  result.addArg "docFull", newLit(doc)

  let args = splitProcArgs(impl)

  if args.len > 0:
    var cmdParams = newStmtList()

    let tmpCmd = genSym(nskVar, "cmd")
    for arg in args:
      if arg.name in conf.ignore:
        continue

      let isArg = arg.default.isNone() or arg.name in conf.positional

      var call = if isArg: newCall("arg") else: newCall("opt")

      call.addArg "name", newLit(arg.name)
      call.addArg "check", newCall(
        "cliCheckFor", arg.argType.regenType())

      let doc = conf.getHelpStr(arg.name)
      if doc.len == 0:
        raise toCodeError(
          procsym,
          &"Proc '{procsym}' has argument '{arg.name}' with missing or empty help string",
          "Add it using `help` argument of the `procConf` parameter.\n" &
            &"help = {{ \"{arg.name}\": \"...\" }}"
        )

      call.addArg "doc", newLit(doc)


      if not isArg:
        call.addarg "default", newCall(
          "cliDefault",
          newCall(
            "toCliValue",
            newCall(
              nnkBracketExpr.newTree(ident"toValue", arg.argType),
              arg.default.get())),
          newLit(arg.default.get().repr()))

      cmdParams.add newCall("add", tmpCmd, call)

    result = quote do:
      block:
        var `tmpCmd` = `result`
        `cmdParams`
        `tmpCmd`

  # echo result.repr


macro cmd*(
    procsym: typed{`proc`},
    argpass: untyped = nil,
    conf: ProcConf{`const`|lit|nkCall} = procConf()
  ): untyped =
  ## Generate CLI subcommand based in implementation of the procedure whose
  ## symbol is passed as a @arg{procsym}.
  newCall("cmdImpl", procsym, argpass, conf)

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

func getCmdName*(val: CliValue): string =
  ## Return currently selected command name
  val.subCmd.get().desc.name

func getName*(val: CliValue): string =
  val.desc.name

func getCmd*(val: CliValue): CliValue =
  ## Return currently selectd command
  val.subCmd.get()

func getArg*(val: CliValue, pos: int = 0): CliValue =
  ## Return argument at index `pos` for command
  val.positional[pos]

func getCmd*(app: CliApp): CliValue =
  ## Return currently selected active *subcommand*
  app.value.getCmd()

func getRootCmd*(app: CliApp): CliValue =
  ## Return application root command. It is different from `getCmd` and
  ## provides access to the always-present root of the processed
  ## command-line options.
  ##
  ## - NOTE :: `app.getRootCmd().getCmd()` and `app.getCmd()` have the
  ##   effect
  app.value

func getOptions*(app: CliApp): Table[string, CliValue] =
  app.value.options

func getCmdName*(app: CliApp): string = app.value.getCmdName()
func getArg*(app: CliApp, pos: int = 0): CliValue = app.value.getArg(pos)
func getOpt*(
    val: CliValue,
    name: string,
    optional: bool = false
  ): CliValue =

  assertKind(val, {cvkCommand})
  if name in val.options:
    result = val.options[name]

  elif optional:
    result = CliValue(kind: cvkSeq)

  else:
    raise newArgumentError(
      &"Could not find option '{name}' for command '{val.desc.name}'.",
      "Available options:",
      val.options.getKeys().joinWords("or"))

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

template assertKind*(
    val: CliValue, target: set[CliValueKind]
  ): untyped {.dirty.} =
  bind joinAnyOf, assertKind, ColoredText, toColoredText
  var targetNames: seq[ColoredText]
  for k in items(target):
    for name in cvkValidatorNames[k]:
      targetNames.add toColoredText(name)

  assertKind(
    val, target,
    ". In order to convert CLI string to value of this kind use " &
      $joinAnyOf(
        targetNames,
        prefix = toColoredText("one of the converters "),
        empty = toColoredText("converters for target kind")))

proc fromCliValue*(val: CliValue, result: var AbsFile) =
  assertKind(val, {cvkFsEntry})
  assertKind(val.fsEntryVal, {pcFile, pcLinkToFile})
  if val.fsEntryVal.file.isRelative:
    result = val.fsEntryVal.file.relFile.toAbsFile()

  else:
    result = val.fsEntryVal.file.absFile

proc fromCliValue*(val: CliValue, result: var AbsDir) =
  assertKind(val, {cvkFsEntry})
  assertKind(val.fsEntryVal, {pcDir, pcLinkToDir})
  if val.fsEntryVal.dir.isRelative:
    result = val.fsEntryVal.dir.relDir.toAbsDir()

  else:
    result = val.fsEntryVal.dir.absDir


proc fromCliValue*(val: CliValue, result: var RelFile) =
  assertKind(val, {cvkFsEntry})
  assertKind(val.fsEntryVal, {pcFile, pcLinkToFile})
  if val.fsEntryVal.file.isRelative:
    result = val.fsEntryVal.file.relFile

  else:
    raise newUnexpectedKindError(
      "Cannot convert absolute input directory to a relative",
      "without base directory.")

proc fromCliValue*(val: ClivAlue, result: var FsDir) =
  assertKind(val, {cvkFsEntry})
  assertKind(val.fsEntryVal, {pcDir, pcLinkToDir})
  result = val.fsEntryVal.dir

proc fromCliValue*(val: CliValue, result: var FsFile) =
  case val.kind:
    of cvkFsEntry:
      assertKind(val.fsEntryVal, {pcFile, pcLinkToFile})
      result = val.fsEntryVal.file

    else:
      raise newUnexpectedKindError(val)

proc fromCLiValue*[E: enum](val: CliValue, result: var E) =
  result = parseEnum[E](val.strVal)

proc fromCliValue*[T](val: CliValue, result: var seq[T]) =
  bind assertKind
  case val.kind:
    of cvkSeq:
      result.setLen(val.seqVal.len)
      for idx, item in val.seqVal:
        fromCliValue(item, result[idx])

    else:
      raise newUnexpectedKindError(val)

proc fromCliValue*[A, B](val: CliValue, result: var (A, B)) =
  assertKind(val, cvkSeq)
  fromCliValue(val.seqVal[0], result[0])
  fromCliValue(val.seqVal[1], result[1])

proc fromCliValue*[T](val: CliValue, result: var Option[T]) =
  bind assertKind
  case val.kind:
    of cvkSeq:
      case val.seqVal.len:
        of 0:
          discard

        of 1:
          var tmp: T
          fromCliValue(val.seqVal[0], tmp)
          result = some(tmp)

        else:
          raise newImplementError()

    else:
      raise newUnexpectedKindError(val, "Convert to Option[T]")

proc fromCliValue*(val: CliValue, result: var string) =
  val.assertKind({cvkString})
  result = val.strVal

proc fromCliValue*(val: CliValue, result: var int) =
  val.assertKind({cvkInt})
  result = val.intVal

proc fromCliValue*(val: CliValue, result: var float) =
  val.assertKind({cvkFloat})
  result = val.floatVal

proc fromCliValue*(val: CliValue, result: var bool) =
  assertKind(val, {cvkBool})
  result = val.boolVal

func fromCliValue*(val: CliValue, target: var ShellExpr) =
  assertKind(val, {cvkString, cvkSpecialString})
  target = ShellExpr(val.strVal)

proc `as`*[T](val: CLiValue, target: typedesc[T]): T =
  mixin fromCliValue
  fromCliValue(val, result)

template updateCliValue*(value: CliValue, inDoc: string, inDesc: CliDesc) =
  let desc =
    if isNil(inDesc):
      CliDesc(doc: CliDoc(docBrief: inDoc))

    else:
      inDesc

  value.desc = desc
  when compiles($cli):
    value.rawValue = $cli

template specialStringCliValue*(value, inDoc: string, inDesc: CliDesc) =
  result = CliValue(kind: cvkSpecialString, strVal: value)
  updateCliValue(result, doc, desc)

func toCliValue*(
    shell: ShellExpr, doc: string = "", desc: CliDesc = nil): CliValue =
  specialStringCliValue(shell.string, doc, desc)

proc toCliValue*(
    cli: FsEntry, doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkFsEntry, fsEntryVal: cli)
  updateCliValue(result, doc, desc)

proc toCliValue*(
    cli: FsFile or FsDir, doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkFsEntry, fsEntryVal: cli.toFsEntry())
  updateCliValue(result, doc, desc)

proc toCliValue*(
    cli: AbsFile or RelFile, doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkFsEntry, fsEntryVal: cli.toFsFile().toFsEntry())
  updateCliValue(result, doc, desc)

proc toCliValue*(
    cli: AbsDir or RelDir, doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkFsEntry, fsEntryVal: cli.toFsDir().toFsEntry())
  updateCliValue(result, doc, desc)

proc toCliValue*[T](
    cli: seq[T], doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkSeq)
  for item in items(cli):
    result.seqVal.add toCliValue(item)

  updateCliValue(result, doc, desc)

proc toCliValue*[A, B](
    cli: (A, B), doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkSeq, seqVal: @[
    toCliValue(cli[0]), toCliValue(cli[1])])

  updateCliValue(result, doc, desc)

proc toCliValue*[T](
    cli: Option[T], doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkSeq)
  if isSome(cli): result.seqVal.add toCliValue(cli.get())
  updateCliValue(result, doc, desc)

func toCliValue*(
    cli: string, doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkString, strVal: cli)
  updateCliValue(result, doc, desc)

func toCliValue*(cli: int, doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkInt, intVal: cli)
  updateCliValue(result, doc, desc)

func toCliValue*(
    cli: bool, doc: string = "", desc: CliDesc = nil): CliValue =
  result = CliValue(kind: cvkBool, boolVal: cli)
  updateCliValue(result, doc, desc)

proc cliCheckFor*[T](value: typedesc[seq[T]]): CliCheck =
  mixin cliCheckFor
  result = checkAnd(checkRepeat(0, high(int)), cliCheckFor(T))

proc cliCheckFor*[A, B](value: typedesc[(A, B)]): CliCheck =
  mixin cliCheckFor
  result = checkAndPos(cliCheckFor(A), cliCheckFor(B))

proc cliCheckFor*[T](value: typedesc[Option[T]]): CliCheck =
  mixin cliCheckFor
  result = checkAnd(checkRepeat(0, 1), cliCheckFor(T))

proc cliCheckFor*(f: typedesc[FsFile]): CLiCheck = checkFileReadable()
proc cliCheckFor*(f: typedesc[AbsFile]): CLiCheck = checkFileReadable()
proc cliCheckFor*(f: typedesc[RelFile]): CLiCheck = checkFileReadable()
proc cliCheckFor*(f: typedesc[AbsDir]): CLiCheck = checkDirExists()
proc cliCheckFor*(f: typedesc[FsDir]): CLiCheck = checkDirExists()
proc cliCheckFor*(str: typedesc[string]): CliCheck = checkAcceptAll()
proc cliCheckFor*(shell: typedesc[ShellExpr]): CliCheck = checkAcceptAll()

proc cliCheckFor*(str: typedesc[int]): CliCheck =
  CliCheck(kind: cckIsIntValue)

proc cliCheckFor*(bol: typedesc[bool]): CliCheck =
  CliCheck(kind: cckIsBoolValue)

export toMapArray

proc cliCheckFor*[En: enum](
    en: typedesc[En],
    docs: openarray[(string, string)]
  ): CliCheck =

  return checkValues(@docs)

proc cliCheckFor*[En: enum](
    en: typedesc[En], docs: array[En, string]): CliCheck =

  var values: seq[(string, string)]
  for val in { low(en) .. high(en) }:
    values.add ($val, docs[val])

  return checkValues(values)

func cliDefault*(str: string): CliDefault =
  CliDefault(kind: cdkConstString, strVal: str, defaultRepr: str)

func cliDefault*(val: CliValue, defaultRepr: string = ""): CliDefault =
  CliDefault(
    kind: cdkCliValue, value: val,
    defaultRepr: if defaultRepr.len > 0: defaultRepr else: val.rawValue,
    defaultDesc: val.docBrief()
  )

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
        checkKind(cckIsWritable),
        checkKind(cckIsCreatable)
      )



    result.add opt


const
  cliNoLoggerConfig* = @["loglevel", "log-output", "quiet", "color", "json"]
  cliDefaultHelpOnly* = cliNoLoggerConfig & @["version", "dry-run", "force"]
  cliNoDefaultOpts* = cliDefaultHelpOnly & @["help"]





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
    of cctCommand:
      tree.subnodes.add other

    of cctDashedKinds:
      tree.subPaths[other.head.keyPath[pathLevel]] = other

    of cctGrouped:
      tree.entries.add other

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
    of cctCommand: result = tree.subnodes.len
    of cctGrouped: result = tree.entries.len
    of cctOpt: result = tree.args.len
    else: discard

iterator items*(tree: CliCmdTree): CliCmdTree =
  case tree.kind:
    of cctCommand:
      for sub in tree.subnodes:
        yield sub

    of cctGrouped:
      for entry in tree.entries:
        yield entry

    else:
      for arg in tree.args:
        yield arg


    # else:
    #   discard

func `[]`*(tree: CliCmdTree, idx: int): CliCmdTree =
  case tree.kind:
    of cctCommand:
      result = tree.subnodes[idx]

    of cctGrouped:
      result = tree.entries[idx]

    of cctOpt:
      result = tree.args[idx]

    else:
      raise newUnexpectedKindError(
        tree, "Index operator")


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
    if code.onException.isSome() and code.onException.get().cstring == ex.name:
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
    doQuit: bool = true,
    argpass: untyped{nkCall} = nil
  ): untyped =

  let mainCall = newCall(mainProc)
  mainCall.addArgpass(argpass)

  let line = app.lineInfoObj()
  let iinfo = newLit((line.filename, line.line, line.column))

  quote do:
    {.line: `iinfo`.}:
      try:
        `mainCall`

      except Exception as e:
        `app`.exitWith(e, `logger`, `doQuit`)

template acceptArgsAndRunBody*(
    app: CliApp,
    logger: HLogger,
    args, body: untyped
  ): untyped =

  if app.acceptArgs(args):
    if app.builtinActionRequested():
      app.showBuiltin(logger)

    else:
      body

  else:
    app.showErrors(logger)

macro runDispatchedProc*(
    app: CliApp,
    procsym: typed,
    logger: HLogger,
    doQuit: bool,
    argpass: untyped,
    conf: static[ProcConf]
  ): untyped =

  var mainCall = newCall(procsym)

  let args = procsym.getImpl().splitProcArgs()
  let currentCmd = genSym(nskLet, "currentCmd")

  var preCall = newStmtList()
  for arg in args:
    if arg.name notin conf.ignore:
      let
        argVar = ident(arg.name)
        argType = arg.argType.regenType()
        argName = newLit(arg.name)

      if arg.name in conf.positional or arg.default.isNone():
        preCall.add quote do:
          let `argVar` = getArg(`currentCmd`, `argName`) as `argType`

      else:
        preCall.add quote do:
          let `argVar` = getOpt(`currentCmd`, `argName`) as `argType`

      mainCall.addArg arg.name, argVar

  mainCall.addArgpass(argpass)

  result = quote do:
    try:
      let `currentCmd` = `app`.getCmd()
      `preCall`
      `mainCall`

    except Exception as e:
      `app`.exitWith(e, `logger`, `doQuit`)


macro runDispatched*(
    app: CliApp,
    proclist: untyped{nkBracket},
    logger: HLogger,
    doQuit: bool,
  ): untyped =

  let cmd = genSym(nskLet, "cmd")
  let cmdName = newCall("getName", cmd)

  var dispatch = nnkIfStmt.newTree()

  for procname in proclist:
    assertNodeKind(procname, {nnkIdent, nnkPar, nnkTupleConstr})
    var call = newCall(
        "runDispatchedProc",
        newEqe("app", app),
        newEqe("logger", logger),
        newEqe("doQuit", doQuit),
    )

    case procname.kind:
      of nnkIdent:
        call.addArg "procsym", procname
        call.addArg "conf", newCall("procConf")
        call.addArg "argpass", newLit(false)

        dispatch.add nnkElifBranch.newTree(
          nnkInfix.newTree(ident("=="),
                           cmdName,
                           newLit(procname.strVal())),
          call)
      else:
        let
          name = procname[0]
          conf = procname[1]

        call.addArg "procsym", name
        call.addArg "conf", conf

        if procname.len == 3:
          call.addArg "argpass", procname[2]

        else:
          call.addArg "argpass", newLit(false)


        dispatch.add nnkElifBranch.newTree(
          newCall("contains",
                  nnkInfix.newTree(ident"&",
                                   nnkDotExpr.newTree(conf, ident"alt"),
                                   newLit(name.strVal())),
                  cmdName),
          call)

  dispatch.add nnkElse.newTree(
    quote do:
      raise newLogicError(
        "Subcommand name '" & `cmdName` &
          "' not handled by auto-generated handlers."))

  ## Dispatch one or more procedures based on subcommand arguments
  result = quote do:
    let `cmd` = `app`.getCmd()
    `dispatch`


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
    of cctOpt, cctArgument:
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

template newCliError*(
    inMsg: string, desc: CliDesc, inKind: CliErrorKind): CliError =
  CliError(
    msg: inMsg,
    location: instantiationInfo(fullpaths = true),
    desc: desc,
    isDesc: true,
    kind: inKind)

proc parseArg(
    lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =
  if lexer[].kind == coArgument:
    result = CliCmdTree(
      kind: cctArgument, head: lexer.pop(), desc: desc)

  else:
    errors.addOrRaise newCliError(
      &"Failed to parse '{desc.name}' - expected argument, but found '{lexer[]}'",
      desc, lexer.pop(), cekFailedParse)

proc matches(opt: CliOpt, check: CliCheck): bool = true

func findRepeatRange(check: CLiCheck): Option[Slice[int]] =
  case check.kind:
    of cckCheckRepeat:
      result = some(check.allowedRepeat)

    of cckAndCheck:
      for sub in check.subChecks:
        result = findRepeatRange(sub)
        if result.isSome():
          return

    else:
      return

proc popArgument(lexer: var HsLexer[CliOpt], desc: CLiDesc): CliCmdTree =
  CliCmdTree(
    desc: desc,
    mainIdx: lexer.pos,
    head: lexer.pop(),
    kind: cctOpt
  )

proc parseOptOrFlag(
    lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =

  assertRef desc
  if lexer[].key in desc.altNames:
    var cnt = 1
    let repeat = desc.check.findRepeatRange()
    result = CliCmdTree(
      kind: tern(
        lexer[coOptionKinds] or
        (repeat.isSome() and 0 < repeat.get().b) or
        desc of coOpt,
        cctOpt,
        cctFlag),
      head: lexer.pop(), desc: desc)

    if ?lexer:
      if result.head.needsValue():
        # Handle separate `--opt value` case based on the head token kind
        result.args.add lexer.popArgument(desc)

      elif desc of coOpt and result.head.canAddValue():
        # Identical check, but based on the CLI description
        result.head.valStr = lexer[].value
        lexer.next()

      if repeat.isSome():
        var idx = 0
        while idx < repeat.get().b and
              ?lexer and
              lexer[{coCommand, coArgument}]:

          inc idx
          result.args.add lexer.popArgument(desc)

        # TODO error message if some items are missing from range



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


proc groupRepeated(trees: seq[CliCmdTree]): seq[CliCmdTree] =
  var table: Table[string, seq[CliCmdTree]]
  for tree in trees:
    table.mgetOrPut(tree.desc.name, @[]).add tree

  for key, group in table:
    if group.len == 1:
      result.add group[0]

    else:
      result.add CliCmdTree(
        desc: group[0].desc,
        head: group[0].head,
        kind: cctGrouped,
        entries: group)

proc parseCmd(lexer: var HsLexer[CliOpt], desc: CliDesc, errors): CliCmdTree =
  var
    argIdx: int
    argParsed: seq[int]

  if lexer[].kind in {coArgument, coCommand} and lexer[].key in desc.altNames:
    result = CliCmdTree(
      kind: cctCommand, head: lexer.pop(), desc: desc)
    argParsed.add 1
    var arguments: seq[CliCmdTree]

    let err = errors.len
    while ?lexer:
      assertRef desc
      arguments.add parseCli(lexer, desc, errors, argIdx, argParsed)

    if errors.len > err:
      for arg in arguments:
        result.add arg

    else:
      for arg in arguments.groupRepeated():
        result.add arg

    let totalParsed = argParsed.sumIt(it)
    var minRequired =
      if desc.subcommands.len > 0:
        1

      else:
        # TODO check for minimum number of required positional arguments
        0

    if totalParsed < minRequired:
      errors.addOrRaise newCliError(&[
        &"Failed to parse '{desc.name}' - missing one or more required subcommands or ",
        "positional arguments",
        $joinAnyOf(
          desc.subcommands.mapIt(it.name),
          prefix = ". Required subcommands - ",
          suffix = ". ",
          empty = ""),
        $joinAnyOf(
          desc.arguments.mapIt(it.name),
          prefix = ". Required positional arguments - ",
          suffix = ". ",
          empty = ""),
      ], desc, cekMissingEntry)

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
              &"No matching option. Got '{lexer[]} ({lexer[].kind})', " &
                "but expected " &
                $desc.options.mapIt(it.altNames).concat().joinWords(
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





proc structureSplit*(
  opts: seq[CliOpt], desc: CliDesc, errors): CliCmdTree =
  ## Convert unstructured sequence of CLI commands/options into structured
  ## unchecked tree.
  assertRef desc

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


proc splitSeqVal*(tree: CliCmdTree): CliCmdTree =
  # echo tree.treeRepr()
  # echo tree.head.lispRepr()
  if ':' in tree.head.valStr:
    result = CliCmdTree(kind: cctGrouped, desc: tree.desc, head: tree.head)
    let split = tree.head.valStr.split(':')
    for part in split:
      result.add tree.withIt do:
        it.head.valStr = part

  # echo treeREpr(result)

proc checkedConvert(
    tree: CliCmdTree, check: CliCheck,
    errors; prevValue: CliValue = nil,
    isSingle: bool = true
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

  # echov cast[int](check)
  # echov str
  # echov tree.head
  # echov check.kind

  case check.kind:
    of cckAcceptAll, cckNoCheck:
      result = CliValue(
        rawValue: str,
        kind: cvkString, strVal: str, desc: tree.desc)

    of cckIsIntValue:
      try:
        if isSingle:
          result = toCliValue(lexcast[int](str), desc = tree.desc)

        else:
          result = toCliValue(lexcast[seq[int]](str), desc = tree.desc)

      except LexcastError as ex:
        errors.addOrRaise newCliError(
          cekCheckFailure,
          &"Could not parse '{hshow(str)}' as integer value")

    of cckIsBoolValue:
      if str.len == 0 and tree.kind == cctFlag:
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

      errors.addOrRaise newCliError(
        cekCheckFailure,
        &"Value '{str}' is not in set of expected values")

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
      # echo "Checking for 'and'"
      let repeat = check.findRepeatRange()
      let isSingle = repeat.isNone() or repeat.get() == 1 .. 1
      let errCount = errors.len()

      if not isSingle and (
        tree.kind == cctGrouped or
        (tree.kind == cctOpt and tree.args.len > 0)
      ):
        result = CliValue(kind: cvkSeq, desc: tree.desc)
        if tree.len notin repeat.get():
          errors.addOrRaise newCliError(
            cekCheckFailure,
            "Incorrect number of repetitions for ",
            tree.desc.name,
            &"expected count in range {check.allowedRepeat}, but got {tree.len}")

          return nil

        else:
          for value in tree:
            var start: CLiValue = nil
            for sub in check.subChecks:
              if check.kind notin {cckCheckRepeat}:
                start = checkedConvert(value, sub, errors, start)
                if errors.len() > errCount:
                  return nil

            result.seqVal.add start

      else:
        for sub in check.subChecks:
          if isSingle or sub.kind != cckCheckRepeat:
            result = checkedConvert(
              tree, sub, errors, result, isSingle = isSingle)

            if errors.len() > errCount: return nil

          else:
            discard # ??? QUESTION

            # pprintObjectTree tree
            # raise newImplementError()

        if not isSingle and result.kind != cvkSeq:
          result = CliValue(kind: cvkSeq, seqVal: @[result], desc: tree.desc)

    of cckAndPosCheck:
      # TODO provide full error information about which element (at which
      # positionl failed)
      result = CliValue(kind: cvkSeq, desc: tree.desc)
      if tree.kind == cctGrouped:
        for (check, entry) in zip(check.subChecks, tree.entries):
          result.seqVal.add checkedConvert(entry, check, errors)

      else:
        let tree = tree.splitSeqVal()
        for (check, entry) in zip(check.subChecks, tree.entries):
          result.seqVal.add checkedConvert(entry, check, errors)

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

    of cckCheckRepeat:
      if tree.len notin check.allowedRepeat:
        errors.addOrRaise newCliError(
          cekCheckFailure,
          "Incorrect number of repetitions for ",
          tree.desc.name,
          &"expected count in range {check.allowedRepeat}, but got {tree.len}")

      else:
        result = prevValue

    else:
      raise newImplementKindError(check)

proc toCliValue*(tree: CliCmdTree, errors): CliValue =
  ## Convert unchecked CLI tree into typed values, without executing
  ## checkers.
  case tree.kind:
    of cctCommand:
      var converted: HashSet[string]
      result = CliValue(
        rawValue: tree.head.value,
        desc: tree.desc, kind: cvkCommand)

      for sub in tree:
        case tern(sub.kind != cctGrouped, sub.kind, sub.entries[0].kind):
          of cctArgument:
            result.positional.add toCliValue(sub, errors)
            converted.incl sub.desc.name

          of cctDashedKinds:
            result.options[sub.head.key] = toCliValue(sub, errors)
            converted.incl sub.desc.name

          of cctCommand:
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



    of cctArgument:
      result = checkedConvert(tree, tree.desc.check, errors)

    of cctGrouped:
      result = checkedConvert(tree, tree.desc.check, errors)

    of cctFlag, cctOpt:
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

    # else:
    #   raise newImplementKindError(tree)

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


  # let rep = desc.check.allowedRepeat
  # if rep.b > 1:
  #   result.add T[&", can be repeated {desc.check.allowedRepeat} times"]

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

    of cckCheckRepeat:
      result = T[
        prefix & &"be used from {check.allowedRepeat.a} to " &
          tern(
            check.allowedRepeat.b == high(int),
            "unlimited",
            $check.allowedRepeat.b
          ) & " times"]

    of cckAndPosCheck:
      result = V[]
      for idx, sub in check.subChecks:
        result.add H[T[&"{idx:<2}"], checkHelp(sub, false)]

      result = V[
        T[&"{prefix}be a tuple of {check.subChecks.len} colon-separated elements"],
        I[4, result]
      ]

    else:
      raise newImplementKindError(check)

proc flagHelp(flag: CliDesc, level: CliShowLevel = cslDefault): LytBlock =
  result = V[]
  var flags: seq[ColoredText]
  for key in flag.altNames:
    if key.len > 1:
      flags.add toGreen("--" & key)

    else:
      flags.add toGreen("-" & key)

  result.add H[T[flags.join(", ")], postLine(flag)]
  result.add I[4, T[flag.doc.docBrief]]
  result.add I[4, checkHelp(flag.check)]

proc optHelp(opt: CliDesc, level: CliShowLevel = cslDefault): LytBlock =
  result = V[]
  var opts: seq[ColoredText]
  for key in opt.altNames:
    if key.len > 1:
      opts.add toGreen("--" & key)

    else:
      opts.add toGreen("-" & key)

  result.add H[T[opts.join(", ")], postLine(opt)]
  if level == cslCommonAlreadyShown:
    result.add I[4, T["(common option, see earlier)"]]

  else:
    result.add I[4, T[opt.doc.docBrief]]

  result.add I[4, checkHelp(opt.check)]


proc argHelp(arg: CliDesc, level: CliShowLevel = cslDefault): LytBlock =
  result = V[]

  result.add T["<" & toColored(arg.name, fgRed) & ">"]
  result.add I[4, T[arg.doc.docBrief]]
  result.add I[4, checkHelp(arg.check)]

proc help(
    desc: CliDesc,
    ignored: HashSet[string],
    commonCount: var Table[string, int],
    level: int = 0
  ): LytBlock =

  result = V[]
  var first = S[]
  let indent = level * 4
  assertRef desc
  if desc.arguments.len > 0:
    var args = V[S[], T[initColored("ARGS:", section)], S[]]

    for arg in desc.arguments:
      if arg.name notin ignored:
        if arg.name notin commonCount or commonCount[arg.name] < 1:
          args.add I[4, argHelp(arg)]
          if arg.name in commonCount:
            inc commonCount[arg.name]

        else:
          args.add I[4, argHelp(arg, cslCommonAlreadyShown)]

    result.add I[indent, args]

  if desc.subcommands.len > 0:
    var cmds = V[S[], T[initColored("SUBCOMMANDS:", section)], S[]]

    var commonArguments: Table[string, int]
    for cmd in desc.subcommands:
      for common in cmd.commonArguments:
        commonArguments[common] = 0

    for cmd in desc.subcommands:
      if cmd.name notin ignored:
        let names =
          cmd.altNames.mapIt(toColored(it, fgMagenta)).join(toColored("/")) &
            " - " & cmd.doc.docBrief

        cmds.add I[4, T[names]]
        cmds.add I[4, help(cmd, ignored, commonArguments, level + 1)]

    result.add I[4, cmds]

  if desc.options.len > 0 and
     desc.options.anyIt(it.groupKind in coFlagKinds):
    var flags = V[S[], T[toColored("FLAGS:", section)], S[]]

    for flag in desc.options:
      if flag.groupKind in coFlagKinds and flag.name notin ignored:

        if flag.name notin commonCount or commonCount[flag.name] < 1:
          flags.add I[4, flagHelp(flag)]
          if flag.name in commonCount:
            inc commonCount[flag.name]

        else:
          flags.add I[4, flagHelp(flag, cslCommonAlreadyShown)]

    if flags.len > 3:
      result.add I[indent, flags]

  if desc.options.len > 0 and
     desc.options.anyIt(it.groupKind in coOptionKinds):
    var opts = V[S[], T[toColored("OPTIONS:", section)], S[]]

    for opt in desc.options:
      if opt.groupKind in coOptionKinds and
         opt.name notin ignored:

        if opt.name notin commonCount or commonCount[opt.name] < 1:
          opts.add I[4, optHelp(opt)]
          if opt.name in commonCount:
            inc commonCount[opt.name]

        else:
          opts.add I[4, optHelp(opt, cslCommonAlreadyShown)]

    result.add I[indent, opts]



proc help(app: CliApp, ignored: HashSet[string]): LytBlock =
  assertRef app.root
  var res = V[
    T[toColored("+", section)],
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

  var commonCount: Table[string, int]
  res.add app.root.help(ignored, commonCount)

  # res.add @[
  #   S[], T["VERSION:".toColored(section)],
  #   S[], I[4, T[&"{app.version} by {app.author}"]]
  # ]

  result = res



proc helpStr*(
    app: CliApp,
    ignored: seq[string] = @[],
    color: bool = true
  ): string =

  return app.help(toHashSet(ignored)).toString().toString(color)

proc help(err: CliError): LytBlock =
  var errmsg: string

  if '\n' notin err.msg:
    errmsg &= wrapOrgLines(err.msg, 80).join("\n")

  else:
    errmsg &= err.msg

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

    else:
      result = T[errmsg]

proc helpStr*(err: CliError): string =
  return err.help().toString().toString()


proc showErrors*(app: CliApp, logger: HLogger) =
  ## Show accumulated errors using `logger`
  for err in app.errors:
    logger.err err.helpStr()
    logger.debug toLink(
      err.location,
      AbsFile(err.location[0]).name() & ":" & $err.location[1])



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
