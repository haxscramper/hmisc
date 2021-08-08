import
  ../algo/[hstring_algo, clformat],
  ../core/all,
  ../macros/introspection,
  ./hjson

import std/[options, strutils, strscans, strformat, macros]

type
  CliOptKind* = enum
    coFlag ## `--flag`
    coDotFlag ## `--help.json`
    coBracketFlag ## `--warn[Relocate]`

    coOpt ## `--opt:value`
    coDotOpt ## `--clang.exe:/bin/clang10`
    coBracketOpt ## `--warn[Noinit]:true`

    coArgument
    coSpecial

    coCommand

const
  coFlagKinds* = { coFlag .. coBracketFlag }
  coOptionKinds* = { coOpt .. coBracketOpt }
  coBracketKinds* = { coBracketFlag, coBracketOpt }
  coDashedKinds* = coFlagKinds + coOptionKinds

type
  CliAddKind* = enum
    caEqual ## `--key=value`
    caPlusEqual ## `--key+=value`
    caColon ## `--key:value`
    caMinusEqual ## `--key-=value`
    caNoSep ## `-kvalue`
    caCarentEqual ## `--key^=value`
    caEqualNone ## `--key=`

  CliSpecialKind* = enum
    cskVerbatimNext
    cskStdinAlias

  CliOpt* = object
    globalPos*: int ## Absolute position in the input sequence of arguments
    rawStr*: string ## Raw string value of the command (with all dashes etc)
    kind*: CliOptKind
    shortDash*: bool
    keyPath*: seq[string] ## Full path to the key. `--cc.exe=json` has path
                          ## `["cc", "exe"]`, regular `--help` has path
                          ## `["help"]`
    keySelect*: string ## Selector value used in bracket options -
                       ## `--warn[Init]:on` has `keySelector = "Init"`
    valStr*: string ## String value of the command
    addKind*: CliAddKind
    optKind*: CliOptKind
    specialKind*: CliSpecialKind

  CliErrPolicy* = enum
    ceColorDiagnostics
    ceJsonDiagnostics

  CliParseConfig* = object
    shortOpts*: set[char]
    specialStart*: seq[string]
    blockedAddKinds*: set[CliAddKind]
    errPolicy*: set[CliErrPolicy]
    hasValue*: proc(arg: CliOpt): bool {.noSideEffect.}
    seqSeparator*: string

  CliFailKind* = enum
    cfNoSuchOption
    cfNoSuchSelector
    cfBadCliSyntax
    cfBadCliValue
    cfMissingValue
    cfMultiFail

  CliFail* = object
    argStr*: string
    case kind*: CliFailKind
      of cfMultiFail:
        subFails*: seq[CliFail]

      of cfNoSuchSelector:
        selector*: string

      of cfNoSuchOption:
        option*: string

      else:
        discard

    jsonMsg*: JsonNode
    strMsg*: string

func key*(opt: CliOpt): string =
  if opt.kind in {coArgument, coCommand}:
    opt.valStr

  else:
    assert opt.keyPath.len > 0, $opt.kind
    opt.keyPath[0]

func value*(opt: CliOpt): string = opt.valStr
func needsValue*(opt: CliOpt): bool =
  opt.kind in coOptionKinds and opt.valStr == ""

func canAddValue*(opt: CliOpt): bool =
  opt.valStr == ""

macro scanpFull*(str: typed, start: int, pattern: varargs[untyped]): untyped =
  result = nnkStmtList.newTree()
  let tmp = genSym(nskVar, "tmp")
  result.add newVarStmt(tmp, start)
  result.add newCall("scanp", str, tmp)
  for patt in pattern:
    result[^1].add patt

  result = nnkInfix.newTree(
    ident("and"),
    nnkStmtListExpr.newTree(result[0], result[1]),
    nnkInfix.newTree(ident("=="), tmp, newCall("len", str))
  )


macro scanpFull*(str: typed, pattern: varargs[untyped]): untyped =
  result = nnkStmtList.newTree()
  let tmp = genSym(nskVar, "tmp")
  result.add newVarStmt(tmp, newLit(0))
  result.add newCall("scanpFull", str, tmp)
  for patt in pattern:
    result[^1].add patt


const CliIdent = IdentChars + {'-'}

func classifyCliArg*(arg: string, config: CliParseConfig): CliOptKind =
  var start = 0
  if scanp(arg, start, '-'{1, 2}):
    if arg in ["--", "-"]:
      result = coSpecial

    elif start == 1 and arg[start] in config.shortOpts:
      if start == arg.high:
        result = coFlag

      else:
        result = coOpt

    elif scanpFull(arg, start, +`CliIdent`):
      result = coFlag

    elif scanpFull(arg, start, +`CliIdent` ^+ '.'):
      result = coDotFlag

    elif scanpFull(arg, start, +`CliIdent` ^* '.', '[', +`CliIdent`, ']'):
      result = coBracketFlag

    elif scanpFull(arg, start, +`CliIdent`, {':', '='}, +`AllChars`):
      result = coOpt

    elif scanpFull(arg, start, +`CliIdent` ^+ '.', {':', '='}, +`AllChars`):
      result = coDotOpt

    elif scanpFull(arg, start,
                   +`CliIdent` ^* '.', '[', +`CliIdent`, ']',
                   {':', '='}, +`AllChars`
    ):
      result = coBracketOpt

    else:
      debugecho "fail", start, " -> ", arg[start .. ^1]

  else:
    result = coArgument

func splitCliArgs*(args: seq[string], config: CliParseConfig): seq[string] =
  for arg in args:
    if arg["--"] or not arg["-"]:
      result.add arg

    elif arg in ["--", "-"]:
      result.add arg

    else:
      var pos: int = 1
      while arg[pos] in config.shortOpts:
        result.add &"-{arg[pos]}"
        inc pos

      result[^1] &= arg[pos .. ^1]

func splitFlag*(arg: string, config: CliParseConfig): tuple[
    keyPath: seq[string], keySelector: string, value: string, dashes: string
  ] =

  ##[

- TODO :: Parse gcc/clang-style flags with selectors `-Wno-relocate`

]##

  var
    pos = 0
    prefix: string

  discard scanp(arg, pos, '-'{1, 2} -> result.dashes.add($_))
  discard scanp(arg, pos, (+`CliIdent` ^* '.') -> prefix.add($_))
  result.keyPath = split(prefix, ".")

  if result.dashes.len == 1 and prefix[0] in config.shortOpts:
    result.value = prefix[1 .. ^1]
    result.keyPath = @[$prefix[0]]

  else:
    discard scanp(arg, pos, '[', +`CliIdent` -> result.keySelector.add($_), ']')
    discard scanp(arg, pos, {':', '='})
    if pos < arg.len:
      discard scanp(arg, pos, +`AllChars` -> result.value.add($_))

  result.value = result.value.strip(chars = {'\''})

func parseFlag*(arg: string, config: CliParseConfig): CliOpt =
  let (path, selector, _, dashes) = splitFlag(arg, config)
  result = CliOpt(keyPath: path, shortDash: dashes.len == 1, rawStr: arg)

  if selector.len == 0:
    if path.len == 1:
      result.kind = coFlag

    else:
      result.kind = coDotFlag

  else:
    result.kind = coBracketFlag
    result.keySelect = selector



func parseOpt*(arg: string, config: CliParseConfig): CliOpt =
  result = parseFlag(arg, config)
  let (_, _, value, _) = splitFlag(arg, config)
  case result.kind:
    of coFlag: result.kind = coOpt
    of coDotFlag: result.kind = coDotOpt
    of coBracketFlag: result.kind = coBracketOpt
    else: discard

  result.valStr = value

func parseArgument*(arg: string, config: CliParseConfig): CliOpt =
  result = CliOpt(kind: coArgument, rawStr: arg, valStr: arg)

func parseSpecial*(arg: string, config: CliParseConfig): CliOpt =
  result = CliOpt(kind: coSpecial, rawStr: arg)
  case arg:
    of "--": result.specialKind = cskVerbatimNext
    of "-": result.specialKind = cskStdinAlias
    else:
     raise newUnexpectedKindError(arg)

func parseCommand*(arg: string, config: CliParseConfig): CliOpt =
  result = CliOpt(kind: coCommand, rawStr: arg, keyPath: @[arg])

func parseCliOpts*(
    args: seq[string],
    config: CliParseConfig = CliParseConfig()
  ): tuple[parsed: seq[CliOpt], failed: seq[CliFail]] =

  let args = splitCliArgs(args, config)
  var pos: int = 0
  while pos < args.len:
    case classifyCliArg(args[pos], config):
      of coFlag, coDotFlag, coBracketFlag:
        var flag = parseFlag(args[pos], config)
        if not isNil(config.hasValue) and config.hasValue(flag):
          # FIXME classify next value, switch flag
          flag.valStr = args[pos + 1]
          inc pos

        result.parsed.add flag

      of coOpt, coDotOpt, coBracketOpt:
        result.parsed.add parseOpt(args[pos], config)

      of coArgument:
        result.parsed.add parseArgument(args[pos], config)

      of coSpecial:
        result.parsed.add parseSpecial(args[pos], config)

      of coCommand:
        result.parsed.add parseCommand(args[pos], config)

    inc pos

func cliParse*(
  arg: string, res: var int, config: CliParseConfig): Option[CliFail] =

  try:
    res = parseInt(arg)

  except ValueError as e:
    result = some CliFail(strMsg: e.msg, kind: cfBadCliValue)

func cliParse*(
  arg: string, res: var float, config: CliParseConfig): Option[CliFail] =

  try:
    res = parseFloat(arg)

  except ValueError as e:
    result = some CliFail(strMsg: e.msg, kind: cfBadCliValue)

func cliParse*(
  arg: string, res: var string, config: CliParseConfig): Option[CliFail] =
  res = arg

func cliParse*[T](
  arg: string, res: var seq[T], config: CliParseConfig): Option[CliFail] =

  let args = arg.split(config.seqSeparator)
  var buf: seq[CliFail]
  for arg in args:
    var tmp: T
    let status = cliParse(arg, tmp, config)
    if status.isSome():
      buf.add status.get()

    else:
      res.add tmp

  if buf.len > 0:
    result = some CliFail(kind: cfMultiFail, subFails: buf)

func cliParse*[En: enum](
  arg: string, res: var En, config: CliParseConfig): Option[CliFail] =

  const map = enumNamesTable(En)
  let arg = arg
  var found = false

  block mainSearch:
    for (names, val) in map:
      for name in names:
        if name == arg:
          res = val
          found = true
          break mainSearch


  if not found:
    var allnames: seq[string]
    for (names, _) in map:
      allnames &= names

    result = some CliFail(
      selector: arg,
      kind: cfNoSuchSelector,
      strMsg: stringMismatchMessage(arg, allnames)
    )

const defaulCliParseConfig* = CliParseConfig(
  seqSeparator: ","
)

func lispRepr*(cli: CliOpt): ColoredText =
  coloredResult()
  add "("
  add "k:"
  add hshow(cli.kind)

  if cli.kind in coDashedKinds:
    add " " & cli.keyPath.join(".")

  if cli.kind in coBracketKinds:
    add &" [{cli.keySelect}]"

  if cli.kind in coOptionKinds:
    add " "
    add hshow(cli.addKind)

  if cli.valStr.len > 0:
    add " "
    add cli.valStr

  add ")"

func `$`*(cli: CliOpt): string =
  case cli.kind:
    of coDashedKinds:
      if cli.keyPath.len == 1 and
         cli.keyPath[0].len == 1:
        result = "-"

      else:
        result = "--"

      result &= cli.keyPath.join(".")

    else:
      discard
      # result &= cli.value

  if cli.kind in coBracketKinds:
    result &= &"[{cli.keySelect}]"

  if cli.kind in coOptionKinds:
    case cli.addKind:
      of caEqual: result &= "="
      of caPlusEqual: result &= "+="
      of caColon: result &= ":"
      of caMinusEqual: result &= "-="
      of caNoSep: result &= ""
      of caCarentEqual: result &= "^="
      of caEqualNone: result &= "="

  if cli.valStr.len > 0:
    result &= cli.valStr
