import ../algo/hstring_algo
import ../hexceptions
import std/[options, strutils]
import ../macros/introspection
import hjson

type
  CliOptKind = enum
    coFlag ## `--flag`
    coDotFlag ## `--help.json`
    coBracketFlag ## `--warn[Relocate]`

    coOpt ## `--opt:value`
    coDotOpt ## `--clang.exe:/bin/clang10`
    coBracketOpt ## `--warn[Noinit]:true`

    coArgument
    coSpecial

  CliAddKind = enum
    caEqual ## `--key=value`
    caPlusEqual ## `--key+=value`
    caColon ## `--key:value`
    caMinusEqual ## `--key-=value`
    caNoSep ## `-kvalue`
    caCarentEqual ## `--key^=value`
    caEqualNone ## `--key=`

  CliOpt = object
    keyPath: seq[string]
    keySelect: string
    valStr: string
    addKind*: CliAddKind
    optKind*: CliOptKind

  CliErrPolicy = enum
    ceColorDiagnostics
    ceJsonDiagnostics

  CliParseConfig = object
    shortOpts*: set[char]
    specialStart*: seq[string]
    blockedAddKinds*: set[CliAddKind]
    errPolicy*: set[CliErrPolicy]
    hasValue*: proc(arg: CliOpt): bool {.noSideEffect.}
    seqSeparator*: string

  CliFailKind = enum
    cfNoSuchOption
    cfBadCliSyntax
    cfBadCliValue
    cfMissingValue

  CliFail = object
    argStr*: string
    kind*: CliFailKind
    jsonMsg*: JsonNode
    strMsg*: string


func classifyCliArg*(arg: string, config: CliParseConfig): CliOptKind =
  if arg["--"]:
    discard

func splitCliArgs*(args: seq[string], config: CliParseConfig): seq[string] =
  discard

func parseFlag*(arg: string, config: CliParseConfig): CliOpt =
  discard

func parseOpt*(arg: string, config: CliParseConfig): CliOpt =
  discard

func parseArgument*(arg: string, config: CliParseConfig): CliOpt =
  discard

func parseSpecial*(arg: string, config: CliParseConfig): CliOpt =
  discard

func parseCliOpts*(args: seq[string], config: CliParseConfig): tuple[
  parsed: seq[CliOpt], failed: seq[CliFail]
] =

  let args = splitCliArgs(args, config)
  var pos: int = 0
  while pos < args.len:
    case classifyCliArg(args[pos], config):
      of coFlag, coDotFlag, coBracketFlag:
        var flag = parseFlag(args[pos], config)
        if config.hasValue(flag):
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
      strMsg: stringMismatchMessage(arg, allnames)
    )

when isMainModule:
  type
    Test = enum
      tNone
      tFirst = "ovewrite"
      tSecond = "overwrite2"

  var res: Test
  let err = cliParse("First", res, CliParseConfig())
  echo res
  # echo err.get().strMsg
