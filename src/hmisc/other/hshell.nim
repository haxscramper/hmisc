import parseutils

when not defined(NimScript):
  import std/[osproc, streams]
  export ProcessOption

else:
  type
    ProcessOption* = enum
      poEchoCmd
      poUsePath
      poEvalCommand
      poStdErrToStdOut
      poParentStreams
      poInteractive
      poDaemon

import oswrap
import std/[strutils, strformat, sequtils, options,
            deques, json, macros, times]

import
  ../core/[all, code_errors],
  ../algo/hlex_base,
  ../types/colorstring

from std/os import quoteShell

const hasStrtabs = cbackend or (NimMajor, NimMinor, NimPatch) > (1, 2, 6)

when hasStrtabs: # https://github.com/nim-lang/Nim/pull/15172
  import std/strtabs

# import ../algo/halgorithm

## - TODO :: better way of building command for execution.
##
## - TODO :: overload for `runShell` that accepts callbacks failed execution.
##
## - TODO :: generate log calls?
##
## - TODO :: easy way to pipe things to stdout
##
## - TODO :: pretty-print long shell commands on failure - can split on `&&`
##   and left-align commands. Highlight `--flags`, `commands` and arguments.
##
## - TODO :: option to force colored output when shell runner
##
## - TODO :: implement functions for callsite checks in program execution
##   Determine if all file parameters are present (create separate `fileArg`
##   procedure), if binary itself is available and so on.
##
## - TODO :: Support command chaining using `&&`, `||` (`and`, `or`) and
##   pipes `|` for redirecting output streams.
##
## - TODO :: move command-line flags collections into separate type to use
##   for working with external libraries that accept list of command-line
##   flags (like libclang for example)
##
## - TODO :: Add 'subshell' command type - for passing strings that are
##   shell expression themselves. Correct quoting etc.
#
## - TODO :: write wrapper for a subset posix-compilant shell - this is
##   useful in cases where you only have very limited access to different
##   installation - like in docker container, over ssh or similar. Second
##   use case: `git rev-list`, `sh -c` and the like. Command that accepts
##   another shell command. You can only send a single string that should
##   contain all necessary commands. In that case it would be very
##   convinient to have builder for such strings - for example in expression
##   like `if [[ (pwd) == "/" ]]` - I'm not even sure I got it right in the
##   first place, and I don't want to remember all details about how shell
##   `if [[ ]]` works too. And I can also analyze all shell expression on
##   the application side - detect missing commands, infer needed
##   dependencies, side effects (writes to file etc.)
##
## - TODO :: Interacting with running process via stdin/stdout. REPL-like
##   processes. Can test on `/bin/sh`
##
## - TODO :: Make it possible to implement own asciinema based on `hshell`.
##   When starting program all necessary controls for process should be
##   exposed, and your application must be able to pretend it is a
##   full-blown terminal emulator.
##
## - IDEA :: Provide `strace`-to-`json` converter.
##
## - IDEA :: raw shell string validation. While it is certainly not simple
##   to parse arbitrary bash code, `80%` of things that are passed to
##   command execution are just `cmd1 && cmd2` and so on. Quite easy to
##   parse.
## - TODO :: somehow enable to read from both output streams at once for
##   long-running shell record iterators.

export ShellVar, ShellExpr

type
  ShellExecEffect = object of IOEffect
  ShellError* = ref object of OSError
    wasLogged*: bool
    cmd*: string ## Command that returned non-zero exit code
    cwd*: AbsDir ## Absolute path of initial command execution directory
    retcode*: int ## Exit code
    # TODO REFACTOR rename to `stdout` and `stderr`
    errstr*: string ## Stderr for command
    outstr*: string ## Stdout for command

  ShellExecResult* = object
    ## Shell command execution result - standard error, output and return
    ## code.
    stdout*: string
    stderr*: string
    code*: int

  ShellResult* = object
    # - REFACTOR :: move `execResult` into `true` branch for `resultOk`
    wasTerminated*: bool
    execResult*: ShellExecResult ## Result of command execution
    hasBeenSet*: bool ## Whether or not result been set
    cmd*: ShellCmd
    case resultOk*: bool ## No failures (return code == 0)
      of true:
        nil

      of false:
        exception*: ShellError ## Addiional information in case of failure. Can
                               ## be raised or immediately inspected on return.

  ShellCmdFlagConf* = enum
    ## Shell command flags syntax
    ccRegularFlags ## `-f` or `--flag`
    ccOneDashFlags ## `-f` or `-flag`

  ShellCmdConf* = object
    flagConf*: ShellCmdFlagConf
    kvSep*: string

  ShellCmdPartKind* = enum
    cpkSubCmd ## Subcommand string
    cpkArgument ## String argument to command
    cpkOption ## Key-value pair
    cpkFlag ## Boolean flag (on/off)
    cpkRaw ## Raw parameter
    cpkSubExpr ## Another shell subexpression,
    ## for things like `sh -c "some code"`

  ShellCmdPart* = object
    case kind*: ShellCmdPartKind
      of cpkSubCmd:
        subcommand*: string
      of cpkArgument:
        argument*: string
      of cpkFlag:
        flag*: string
      of cpkOption:
        key*: string
        val*: string

        case overrideKv*: bool ## Override key-value separator for
          ## configuration. Used in cases like `-I` flag in C
          ## compilers that othewise handle `--key=value` pairs.
          of true:
            kvSep*: string
          of false:
            discard

      of cpkRaw:
        rawstring*: string
      of cpkSubExpr:
        expr*: ShellAst

  ShellCmd* = object
    bin: string
    opts: seq[ShellCmdPart]
    conf*: ShellCmdConf
    envVals: seq[tuple[key, val: string]]


  ShellGlob* = distinct string
  ShellAstKind* = enum
    # I don't want to use `{.requiresinit.}` on the shell ast, so 'empty'
    # kind has been added only to have somewhat meaningful default value.
    # It is not really used anywhere and most procs just ignore it. For
    # example concatenating `Empty() && StrLit()` should result in just
    # `StrLit()`. And `sakEmpty` is considered invalid for `AST->string`
    # conversion because it should've been removed in earlier operations.
    sakEmpty

    sakCmd
    sakVar
    sakGlob
    sakWord
    sakArithmExpr
    sakRawExpr
    sakStrLit


    sakWhile
    sakIf
    sakCase
    sakAsgn
    sakFor
    sakMath

    sakOrList ## ||
    sakAndList ## &&
    sakSequentialList ## ;
    sakAsyncList ## &
    sakPipeList ## |

    sakStmtList
    sakSubcommand

    sakStdoutOverwrite ## >
    sakStdoutAppend ## >>
    sakStdin ## <

  ShellAst* = object
    case kind*: ShellAstKind
      of sakRawExpr:
        rawExpr*: ShellExpr
      of sakCmd:
        cmd*: ShellCmd
      of sakVar:
        shVar*: ShellVar
      of sakGlob:
        pattern*: ShellGlob
      of sakWord, sakStrLit:
        strVal*: string
      of sakMath:
        mathOp*: string
        mathArgs*: seq[ShellAst]
      else:
        exportVar*: bool
        subnodes*: seq[ShellAst]

  ShellSomething = ShellAst | ShellCmd | ShellExpr | ShellVar
  ShellMathExpr = ShellAst | ShellVar | int



const
  GnuShellCmdConf* = ShellCmdConf(
    flagConf: ccRegularFlags,
    kvSep: "="
  )

  SpaceShellCmdConf* = ShellCmdConf(
    flagConf: ccRegularFlags,
    kvSep: " "
  )

  NimShellCmdConf* = ShellCmdConf(
    flagConf: ccRegularFlags,
    kvSep: ":"
  )

  X11ShellCmdConf* = ShellCmdConf(
    flagConf: ccOneDashFlags,
    kvSep: " "
  )

  sakListKinds* = {sakAndList, sakOrList, sakPipeList}

func initShellCmdConf*(): ShellCmdConf = discard

func isOk*(
    shellRes: ShellResult,
    allowedCodes: seq[int] = @[]
  ): bool =

  shellRes.resultOk or shellRes.execResult.code in allowedCodes

func getStdout*(err: ShellError): string = err.outstr
func getStderr*(err: ShellError): string = err.errstr

func getStdout*(shellRes: ShellResult): string =
  shellRes.execResult.stdout

func getStderr*(shellRes: ShellResult): string =
  shellRes.execResult.stderr

func split*(res: ShellResult): tuple[stdout, stderr: string, code: int] =
  (res.execResult.stdout, res.execResult.stderr, res.execResult.code)

func `[]`*(sa: ShellAst, idx: int): ShellAst = sa.subnodes[idx]
func len*(sa: ShellAst): int = sa.subnodes.len
func toJson*(v: ShellGlob): JsonNode = newJString(v.string)
func bin*(v: ShellCmd): string = v.bin

converter toShellCmd*(a: ShellExpr): ShellCmd =
  ## Implicit conversion of string to command
  ##
  ## WARNING: `cmd` will be treated as `bin` and if `poEvalCommand` is
  ## used, execution of command will most likely fail at runtime.
  ##
  ## NOTE: `GnuShellCmdConf` is used
  result.conf = GnuShellCmdConf
  result.bin = a.string

func initCmdOption*(key, val: string): ShellCmdPart =
  ## Create shell command option
  ShellCmdPart(kind: cpkOption, key: key, val: val)

func initCmdFlag*(fl: string): ShellCmdPart =
  ## Create shell command flag
  ShellCmdPart(kind: cpkFlag, flag: fl)

proc initCmdEnvOrOption*(
  env: ShellVar,
  key, val: string, allowEmpty: bool = false): ShellCmdPart =
  ## Init shell command value for key `key` either from environment variable
  ## `env` or using provided fallback value `val`. `allowEmpty` - whether or not
  ## to use value if environment variable is empty (but exists).

  result = ShellCmdPart(kind: cpkOption, key: key)
  if existsEnv(env) and (getEnv(env).len > 0 or allowEmpty):
    result.val = getEnv(env)
  else:
    result.val = val


func isEmpty*(cmd: ShellCmd): bool =
  (cmd.bin.len == 0) and (cmd.opts.len == 0)

func flag*(cmd: var ShellCmd, fl: string) =
  ## Add flag for command
  cmd.opts.add ShellCmdPart(kind: cpkFlag, flag: fl)

func opt*[T](cmd: var ShellCmd, inKey: string, val: T) =
  ## Add option (key-value pairs) for command
  cmd.opts.add ShellCmdPart(
    kind: cpkOption, key: inKey, val: $val)

func env*(cmd: var ShellCmd, key, val: string): void =
  ## Add environment variable configuration for command
  cmd.envVals.add (key, val)

func opt*(cmd: var ShellCmd, opts: openarray[tuple[key, val: string]]) =
  ## Add sequence of key-value pairs
  for (key, val) in opts:
    cmd.opt(key, val)

func opt*(cmd: var ShellCmd, key, sep, val: string) =
  cmd.opts.add ShellCmdPart(
    kind: cpkOption, key: key, val: val, overrideKv: true,
    kvSep: sep)

func cmd*(cmd: var ShellCmd, sub: string) =
  ## Add subcommand
  cmd.opts.add ShellCmdPart(kind: cpkSubCmd, subcommand: sub)

func raw*(cmd: var ShellCmd, str: string) =
  ## Add raw string for command (for things like `+2` that are not
  ## covered by default options)
  cmd.opts.add ShellCmdpart(kind: cpkRaw, rawstring: str)

func expr*(cmd: var ShellCmd, subexpr: ShellExpr) =
  cmd.opts.add ShellCmdPart(
    kind: cpkSubExpr, expr: ShellAst(kind: sakRawExpr, rawExpr: subexpr)
  )

func expr*(cmd: var ShellCmd, subexpr: ShellCmd) =
  cmd.opts.add ShellCmdPart(
    kind: cpkSubExpr, expr: ShellAst(kind: sakCmd, cmd: subexpr)
  )

func expr*(cmd: var ShellCmd, expr: ShellAst) =
  cmd.opts.add ShellCmdPart(kind: cpkSubExpr, expr: expr)

func arg*(cmd: var ShellCmd, arg: string | AnyPath) =
  ## Add argument for command
  cmd.opts.add ShellCmdPart(
    kind: cpkArgument, argument: arg.getStr())

func arg*(cmd: var ShellCmd, arg: int) = cmd.arg($arg)

func add*(cmd: var ShellCmd, part: ShellCmdPart) =
  cmd.opts.add part

func `-`*(cmd: var ShellCmd, fl: string) =
  ## Add flag for command
  cmd.flag fl

func `-`*(cmd: var ShellCmd, fl: char) = cmd.flag($fl)

func `-`*(cmd: var ShellCmd, path: AnyPath) =
  ## Overload to add filesystem entry as command argument
  cmd - path.getStr()

func `-`*[Path: AnyPath](cmd: var ShellCmd, kv: (string, Path)) =
  cmd.opt(kv[0], kv[1].getStr())

func `-`*(cmd: var ShellCmd, kv: (string, string)) =
  ## Add key-value pair for command
  cmd.opt(kv[0], kv[1])

func `-`*(cmd: var ShellCmd, kv: tuple[key, sep, val: string]) =
  cmd.opt(kv.key, kv.sep, kv.val)

func makeShellCmd*(conf: ShellCmdConf, bin: string): ShellCmd =
  result.conf = conf
  result.bin = bin

const
  gnuShellCmdsList* = ["ls"]
  nimShellCmdsList* = ["nimble"]
  spaceShellCmdsList* = [
    "curl",
    "wget"
  ]
  x11ShellCmdsList* = [
    "xclip",
    "pdflatex",
    "java",
    "wmctrl",
    "sqlite3"
  ]

func makeShellCmd*(bin: string): ShellCmd =
  if bin in gnuShellCmdsList:
    result.conf = GnuShellCmdConf

  elif bin in nimShellCmdsList:
    result.conf = NimShellCmdConf

  elif bin in x11ShellCmdsList:
    result.conf = X11ShellCmdConf

  elif bin in spaceShellCmdsList:
    result.conf = SpaceShellCmdConf

  else:
    if bin.startsWith("x"):
      result.conf = X11ShellCmdConf

    else:
      result.conf = GnuShellCmdConf

  result.bin = bin

func makeShellCmd*(
  bin, prefix, sep: string): ShellCmd =
  result.conf = ShellCmdConf(kvSep: sep)
  if prefix == "--":
    result.conf.flagConf = ccRegularFlags

  else:
    result.conf.flagConf = ccOneDashFlags

  result.bin = bin

func makeNimShellCmd*(bin: string): ShellCmd =
  ## Create command for nim core tooling (":" for key-value separator)
  result.conf = NimShellCmdConf
  result.bin = bin

func makeX11ShellCmd*(bin: string): ShellCmd =
  ## Create command for `X11` cli tools (single dash)
  result.conf = X11ShellCmdConf
  result.bin = bin

func makeGnuShellCmd*(bin: string): ShellCmd =
  ## Create command for CLI applications that conform to GNU standard
  ## for command line interface `link
  ## <https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html>`_
  result.conf = GnuShellCmdConf
  result.bin = bin

func makeFileShellCmd*(
  file: string, conf: ShellCmdConf = GnuShellCmdConf): ShellCmd =
  result.conf = conf
  if file.startsWith("/"):
    result.bin = file
  else:
    result.bin = "./" & file

func makeFileShellCmd*(
  file: AnyFile, conf: ShellCmdConf = GnuShellCmdConf): ShellCmd =
  makeFileShellCmd(file.getStr(), conf)

func quoteShell*(str: string): string =
  if str == "]": "]" else: os.quoteShell(str)



func toStr*(inAst: ShellAst, oneline: bool = false): string

func toStr*(
    part: ShellCmdPart, conf: ShellCmdConf, colored: bool = false
  ): ColoredText=
  ## Convret shell command part to string representation
  let longPrefix =
    case conf.flagConf:
      of ccRegularFlags: "--"
      of ccOneDashFlags: "-"

  case part.kind:
    of cpkRaw:
      return toRed(part.rawstring, colored)

    of cpkSubCmd:
      return toBlue(part.subcommand, colored)

    of cpkFlag:
      if part.flag.len > 1:
        return longPrefix & toGreen(part.flag, colored)

      else:
        return "-" & toGreen(part.flag, colored)

    of cpkOption:
      let kv = if part.overrideKv: part.kvSep else: conf.kvSep
      if part.key.len > 1:
        return longPrefix & toGreen(part.key, colored) &
          kv & part.val.quoteShell()

      else:
        return "-" & toGreen(part.key, colored) & kv &
          part.val.quoteShell()

    of cpkArgument:
      return toYellow(part.argument.quoteShell(), colored)

    of cpkSubExpr:
      if part.expr.kind != sakVar:
        return clt(part.expr.toStr().quoteShell())

      else:
        return clt(part.expr.toStr())

func toStrSeq*(cmd: ShellCmd): seq[ColoredText] =
  result = @[ clt(cmd.bin) ]
  for op in cmd.opts:
    result &= op.toStr(cmd.conf)

func toStr*(cmd: ShellCmd): string = cmd.toStrSeq().join(" ")
func `$`*(cmd: ShellCmd): string = cmd.toStr()


macro precompute(expr, varn: untyped, args: static[openarray[int]]): untyped =
  let inVarn = copyNimNode(varn)
  func aux(nnode: NimNode, newExpr: NimNode): NimNode =
    # debugecho nnode.lispRepr(), nnode.repr, varn.repr
    if nnode.kind in {nnkIdent, nnkSym} and
       eqIdent(nnode.repr, inVarn.repr):
      result = newExpr

    elif nnode.kind in {
      nnkStrLit..nnkTripleStrLit, nnkFloatLit..nnkFloat64Lit, nnkCharLit..nnkUInt64Lit,
      nnkCommentStmt, nnkIdent, nnkSym, nnkNone, nnkEmpty, nnkNilLit
    }:
      result = nnode

    elif nnode.kind in {nnkHiddenStdConv}:
      result = aux(nnode[1], newExpr)

    else:
      result = newTree(nnode.kind)
      for elem in nnode:
        result.add aux(elem, newExpr)

  result = nnkCaseStmt.newTree(varn)
  for arg in args:
    let subs = aux(expr, newLit(arg))
    result.add nnkOfBranch.newTree(
      newLit(arg),
      nnkStmtList.newTree(
        nnkConstSection.newTree(
          nnkConstDef.newTree(ident("res"), newEmptyNode(), subs)
        ),
        ident("res")
      )
    )

  result.add nnkElse.newTree(expr)

func toStr*(inAst: ShellAst, oneline: bool = false): string =
  func aux(ast: ShellAst, level: int, inExpr: bool): string =
    let pref = if inExpr or oneline:
                 ""
               else:
                 repeat("  ", level).precompute(level, [0, 1, 2, 3, 4, 5, 6])

    case ast.kind:
      of sakEmpty:
        raise newUnexpectedKindError(
          ast, "'sakEmpty' cannot be converted to string and ",
          "should not exist withing final AST.")

      of sakListKinds:
        var buf: seq[string]
        for sn in ast.subnodes:
          if sn.kind in sakListKinds:
            if sn.len == 0:
              discard

            else:
              buf.add &"({sn.toStr()})"

          else:
            buf.add sn.toStr()

        let sep =
          case ast.kind:
            of sakAndList: "&&"
            of sakOrList: "||"
            of sakPipeList: "|"
            else: raiseImplementError(&"For kind {ast.kind}")

        result = buf.join(" " & sep & " ")

      of sakCmd:
        if inExpr:
          result = ast.cmd.toStr()

        else:
          result = pref & ast.cmd.toStr()

      of sakVar:
        result = pref & "$" & ast.shVar.string

      of sakStrLit:
        result = ast.strVal.quoteShell()

      of sakStmtList:
        for idx, stmt in ast.subnodes:
          if idx > 0:
            result &= (if oneline: "; " else: "\n")

          result &= pref & aux(stmt, level + 1, inExpr)

      of sakAsgn:
        var rhs = ast[1].aux(level + 1, inExpr = true)
        if ast[1].kind == sakMath:
          rhs = &"$(({rhs}))"

        result = pref & ast[0].shVar.string & "=" & rhs

      of sakMath:
        let lhs = ast.mathArgs[0].aux(level + 1, inExpr = true)
        let rhs = ast.mathArgs[1].aux(level + 1, inExpr = true)

        result = &"({lhs}){ast.mathOp}({rhs})"

        if not inExpr:
          result = &"$(({result}))"

      of sakWhile:
        if oneline:
          result = "while " & ast[0].aux(level + 1, true) & "; do " &
            ast[1].aux(level, inExpr) & "; done"

        else:
          result = &"""
{pref}while {ast[0].aux(level + 1, true)}
{pref}do
{ast[1].aux(level, inExpr)}
{pref}done
"""

      of sakAsyncList:
        result = ast[0].aux(level + 1, true) & " &"

      else:
        raiseImplementError(&"For kind {ast.kind} {instantiationInfo()}")

  return aux(inAst, 0, false)



func toShellCmd*(ast: ShellAst): ShellCmd =
  ShellCmd(bin: ast.toStr())

iterator items*(cmd: ShellCmd): ShellCmdPart =
  for item in items(cmd.opts):
    yield item

func `[]`*(cmd: ShellCmd, idx: int): ShellCmdPart =
  cmd.opts[idx]

iterator pairs*(cmd: ShellCmd): (int, ShellCmdPart) =
  for idx, item in pairs(cmd.opts):
    yield (idx, item)

func toLogStr*(cmd: ShellCmd): ColoredText =
  ## Convert shell command to pretty-printed shell representation
  # TODO add newline escapes `\` at the end of the string
  for str in cmd.toStrSeq():
    if result.len + str.len + 1 > 80:
      result &= "\n"
    elif result.len > 0:
      result &= " "

    result &= str

func splitShell*(str: string): seq[string] =
  # IMPLEMENT
  str.split(" ")

func wrapShell*(str: string, maxw: int = 80): string =
  var buf: seq[string] = @[""]
  for str in str.splitShell():
    if buf[^1].len + str.len + 1 > maxw - 2:
      buf[^1] = buf[^1].alignLeft(maxw - 2) & " \\\n"
      buf.add ""
    elif buf[^1].len > 0:
      buf[^1] &= " "

    buf[^1] &= str

  return buf.join("")

func listToInfix(infix: string, list: seq[NimNode], name: string): NimNode =
  var cmds: seq[NimNode]
  if list.len == 1 and list[0].kind == nnkStmtList:
    for arg in list[0]:
      cmds.add arg
  else:
    for arg in list:
      cmds.add arg



  result = cmds.foldl(nnkInfix.newTree(ident infix, a, b))

func extendList(
  kind: ShellAstKind, e1: var ShellAst, e2: ShellAst) =
  if e1.kind == sakEmpty:
    e1 = e2

  elif e1.kind != kind:
    raise newArgumentError(
      "Cannot extend shell list of kind ",
      $e1.kind,
      " as ",
      $kind,
      ". Use ",
      case kind:
        of sakAndList: "&&="
        of sakOrList: "||="
        of sakAsyncList: "&="
        of sakPipeList: "|="
        of sakSequentialList: raise newImplementError("WIP")
        else:raise newArgumentError(
          "Cannot extend element of kind '" & $kind &
            "' - not a list kind"
        ),
       " instead."
    )
  else:
    e1.subnodes.add e2

func extendList(kind: ShellAstKind, e1, e2: ShellAst): ShellAst =
  if e1.kind == kind:
    result = e1
    result.subnodes.add e2
  else:
    result = ShellAst(kind: kind)
    if e1.kind != sakEmpty:
      result.subnodes.add e1

    result.subnodes.add e2

func toShellAst*(cmd: ShellCmd): ShellAst = ShellAst(kind: sakCmd, cmd: cmd)
func toShellAst*(ast: ShellAst): ShellAst = ast
func toShellAst*(str: string): ShellAst = ShellAst(kind: sakStrLit, strVal: str)
func toShellAst*(v: ShellVar): ShellAst = ShellAst(kind: sakVar, shVar: v)
func toShellAst*(i: int): ShellAst = ShellAst(kind: sakStrLit, strVal: $i)

func `&&`*(exprs: openarray[ShellSomething]): ShellAst =
  ShellAst(kind: sakAndList, subnodes: exprs.mapIt(it.toShellAst()))

func `&&`*[T1, T2: ShellSomething](e1: T1, e2: T2): ShellAst =
  extendList(sakAndList, toShellAst(e1), toShellAst(e2))

func `||`*(exprs: openarray[ShellSomething]): ShellAst =
  ShellAst(kind: sakOrList, subnodes: exprs.mapIt(it.toShellAst()))

func `||`*[T1, T2: ShellSomething](e1: T1, e2: T2): ShellAst =
  extendList(sakOrList, toShellAst(e1), toShellAst(e2))

func `|`*(exprs: openarray[ShellSomething]): ShellAst =
  ShellAst(kind: sakPipeList, subnodes: exprs.mapIt(it.toShellAst()))

func `|`*[T1, T2: ShellSomething](e1: T1, e2: T2): ShellAst =
  extendList(sakPipeList, toShellAst(e1), toShellAst(e2))

func `&`*(exprs: openarray[ShellSomething]): ShellAst =
  ShellAst(kind: sakAsyncList, subnodes: exprs.mapIt(it.toShellAst()))

func `&`*[T1, T2: ShellSomething](e1: T1, e2: T2): ShellAst =
  extendList(sakAsyncList, toShellAst(e1), toShellAst(e2))

func `&&=`*(e1: var ShellAst, e2: ShellSomething) =
  extendList(sakAndList, e1, toShellAst(e2))

func `||=`*(e1: var ShellAst, e2: ShellSomething) =
  extendList(sakOrList, e1, toShellAst(e2))

func `&=`*(e1: var ShellAst, e2: ShellSomething) =
  extendList(sakAsyncList, e1, toShellAst(e2))

func `|=`*(e1: var ShellAst, e2: ShellSomething) =
  extendList(sakPipeList, e1, toShellAst(e2))

macro shAnd*(arg: varargs[untyped]): untyped =
  listToInfix("&&", toSeq(arg), "shAnd")

macro shOr*(arg: varargs[untyped]): untyped =
  listToInfix("||", toSeq(arg), "shOr")

macro shPipe*(arg: varargs[untyped]): untyped =
  listToInfix("|", toSeq(arg), "shPipe")

func shAsync*(arg: ShellAst): ShellAst =
  ShellAst(kind: sakAsyncList, subnodes: @[arg])

func shAsync*(arg: ShellCmd): ShellAst =
  ShellAst(kind: sakAsyncList, subnodes: @[
    ShellAst(kind: sakCmd, cmd: arg)])

func shStmtList*(args: varargs[ShellAst]): ShellAst =
  ShellAst(kind: sakStmtList, subnodes: toSeq(args))

func shAsgn*(
  v: ShellVar, expr: string, exportVar: bool = false): ShellAst =
  ShellAst(
    kind: sakAsgn,
    subnodes: @[toShellAst(v), toShellAst(ShellExpr(expr))],
    exportVar: exportVar
  )


func shAsgn*(
  v: ShellVar, expr: ShellSomething,
  exportVar: bool = false): ShellAst =
  ShellAst(
    kind: sakAsgn, subnodes: @[toShellAst(v), toShellAst(expr)],
    exportVar: exportVar
  )

func shWhile*(expr: ShellAst, body: varargs[ShellAst, toShellAst]): ShellAst =
  ShellAst(kind: sakWhile, subnodes: @[expr] & shStmtList(body))

func makeTestBracketCmd*(
  e1, e2: ShellAst, op: string): ShellAst =
  var cmd = makeShellCmd(X11ShellCmdConf, "[")
  cmd.expr e1
  cmd - op
  cmd.expr e2
  cmd.arg "]"
  ShellAst(kind: sakCmd, cmd: cmd)


func `<`*[T1, T2: ShellMathExpr](lhs: T1, rhs: T2): ShellAst =
  makeTestBracketCmd(toShellAst(lhs), toShellAst(rhs), "lt")

func `+`*[T1, T2: ShellMathExpr](lhs: T1, rhs: T2): ShellAst =
  ShellAst(kind: sakMath, mathOp: "+", mathArgs: @[
    toShellAst(lhs), toShellAst(rhs)
  ])



func toShellArgument(arg: NimNode): NimNode =
  case arg.kind:
    of nnkExprEqExpr:
      nnkPar.newTree(arg[0].toShellArgument(), arg[1].toShellArgument())

    of nnkIdent, nnkIntLit:
      arg.toStrLit()

    of nnkAccQuoted:
      var buf: string
      for sub in arg:
        buf.add sub.strVal()
      newLit(buf)

    of nnkStrLit, nnkPrefix:
      arg

    else:
      raiseImplementError(&"{treeRepr(arg)}")



func shellCmdRaw*(args: seq[string]): ShellCmd =
  result.bin = args[0]
  for arg in args[1 .. ^1]:
    result.raw arg

func shellCmdX11*(bin: string): ShellCmd =
  ## Create command for `X11` cli tools (single dash)
  result.conf = X11ShellCmdConf
  result.bin = bin

func shellCmdGnu*(bin: string): ShellCmd =
  ## Create command for CLI applications that conform to GNU standard
  ## for command line interface `link
  ## <https://www.gnu.org/prep/standards/html_node/Command_002dLine-Interfaces.html>`_
  result.conf = GnuShellCmdConf
  result.bin = bin


macro shellCmd*(cmd: untyped, args: varargs[untyped]): untyped =
  let
    onCallHead = "shellCmd does not support call arguments due to possible ambiguous interpretation"
    onCallAnnot = "Can be interpreted as both value of\n the expression $() or subcommand ''"

  let shCmd = if cmd.kind == nnkIdent:
                cmd.toStrLit()

              elif cmd.kind == nnkStrLit:
                cmd

              elif cmd.kind == nnkAccQuoted:
                cmd.mapIt(it.toStrLit().strVal()).join("").newLit()

              elif cmd.kind in { nnkCall, nnkCommand }:
                raise cmd.toCodeError(onCallHead, onCallAnnot)

              else:
                raiseImplementError(&"#[ IMPLEMENT for kind {cmd.kind} ]#")

  let resId = genSym(nskVar, "res")
  result = newStmtList()
  result.add quote do:
    var `resId` = makeShellCmd(`shCmd`)

  for arg in args:
    case arg.kind:
      of nnkPrefix:
        if arg[0].eqIdent("--") or
           arg[0].eqIdent("-"):
          result.add nnkInfix.newTree(
            ident("-"), resId, toShellArgument(arg[1]))

        else:
          result.add newCall("arg", resId, arg)

      of nnkStrLit, nnkIntLit:
        result.add newCall("arg", resId, arg)

      of nnkIdent:
        result.add newCall("arg", resId, arg.toStrLit())

      of nnkExprEqExpr:
        result.add newCall("-", resId, toShellArgument(arg))

      of nnkAccQuoted:
        var res: string
        for node in arg:
          res &= node.repr

        result.add newCall("arg", resId, newLit(res))

      of nnkCall, nnkCommand:
        raise arg.toCodeError(onCallHead, onCallAnnot)

      else:
        raiseImplementError(&"#[ IMPLEMENT for kind {arg.kind} ]#")

  result.add quote do:
    `resId`

  result = quote do:
    block:
      `result`




macro shCmd*(cmd: untyped, args: varargs[untyped]): untyped
  {.deprecated: "Use `shellCmd` instead".} =
  ##[

DSL for consturction of the new shell commands, with syntax similar to
regular shell.

  ]##
  result = newCall("shellCmd")
  result.add cmd
  for arg in args:
    result.add arg




when not defined(NimScript):
  proc printShellError*() =
    when defined(NimScript):
      echo getCurrentExceptionMsg()
    else:
      let err = ShellError(getCurrentException())
      echo err.errstr

      echo err.outstr

iterator iterstdout*(command: ShellExpr): string =
  ## Iterate every line of shell expression output
  # TODO raise exception on failed command
  # REVIEW how cleanup is performed when iterator finishes main loop?
  when defined(NimScript):
    let (res, code) = gorgeEx(command.string, "", "")
    for line in res.split("\n"):
      yield line
  else:
    let pid = startProcess(command.string, options = {poEvalCommand})

    let outStream = pid.outputStream
    var line = ""

    while pid.running:
      try:
        let streamRes = outStream.readLine(line)
        if streamRes:
          yield line
      except IOError, OSError:
        assert outStream.isNil

    let rem = outStream.readAll().split("\n")
    for line in (if rem.len > 0: rem[0..^2] else: rem):
      yield line

proc updateException(res: var ShellResult, cmd: ShellCmd, maxErrorLines: int) =
  res.cmd = cmd
  let
    env = cmd.envVals
    command = cmd.toStr()

  if res.execResult.code != 0:
    # echo res
    let envAdd =
      if env.len > 0:
        "With env variables " &
          env.mapIt(&"{it.key}={it.val}").join(" ") & "\n"
      else:
        ""

    var msg = &"Command '{command}' exited with " &
        &"non-zero code {res.execresult.code}:\n"

    let split = res.execResult.stderr.split("\n") &
      res.execResult.stdout.split("\n")

    # echov maxErrorLines
    # assert maxErrorLines == 12

    msg.add split[0 ..< min(split.len(), maxErrorLines)].join("\n")

    res.exception = ShellError(
      msg: msg,
      retcode: res.execResult.code,
      errorCode: int32(res.execResult.code),
      errstr: res.execResult.stderr,
      outstr: res.execResult.stdout,
      cwd: cwd(),
      cmd: command
    )
    # echo "Exception"
  else:
    # echo "ALl ok"
    res = ShellResult(resultOk: true, execResult: res.execResult)


when cbackend:
  proc startShell*(
      cmd: ShellCmd,
      options: set[ProcessOption] = {poUsePath},
      stdin: string = ""
    ): Process =
    ## Launch process using shell command

    let env = cmd.envVals
    result =
      if poEvalCommand in options:
        startProcess(
          cmd.toStr(),
          options = options,
          env = if env.len > 0: newStringTable(env) else: nil
        )
      else:
        let args = cmd.opts.mapIt($it.toStr(cmd.conf, colored = false))
        let env = if env.len > 0: newStringTable(env) else: nil

        startProcess(
          cmd.bin,
          options = options,
          args = args,
          env = env
        )

    if result.isNil:
      raise ShellError(
        msg: "Command '" & cmd.toStr() & "' failed to start",
        cwd: getCurrentDir(),
        cmd: cmd.toStr()
      )

    elif stdin.len > 0:
      let ins = result.inputStream()
      ins.write(stdin)
      ins.close()

  import std/[posix]
  import os
  iterator runShellResult*[T](
      cmds: seq[tuple[cmd: ShellCmd, data: T]],
      fullParams: set[ProcessOption] = {poEvalCommand},
      maxPool: int = 8,
      beforeStart: proc(cmd: ShellCmd, data: T) = nil
    ): tuple[res: ShellResult, data: T] =

    assert 0 < maxPool

    var processList: seq[Process] = newSeq[Process](maxPool)
    var commandMap: seq[int] = newSeq[int](maxPool)

    let maxPool = min(maxPool, cmds.len)

    var leftCount = len(cmds)
    var cmdIdx = 0

    while cmdIdx < maxPool:
      if not isNil(beforeStart):
        beforeStart(cmds[cmdIdx].cmd, cmds[cmdIdx].data)

      processList[cmdIdx] = startShell(cmds[cmdIdx].cmd, fullParams)
      commandMap[cmdIdx] = cmdIdx
      inc(cmdIdx)

    while leftCount > 0:
      var exitedIdx = -1
      var status: cint = 1
      let res: Pid = waitpid(-1, status, 0)
      if res > 0:
        for idx, process in mpairs(processList):
          if not isNil(process) and process.processID() == res:
            if WIFEXITED(status) or WIFSIGNALED(status):
              # process.exitFlag = true
              # process.exitStatus = status
              exitedIdx = idx
              break

      else:
        let err = osLastError()
        if err == OSErrorCode(ECHILD):
          # some child exits, we need to check our childs exit codes
          for idx, process in mpairs(processList):
            if (not isNil(process)) and (not running(process)):
              # process.exitFlag = true
              # process.exitStatus = status
              exitedIdx = idx
              break

        elif err == OSErrorCode(EINTR):
          # signal interrupted our syscall, lets repeat it
          continue

        else:
          # all other errors are exceptions
          raiseOSError(err)

      if exitedIdx >= 0:
        var res: ShellResult

        let p = processList[exitedIdx]

        res.execResult.stdout = p.outputStream.readAll()
        res.execResult.stderr = p.errorStream.readAll()
        res.execResult.code = p.peekExitCode()
        updateException(res, cmds[commandMap[exitedIdx]].cmd, 50)
        yield (res, cmds[commandMap[exitedIdx]].data)

        close(processList[exitedIdx])
        if cmdIdx < len(cmds):
          if not isNil(beforeStart):
            beforeStart(cmds[cmdIdx].cmd, cmds[cmdIdx].data)

          processList[exitedIdx] = startShell(
            cmds[cmdIdx].cmd, fullParams)

          commandMap[exitedIdx] = cmdIdx
          inc(cmdIdx)

        else:
          processList[exitedIdx] = nil


        dec(leftCount)







type
  StreamConverter*[Cmd, Rec, State] =
    proc(stream: var PosStr, cmd: Cmd, state: var Option[State]):
    Option[Rec]



when cbackend:
  template yieldStream(que, cmd: typed) =
    que.addLast line
    let res = outConvert(que, cmd)
    if res.isSome():
      yield res.get()

  proc makeShellRecordIter*[Cmd, OutRec, ErrRec, State](
      cmd: Cmd,
      outConvert: StreamConverter[Cmd, OutRec, State],
      errConvert: StreamConverter[Cmd, ErrRec, State],
      options: set[ProcessOption] = {poEvalCommand, poUsePath},
      maxErrorLines: int = 12,
      doRaise: bool = true,
      state: Option[State] = none(State),
      execTimeoutMs: int = high(int)
    ): tuple[stdout: iterator(): OutRec, stderr: iterator(): ErrRec] =
    ## Iterate over output of the shell command converted to json
    let
      # FIXME allow different types to be used as shell commands (to
      # avoid reparsing configuration each time). Right now it is
      # 'generic', but `startShell` only works for `ShellCmd`
      process = startShell(cmd, options)
      stdoutStream = process.outputStream()
      stderrStream = process.errorStream()

    template makeReader(convert, inStream: untyped): untyped =
      block:
        iterator resIter(): OutRec =
          var str = initPosStr(inStream)
          var
            state: Option[State] = state
            start = getTime()
            hasTimeout = execTimeoutMs < high(int)

          while process.running:
            if hasTimeout:
              if inMilliseconds(getTime() - start) > execTimeoutMs:
                kill(process)

            let res = convert(str, cmd, state)
            if res.isSome():
              yield res.get()


          while not str.finished():
            let res = convert(str, cmd, state)
            if res.isSome():
              yield res.get()

          var res = ShellResult()
          res.execResult.code = process.peekExitCode()
          res.execResult.stderr = stderrStream.readAll()
          res.execResult.stdout = stdoutStream.readAll()

          updateException(res, cmd, maxErrorLines)

          if not res.resultOk and doRaise:
            raise res.exception

        resIter

    result.stdout = makeReader(outConvert, stdoutStream)
    result.stderr = makeReader(errConvert, stderrStream)

proc shellResult*(
    cmd: ShellCmd,
    stdin: string               = "",
    options: set[ProcessOption] = {poEvalCommand},
    maxErrorLines: int          = high(int),
    maxOutLines: int            = high(int),
    discardOut: bool            = false,
    execTimeoutMs: int           = high(int)
  ): ShellResult {.tags: [
    ShellExecEffect,
    ExecIOEffect,
    ReadEnvEffect,
    RootEffect
  ].} =

  var options = options

  var reprintOut = false
  if (poParentStreams in options) and
     (maxErrorLines < high(int) or maxOutLines < high(int)):
    reprintOut = true
    options.excl poParentStreams


  if not discardOut and (poParentStreams in options):
    # TODO add support for showing output *and* piping results. This
    # will certainly involve some fuckery with filtering out ansi
    # codes (because colored output is basically /the/ reason to have
    # piping to parent shell).
    raise newArgumentError(
      "Stream access not allowed when you use poParentStreams. ",
      "Either set `discardOut` to true or remove `poParentStream` ",
      "from options"
    )

  const nl = "\n"

  let hasTimeout = execTimeoutMs < high(int)
  var wasTerminated = false
  when not defined(NimScript):
    let pid = startShell(cmd, options, stdin)
    if hasTimeout:
      sleep(execTimeoutMs)
      if pid.running():
        wasTerminated = true
        pid.kill()

    if discardOut and not reprintOut:
      while pid.running:
        discard

    else:
      var
        stdoutBuf: string
        stderrBuf: string

      let
        outStream = pid.outputStream()
        errStream = pid.errorStream()

      while pid.running:
        stdoutBuf.add outStream.readAll()
        stderrBuf.add errStream.readAll()

      stdoutBuf.add outStream.readAll()
      stderrBuf.add errStream.readAll()

      var outLineCount = 0
      for line in stdoutBuf.splitLines():
        if outLineCount < maxOutLines:
          if reprintOut:
            stdout.write line, nl

          else:
            result.execResult.stdout.add line & nl

          inc outLineCount

      var errLineCount = 0
      for line in stderrBuf.splitLines():
        if errLineCount < maxErrorLines:
          if reprintOut:
            stderr.write line, nl

          else:
            result.execResult.stderr.add line & nl

          inc errLineCount

    result.execResult.code = pid.peekExitCode()
    close(pid)

  else:
    let nscmd = &"cd {cwd()} && " & cmd.toStr()
    if poParentStreams in options and not reprintOut:
      exec(nscmd)
    else:
      let (res, code) = gorgeEx(nscmd, "", "")

      if not discardOut:
        if maxOutLines == high(int):
          result.execResult.stdout = res

        else:
          for idx, line in enumerate(lines(res)):
            if idx > maxOutLines:
              break

            else:
              if reprintOut:
                echo line

              else:
                result.execResult.stdout.add line & nl

        result.execResult.code = code

  if not reprintOut:
    withResIt result.execResult.stderr:
      var idx = high(it)
      while 0 <= idx and it[idx] == '\n':
        dec idx

      if 0 <= idx:
        it.setLen(idx)

    withResIt result.execResult.stdout:
      var idx = high(it)
      while 0 <= idx and it[idx] == '\n':
        dec idx

      if 0 <= idx:
        it.setLen(idx)


  updateException(result, cmd, maxErrorLines)
  result.wasTerminated = wasTerminated




proc runShell*(
    cmd: ShellCmd,
    doRaise: bool = true,
    stdin: string = "",
    options: set[ProcessOption] = {poEvalCommand},
    maxErrorLines: int = high(int),
    maxOutLines: int = high(int),
    discardOut: bool = false,
    execTimeoutMs: int = high(int)
  ): ShellExecResult =
  ## Execute shell command and return it's output. `stdin` - optional
  ## parameter, will be piped into process. `doRaise` - raise
  ## exception (default) if command finished with non-zero code.
  ## `command` - text of the command.
  ## ## Arguments
  ## :maxErrorLines: max number of stderr lines that would be appended to
  ##   exception. Any stderr beyond this range will be truncated
  let output = shellResult(
    cmd, stdin,
    options = options,
    maxErrorLines = maxErrorLines,
    maxOutLines = maxOutLines,
    discardOut = discardOut,
    execTimeoutMs = execTimeoutMs
  )

  result = output.execResult

  if (not output.resultOk) and doRaise:
    raise output.exception

proc execShell*(cmd: ShellExpr): void =
  ## `execShell` overload for regular string.
  ##
  ## WARNING see implicit `toShellCmd` documentation for potential
  ## pitfalls. It is recommended to use `execShell(cmd: ShellCmd)` overload
  ## - this version exists only for quick prototyping.
  discard runShell(cmd, discardOut = true, options = {
    poEvalCommand, poParentStreams, poUsePath})

proc hasCmd*(cmd: ShellCmd): bool =
  for dir in get($$PATH).split(':'):
    let dir = AbsDir(dir)
    if exists(dir /. cmd.bin):
      return true
    # if exists(dir):

    #   for file in walkDir(dir, RelFile):
    #     if file.name() == cmd.bin:
    #       return true

  return false

proc execShell*(cmd: ShellAst): void =
  discard runShell(ShellCmd(bin: cmd.toStr()), discardOut =  true, options = {
    poEvalCommand, poParentStreams, poUsePath})

proc evalShell*(cmd: ShellExpr): auto =
  var opts = {poEvalCommand, poUsePath}
  runShell(cmd, options = opts)


proc evalShell*(ast: ShellAst): auto =
  var opts = {poEvalCommand, poUsePath}
  runShell(ShellCmd(bin: ast.toStr()), options = opts)


proc evalShellStdout*(cmd: ShellExpr): string =
  runShell(cmd, options = {poEvalCommand, poUsePath}).stdout.strip()


proc evalShellStdout*(cmd: ShellAst, stdin: string = ""): string =
  runShell(
    ShellCmd(bin: cmd.toStr()),
    options = {poEvalCommand, poUsePath},
    stdin = stdin
  ).stdout.strip()



proc evalShellStdout*(
    cmd: ShellCmd,
    stdin: string = "",
    maxErrorLines: int = high(int)
  ): string =
  runShell(
    cmd,
    stdin = stdin,
    options = {poEvalCommand, poUsePath},
    maxErrorLines = maxErrorLines
  ).stdout.strip()


proc evalShell*(cmd: ShellCmd, stdin: string = ""): auto =
  runShell(cmd, stdin = stdin, options = {poEvalCommand, poUsePath})

proc execShell*(
    cmd: ShellCmd, doRaise: bool = true,
    limitErr: int = high(int), limitOut: int = high(int),
  ): void =
  ## Execute shell command with stdout/stderr redirection into parent
  ## streams. To capture output use `runShell`, `eval` or `evalShellStdout`
  ##
  ## - @arg{limitErr}, @arg{limitOut} :: Some command-line applications
  ##   (line nim compiler for example) can't limit amount of garbage they
  ##   dump to end user, so you might only want to show first `N` lines
  ##   (for example when running `nim check` and you don't want to drown
  ##   in overload candidate messages it make sense to put something like
  ##   `limitErr = 30`).
  if limitOut < high(int) or limitErr < high(int):
    let res = runShell(
      cmd,
      doRaise = doRaise,
      discardOut = true,
      options = { poParentStreams, poUsePath, poEvalCommand },
      maxOutLines = limitOut,
      maxErrorLines = limitErr
    )

    # echo res.stdout
    # echo res.stderr

  else:
    discard runShell(cmd, doRaise = doRaise, discardOut = true, options = {
      poParentStreams, poUsePath, poEvalCommand})

proc eval*(expr: ShellExpr): string =
  shellResult(expr).execResult.stdout.strip()

proc eval*(cmd: ShellCmd, stdin: string = ""): string =
  evalShellStdout(cmd, stdin = stdin)

export get, isSome, isNone

proc interpolateShell*(
  expr: ShellExpr,
  allowEmpty: bool = false,
  doRaise: bool = false): Option[string] =
  var buf: string
  for (kind, val) in interpolatedFragments(expr.string):
    case kind:
      of ikStr: buf &= val
      of ikDollar: buf &= "$"
      of ikVar:
        if not (existsEnv(ShellVar val) or allowEmpty):
          return none(string)
        else:
          let envVal = getEnv(ShellVar val)
          if envVal.len == 0 and not allowEmpty:
            return none(string)
          else:
            buf &= envVal
      of ikExpr:
        let res = shellResult(ShellExpr val)
        if not res.resultOk and doRaise:
          raise res.exception
        elif res.execResult.stdout.len == 0 and not allowEmpty:
          return none(string)
        else:
          buf &= res.execResult.stdout


  return some(buf)


proc initCmdInterpOrOption*(
  interpol: ShellExpr,
  key, val: string, allowEmpty: bool = false): ShellCmdPart =
  result = ShellCmdPart(kind: cpkOption, key: key)

  let res = interpolateShell(interpol, allowEmpty = allowEmpty)

  if res.isSome():
    result.val = res.get()
  else:
    result.val = val
