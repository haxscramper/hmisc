import parseutils

when not defined(NimScript):
  import osproc, streams
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
import std/[strutils, strformat, sequtils, options, deques, json, macros]

import ../hexceptions
import ../base_errors

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

export ShellVar, ShellExpr

type
  ShellExecEffect = object of IOEffect
  ShellError* = ref object of OSError
    cmd*: string ## Command that returned non-zero exit code
    cwd*: AbsDir ## Absolute path of initial command execution directory
    retcode*: int ## Exit code
    # TODO REFACTOR rename to `stdout` and `stderr`
    errstr*: string ## Stderr for command
    outstr*: string ## Stdout for command

  ShellExecResult* = tuple[stdout, stderr: string, code: int] ## Shell command
  ## execution result - standard error, output and return code.
  ShellResult* = object
    # - REFACTOR :: move `execResult` into `true` branch for `resultOk`
    execResult*: ShellExecResult ## Result of command execution
    hasBeenSet*: bool ## Whether or not result been set
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

  ShellCmdConf = object
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
    conf: ShellCmdConf
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

  NimShellCmdConf* = ShellCmdConf(
    flagConf: ccRegularFlags,
    kvSep: ":"
  )

  X11ShellCmdConf* = ShellCmdConf(
    flagConf: ccOneDashFlags,
    kvSep: " "
  )

  sakListKinds* = {sakAndList, sakOrList}

func `[]`*(sa: ShellAst, idx: int): ShellAst = sa.subnodes[idx]
func len*(sa: ShellAst): int = sa.subnodes.len
func toJson*(v: ShellGlob): JsonNode = newJString(v.string)

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

func opt*(cmd: var ShellCmd, inKey, val: string) =
  ## Add option (key-value pairs) for command
  cmd.opts.add ShellCmdPart(kind: cpkOption, key: inKey, val: val)

func env*(cmd: var ShellCmd, key, val: string): void =
  ## Add environment variable configuration for command
  cmd.envVals.add (key, val)

func opt*(cmd: var ShellCmd, opts: openarray[tuple[key, val: string]]) =
  ## Add sequence of key-value pairs
  for (key, val) in opts:
    cmd.opt(key, val)

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
  cmd.opts.add ShellCmdPart(
    kind: cpkOption, key: kv.key, val: kv.val, overrideKv: true,
    kvSep: kv.sep)

func makeShellCmd*(conf: ShellCmdConf, bin: string): ShellCmd =
  result.conf = conf
  result.bin = bin

const
  gnuShellCmdsList* = ["ls"]
  nimShellCmdsList* = ["nimble"]
  x11ShellCmdsList* = [
    "xclip",
    "pdflatex",
    "java"
  ]

func makeShellCmd*(bin: string): ShellCmd =
  if bin in gnuShellCmdsList:
    result.conf = GnuShellCmdConf

  elif bin in nimShellCmdsList:
    result.conf = NimShellCmdConf

  elif bin in x11ShellCmdsList:
    result.conf = X11ShellCmdConf

  else:
    if bin.startsWith("x"):
      result.conf = X11ShellCmdConf

    else:
      result.conf = NimShellCmdConf

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

func toStr*(part: ShellCmdPart, conf: ShellCmdConf): string =
  ## Convret shell command part to string representation
  let longPrefix =
    case conf.flagConf:
      of ccRegularFlags: "--"
      of ccOneDashFlags: "-"

  case part.kind:
    of cpkRaw:
      return part.rawstring
    of cpkSubCmd:
      return part.subcommand
    of cpkFlag:
      if part.flag.len > 1:
        return longPrefix & part.flag
      else:
        return "-" & part.flag
    of cpkOption:
      let kv = if part.overrideKv: part.kvSep else: conf.kvSep
      if part.key.len > 1:
        return longPrefix & part.key & kv & part.val.quoteShell()
      else:
        return "-" & part.key & kv & part.val.quoteShell()
    of cpkArgument:
      return part.argument.quoteShell()
    of cpkSubExpr:
      if part.expr.kind != sakVar:
        return part.expr.toStr().quoteShell()
      else:
        return part.expr.toStr()


func toStrSeq*(cmd: ShellCmd): seq[string] =
  result = @[ cmd.bin ]
  for op in cmd.opts:
    result &= op.toStr(cmd.conf)

func toStr*(cmd: ShellCmd): string = cmd.toStrSeq().join(" ")

macro precompute(expr, varn: untyped, args: static[openarray[int]]): untyped =
  let inVarn = copyNimNode(varn)
  func aux(nnode: NimNode, newExpr: NimNode): NimNode =
    # debugecho nnode.lispRepr(), nnode.repr, varn.repr
    if nnode.kind in {nnkIdent, nnkSym} and
       eqIdent(nnode.repr, inVarn.repr):
      result = newExpr

    elif nnode.kind in {
      # Boring stuff, just list all nodes that cannot have subnodes
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
        raiseArgumentError("'sakEmpty' cannot be converted to string and " &
          "should exist withing final AST.")

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

      else:
        raiseImplementError(&"For kind {ast.kind} {instantiationInfo()}")

  return aux(inAst, 0, false)



func toLogStr*(cmd: ShellCmd): string =
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
    raiseArgumentError(
      "Cannot extend shell list of kind " & $e1.kind &
      " as " & $kind & ". Use " & (
        case kind:
          of sakAndList: "&&="
          of sakOrList: "||="
          of sakAsyncList: "&="
          of sakPipeList: "|="
          of sakSequentialList: raiseImplementError("WIP")
          else: raiseArgumentError(
            "Cannot extend element of kind '" & $kind &
              "' - not a list kind"
          )
      ) & " instead."
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

macro shAsync*(arg: varargs[untyped]): untyped =
  listToInfix("&", toSeq(arg), "shAsync")

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

    of nnkIdent:
      arg.toStrLit()

    else:
      raiseImplementError(&"#[ IMPLEMENT for kind {arg.kind} ]#")



macro shCmd*(cmd: untyped, args: varargs[untyped]): untyped =
  ##[

DSL for consturction of the new shell commands, with syntax similar to
regular shell.

  ]##
  let shCmd = if cmd.kind == nnkIdent:
                cmd.toStrLit()
              elif cmd.kind == nnkStrLit:
                cmd
              elif cmd.kind == nnkAccQuoted:
                cmd.mapIt(it.toStrLit().strVal()).join("").newLit()
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
      of nnkCall, nnkCommand:
        raise arg.toCodeError(
          "shCmd does not support call arguments due to " &
            "possible ambiguous interpretation",
          "Can be interpreted as both value of\n" &
            "the expression $() or subcommand ''"
        )
      else:
        raiseImplementError(&"#[ IMPLEMENT for kind {arg.kind} ]#")

  result.add quote do:
    `resId`

  result = quote do:
    block:
      `result`





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

when cbackend:
  proc startShell*(
    cmd: ShellCmd,
    options: set[ProcessOption] = {poUsePath}): Process =
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
        startProcess(
          cmd.bin,
          options = options,
          args = cmd.opts.mapIt(it.toStr(cmd.conf)),
          env = if env.len > 0: newStringTable(env) else: nil
        )

    if result.isNil:
      raise ShellError(
        msg: "Command '" & cmd.toStr() & "' failed to start",
        cwd: getCurrentDir(),
        cmd: cmd.toStr()
      )

proc updateException(
  res: var ShellResult, cmd: ShellCmd, maxErrorLines: int) =
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

    var msg = &"Command '{command}'\nExecuted in directory " &
      $cwd() & &"\n{envAdd}Exited with non-zero code:\n"

    let split = res.execResult.stderr.split("\n") &
      res.execResult.stdout.split("\n")

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
    res = ShellResult(
      resultOk: true,
      execResult: res.execResult
    )

type
  StrQue* = Deque[string]
  StreamConverter* = proc(
    que: var StrQue, cmd: ShellCmd): Option[JsonNode] {.noSideEffect.}

when cbackend:
  proc makeShellJsonIter*(
    cmd: ShellCmd,
    outConvert: StreamConverter,
    errConvert: StreamConverter,
    options: set[ProcessOption] = {poEvalCommand, poUsePath},
    maxErrorLines: int = 12,
    doRaise: bool = true
       ): iterator(): JsonNode =
    ## Iterate over output of the shell command converted to json
    return iterator(): JsonNode =
      let
        process = startShell(cmd, options)
        stdoutStream = process.outputStream()
        stderrStream = process.errorStream()

      var
        stdoutQue: StrQue
        stderrQue: StrQue

      template yieldStream(strm: untyped) =
        `strm Que`.addLast line
        let `strm Res` = outConvert(`strm Que`, cmd)
        if `strm Res`.isSome():
          yield `strm Res`.get()

      block:
        var line = ""
        while process.running:
          discard stdoutStream.readLine(line)
          yieldStream(stdout)

          discard stderrStream.readLine(line)
          yieldStream(stderr)

      block:
        for line in stdoutStream.readAll().split("\n"):
          yieldStream(stdout)

        for line in stderrStream.readAll().split("\n"):
          yieldStream(stderr)

      var res = ShellResult()
      res.execResult.code = process.peekExitCode()
      updateException(res, cmd, maxErrorLines)

      if not res.resultOk and doRaise:
        raise res.exception



proc shellResult*(
  cmd: ShellCmd,
  stdin: string = "",
  options: set[ProcessOption] = {poEvalCommand},
  maxErrorLines: int = 12,
  discardOut: bool = false): ShellResult {.tags: [
    ShellExecEffect,
    ExecIOEffect,
    ReadEnvEffect,
    RootEffect
  ].} =

  if not discardOut and (poParentStreams in options):
    # TODO add support for showing output *and* piping results. This
    # will certainly involve some fuckery with filtering out ansi
    # codes (because colored output is basically /the/ reason to have
    # piping to parent shell).
    raiseArgumentError(
      "Stream access not allowed when you use poParentStreams. " &
        "Either set `discardOut` to true or remove `poParentStream` from options"
    )

  when not defined(NimScript):
    let pid = startShell(cmd, options)
    if not discardOut:
      let ins = pid.inputStream()
      ins.write(stdin)
      # ins.flush()
      ins.close()

      let outStream = pid.outputStream
      var line = ""

      while pid.running:
        try:
          let streamRes = outStream.readLine(line)
          if streamRes:
            result.execResult.stdout &= line & "\n"
            # WARNING remove trailing newline on the stdout
        except IOError, OSError:
          assert outStream.isNil
          echo "process died" # NOTE possible place to raise exception

      result.execResult.stdout &= outStream.readAll()
      result.execResult.stderr = pid.errorStream.readAll()
    else:
      while pid.running():
        discard

    result.execResult.code = pid.peekExitCode()
    close(pid)

  else:
    let nscmd = &"cd {cwd()} && " & cmd.toStr()
    if poParentStreams in options:
      exec(nscmd)
    else:
      let (res, code) = gorgeEx(nscmd, "", "")

      if not discardOut:
        result.execResult.stdout = res
        result.execResult.code = code


  updateException(result, cmd, maxErrorLines)



proc runShell*(
  cmd: ShellCmd,
  doRaise: bool = true,
  stdin: string = "",
  options: set[ProcessOption] = {poEvalCommand},
  maxErrorLines: int = 12,
  discardOut: bool = false): tuple[stdout, stderr: string, code: int] =
  ## Execute shell command and return it's output. `stdin` - optional
  ## parameter, will be piped into process. `doRaise` - raise
  ## exception (default) if command finished with non-zero code.
  ## `command` - text of the command.
  ## ## Arguments
  ## :maxErrorLines: max number of stderr lines that would be appended to
  ##   exception. Any stderr beyond this range will be truncated
  let output = shellResult(
    cmd, stdin, options, maxErrorLines, discardOut)
  result = output.execResult

  if (not output.resultOk) and doRaise:
    raise output.exception


proc execShell*(cmd: ShellExpr): void =
  ## `shExec` overload for regular string.
  ##
  ## WARNING see implicit `toShellCmd` documentation for potential
  ## pitfalls. It is recommended to use `shExec(cmd: ShellCmd)` overload -
  ## this version exists only for quick prototyping.
  discard runShell(cmd, discardOut = true, options = {
    poEvalCommand, poParentStreams, poUsePath})

proc execShell*(cmd: ShellAst): void =
  discard runShell(ShellCmd(bin: cmd.toStr()), discardOut =  true, options = {
    poEvalCommand, poParentStreams, poUsePath})

proc evalShell*(cmd: ShellExpr): auto =
  var opts = {poEvalCommand, poUsePath}
  runShell(cmd, options = opts)


proc evalShellStdout*(cmd: ShellExpr): string =
  runShell(cmd, options = {poEvalCommand, poUsePath}).stdout.strip()


proc evalShellStdout*(cmd: ShellAst, stdin: string = ""): string =
  runShell(
    ShellCmd(bin: cmd.toStr()),
    options = {poEvalCommand, poUsePath},
    stdin = stdin
  ).stdout.strip()



proc evalShellStdout*(cmd: ShellCmd, stdin: string = ""): string =
  runShell(
    cmd,
    stdin = stdin,
    options = {poEvalCommand, poUsePath},
  ).stdout.strip()


proc evalShell*(cmd: ShellCmd, stdin: string = ""): auto =
  runShell(cmd, stdin = stdin, options = {poEvalCommand, poUsePath})

proc execShell*(cmd: ShellCmd, doRaise: bool = true): void =
  ## Execute shell command with stdout/stderr redirection into parent
  ## streams. To capture output use `runShell`, `eval` or `evalShellStdout`
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
