import macroutils
import hmisc/helpers
import tables
import sequtils
import macros
import strformat
import sugar
import strformat
import strutils
import os

import colechopkg/types
export types

import ast_pattern_matching

## Implementation of common lisp's loop macro


template iterValType*(arg: untyped): untyped =
  when compiles(arg[0]):
    typeof(arg[0])
  else:
    typeof(arg)

proc codeAssert*(cond: bool,
                 msg: string, annots: varargs[ErrorAnnotation]): void =
  if not cond:
    raise CodeError(
      annots: toSeq(annots),
      # line: info.line,
      # column: info.column,
      # filename: info.filename,
      # expr: expr,
      # annotation: annotation,
      msg: msg
    )


proc makeClosureIteratorDef(iter: NimNode): NimNode =
  nnkIteratorDef.newTree(
    Empty(),
    Empty(),
    Empty(),
    nnkFormalParams.newTree(
      nnkCall.newTree(
        newIdentNode("iterValType"),
        iter
      )
    ),
    Empty(),
    Empty(),
    nnkStmtList.newTree(
      nnkForStmt.newTree(
      newIdentNode("val"),
      iter,
      nnkStmtList.newTree(
          nnkYieldStmt.newTree(
            newIdentNode("val")
          )
        )
      )
    )
  )

proc foldlInfix(nodes: seq[NimNode], inf: NimNode): NimNode =
  result = nodes.foldl(nnkInfix.newTree(
    inf, nnkPar.newTree(a), nnkPar.newTree(b)))

proc substituteEnv(expr: NimNode, env: seq[(NimNode, NimNode)]): NimNode =
  ## Recursively substitue all occurrenices of variables from `env` to
  ## corresponding expressions.
  let idx = env.findIt(it[0] == expr)
  if idx != -1:
    result = env[idx][1]
    result = quote do: (`result`)
  else:
    case expr.kind:
      of nnkIdent, nnkStrLit:
        return expr
      else:
        result = newTree(
          expr.kind,
          toSeq(expr.children()).mapIt(it.substituteEnv(env))
        )

proc makeIterFor(v: NimNode): NimNode =
  ident("iterFor__" & $v)

type
  LoopStmtKind = enum
    lskCollect
    lskMax
    lskMin

    lskAllOf
    lskNoneOf
    lskAnyOf

  LoopStmts = object
    expression: seq[NimNode] ## Expressions of statement arguments
    kind: LoopStmtKind ## Kind of expression
    case genVal: bool ## Whether or not this expression adds value to
                      ## result type
    of true:
      tag: int
    of false:
      nil


  LoopGenConf = object
    case retTuple: bool ## Single or multiple value return
      of true:
        flds: seq[string] ## | Multiple
        ## value return. List of tuple field names along with return
        ## types.
      of false:
        rtype: NimNode ## |
        ## Single value return

    defType: Opt[NimNode] ## Default return type, if any

proc fillLoopConf(conf: var LoopGenConf, body: NimNode): void =
  matchAstRecursive(body):
  of nnkCall(`fld`, `expr`):
    conf.flds.add $fld
  of nnkCommand(`fld`, `expr`):
    echo "found lcoll"


macro loop*(arg, body: untyped): untyped =
  var conf = LoopGenConf() # IMPLEMENT determine loop configuration

  conf.fillLoopConf(body)

  result = quote do:
    1


macro loop1*(body: untyped): untyped =
  quote do:
    loop((), `body`)
