import strutils, sequtils, strformat, sugar

import types/[hterm_buf, seq2d, hdrawing]
import helpers
import nim_trs
import hpprint

proc treeRepr*[V, F](term: Term[V, F], cb: TermImpl[V, F], depth: int = 0): string =
  let ind = "  ".repeat(depth)
  case getKind(term):
    of tkConstant:
      return cb.valStrGen(getValue(term))
        .split("\n").mapIt(ind & "cst " & it).join("\n")
    of tkPlaceholder:
      return ind & "plh _"
    of tkVariable:
      return ind & "var " & getVName(term)
    of tkFunctor:
      return (
        @[ ind & "fun " & $(getFSym(term)) ] &
        getSubt(term).mapIt(treeRepr(it, cb, depth + 1))
      ).join("\n")

proc treeRepr*[V, F](val: V, cb: TermImpl[V, F], depth: int = 0): string =
  let ind = "  ".repeat(depth)
  if cb.isFunctor(val):
    return (
      @[ ind & "fun " & $(cb.getSym(val)) ] &
      cb.getSubt(val).mapIt(treeRepr(it, cb, depth + 1))
    ).join("\n")
  else:
    return cb.valStrGen(val)
      .split("\n").mapIt(ind & "cst " & it).join("\n")

proc exprRepr*(vs: VarSym): string = "_" & vs
proc exprRepr*[V, F](term: Term[V, F], cb: TermImpl[V, F]): string =
  case term.getKind():
    of tkConstant:
      "'" & cb.valStrGen(term.getValue()) & "'"
    of tkVariable:
      "_" & $term.getVName()
    of tkFunctor:
      if ($getSym(term)).validIdentifier():
        $getSym(term) & "(" & term.getSubt().mapIt(it.exprRepr(cb)).join(", ") & ")"
      else:
        let subt = term.getSubt()
        case subt.len():
          of 1: &"{term.getSym()}({subt[0]})"
          of 2: &"{subt[0]} {term.getSym()} {subt[1]}"
          else:
            $term.getSym() & "(" & subt.mapIt(it.exprRepr(cb)).join(", ") & ")"
    of tkPlaceholder:
      "_"

proc exprReprImpl*[V, F](matchers: MatcherList[V, F], cb: TermImpl): TermBuf
proc exprRepr*[V, F](matcher: TermMatcher[V, F], cb: TermImpl[V, F]): TermBuf =
  let header = matcher.isPattern.tern(
    exprRepr(matcher.patt, cb), "proc"
  ).toTermBufFast()
  # var tmp: Seq2D[TermBuf]
  # tmp.appendRow(@[

  # ], emptyTermBuf)

  var subvars: Seq2D[TermBuf]
  for varn, subp in matcher.subpatts:
    subvars.appendRow(
      @[
        (varn & ": ").toTermBufFast(),
        subp.exprReprImpl(cb)
      ],
      emptyTermBuf
    )

  if subvars.len > 0:
    result = subvars.toTermBuf()
    result = @[@[header], @[result]].toTermBuf()
  else:
    result = header


proc exprRepr*[V, F](env: TermEnv[V, F], cb: TermImpl[V, F], forceOneLine: bool = false): string =
  if env.len > 3 and not forceOneLine:
    "{\n" & env.mapPairs(&"  {lhs.exprRepr()} -> {rhs.exprRepr(cb)}").joinl() & "\n}"
  else:
    "{" & env.mapPairs(
      &"({lhs.exprRepr()} -> {rhs.exprRepr(cb)})"
    ).join(" ") & "}"

proc exprReprImpl*[V, F](matchers: MatcherList[V, F], cb: TermImpl): TermBuf =
  var blocks: Seq2D[TermBuf]
  for idx, patt in matchers.patterns:
    let pref: string = if matchers.patterns.len == 1: "" else: $idx & ": "
    let bufs = @[pref.toTermBufFast(), patt.exprRepr(cb)]
    blocks.appendRow(bufs, emptyTermBuf)

  return blocks.toTermBuf()

proc exprReprImpl*[V, F](rule: RulePair[V, F], cb: TermImpl[V, F]): seq[TermBuf] =
  let rhs: TermBuf = rule.gen.isPattern.tern(
    exprRepr(rule.gen.patt),
    "proc"
  ).toTermBufFast()

  return @[ rule.matchers.exprReprImpl(cb), (" ~~> ").toTermBufFast(), rhs ]

proc exprRepr*[V, F](rule: RulePair[V, F], cb: TermImpl[V, F]): string =
  @[exprReprImpl(rule, cb)].toTermBuf().toString()

proc exprReprImpl*[V, F](sys: RedSystem[V, F], cb: TermImpl[V, F]): TermBuf =
  sys.mapPairs(
    @[ ($idx & ": ").toTermBufFast() ] & rhs.exprReprImpl(cb)
  ).toTermBuf()

proc exprRepr*[V, F](sys: RedSystem[V, F], cb: TermImpl[V, F]): string =
  exprReprImpl(sys, cb).toString().split("\n").mapIt(it.strip(leading = false)).join("\n")
