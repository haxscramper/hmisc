## Term algorithms. Implmenetation uses callback functions for getting
## values/types from terms.

import hashes, sequtils, tables, strformat, strutils, sugar
import options
import deques, intsets, sets
export tables, intsets

import types/[htrie, hprimitives]
import algo/[halgorithm, hseq_mapping, htree_mapping]
import helpers
import hpprint

type
  InfoException = ref object of CatchableError

  GenException*[T] = ref object of InfoException
    info*: T


proc raiseGenEx[T](msg: string, info: T): void =
  var tmp = new GenException[T]
  tmp.msg = msg & " [generic exception, T is `" & $typeof(T) & "`]"
  tmp.info = info
  raise tmp

template getGEx*[T](): untyped = cast[GenException[T]](getCurrentException())

type
  TermKind* = enum
    tkVariable
    tkFunctor
    tkConstant
    tkPlaceholder

  VarSym* = string
  VarSet* = HashSet[VarSym]

  SubstitutionErrorInfo* = object
    path*: TreePath
    case kind*: TermKind:
      of tkVariable:
        vname*: VarSym
      else:
        discard



  Term*[V, F] = object
    case tkind*: TermKind
      of tkFunctor:
        functor: F
        subterms: seq[Term[V, F]]
      of tkConstant:
        csym: F
        value: V
      of tkVariable:
        name: VarSym
      of tkPlaceholder:
        nil


  TermImpl*[V, F] = object
    ##[

Callback procs for concrete types.

'Implementation' of certain actions for concrete types `V` and `F`.

## Example

Example for `NimNode` and `NimNodeKind`

.. code-block:: nim

    func isFunctor*(nnk: NimNodeKind): bool =
      nnk notin {
        nnkNone, nnkEmpty, nnkNilLit, # Empty node
        nnkCharLit..nnkUInt64Lit, # Int literal
        nnkFloatLit..nnkFloat64Lit, # Float literal
        nnkStrLit..nnkTripleStrLit, nnkCommentStmt, nnkIdent, nnkSym # Str lit
      }

    const nimAstImpl* = TermImpl[NimNode, NimNodeKind](
      getsym: (proc(n: NimNode): NimNodeKind = n.kind),
      isFunctorSym: (proc(kind: NimNodeKind): bool = kind.isFunctor()),
      makeFunctor: (
        proc(op: NimNodeKind, sub: seq[NimNode]): NimNode =
          if sub.len == 0: newNimNode(op)
          else: newTree(op, sub)
      ),
      getSubt: (proc(n: NimNode): seq[NimNode] = toSeq(n.children)),
      valStrGen: (proc(n: NimNode): string = $n.toStrLit()),
    )

    ]##
    isFunctorSym*: proc(val: F): bool
    getSubt*: proc(val: V): seq[V]
    getSym*: proc(val: V): F
    makeFunctor*: proc(sym: F, subt: seq[V]): V
    valStrGen*: proc(val: V): string ## Conver value to string.

  TermEnv*[V, F] = object
    ## Mapping between variable symbols and values
    values*: Table[VarSym, Term[V, F]]

  GenProc*[V, F] = proc(env: TermEnv[V, F]): Term[V, F] ## Proc for
  ## generaing Values during rewriting.
  DefaultGenProc*[V, F] = proc(env: TermEnv[V, F]): TermEnv[V, F] ## Generate
  ## default values for variables

  MatchProc*[V, F] = proc(test: Term[V, F]): Option[TermEnv[V, F]]

  TermGenerator*[V, F] = object
    case isPattern*: bool
      of true:
        patt*: Term[V, F]
      of false:
        gen*: GenProc[V, F]

  MatcherList*[V, F] = object
    first: Table[F, seq[int8]]
    forceTry: seq[int8] ## List of matchers to always try
    patterns*: seq[TermMatcher[V, F]]
    # default: DefaultGenProc[V, F]

  PattList*[V, F] = Table[VarSym, MatcherList[V, F]]

  TermMatcher*[V, F] = object
    case isPattern*: bool
      of true:
        patt*: Term[V, F]
      of false:
        matcher*: MatchProc[V, F]

    subpatts*: Table[VarSym, MatcherList[V, F]] ## Submatches
    ## on generated variables
    varlist: VarSet ## List of generated variables
    # `varlist` can be automatically genrated for pattern-based term
    # matchers. If matcher proc is used user should supply list of
    # variable names.
    default: DefaultGenProc[V, F] ## Generate missing variables not produced
    ## by subpattern matches
    optVars: VarSet ## Optional variables - `default` is not required
                    ## to generate them.


  RulePair*[V, F] = object
    matchers*: MatcherList[V, F]
    # rules*: seq[TermMatcher[V, F]] # List of patterns/matchers
    # first: Table[F, seq[int8]] # Functor -> Possible pattern
    # matchers: seq[int8] # List of matchers
    gen*: TermGenerator[V, F] # Proc to generate final result

  RuleId* = int16
  RedSystem*[V, F] = object
    first: Table[F, seq[RuleId]]
    matchers: set[RuleId] # Matcher procs - always have to try
    rules: seq[RulePair[V, F]]

#=====================  reduction constraint types  ======================#

type
  ReduceConstraints* = enum
    rcNoConstraints
    rcRewriteOnce
    rcApplyOnce

  ReductionState = object
    rewPaths: Trie[int, set[RuleId]]
    constr: ReduceConstraints
    maxDepth: int


proc registerUse(rs: var ReductionState, path: TreePath, id: RuleId): void =
  case rs.constr:
    of rcApplyOnce:
      if path notin rs.rewPaths:
        var tmp: set[RuleId]
        rs.rewPaths[path] = tmp

      rs.rewPaths[path].incl id
    of rcRewriteOnce:
      var tmp: set[RuleId]
      rs.rewPaths[path] = tmp

    else:
      discard

proc canRewrite(rs: ReductionState, path: TreePath): bool =
  path.len < rs.maxDepth and not (
    # Avoid rewriting anyting on this path
    rs.constr == rcRewriteOnce and
    rs.rewPaths.prefixHasValue(path)
  )


proc cannotUse(rs: ReductionState, path: TreePath, rule: RuleId): bool =
    # Avoid using this rule again on the same path
    (rs.constr == rcApplyOnce) and
    (rs.rewPaths.prefixHasValue(path)) and
    toSeq(rs.rewPaths.prefixedValues(path)).anyOfIt(rule in it)

#==========================  making new terms  ===========================#


func makePlaceholder*[V, F](): Term[V, F] =
  Term[V, F](tkind: tkPlaceholder)

func makeConstant*[V, F](val: V, csym: F): Term[V, F] =
  Term[V, F](tkind: tkConstant, value: val, csym: csym)

func makeVariable*[V, F](name: VarSym): Term[V, F] =
  Term[V, F](tkind: tkVariable, name: name)

func makeFunctor*[V, F](
  sym: F, subt: seq[Term[V, F]]): Term[V, F] =
  Term[V, F](tkind: tkFunctor, functor: sym, subterms: subt)

#======================  accessing term internals  =======================#

func getKind*[V, F](t: Term[V, F]): TermKind =
  t.tkind

func getVName*[V, F](t: Term[V, F]): VarSym =
  assert t.getKind() == tkVariable
  t.name

func getFSym*[V, F](t: Term[V, F]): F =
  assert t.getKind() == tkFunctor
  t.functor

func getSym*[V, F](t: Term[V, F]): F =
  case t.getKind():
    of tkFunctor: t.functor
    of tkConstant: t.csym
    else:
      raiseAssert(
        "Invalid term kind - cannot get symbol from " & $t.getKind())

func getNth*[V, F](
  t: Term[V, F], idx: int): Term[V, F]=
  assert t.getKind() == tkFunctor
  t.subterms[idx]

func getNthMod*[V, F](
  t: var Term[V, F], idx: int): var Term[V, F]=
  assert t.getKind() == tkFunctor
  t.subterms[idx]

func getSubt*[V, F](
  t: Term[V, F]): seq[Term[V, F]] =
  assert t.getKind() == tkFunctor
  t.subterms

func setSubt*[V, F](
  t: var Term[V, F], subt: seq[Term[V, F]]): void =
  assert t.getKind() == tkFunctor
  t.subterms = subt

func getValue*[V, F](self: Term[V, F]): V =
  assert self.getKind() == tkConstant
  self.value

#======================  checking possible matches  ======================#

func hasForceTry[V, F](rule: RulePair[V, F]): bool =
  rule.matchers.forceTry.len > 0

#=======================  converting to/from term  =======================#

proc isFunctor*[V, F](cb: TermImpl[V, F], val: V): bool =
 cb.isFunctorSym(cb.getSym(val))

proc toTerm*[V, F](val: V, cb: TermImpl[V, F]): Term[V, F] =
  if cb.isFunctor(val):
    return makeFunctor[V, F](cb.getSym(val), cb.getSubt(val).mapIt(it.toTerm(cb)))
  else:
    return makeConstant[V, F](val, cb.getSym(val))

proc fromTerm*[V, F](
  term: Term[V, F], cb: TermImpl[V, F], path: TreePath = @[0]): V =
  if term.getKind() notin {tkFunctor, tkConstant}:
    raiseGenEx(
      "Cannot convert under-substituted term back to tree. " &
      $term.getKind() & " has to be replaced with value",
      case term.getKind():
        of tkVariable:
          SubstitutionErrorInfo(
            path: path,
            kind: tkVariable,
            vname: term.getVName()
          )
        else:
          SubstitutionErrorInfo(path: path)
    )

  if term.getKind() == tkFunctor:
    result = cb.makeFunctor(
      term.getFSym(),
      term.getSubt().mapPairs(rhs.fromTerm(cb, path & @[lhs])))
  else:
    result = term.getValue()

#==================================  2  ==================================#
proc assertCorrect*[V, F](impl: TermImpl[V, F]): void =
  ## Check if all fields in `impl` have been correctly initalized
  for name, value in impl.fieldPairs():
    assert (not value.isNil()), name & " cannot be nil"

func makeMatcherList*[V, F](matchers: seq[TermMatcher[V, F]]): MatcherList[V, F] =
  result.patterns = matchers

  for id, rule in matchers:
    case rule.isPattern:
      of true:
        result.first.mgetOrPut(rule.patt.getSym(), @[]).add id.int8
      of false: result.forceTry.add id.int8


func makeRulePair*[V, F](
  rules: seq[TermMatcher[V, F]], gen: TermGenerator[V, F]): RulePair[V, F] =
  result.gen = gen
  result.matchers = makeMatcherList(rules)


func varlist*[V, F](term: Term[V, F], path: TreePath = @[0]): seq[(Term[V, F], TreePath)] =
  ## Output list of all variables in term
  case getKind(term):
    of tkConstant, tkPlaceholder:
      return @[]
    of tkVariable:
      return @[(term, path)]
    of tkFunctor:
      for idx, sub in getSubt(term):
        result &= sub.varlist(path & @[idx])

func makeMatcher*[V, F](matcher: MatchProc[V, F]): TermMatcher[V, F] =
  ## Create term matcher instance for matching procs
  TermMatcher[V, F](isPattern: false, matcher: matcher)

func makeMatcher*[V, F](patt: Term[V, F]): TermMatcher[V, F] =
  TermMatcher[V, F](isPattern: true, patt: patt)

func mergeVarlists[V, F](matchers: PattList[V, F]): VarSet =
  ## Combine sets of exported variables from multiple matchers
  if matchers.len > 0:
    return matchers.mapPairs(
      rhs.patterns.mapIt(it.varlist).foldl(union(a, b))
    ).foldl(union(a, b))

func getExportedVars[V, F](patt: TermMatcher[V, F]): VarSet =
  if patt.isPattern:
    for (v, path) in patt.patt.varlist():
      result.incl v.name

func getExportedVars*[V, F](patts: PattList[V, F]): VarSet =
  ## Get list of variables that might be generated by patters in `patts`
  result = mergeVarlists(patts)
  for vars, matcherList in patts:
    for patt in matcherList.patterns:
      result.incl patt.getExportedVars()

func exportedVars*[V, F](matcher: TermMatcher[V, F]): VarSet = matcher.varlist

func toPattList*[V, F](patts: varargs[(VarSym, Term[V, F])]): PattList[V, F] =
  toTable(patts.mapPairs((lhs, makeMatcherList(@[ rhs.makeMatcher() ]))))

func makeRulePair*[V, F](
  rule: TermMatcher[V, F], gen: TermGenerator[V, F]): RulePair[V, F] =
  ## Create rule pair insance
  makeRulePair(@[rule], gen)

func setSubpatterns*[V, F](
  matcher: var TermMatcher[V, F],
  subpatts: PattList[V, F],
  default: DefaultGenProc[V, F],
  optVars: seq[VarSym]): void =
  matcher.default = default
  matcher.subpatts = subpatts
  matcher.varlist.incl getExportedVars(matcher)
  matcher.varlist.incl getExportedVars(subpatts)
  matcher.optVars = optVars.toSet()

func makeMatcher*[V, F](
  matcher: MatchProc[V, F],
  subpatt: PattList[V, F],
  default: GenProc[V, F],
  optVars: seq[VarSym] = @[]): TermMatcher[V, F] =
  ## Create term matcher instance for matching procs
  result = TermMatcher[V, F](isPattern: false, matcher: matcher)
  result.setSubpatterns(subpatt, default, optVars)

func makeMatcher*[V, F](
  patt: Term[V, F],
  subpatt: PattList[V, F],
  default: DefaultGenProc[V, F],
  optVars: seq[VarSym] = @[]): TermMatcher[V, F] =
  result = TermMatcher[V, F](isPattern: true, patt: patt)
  result.setSubpatterns(subpatt, default, optVars)


func makeMatcher*[V, F](
  matchers: seq[TermMatcher[V, F]],
  default: DefaultGenProc[V, F],
  optVars: seq[VarSym] = @[]): TermMatcher[V, F] =
  ## Create new toplevel term matcher from multiple smaller ones. New
  ## variable `auxToplevelCatchall` is introduced.
  let catchallName = "auxToplevelCatchall"
  makeMatcher(
    makeVariable[V, F](catchallName),
    {
      catchallName : makeMatcherList(matchers)
    }.toTable(),
    default,
    optVars
  )


func makeGenerator*[V, F](obj: Term[V, F]): TermGenerator[V, F] =
  ## Create closure proc that will output `obj` as value
  TermGenerator[V, F](isPattern: true, patt: obj)

func makeGenerator*[V, F](obj: GenProc[V, F]): TermGenerator[V, F] =
  ## Create closure proc that will output `obj` as value
  TermGenerator[V, F](isPattern: false, gen: obj)

func makeEnvironment*[V, F](values: seq[(VarSym, Term[V, F])] = @[]): TermEnv[V, F] =
  ## Create new environment using `values` as initial binding values
  TermEnv[V, F](values: values.toTable())

func makeReductionSystem*[V, F](
  rules: seq[RulePair[V, F]]): RedSystem[V, F] =
  result.rules = rules
  for id, rule in rules:
    for fsym, _ in rule.matchers.first:
      # Try pattern only if first functor matches
      result.first.mgetOrPut(fsym, @[]).add RuleId(id)

    if rule.hasForceTry():
      # Always call matcher procs
      result.matchers.incl RuleId(id)

  for fsym, rules in result.first:
    result.first[fsym] = rules.deduplicate()



func isBound*[V, F](env: TermEnv[V, F], term: VarSym): bool =
  ## Check if variable is bound to somethin in `env`
  (term in env.values) # and env[term] != term

func `[]`*[V, F](e: TermEnv[V, F], t: VarSym): Term[V, F] =
  ## Access value from environment.
  try:
    e.values[t]
  except KeyError:
    # TODO check if `VarSym` can be converter to string
    # TODO use define to constrol exception verbosity
    let vars = e.mapPairs($lhs).joinq()
    raise newException(
      KeyError,
      &"Missing variable `{t}` in environment. Have vars: {vars}")

func `[]=`*[V, F](system: var RedSystem[V, F], lhs, rhs: Term[V, F]): void =
  ## Add rule to environment
  system.rules[lhs] = rhs

func `[]=`*[V, F](env: var TermEnv[V, F], variable: VarSym, value: Term[V, F]): void =
  ## Set value for variable in environemt
  env.values[variable] = value


func `==`*[V, F](lhs, rhs: Term[V, F]): bool =
  lhs.tkind == rhs.tkind and (
    case lhs.tkind:
      of tkConstant: lhs.value == rhs.value
      of tkVariable: lhs.name == rhs.name
      of tkFunctor: lhs.functor == rhs.functor and subnodesEq(lhs, rhs, subterms)
      of tkPlaceholder: true
  )

iterator items*[V, F](system: RedSystem[V, F]): RulePair[V, F] =
  ## Iterate over all rules in rewriting system
  for pair in system.rules:
    yield pair

iterator pairs*[V, F](system: RedSystem[V, F]): (RuleId, RulePair[V, F]) =
  ## Iterate over all rules with their indices in rewriting system
  for idx, pair in system.rules:
    yield (RuleId(idx), pair)


iterator pairs*[V, F](env: TermEnv[V, F]): (VarSym, Term[V, F]) =
  ## Iterate over all variables and values in evnironment
  for lhs, rhs in pairs(env.values):
    yield (lhs, rhs)

func contains*[V, F](env: TermEnv[V, F], vsym: VarSym): bool =
  vsym in env.values

func hasAll*[V, F](env: TermEnv[V, F], varlist: VarSet): bool =
  result = true
  for vname in varlist:
    if vname notin env:
      return false

func missingVars*[V, F](env: TermEnv[V, F], varlist: VarSet): VarSet =
  for vname in varlist:
    if vname notin env:
      result.incl vname

func varlist*[V, F](env: TermEnv[V, F]): VarSet =
  for vname, _ in env:
    result.incl vname

func len*[V, F](env: TermEnv[V, F]): int =
  ## Get number of itesm in enviroenmt
  env.values.len()

func bindTerm[V, F](
  variable, value: Term[V, F], env: TermEnv[V, F], ): TermEnv[V, F]

func copy*[V, F](term: Term[V, F], env: TermEnv[V, F]): (Term[V, F], TermEnv[V, F]) =
  ## Create copy of a term. All variables are replaced with new ones.
  # DOC what is returned?
  let inputEnv = env
  case getKind(term):
    of tkConstant:
      return (term, inputEnv)
    of tkVariable:
      let deref = term.dereference(env)
      if getKind(deref) == tkVariable:
        var newVar = term
        # inc newVar.genIdx
        var resEnv = bindTerm(deref, newVar, env)
        return (newVar, resEnv)
      else:
        return (deref, inputEnv)

    of tkFunctor:
      var resEnv = env
      var subterms: seq[Term[V, F]]
      for arg in getSubt(term):
        let (tmpArg, tmpEnv) = arg.copy(resEnv)
        resEnv = tmpEnv
        subterms.add tmpArg

      return (makeFunctor(getFSym(term), subterms), resEnv)

    of tkPlaceholder:
      return (term, inputEnv)

func bindTerm[V, F](variable, value: Term[V, F], env: TermEnv[V, F]): TermEnv[V, F] =
  ## Create environment where `variable` is bound to `value`
  result = env
  case getKind(value):
    of tkConstant, tkVariable, tkPlaceholder:
      result[getVName(variable)] = value
    of tkFunctor:
      let (newTerm, newEnv) = value.copy(env)
      result = newEnv
      result[getVName(variable)] = newTerm

func dereference*[V, F](
  term: Term[V, F], env: TermEnv[V, F], ): Term[V, F]=
  ## Traverse binding chain in environment `env` and return value of
  ## the `term`
  result = term

  while getKind(result) == tkVariable and isBound(env, getVName(result)):
    let value = env[getVName(result)]
    if getKind(value) == tkConstant or value == result:
      result = value
      break

    result = value

func unif*[V, F](
  t1, t2: Term[V, F],
  env: TermEnv[V, F] = makeEnvironment[V, F]()): Option[TermEnv[V, F]] =
  ## Attempt to unify two terms. On success substitution (environment)
  ## is return for which two terms `t1` and `t2` could be considered
  ## equal.
  let
    val1 = dereference(t1, env)
    val2 = dereference(t2, env)
    k1 = getKind(val1)
    k2 = getKind(val2)

  if k1 == tkConstant and k2 == tkConstant:
    if val1 == val2:
      return some(env)
    else:
      return none(TermEnv[V, F])
  elif k1 == tkVariable:
    return some(bindTerm(val1, val2, env))
  elif k2 == tkVariable:
    return some(bindTerm(val2, val1, env))
  elif (k1, k2) in @[(tkConstant, tkFunctor), (tkFunctor, tkConstant)]:
    return none(TermEnv[V, F])
  else:
    var tmpRes = env
    if getFSym(val1) != getFSym(val2):
      return none(TermEnv[V, F])

    if getSubt(val1).len != getSubt(val2).len:
      # TEST with different-sized term unification
      # TODO provide `reason` for failure
      return none(TermEnv[V, F])

    for idx, (arg1, arg2) in zip(getSubt(val1), getSubt(val2)):
      let res = unif(arg1, arg2, tmpRes)
      if res.isSome():
        tmpRes = res.get()
      else:
        return none(TermEnv[V, F])

    return some(tmpRes)

iterator redexes*[V, F](
  term: Term[V, F], ): tuple[red: Term[V, F], path: TreePath] =
  ## Iterate over all redex in term
  var que: Deque[(Term[V, F], TreePath)]
  que.addLast((term, @[0]))
  while que.len > 0:
    let (nowTerm, path) = que.popFirst()
    if getKind(nowTerm) == tkFunctor:
      for idx, subTerm in getSubt(nowTerm):
        que.addLast((subTerm, path & @[idx]))

    yield (red: nowTerm, path: path)


proc setAtPath*[V, F](term: var Term[V, F], path: TreePath, value: Term[V, F]): void =
  case getKind(term):
    of tkFunctor:
      if path.len == 1:
        term = value
      else:
        setAtPath(
          term = getNthMod(term, path[1]),
          path = path[1 .. ^1],
          value = value)
    of tkVariable:
      term = value
    of tkPlaceholder:
      assert false, "Cannot assign to placeholder: " & $term & " = " & $value
    of tkConstant:
      term = value
      # assert false, "Cannot assign to constant: " & $term & " = " & $value

proc substitute*[V, F](term: Term[V, F], env: TermEnv[V, F]): Term[V, F] =
  ## Substitute all variables in term with their values from environment
  result = term
  for (v, path) in term.varlist():
    if env.isBound(getVName(v)):
      result.setAtPath(path, v.dereference(env))


proc mergeEnv*[V, F](env: var TermEnv[V, F], other: TermEnv[V, F]): void =
  for vsym, value in other:
    if vsym notin env:
      env[vsym] = value

proc match*[V, F](
  redex: Term[V, F],
  matchers: MatcherList[V, F]): Option[TermEnv[V, F]]

proc match*[V, F](redex: Term[V, F], matcher: TermMatcher[V, F]): Option[TermEnv[V, F]] =
  # NOTE actually I might have to use full-blown backtracking here:
  # each rule might have one or more nested terms. Rule pair is
  # already almost like a clause. The only difference is (1)
  # additional baggage in form of generator and (2) support for
  # matches that are not terms.
  let unifRes =
    if matcher.isPattern:
      unif(matcher.patt, redex)
    else:
      matcher.matcher(redex)

  if unifRes.isSome():
    var res: TermEnv[V, F] = unifRes.get()
    for vname, submatch in matcher.subpatts:
      #[ IMPLEMENT check if variable is actually present in environment ]#
      #[ IMPLEMENT check if variable generated from submatch does not overide any of
                   already existing variables
      ]#
      let submRes = match(res[vname], submatch)
      if submRes.isSome():
        res.mergeEnv(submRes.get())

    mixin difference
    if not(res.hasAll(difference(matcher.varlist, matcher.optVars))):
      if matcher.default == nil:
        raiseAssert(msgjoin(
          "Cannot get default value for variables:", res.missingVars(
            matcher.varlist - matcher.optVars
          ), " `default` callback for term matcher is nil"
        ))

      let default: TermEnv[V, F] = matcher.default(res)
      for vname in matcher.varlist:
        if (vname notin res) and (vname notin matcher.optVars):
          if vname notin default:
            raiseAssert(msgjoin(
              "Variable '", vname, "' is missing from default environment. Need to get",
              "values for variables:", res.missingVars(matcher.varlist - matcher.optVars),
              ", but default enviornment provides:", default.varlist()
            ))

          res[vname] = default[vname]

    return some(res)

proc match*[V, F](
  redex: Term[V, F],
  matchers: MatcherList[V, F]): Option[TermEnv[V, F]] =
  for id in matchers.first[redex.getSym()] & matchers.forceTry:
    result = match(redex, matchers.patterns[id])
    if result.isSome():
      break

proc apply*[V, F](redex: Term[V, F], rule: RulePair[V, F]): Option[TermEnv[V, F]] =
  ## Match pattern from `rule` with `redex` and return unification
  ## environment.
  return match(redex, rule.matchers)
  # for id in rule.first[redex.getSym()] & rule.matchers:
  #   return match(redex, rule.rules[id])

iterator possibleMatches*[V, F](system: RedSystem[V, F], redex: Term[V, F]): RuleId =
  let fsym = redex.getSym()
  for pattId in system.first.getOrDefault(fsym, @[]):
    yield pattId

  for pattId in system.matchers:
    yield pattId

proc findApplicable*[V, F](
  system: RedSystem[V, F],
  redex: Term[V, F],
  rs: ReductionState,
  path: TreePath # REVIEW is it necessary?
                         ): Option[(RuleId, TermEnv[V, F], RulePair[V, F])] =
  ## Return first rule in system that can be applied for given `redex`
  ## and unification environment under which rule matches with pattern
  ## in rule.
  for id in system.possibleMatches(redex):
    let rule = system.rules[id]
    let env = redex.apply(rule)
    if env.isSome():
      return some((id, env.get(), rule))

proc generate*[V, F](rule: RulePair[V, F], env: TermEnv[V, F]): Term[V, F] =
  ## Apply generator in `rule` using environment `env`
  if rule.gen.isPattern:
    rule.gen.patt.substitute(env)
  else:
    rule.gen.gen(env)

proc getNthRule*[V, F](system: RedSystem[V, F], idx: int): RulePair[V, F] =
  system.rules[idx]

template reductionTriggersBFS*[V, F](
  redex: Term[V, F], system: RedSystem[V, F], body: untyped): untyped =

  var rs = ReductionState()
  iterateItBFS(redex, it.getSubt(), it.getKind() == tkFunctor):
    let rule = system.findApplicable(it, rs, emptyTreePath)
    if rule.isSome():
      let (ruleId {.inject.}, env {.inject.}, rulePair {.inject.}) = rule.get()
      block:
        body


template reductionTriggersDFS*[V, F](
  redex: Term[V, F], system: RedSystem[V, F], body: untyped): untyped =

  var rs = ReductionState()
  iterateItDFS(redex, it.getSubt(), it.getKind() == tkFunctor):
    let rule = system.findApplicable(it, rs, emptyTreePath)
    if rule.isSome():
      let (ruleId {.inject.}, env {.inject.}, rulePair {.inject.}) = rule.get()
      block:
        body

template matchPattern*[V, F](
  redex: Term[V, F], matcher: TermMatcher[V, F], body: untyped): untyped =
  let env {.inject.} = redex.match(matcher)
  block:
    body


template matchPattern*[V, F](
  redex: Term[V, F], matchers: MatcherList[V, F], body: untyped): untyped =
  let env {.inject.} = redex.match(matchers)
  block:
    body

template matchPattern*[V, F](
  redex: Term[V, F], sys: RedSystem[V, F], body: untyped): untyped =
  let rule {.inject.} = redex.match(matcher)
  block:
    body

proc reduce*[V, F](
  term: Term[V, F],
  system: RedSystem[V, F],
  maxDepth: int = 40,
  reduceConstraints: ReduceConstraints = rcApplyOnce
                ): tuple[term: Term[V, F], ok: bool, rewPaths: Trie[int, set[RuleId]]] =
  ##[

Perform reduction of `term` using `system` rules.

Iterate over all subterms (redexes) in `term` and try each reduction
rule in `system`. If rule matches, replace subterm with output of the
rule value generator.

## Parameters

:term: term to reduce
:system: collection of rules (matcher - generator pairs)
:cb: implementation callbacks for term
:maxDepth: do not reduce terms deeper than this value
:reduceConstaints: Configuration for continous application of
                   reduction rules on the same paths.
   - **rcNoConstaints** Reduce as long as reduction can take
     place
   - **rcRewriteOnce** do not rewrite subterm or any of it's
     descendants after it has been reduced once
   - **rcApplyOnce** do not use the same rule on term or any of
     it's descendants after rule has been applied once. Reduction
     of the term (or descendants) might still take place but
     using different rules.

  ]##
  var term = term
  var rs = ReductionState(constr: reduceConstraints, maxDepth: maxDepth)
  defer:
    result.rewPaths = rs.rewPaths

  var canReduce = true
  while canReduce:
    canReduce = false
    for (redex, path) in term.redexes():
      let rule = system.findApplicable(redex, rs, path)
      if rule.isSome():
        canReduce = true
        result.ok = true
        let (idx, env, rule) = rule.get()
        let genRes = rule.generate(env).substitute(env)
        term.setAtPath(path, genRes)

  result.term = term
