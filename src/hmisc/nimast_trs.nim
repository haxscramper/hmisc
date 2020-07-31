import macros, tables, strutils, sugar, sequtils
import hmisc/[nim_trs, helpers]
import hmisc/algo/[halgorithm, htree_mapping]
export nim_trs, macros, halgorithm
import nim_trs_pprint

type
  NodeTerm = Term[NimNode, NimNodeKind]
  NodeReduction* = RedSystem[NimNode, NimNodeKind]
  NodeMatcher* = TermMatcher[NimNode, NimNodeKind]
  NodeEnv* = TermEnv[NimNode, NimNodeKind]

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

func makeNimNodeVariable*(name: VarSym): NodeTerm =
  makeVariable[NimNode, NimNodeKind](name)

func makeNimNodePlaceholder*(name: VarSym): NodeTerm =
  makePlaceholder[NimNode, NimNodeKind]()

func makeNimNodeFunctor*(sym: NimNodeKind, subt: seq[NodeTerm]): NodeTerm =
  makeFunctor(sym, subt)

func makeNimNodeConstant*(val: NimNode): NodeTerm =
  makeConstant(val, val.kind)

proc toTerm*(nt: NimNode): NodeTerm =
  nt.toTerm(nimAstImpl)

proc fromTerm*(nt: NodeTerm): NimNode =
  nt.fromTerm(nimAstImpl)

proc treeRepr*(nt: NodeTerm): string =
  treeRepr(nt, nimAstImpl)

proc buildPatternDecl(
  node: NimNode, path: seq[int],
  subt: seq[NimNode], vars: var seq[string]): NimNode =
  ## Convert pattern declaraion into pattern matcher.
  ##
  ## :params:
  ##    :node: current node to convert into pattern matchers
  ##    :path: path for current node
  ##    :subt: List of subterms for which patterns have already been
  ##           created
  ##    :vars: List of variables discovered during tree traversal.
  ##           Newfound variables are appended to it.
  # IDEA TODO write generalized version of this proc for generatic
  # patern matchers for any kind of homogenous AST wit enum as functor

  # AST to declare part of the matcher rule
  if node.kind == nnkBracket and node[0].kind == nnkBracket:
    # Nested brackets are used to annotate variables
    let content = toSeq(node[0].children())
    let varStr = newStrLitNode($content[0])
    vars.add $content[0]
    return quote do:
      makeNimNodeVariable(`varStr`)

  elif node.kind == nnkIdent and node.strVal == "_":
    # Placeholder variable
    return quote do:
      makeNimNodePlaceholder()
  else:
    # not placeholder, not variable => regular term or functor
    if node.kind == nnkCall and ($node[0])[0].isUpperAscii():
      let callSym = $node[0]
      let funcName = ident("nnk" & callSym)
      let funcEnum: NimNodeKind = parseEnum[NimNodeKind]("nnk" & callSym)

      if nimAstImpl.isFunctorSym(funcEnum): # funcEnum in functorNodes
        # Genreate matcher for functor
        let subtMatchers = newTree(nnkBracket, subt.filterIt(not it.isNil))
        return quote do:
          makeNimNodeFunctor(
            `funcName`, # Literal value for functor kind
            @`subtMatchers` # Wrap all subnode matchers into sequence
          )
      else:
        # Literal value. It is necessary to implement two-level hop
        # with values: *generate code* that will generate NimNode
        # literals.
        var termValue: NimNode
        if callSym.endsWith("Lit"):
          let nodeTyle = callSym[0..^3]
          let valueLit = quote do: 12
          let valueSym = ident "value"
          termValue = quote do:
            ((let `valueSym` = `valueLit`; newLit(value)))

        elif callSym in @["CommentStmt", "Ident", "Sym"]:
          let strLit = node[1]
          let nodeMaker = ident("new" & callSym & "Node")
          # let valueSym = ident "value"
          termValue = quote do:
            ((let valueSym = `strLit`; `nodeMaker`(`strLit`)))

        return quote do:
          makeNimNodeConstant(`termValue`)
    else:
      assert (node.kind notin {nnkStmtList}),
            "Unexpected element kind: " &
              $node.kind() & " lit: " & $node.toStrLit()


proc makePatternDecl(
  sectBody: NimNode): tuple[node: NimNode, vars: seq[string]] =
  ## Declare pattern matcher for section body
  var vars: seq[string]
  let ruleMatcherDef = sectBody.mapItDFS(
    toSeq(it.children),
    NimNode,
    buildPatternDecl(it, path, subt, vars))

  return (node: ruleMatcherDef, vars: vars)


proc makeGeneratorDecl(sectBody: NimNode, vars: seq[string]): NimNode =
  ## Declare section for value generator
  let varDecls = collect(newSeq):
    for v in vars:
      let id = ident(v)
      let strl = newLit(v)
      quote do:
        let `id`: NimNode = env[`strl`].fromTerm(nimAstImpl)

  let varStmts = varDecls.newStmtList()

  let envs = ident "env"
  result = quote do:
    block:
      proc tmp(`envs`: NodeEnv): NodeTerm =
        `varStmts`
        let res {.inject.} =
          block:
            `sectBody`

        res.toTerm(nimAstImpl)

      tmp



macro makeNodeRewriteSystem*(body: untyped): untyped =
  ## Create term rewriting system instance for nim node ast
  let rules = collect(newSeq):
    for node in body:
      if node.kind == nnkCall and node[0] == ident("rule"):
        node

  var matcherTuples: seq[NimNode]
  for rule in rules:
    let pattSection = toSeq(rule[1].children()).findItFirst(
      it[0].strVal() == "patt")[1][0]

    let (matcherDecl, varList) = makePatternDecl(pattSection)

    let genSection = toSeq(rule[1].children()).findItFirst(
      it[0].strVal() == "outp")[1].newStmtList()

    let generator = makeGeneratorDecl(genSection, varList)

    matcherTuples.add quote do:
      makeRulePair(
        makeMatcher(`matcherDecl`),
        makeGenerator(`generator`)
      )

  result = newTree(nnkBracket, matcherTuples)
  result = quote do:
    makeReductionSystem(@`result`)
