type
  PathKind = enum
    xpkChoice
    xpkSlash
    xpkPredicate

  PathElem = object
    case kind*: PathKind
      of xpkPredicate:
        predicates*: seq[PathElem]

      else:
        discard

  Path = seq[PathElem]

  NodeSet = object

proc `[]`(node: Path, idx: int): Path =
  discard

# proc eval_by_cnode_only(node: Path, context: NodeSet)
#   if {‘cp’, ‘cs’} * Relev(node):
#     for subnode in node:
#       eval_by_cnode_only(subnode, context)

#   elif node[0].kind == xpkSlash and node.len == 2:
#     # expr (N) = π
#     table(node) = eval_inner_locpath(π, X)

#   else:
#     # let expr (N ) = Op(e1 , . . . , ek);
#     for e in node.predicates:
#       eval_by_cnode_only(e, X)

#     # IMPLEMENT FIXME

#     # table(N ) := {(c, F[[Op]](r1 , . . . , rk ) | ∃c ∈ X s.t.
#     # (∀i ∈ {1, . . . , k}) (ci , ri ) ∈ table(node(ei )) holds,
#     # where ci is the projection of c to the relevant context
#     # of node(e i )};
#     # fi;


# proc evalOutermostLocpath(node: Path, context: NodeSet):
#   NodeSet {.used.} =

#   discard

#   if node[0].kind == xpkSlash and node.len == 2:
#     # if expr(N) != /path:
#     return evalOutermostLocpath(node[1], root)

#   elif node[0].kind == xpkChoice:
#     let
#       Y1 = evalOutermostLocpath(node[1], context)
#       Y2 = evalOutermostLocpath(node[2], context)

#     return Y1 * Y2

#   elif node[0].kind == xpkSlash:
#     let Y = evalOutermostLocpath(node[1], X)
#     return evalOutermostLocpath(node[2], Y)

#   elif node[0].kind == xpkPredicate:
#     var Y: NodeSet # IMPLEMENT Y := nodes reachable from X via χ :: t;
#     for predicate in node[0].predicates:
#       eval_by_cnode_only(predicate, Y)

#     var R := ∅;
#     var holds = true
#     for predicate in node[0].predicates:
#       if len({'cp', 'cs'} * Relev(predicate)) == 0:
#         discard

#       else:
#         holds = false

#     if holds:
#       for y in Y:
#         var ok = true
#         for predicate in node[0].predicates:
#           if eval_single_context(predicate, (y, '*', '*')):
#             R = R + {y}

#     else:
#       for x in X:
#         # Z := {z ∈ Y | xχz};
#       for i in 0 ..< node[0].predicates.len:
#         let Z = {z1 , . . . , zm } ordered according to axis χ;
#         /* i.e., in document order or in reverse order */
#         Z 0 := ∅;
#         for j := 1 to m:
#           if eval single context (node(ei ), hzj , j, mi) = true:
#             Z 0 := Z 0 ∪ {zj };
#         Z := Z’;
#      R := R ∪ Z;
#   return R;

# proc eval_single_context(node: Path, triple: ContextTriple) =
#   output: result value of expr (N ) for the context hcn, cp, csi.
#   if {‘cp’, ‘cs’} ∩ Relev (N ) = ∅:
#     let (c, r) ∈ table(N ) with c = proj N (hcn, cp, csi)
#     return r

#   else:
#     let expr (N ) = Op(e1 , . . . , e k );

#     for i := 1 to k:
#       eval single context (node(ei), hcn, cp, csi); od;

#     return F[[Op]](r1 , . . . , rk );


# proc eval_inner_locpath(node: Path, context: NodeSet)
# # output: table(N ) ⊆ dom × 2begin
#   if expr (N) = /π:
#     R0 := eval inner locpath(node(π), {root})
#     return {(x0, x) | x0 ∈ X ∧ (root, x) ∈ R0 }

#   elif expr (N) = π1 | π2:
#     R1 := eval_inner_locpath(node(π1), X)
#     R2 := eval_inner_locpath(node(π2), X)
#     return R1 ∪ R2 ;

#   elif expr(N) = π1 / π2:
#     R1 := eval inner locpath(node(π1), X)
#     let Y := {x | ∃x0 : (x0 , x) ∈ R1 }
#     R2 := eval inner locpath(node(π2), Y)
#     return {(x0, x) | ∃x1: (x0 , x 1) ∈ R1 ∧ (x1 , x) ∈ R2}

#   elif expr (N) = χ :: t[e1 ] . . . [eq]:
#     Y := nodes reachable from X via χ :: t;
#     for i := 1 to q
#       eval_by_cnode_only(node(ei), Y)


#     if (∀i ∈ {1, . . . , q}) ({‘cp’, ‘cs’} ∩ Relev (node(ei ))) = ∅ holds:
#       Y 0 := ∅;
#       for y in Y:
#         if ∀i ∈ {1, . . . , q}:
#           eval_single_context(node(ei), (hy, ∗, ∗i)) = true:
#             Y 0 := Y 0 ∪ {y}

#       R := {(x, y) | x ∈ X ∧ y ∈ Y 0 ∧ xχy};
#     else:
#       R := ∅;
#       for each x ∈ X:
#         Z := {z ∈ Y | xχz}
#         for i in 1 .. q:
#           let Z = {z1, . . . , zm } ordered according to axis χ

#           Z0 := ∅;
#         for j := 1 to m do
#           if eval_single_context(node(ei), (hzj , j, mi)):
#             Z0 := Z0 ∪ {zj}

#         Z := Z’
#       R := R ∪ ({x} × Z);
#   return R;


# proc eval_bottomup_path(node: Path, context: )
# input: node N in the parse tree
# expr (N ) ≡ boolean(π) or expr (N ) ≡ π RelOp s, s.t.
# π is a “bottom-up location path”, s is independent of
# the context, and s is of type nset, str, or num.
# output: The global data structure table(N ) is filled in.
# begin
#   if expr(N) = boolean(π):
#     Y := dom;
#   elif expr(N) = π RelOp s:
#     eval_by_cnode_only(node(s), {'*'})

#     if s of nset:
#       Y := {y | ∃z ∈ table(node(s)) | strval(y) RelOp strval(z)}

#     elif s of string:
#       let val denote the only element in table(node(s))
#       Y := {y | strval(y) RelOp val}

#     elif s of int:
#       let val denote the only element in table(node(s))
#       Y := {y | to number(strval(y)) RelOp val}

#   let M1 := node(π);
#   let M2 denote the node in the parse tree corresponding to the
#   X := propagate_path_backwards (Y, M1 , M2);
#   table(N ) := {(x, true) | x ∈ X}∪ {(x, false) | x ∈ (dom − X)};


# proc propagate_path_backwards() =
#   if Y = ∅:
#     return ∅

#   if location step at M2 is ‘/’ then
#     R := dom

#   elif location step at M2 is id:
#     R := F[[Op]]−1 (Y);

#   elif location step at M2 is χ :: t[e1 ] . . . [eq ]:

#     Y 0 := {y ∈ Y | node test t is true for y}
#     for i := 1 to q:
#       eval_by_cnode_only(node(ei), Y0)

#     if (∀i ∈ {1, . . . , q}) ({‘cp’, ‘cs’} ∩ Relev (node(ei ))) = ∅ holds:
#       Y00 := ∅

#       for y ∈ Y0 do:
#         if ∀i ∈ {1, . . . , q} eval_single_context(node(ei), hy, ∗, ∗i):
#           Y00 := Y00 ∪ {y};

#       R := χ−1 (Y00);

#     else:
#       X0 := χ−1 (Y0)
#       R := ∅
#       for each x ∈ X0:
#         Z := {z ∈ Y0 | xχz};
#         for i := 1 to q:
#           let Z = {z1, . . . , zm } ordered according to axis χ
#           Z0 := ∅
#           for j := 1 to m:
#             if eval single context (node(ei ), hzj , j, mi):
#               Z0 := Z 0 ∪ {zj}

#           Z := Z’
#       if Z 6= ∅ then R := R ∪ {x};
#       od;
#     fi
#   fi

#   if M1 = M2:
#     return R

#   else:
#     let M2' be the father node of M2:
#     return propagate_path_backwards(R, M1, M2')
