import unittest
import hmisc/[nim_trs]
import hmisc/algo/halgorithm
import strutils, sequtils, strformat, sugar, options

type
  ArithmOp = enum
    aopVal

    aopSucc
    aopAdd
    aopMult

  Arithm = object
    case tsym: ArithmOp
      of aopVal:
        tval: int
      else:
        tsubt: seq[Arithm]

proc `==`(lhs, rhs: Arithm): bool =
  lhs.tsym == rhs.tsym and (
    case lhs.tsym :
      of aopVal: lhs.tval == rhs.tval
      else:
        lhs.tsym == rhs.tsym and
        zip(lhs.tsubt, rhs.tsubt).allOfIt(it[0] == it[1])
  )

proc `$`*(term: Arithm): string =
  case term.tsym:
    of aopVal:
      "'" & $term.tval & "'"
    else:
      let symName =
        case term.tsym:
          of aopSucc: "S"
          of aopAdd: "+"
          of aopMult: "*"
          else: ""

      if ($term.tsym).validIdentifier():
        symName & "(" & term.tsubt.mapIt($it).join(", ") & ")"
      else:
        case term.tsubt.len():
          of 1: &"{symName}({term.tsubt[0]})"
          of 2: &"{term.tsubt[0]} {symName} {term.tsubt[1]}"
          else:
            symName & "(" & term.tsubt.mapIt($it).join(", ") & ")"

type ATerm = Term[Arithm, ArithmOp]

func nOp(op: ArithmOp, subt: seq[ATerm]): ATerm =
  makeFunctor[Arithm, ArithmOp](op, subt)

func nVar(n: string): ATerm =
  makeVariable[Arithm, ArithmOp](n)

func nConst(n: int): ATerm =
  makeConstant[Arithm, ArithmOp](Arithm(tsym: aopVal, tval: n), aopVal)

func mkOp(op: ArithmOp, sub: seq[Arithm]): Arithm =
  case op:
    of aopVal:
      discard
    else:
      return Arithm(tsym: op, tsubt: sub)

func mkVal(val: int): Arithm =
  Arithm(tsym: aopVal, tval: val)


suite "Hterms callback/arithmetic":
  test "Arithmetic addition":

    let cb = TermImpl[Arithm, ArithmOp](
      getSym: (proc(n: Arithm): ArithmOp = n.tsym),
      isFunctorSym: (proc(n: ArithmOp): bool = (n != aopVal)),
      makeFunctor: (
        proc(op: ArithmOp, sub: seq[Arithm]): Arithm =
          result = Arithm(tsym: op)
          result.tsubt = sub
      ),
      getSubt: (proc(n: Arithm): seq[Arithm] = n.tsubt),
      valStrGen: (proc(n: Arithm): string = "[[ TODO ]]"),
    )

    assertCorrect(cb)

    let rSystem = makeReductionSystem(
      # NOTE this madness is intended to be generated from some kind of
      # DSL, not written by hand.
      @[
        # A + 0 -> A
        makeRulePair(
          nOp(aopAdd, @[nVar("A"), nConst(0)]).makeMatcher(),
          nVar("A").makeGenerator()
        ),

        # A + S(B) -> S(A + B)
        makeRulePair(
          nOp(aopAdd, @[
            nVar("A"), nOp(aopSucc, @[ nVar("B") ]) ]).makeMatcher(),
          nOp(aopSucc, @[
            nOp(aopAdd, @[ nVar("A"), nVar("B") ]) ]).makeGenerator()
        ),

        # A * 0 -> 0
        makeRulePair(
          nOp(aopMult, @[ nVar("A"), nConst(0) ]).makeMatcher(),
          nConst(0).makeGenerator()
        ),

        # A * S(B) -> A + (A * B)
        makeRulePair(
          nOp(aopMult, @[
            nVar("A"), nOp(aopSucc, @[ nVar("B") ])]).makeMatcher(),
          nOp(aopAdd, @[
            nVar("A"),
            nOp(aopMult, @[ nVar("A"), nVar("B") ])]).makeGenerator()
        )
      ]
    )

    let res = reduce(
      # S(0) + S(0)
      mkOp(aopAdd, @[
        mkOp(aopSucc, @[ mkOp(aopSucc, @[ mkVal(0) ]) ]),
        mkOp(aopSucc, @[ mkOp(aopSucc, @[ mkVal(0) ]) ])
      ]).toTerm(cb),
      rSystem,
      reduceConstraints = rcNoConstraints
    )

    echo $res[0].fromTerm(cb)
    assert "S(S(S(S('0'))))" == $res[0].fromTerm(cb)
