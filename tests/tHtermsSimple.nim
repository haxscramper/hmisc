import hmisc/hterms

type
  Arithm = Term[string, int]
  ArithmEnv = TermEnv[string, int]
  ArithmSys = RedSystem[string, int]


let mf = makeFunctor[string, int]
let mc = makeConstant[string, int]
let mp = makePlaceholder[string, int]
let mv = makeVariable[string, int]

let t1 = Arithm()
let t2 = Arithm()
var env = ArithmEnv()
var sys = ArithmSys()

# A + 0 -> A
sys["+".mf(@[mv "A", mc 0])] = mv "A"

# A + S(B) -> S(A + B)
sys["+".mf(@[mv "A", "S".mf(@[mv "B"])])] =
  "S".mf(@["+".mf(@[mv "A", mv "B"])])

# A * 0 -> 0
sys["*".mf(@[mv "A", mc 0])] = mc 0

# A * S(B) -> A + (A * B)
sys["*".mf(@[mv "A", "S".mf(@[mv "B"])])] =
  "+".mf(@[mv "A", "+".mf(@[mv "A", mv "B"])])

let sum = "+".mf(@[
    "S".mf(@["S".mf(@[mc 0])]),
    "S".mf(@["S".mf(@[mc 0])])
  ])

echo reduce(sum, sys)
