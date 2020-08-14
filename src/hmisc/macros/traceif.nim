import macros, strutils, strformat

func traceIfImpl(head, body: NimNode): NimNode =
  # TODO also trigger `case`
  # TODO use `when` for `head`
  # TODO add indentation for echo output
  case body.kind:
    of nnkIfExpr, nnkIfStmt:
      result = nnkIfExpr.newTree()
      # debugecho "found if"
      for subn in body:
        # debugecho subn.toStrLit().strVal()
        # debugecho subn.kind
        case subn.kind:
          of nnkElifBranch:
            let
              cond = subn[0]
              expr = traceIfImpl(head, subn[1])
              condpos = cond.lineInfoObj().line
              condstr = (&"\e[33m{condpos}\e[39m {cond.toStrLit().strVal()}").newLit

            result.add nnkElifExpr.newTree(
              cond,
              quote do:
                if `head`:
                  debugecho `condstr`
                `expr`
            )

          of nnkElse:
            let
              expr = traceIfImpl(head, subn[0])
              condpos = expr.lineInfoObj().line
              condstr = (&"\e[33m{condpos}\e[39m else").newLit

            result.add nnkElseExpr.newTree(
              quote do:
                if `head`:
                  debugecho `condstr`
                `expr`
            )

          else:
            discard

      # debugecho result.toStrLit().strVal()
    of nnkStmtList:
      # debugecho body.toStrLit().strVal()
      result = newTree(body.kind)
      for subn in body:
        result.add traceIfImpl(head, subn)

      # debugecho result.toStrLit().strVal()
    else:
      return body

macro traceIf*(head, body: untyped): untyped =
  result = traceIfImpl(head, body)
