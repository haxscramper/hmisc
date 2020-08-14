import macros
import ../hexceptions

macro typeCondIt*(head, body: untyped): untyped =
  assert body.kind == nnkStmtList
  result = nnkWhenStmt.newTree()
  for line in body:
    case line.kind:
      of nnkBracket:
        assertNodeIt(
          line, it.len == 2, "Expected bracket with two elements")
        let
          ntype = line[0]

        result.add nnkElifBranch.newTree(
          nnkInfix.newTree(ident "is", ident "it", line[0]), line[1])
      else:
        assertNodeIt(line, false, "Expected bracket with two elements")

  result.add nnkElse.newTree(head)

  result = quote do:
    block:
      let it {.inject.} = `head`
      `result`
