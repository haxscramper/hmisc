import macros

macro where(expr, body: untyped): untyped =
  result = quote do:
    block:
      `body`
      `expr`

  echo result.toStrLit()

when isMainModule:
  block:
    let a = where(12 + 3 + a):
      let a = 74

  block:
    let b = (12 + 3 + a).where:
      let a = 74
