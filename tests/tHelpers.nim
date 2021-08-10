import std/[strformat, options]

import
  hmisc/preludes/unittest,
  hmisc/core/all,
  hmisc/algo/[htemplates, halgorithm],
  hmisc/macros/iflet

func empty[T](): seq[T] = discard

suite "Misc helper functions":
  test "Split list":
    expect ArgumentError:
      discard empty[int]().splitList()

    check @[1].splitList() == (1, empty[int]())
    check @[1, 2].splitList() == (1, @[2])

  test "{dedent} :proc:value:":
    check "  a".dedent == "a"
    check "A\n  a".dedent == "A\n  a"

  test "{enclosedIn} :proc:value:":
    check "-+-".enclosedIn(("-", "-"))
    check "-+-".enclosedIn("-")
    check not "+".enclosedIn("-")

  test "{enumerate} :template:value:":
    check @["cat", "dog"].enumerate() == @[(0, "cat"), (1, "dog")]

  test "{join*} string joining functions":
    check @["1", "2"].joinq(", ") == "\"1\", \"2\""
    check @["1", "2"].joinl() == "1\n2"
    check @["1", "2"].joinw() == "1 2"

  test "{tern} :template:":
    check (false).tern(1, 3) == 3
    check (true).tern(-1, raiseAssert("!!!")) == -1

  test "{`==`} Option comparison :generic:":
    check some(12) == 12
    check not (none(int) == 2)
    check (some(12), some(2)) == (12, 2)

suite "If let":
  test "{iflet} Simple :macro:":
    var ok: bool = false
    iflet (val = none(int)):
      echo "none is something and it has value of ", val
      fail()
    else:
     ok = true

    check ok

  test "{iflet} Else-if brances :macro:":
    var final: int = 0
    iflet (val = none(int)):
      final = 3
    elif 2 == 3:
      final = 5
    else:
      final = 1

    check final == 1

  test "{iflet} Return value from body using block :macro:":
    let final = block:
      iflet (val = none(int)):
        3

      elif 2 == 3:
        5

      else:
        1

    check final == 1

  test "{iflet} Iflet in generic function :macro:generic:":
    proc g[T](arg: T): T =
      var res = some(arg)
      iflet (resVal = res):
        doAssert resVal == arg
        return resVal

      else:
        raise newImplementError()

    check g(12) == 12

  test "{iflet} inside of template :macro:template:":
    template whileLet(expr, body: untyped): untyped =
      var ok: bool = true
      while ok:
        iflet (val = expr):
          body
        else:
          ok = false

    var cnt: int = 0
    whileLet(none(int)):
      inc cnt

    check cnt == 0

suite "Slice clamping":
  test "test":
    check clamp(0 .. 2, 0 .. 2) == 0 .. 2
    check clamp(1 ..^ 2, 2).a == 1
    check clamp(1 ..^ 2, 0).a == 0
    check @(clamp(1 ..^ 2, 0)).len == 1
    check @(clamp(1 .. 10, 0 .. 0)).len == 0
    check clamp(0 .. 90, 3) == 0 .. 3
    check intersect(0 .. 10, 10 .. 20) == 10 .. 10

    block:
      let s = @[0, 1, 3, 4]
      check clamp(0 .. 102, s.high) == 0 .. s.high
      check clamp(0 .. ^1, s.high) == 0 .. s.high

  test "with it":
    let val = @[0].withIt do:
      it.add 10
