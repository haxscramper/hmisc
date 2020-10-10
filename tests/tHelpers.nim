import unittest

import strformat, options
import hmisc/[helpers, hexceptions]
import hmisc/macros/iflet

func empty[T](): seq[T] = discard

suite "Misc helper functions":
  test "Long assertion fail":
    try:
      assertionFail:
        "Nice {12 + 2}"
        "12"
    except:
      discard

  test "Split list":
    expect AssertionError:
      discard empty[int]().splitList()

    assert @[1].splitList() == (1, empty[int]())
    assert @[1, 2].splitList() == (1, @[2])

  test "{dedent} :proc:value:":
    assertEq "  a".dedent, "a"
    assertEq "A\n  a".dedent, "A\n  a"
    expect AssertionError:
      discard "   a\n sdfasdf".dedent

  test "{enclosedIn} :proc:value:":
    assert "-+-".enclosedIn(("-", "-"))
    assert "-+-".enclosedIn("-")
    assert not "+".enclosedIn("-")

  test "{wrapTwoColumns} :proc:value:":
    assertEq @[("hello", "world"), ("", "nice")].wrapTwoColumns(
      widthColLimits = (5, 5)
    ).join("\n"), """
      hello world
            nice""".dedent

  test "{enumerate} :template:value:":
    assert @["cat", "dog"].enumerate() == @[(0, "cat"), (1, "dog")]

  test "{join*} string joining functions":
    assertEq @["1", "2"].joinq(", "), "\"1\", \"2\""
    assertEq @["1", "2"].joinl(), "1\n2"
    assertEq @["1", "2"].joinw(), "1 2"

  test "{tern} :template:":
    assert (false).tern(1, 3) == 3
    # If second branch is executed it will raise exception - due to
    # lazy evaluation it does not happen.
    assert (true).tern(-1, raiseAssert("!!!")) == -1

  test "{`==`} Option comparison :generic:":
    assert some(12) == 12
    assert not (none(int) == 2)
    assert (some(12), some(2)) == (12, 2)

suite "If let":
  test "{iflet} Simple :macro:":
    var ok: bool = false
    iflet (val = none(int)):
      echo "none is something and it has value of ", val
      fail()
    else:
     ok = true

    assert ok

  test "{iflet} Else-if brances :macro:":
    var final: int = 0
    iflet (val = none(int)):
      final = 3
    elif 2 == 3:
      final = 5
    else:
      final = 1

    assert final == 1

  test "{iflet} Return value from body using block :macro:":
    let final = block:
      iflet (val = none(int)):
        3
      elif 2 == 3:
        5
      else:
        1

    assert final == 1

  test "{iflet} Iflet in generic function :macro:generic:":
    proc g[T](arg: T): T =
      var res = some(arg)
      iflet (resVal = res):
        assert resVal == arg
        return resVal
      else:
        fail()

    assert g(12) == 12

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

    assert cnt == 0
