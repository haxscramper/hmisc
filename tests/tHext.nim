import
  hmisc/hasts/hext_template,
  hmisc/hdebug_misc,
  hmisc/algo/[halgorithm, hparse_base]

import std/[unittest, streams]

startHax()

proc showLex(str: string) =
  echo lexHext(str).toColored(htoColorMap)

suite "Lexer":
  test "Basic syntax lexer":
    showLex("""
{% for item in seq -%}
    {{ item }}
{%- endfor %}
""")

  test "Raw strings":
    showLex("{% raw %} {{notraw}} {% endraw %} {{expr}}")

suite "Parser":
  test "For loop parser":
    let tree = parseHext(
      "{% for item in seq %} {{item}} {%else%} No iteration {%end%}")

    echo tree.treeRepr()

suite "Output generation":
  test "For loop generator":
    let tree = parseHext(
      "{% for item in seq %} <<{{item}}>> {%else%} No iteration {%end%}")

    echo treeRepr(tree)

    evalHext(tree, newFileStream(stdout), {
      "seq": boxValue(HextValue[int], @[1,2,3,4,4])
    })
