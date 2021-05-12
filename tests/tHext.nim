import
  hmisc/hasts/hext_template,
  hmisc/hdebug_misc,
  hmisc/algo/halgorithm

import std/[unittest]

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
