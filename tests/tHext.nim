import hmisc/preludes/unittest

testFileStarted()


import
  hmisc/preludes/unittest

import
  hmisc/hasts/hext_template,
  hmisc/algo/[halgorithm, hparse_base]

import std/[streams, tables]

startHax()

proc lex(str: string): ColoredRuneGrid =
  lexHext(str).toColored(htoColorMap)

suite "Lexer":
  test "Basic syntax lexer":
    show lex("""
{% for item in seq -%}
    {{ item }}
{%- endfor %}
""")

  test "Raw strings":
    show lex("{% raw %} {{notraw}} {% endraw %} {{expr}}")

suite "Parser":
  test "For loop parser":
    let tree = parseHext(
      "{% for item in seq %} {{item}} {%else%} No iteration {%end%}")

    show tree.treeRepr()

iterator boxedItems(t: typedesc[HextValue[int]], val: int): HextValue[int] =
  discard

suite "Output generation":
  test "For loop generator":
    let tree = parseHext(
      "{% for item in seq %} <<{{item}}>> {%else%} No iteration {%end%}")

    show treeRepr(tree)

    evalHext(tree, newFileStream(stdout), {
      "seq": boxValue(HextValue[int], @[1,2,3,4,4])
    })

type
  DocDb = ref object
    entries: Table[int, DocEntry]

  DocEntry = object
    db: DocDb
    id: int
    doc, name, kind: string

  UserBoxKind = enum
    ubkDb
    ubkEntry

  UserBox = object
    case kind: UserBoxKind
      of ubkDb:
        db: DocDb

      of ubkEntry:
        entry: DocEntry

  DValue = HextValue[UserBox]

proc boxValue(t: typedesc[DValue], val: DocDb): DValue =
  boxValue(DValue, UserBox(kind: ubkDb, db: val), ubkDb.int)

proc boxValue(t: typedesc[DValue], val: DocEntry): DValue =
  boxValue(DValue, UserBox(kind: ubkEntry, entry: val), ubkEntry.int)

proc getField(t: typedesc[DValue], box: UserBox, name: string): DValue =
  case box.kind:
    of ubkEntry:
      case name:
        of "name": return boxValue(t, box.entry.name)
        of "kind": return boxValue(t, box.entry.kind)

    else:
      assert false

iterator boxedItems(t: typedesc[DValue], val: UserBox): DValue =
  case val.kind:
    of ubkDb:
      for _, entry in val.db.entries:
        yield boxValue(t, entry)

    else:
      assert false, "asfd"

suite "haxdoc query emulation":
  test "Generate documentation listing":
    const genTemplate = """
{% for entry in db %}
name: {{entry.name}}
kind: {{entry.kind}}
doc: {{entry.doc}}
{% end %}
"""

    let tree = parseHext(genTemplate)

    var db = DocDb()
    db.entries[1] = DocEntry(
      name: "test",
      doc: "hello"
    )

    evalHext(tree, newFileStream(stdout), {
      "db": boxValue(DValue, db)
    })

testFileEnded()