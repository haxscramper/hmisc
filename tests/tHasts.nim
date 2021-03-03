# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import std/[unittest, terminal, strformat, strutils]
import hmisc/hasts/[html_ast, graphviz_ast]
import hmisc/types/colorstring
import hmisc/hdebug_misc
import hmisc/algo/halgorithm

startHax()

func cs(str: string, fg: ForegroundColor): ColoredString =
  initColoredString(str, fg = fg)

func cs(str: ColoredString, bg: BackgroundColor): ColoredString =
  str.withIt:
    it.styling.bg = bg

func cs(str: ColoredString, ss: Style): ColoredString =
  str.withIt: it.styling.style.incl ss

let styling = """
div.termline { display:flex; }
span.term { display:inline-block; }
span.term-fgred { color: red; }
span.term-fggreen { color: green; }
span.term-bgblue { background-color: blue; }
"""


suite "HTML ast":
  test "Convert from colored string chunks":
    let strs = @[
      "Hello world".cs(fgRed),
      "Hello world 2".cs(fgGreen),
      "Hello world 2".cs(fgYellow).cs(styleBright),
      "Hello world 2".cs(fgGreen).cs(styleItalic),
    ]

    let doc = @[
      strs.toHTML(),
      newHtmlText("--- === ---").wrap("p"),
      strs.toHtml(false)
    ].toDocument(styling)

    # echo doc
    # "/tmp/page.html".writeFile(doc)

  test "Convert colored chuinks":
      let text = &"""
   hello
wer asdf {"werwer".toRed()} 2
      as
asd
  fa
sdfas
"""
      # echo text
      let colored = text.splitSGR_sep()
      let doc = colored.toHTML(false).toDocument(styling)

      # echo doc
      "/tmp/page.html".writeFile(doc)


suite "Graphviz generation":
  var topGraph = makeDotGraph()

  test "Colored note text":
    var graph = makeDotGraph()
    graph.idshift = 1
    graph.add makeColoredDotNode(0, toGreen("<<<<GREEN>>>>"))
    graph.add makeColoredDotNode(1, toYellow("<<<<GREEN>>>>"))
    graph.add makeDotEdge(0, 1)
    graph.toPng("/tmp/res.png")

    topGraph.addSubgraph(graph)

  test "Record nodes":
    var record = makeDotGraph()
    record.idshift = 2
    record.add makeRecordDotNode(0, @[
      makeDotRecord(1, "test-0-1"),
      makeDotRecord(2, "test-0-2", @[
        makeDotRecord(3, "test-0-1-1"),
        makeDotRecord(4, "test-0-1-2")
      ]),
    ])
    record.add makeRecordDotNode(1, @[
      makeDotRecord(1, "test-1-1"),
      makeDotRecord(2, "test-1-2", @[
        makeDotRecord(3, "test-1-1-1"),
        makeDotRecord(4, "test-1-1-2")
      ]),
    ])

    record.add makeDotEdge(toDotPath(0, 1), toDotPath(1, 1))
    record.add makeDotEdge(toDotPath(0, 3), toDotPath(1, 4))

    topGraph.addSubgraph(record)

  topGraph.toPng("/tmp/res.png")
