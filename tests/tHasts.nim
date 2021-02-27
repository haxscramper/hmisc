# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest, terminal, strformat

import ../src/hasts/[html_ast, graphviz_ast]
import hmisc/types/colorstring
import hmisc/algo/halgorithm

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
