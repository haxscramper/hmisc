import strutils, sequtils, strformat
import ../other/hshell

func makeRstCodeBlock*(str: string, lang: string = "nim"): string =
  let str = str.split("\n").mapIt("    " & it).join("\n")
  &"""
.. code-block:: {lang}
{str}
"""

func makeRstSection*(body, header: string, level: int): string =
  &"""
{"#".repeat(level)} {header}

{body}
"""

func makeRstSection*(
  level: int, header: string, body: varargs[string]): string =
  makeRstSection(body.join("\n\n"), header, level)

func makeRstList*(elems: seq[string], ident: int = 0): string =
  elems.mapIt("  ".repeat(ident) & it).join("\n")

func makeRstImage*(img: string, center: bool = true): string =
  &"""
.. image:: {img}
   :align: center
"""

proc rst2html*(str: string, file: string): void =
  let tmpfile = "/tmp/rstwrite.rst" # TODO use actual temporary file
                                    # generato
  tmpfile.writeFile(str)
  let cmd = &"nim rst2html -o:{file} {tmpfile}"
  discard runShell(cmd)
