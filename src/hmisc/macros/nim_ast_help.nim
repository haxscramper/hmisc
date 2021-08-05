import std/[macros]


func lineIInfo*(node: NimNode): NimNode =
  ## Create tuple literal for `{.line: .}` pragma
  let iinfo = node.lineInfoObj()
  newLit((filename: iinfo.filename, line: iinfo.line))

func eqIdent*(node: NimNode, strs: openarray[string]): bool =
  for str in strs:
    if node.eqIdent(str):
      return true
