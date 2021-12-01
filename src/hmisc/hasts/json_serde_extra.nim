import std/[unicode]
import ./json_serde, ./json_ast

proc loadJson*(reader: var JsonDeserializer, target: var Rune) =
  var tmp: string
  loadJson(reader, tmp)
  target = runeAt(tmp, 0)

proc writeJson*(writer: var JsonSerializer, value: Rune) =
  writeJson(writer, $value)
