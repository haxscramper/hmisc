import std/[unicode]
import ./json_serde, ./json_ast
import hmisc/other/oswrap


jsonSerdeFor(AbsFile, loadJsonDistinct, writeJsonDistinct)
jsonSerdeFor(RelFile, loadJsonDistinct, writeJsonDistinct)
jsonSerdeFor(AbsDir, loadJsonDistinct, writeJsonDistinct)
jsonSerdeFor(RelDir, loadJsonDistinct, writeJsonDistinct)


proc loadJson*(reader: var JsonDeserializer, target: var Rune) =
  var tmp: string
  loadJson(reader, tmp)
  target = runeAt(tmp, 0)

proc writeJson*(writer: var JsonSerializer, value: Rune) =
  writeJson(writer, $value)
