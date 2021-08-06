import std/[strscans, strutils, strformat]

func setPart*(buf: var seq[seq[string]], row, col: int, str: string) =
  while buf.high < row:
    buf.add @[]

  while buf[row].high < col:
    buf[row].add " "

  buf[row][col] = str

func setCell*(buf: var seq[seq[string]], row, col: int, str: string) =
  var rowIdx = 0
  for line in split(str, '\n'):
    for colIdx, ch in line:
      buf.setPart(row + rowIdx, col + colIdx, $ch)

    inc rowIdx

func joinBuf*(str: seq[seq[string]]): string =
  for idx, line in pairs(str):
    if idx > 0: result.add "\n"
    result.add line.join("")


func toRedStr*(str: string, colored: bool = true): string =
  if colored: "\e[31m" & str & "\e[39m" else: str

func toMagentaStr*(str: string, colored: bool = true): string =
  if colored: "\e[35m" & str & "\e[39m" else: str

func toBlueStr*(str: string, colored: bool = true): string =
  if colored: "\e[34m" & str & "\e[39m" else: str

func toCyanStr*(str: string, colored: bool = true): string =
  if colored: "\e[36m" & str & "\e[39m" else: str

func toGreenStr*(str: string, colored: bool = true): string =
  if colored: "\e[32m" & str & "\e[39m" else: str

func toYellowStr*(str: string, colored: bool = true): string =
  if colored: "\e[33m" & str & "\e[39m" else: str

func toWhiteStr*(str: string, colored: bool = true): string =
  if colored: "\e[39m" & str & "\e[39m" else: str

func toBlackStr*(str: string, colored: bool = true): string =
  if colored: "\e[30m" & str & "\e[39m" else: str

func toLink*(link: string, desc: string = link): string =
  &"\e]8;;{link}\e\\{desc}\e]8;;\e\\"

func toLink*(
    pos: (string, int, int),
    desc: string = "file://" & pos[0] & ":" & $pos[1] & ":" & $pos[2]
  ): string =

  toLink(&"file://{pos[0]}:{pos[1]}:{pos[2]}", desc)


func stripSGR*(str: string): string =
  var
    prev: int = 0
    pos: int = 0
    sgrbuf: string

  while scanp(str, pos, (
    *(~ '\e'), ("\e[", ({'0' .. '9'}){1,3} -> sgrbuf.add($_), 'm'))
  ):
    let termsym = sgrbuf.len + 2
    let substr = str[prev ..< (pos - termsym - 1)]
    result &= substr
    prev = pos
    sgrbuf = ""

  if prev < pos:
    result &= str[prev ..< pos]
