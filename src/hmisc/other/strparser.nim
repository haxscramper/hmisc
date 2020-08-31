import strformat, strutils, sequtils, options
import re
import ../helpers

## Simple string parsing algorithm for splitting things like `[a, b]`
## into `["a", "b"]`. Not fully finished & usable, but there are unit
## tests in `tHalgorithm.nim::Strparser`

proc toInt*(str: string): int = str.parseInt()
proc toStr*(str: string): string = str
proc toBool*(str: string): bool = str == "1" # IMPLEMENT

proc unwrap(str: string, wraps: set[char]): string =
  if str[0] == str[^1] and str[0] in wraps:
    str[1..^2]
  else:
    str

proc toCharset(str: string): set[char] =
  for c in str:
    result.incl(c)

template mapItIf(s: typed, cond: bool, body: untyped): untyped =
  if cond: s.mapIt(body)
  else: s.mapIt(it)

proc fmtArgs*(
  str: string,
  until: string,
  default: seq[string] = @[]):
    tuple[args: seq[string], argl: int] =

  let argEnd = str.find(until)
  var res: seq[string]

  res = str[1..argEnd - 1].split(",")

  res = concat(
    res,
    newSeqWith(default.len - res.len, ""))

  for i, arg in default:
    if res[i] == "":
      res[i] = default[i]

  result = (res, argEnd + 1)


proc toStrSeq*(str: string): seq[string] =
  ## Parser string to sequence of strings
  let defaults = @[",", "'\"", "t"]
  let (args, left) =
    if str =~ re"^~.,?(.*,?)+\[.*?\]":
      fmtArgs(str, "[", defaults)
    elif str.enclosedIn(("[", "]")):
      (defaults, 1)
    else:
      (defaults, 0)

  let right =
    if str.endsWith("]"): 2
    else: 1

  let body = str[left..^right]
  result =
    body
    .split(args[0])
    .mapItIf(args[2] in @["b", "t"], it.strip())
    .mapIt(it.unwrap(args[1].toCharset()))
    .mapItIf(args[2] in @["a", "t"], it.strip())


proc toIntSeq*(str: string): seq[int] =
  str.toStrSeq().mapIt(it.parseInt)

proc parseTo*(val: string, t: string): Option[string] =
  some(val)

proc parseTo*(str: string, t: int): Option[int] =
  try:
    some(parseInt(str))
  except ValueError:
    none(int)

proc parseTo*(str: string, t: seq[string]):
    Option[seq[string]] =

  some(str.toStrSeq())

proc tupleSplit*(str: string, delim: string): (string, string) =
  let tmp = str.split(delim)
  result = (tmp[0], tmp[1])

proc toTuple*[A](str: string): (A,A) =
  let res =
    if str.enclosedIn(("(", ")")):
      str[1..^2].tupleSplit(",")
    elif str =~ re"^~(.+,?)+\(.*":
      let args = fmtArgs(str, "(", @["*"])
      str[args.argl..^2].tupleSplit(args.args[0])
    else:
      str.tupleSplit(",")

  var a: A
  result = (
    parseTo(res[0], a).get(),
    parseTo(res[1], a).get())
