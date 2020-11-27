## Collection of helper procs to convert output of various command
## programs to json

import hshell
import ../algo/[tscanf, hstring_algo]
import hjson
import std/[strscans, strutils, sequtils, deques, options, segfaults]

func withIdent(strs: seq[string], ident: int): seq[string] =
  for str in strs:
    if str.startsWith(" ".repeat(ident)):
      result.add str
    else:
      break

func join*(que: StrQue, sep: string = " "): string =
  for idx, val in que:
    if idx > 0:
      result &= sep

    result &= val

func parseNimTypeError*(errl: seq[string]): JsonNode =
  var posLines = newJArray()

  var idx = 0
  while tscanf(errl[idx], "${until({'('})}($i, $i)$[anything]"):
    posLines.add %{
      "file" : %ml[0],
      "line" : %ml[1],
      "column" : %ml[2]
    }

    inc idx

  debugecho errl[idx]
  assert errl[idx]["but expected one of:"]
  inc idx

  while errl[idx][["proc", "template", "macro"]]:
    let forProc = errl[idx] & errl[idx + 1 ..^ 1].withIdent(2)
    idx += forProc.len
    debugecho forProc.join("\n")

  result = %{
    "position-trace" : posLines
  }


func popFirstToSeq*[T](que: var Deque[T], cnt: int): seq[T] =
  for _ in 0 .. cnt:
    result.add que.popFirst()

func nimCmdOutConverter*(
  que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  if "Error: type mismatch" in que[0]:
    var idx = 0
    var foundEnd = false
    while idx < que.len:
      if que[idx].startswith("expression:"):
        foundEnd = true
        break

      inc idx

    if foundEnd:
      return some(parseNimTypeError(que.popFirstToSeq(idx)))


func nimCmdErrConverter*(
  que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  discard

func lslOutConverter*(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  let line = que.popFirst().split(" ").filterIt(it.len != 0)
  if line.len == 0 or line[0] == "total":
    return
  else:
    return some(
      %*{
        "entry-type" : line[0][0],
        "permissions" : {
          "root" : line[0][1..3],
          "group" : line[0][4..6],
          "user" : line[0][7..9],
         },
        "owner" : line[2],
        "group" : line[3],
        "size" : line[4],
        "created" : line[5..7].join(" "),
        "filename" : line[8..^1].join(" ")
      }
    )

func lslErrConverter*(que: var StrQue, cmd: ShellCmd): Option[JsonNode] =
  discard
