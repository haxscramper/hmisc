import parseutils, strutils, macros, strscans, sequtils, sugar

import strscans
export strscans

func matcherTypeGetter_Impl*[T](
  arg: proc(s: string, arg: var T, start: int): int): T =
    discard

func matcherTypeGetter_Impl*[T, T1](
  arg: proc(s: string, arg: var T, start: int, a1: T1): int): T =
    discard

func matcherTypeGetter_Impl*[T, T1, T2](
  arg: proc(s: string, arg: var T, start: int, a1: T1, a2: T2): int): T =
    discard

func matcherTypeGetter_Impl*[T, T1, T2, T3](
  arg: proc(s: string, arg: var T, start: int, a1: T1, a2: T2, a3: T3): int
                               ): T =
    discard


func anything*(input: string, argument: var string, start: int): int =
  let diff = input.len - start
  argument = input[start..^1]
  return diff

func anything*(input: string, start: int): int =
  input.len - start

func until*(
  input: string, argument: var string, start: int, stop: set[char]): int =
  let endpos = input.skipUntil(stop, start)
  argument = input[start ..< start + endpos]
  return endpos



macro tscanf*(input, pattNode: string): untyped =
  ## Statically typed `scanf` wrapper. Similar to `=~` template from
  ## `re` module. The `ml` variable is implicitly declared.
  ##
  ## `ml` is a tuple. Types are inferred from pattern. User
  ## defined matchers are supported too.
  # TODO DOC types of injected variables
  runnableExamples:
    proc matcher1(s: string, arg: var seq[string], start: int): int =
      arg = @["##", "$$"]
      return s.len - start

    if tscanf("12x12---%1,1,1,1", "$ix$i$+%${matcher1}"):
      echo ml[0] / 2, " = ", ml[2]," ", ml[3]

      assert declared(ml)
      assert type(ml[3]) is seq[string]
    else:
      assert not declared(ml)
      echo "does not match"

    assert not declared(ml)


  var matchers: seq[(InterpolatedKind, string)]
  var p = 0
  let pattern: string = $pattNode.toStrLit()
  while p < pattern.len:
    if pattern[p] == '$':
      inc p
      case pattern[p]
        of '$': discard
        of 'w', 'b', 'o', 'i', 'h', 'f', '+', '*':
          matchers.add (ikVar, $pattern[p])
          inc p
        of '{':
          inc p
          var nesting = 0
          let start = p
          while true:
            case pattern[p]
              of '{': inc nesting
              of '}':
                if nesting == 0: break
                dec nesting
              of '\0': error("expected closing '}'")
              else: discard
            inc p
          let expr = pattern.substr(start, p-1)
          matchers.add (ikExpr, expr)
        else:
          inc p
    else:
      inc p

  var tupleType: seq[NimNode]
  for idx, str in matchers:
    if str[0] == ikVar:
      tupleType.add ident(
        case str[1][0]:
          of 'i', 'o', 'b', 'h': "int"
          of 'f': "float"
          of '*', '+': "string"
          else: "float"
      )
    else:
      let node = parseExpr str[1].replace("\\'", "'")

      tupleType.add newCall(
        "typeof", newCall("matcherTypeGetter_Impl",
                          if node.kind == nnkIdent:
                            node
                          else:
                            node[0]
      ))

  tupleType.add ident("int")
      # echo getType(Call("matcherTypeGetter_Impl", ident str[1]))


  let par = nnkPar.newTree(tupleType)
  let ml: NimNode = quote do:
    var ml {.inject.}: `par`

  var arg = @[ident "scanf", input, pattNode]
  for idx, _ in matchers:
    arg.add nnkBracketExpr.newTree(ident "ml", newIntLitNode(idx))

  var call = nnkCall.newTree(arg)

  result = quote do:
    `ml`
    `call`

  # echo result.toStrLit()
