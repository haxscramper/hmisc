import ../base_errors
import ../types/colorstring
import std/[sequtils, strutils, re]
export re

type
  RxFlavor* = enum
    rxfEmacs
    rxfPerl
    rxfDebug
    rxfLisp
    rxfPosix
    rxfExtendedPosix

  RxSpecialKind* = enum
    rskLineStart
    rskLineEnd

    rskStringStart
    rskStringEnd
    rskStringBoundary
    rskStringOrLineEnd

    rskWord
    rskNotWord
    rskSpace
    rskNotSpace
    rskDigit
    rskNotDigit

    rskAny

  RxKind* = enum
    rxkText
    rxkCharset

    rxkSpecial

    rxkOneOrMoreGreedy
    rxkOneOrMoreLazy
    rxkZeroOrMoreGreedy
    rxkZeroOrMoreLazy

    rxkOptional

    rxkAlt
    rxkConcat
    rxkGroup
    rxkNonCapturingGroup

    rxkRepeatNTimes
    rxkRepeatNtoMTimes

  RxSetElemKind* = enum
    rseItem
    rseRange

  RxSetElem* = object
    case kind*: RxSetElemKind
      of rseItem:
        item: char

      of rseRange:
        start, finish: char

const
  rxkGroupKinds* = {
    rxkGroup, rxkNonCapturingGroup
  }

  rxkSingleArgKinds* = {
    rxkOneOrMoreGreedy,
    rxkOneOrMoreLazy,
    rxkZeroOrMoreGreedy,
    rxkZeroOrMoreLazy,

    rxkOptional,

    rxkRepeatNtimes,
    rxkRepeatNtoMTimes
  }

  rxkRepeatedArgKinds* = {rxkAlt, rxkConcat} + rxkGroupKinds

type
  Rx* = object
    case kind*: RxKind
      of rxkText:
        text*: string

      of rxkSpecial:
        special*: RxSpecialKind

      of rxkCharSet:
        charElems: seq[RxSetElem]

      of rxkSingleArgKinds, rxkRepeatedArgKinds:
        args: seq[Rx]
        repeatMin*, repeatMax*: int



func initRx*(kind: RxKind, args: varargs[Rx]): Rx =
  result = Rx(kind: kind)
  case kind:
    of rxkText, rxkCharSet, rxkSpecial:
      raiseArgumentError(
        "Invalid kind for nested regex construction: " & $kind
      )

    of rxkSingleArgKinds:
      if args.len != 1:
        raiseArgumentError(
          "Invalid number of arguments for regext construction: " &
            $kind & " expected exactly one argument, but got " &
            $args.len
        )

      else:
        result.args.add args

    of rxkRepeatedArgKinds:
      result.args.add args

template rxEscape*(ch: char): string =
  case ch:
    of '.': "\\."
    else: $ch

func rxEscape*(str: string): string =
  for ch in str:
    result.add rxEscape(ch)


func toStr*(elem: RxSetElem): string =
  case elem.kind:
    of rseItem:
      result = rxEscape(elem.item)

    of rseRange:
      result = rxEscape(elem.start) & "-" & rxEscape(elem.finish)

func treeRepr*(rx: Rx, colored: bool = true, level: int = 0): string =
  let pref = "  ".repeat(level)
  let name = ($rx.kind)[3 .. ^1]
  case rx.kind:
    of rxkRepeatedArgKinds, rxkSingleArgKinds:
      result = pref & name & "\n"
      for idx, arg in pairs(rx.args):
        result &= treeRepr(arg, colored, level + 1)
        if idx < rx.args.high:
          result &= "\n"

    of rxkText:
      result = pref & name & " " & toYellow(
        "\"" & rx.text.rxEscape() & "\"", colored)

    of rxkCharset:
      result = pref & name & "[" &
        join(mapIt(rx.charElems, toStr(it)), "") & "]"

    of rxkSpecial:
      result = pref & name & ($rx.special)


func lispRepr*(rx: Rx, colored: bool = true, level: int = 0): string =
  result = "(" & ($rx.kind)[3..^1] & " "
  case rx.kind:
    of rxkRepeatedArgKinds, rxkSingleArgKinds:
      for idx, arg in pairs(rx.args):
        if idx > 0:
          result &= " "

        result &= lispRepr(arg, colored, level + 1)

    of rxkText:
      result &= toYellow("\"" & rx.text & "\"", colored)

    of rxkCharset:
      result &= "[" & join(mapIt(rx.charElems, toStr(it)), "") & "]"

    of rxkSpecial:
      result &= $rx.special

  result &= ")"

func `$`*(rx: Rx): string = lispRepr(rx)

func toStr*(special: RxSpecialKind, flavor: RxFlavor = rxfPerl): string =
  case special:
    of rskLineStart: "^"
    of rskLineEnd: "$"
    of rskDigit: "\\d"
    of rskNotDigit: "\\D"
    of rskSpace: "\\s"
    of rskNotSpace: "\\S"
    of rskWord: "\\w"
    of rskNotWord: "\\W"
    of rskStringStart: "\\A"
    of rskStringEnd: "\\z"
    of rskStringBoundary: "\\b"
    of rskStringOrLineEnd: "\\z"
    of rskAny: "."



func toStr*(kind: RxKind): string =
  case kind:
    of rxkText, rxkCharset, rxkSpecial:
      raiseArgumentError(
        $kind & " is not directly convertible to string")

    of rxkOneOrMoreGreedy: "+"
    of rxkOneOrMoreLazy: "+?"
    of rxkZeroOrMoreGreedy: "*"
    of rxkZeroOrMoreLazy: "*?"
    of rxkAlt: "|"
    of rxkOptional: "?"
    of rxkGroupKinds: ""
    of rxkRepeatNtoMTimes, rxkRepeatNTimes, rxkConcat: ""

func toStr*(rx: Rx, flavor: RxFlavor = rxfPerl): string =
  case flavor:
    of rxfDebug:
      result = treeRepr(rx, false)

    else:
      case rx.kind:
        of rxkText:
          result = rx.text.rxEscape()

        of rxkSingleArgKinds:
          let a0 = rx.args[0]
          result = toStr(a0, flavor)

          if rx.kind == rxkOptional and
             # Is an optional wrapper and first argument cannot be used
             # without parenthesis.
             not(
               # Optional charset has it's own wrapping - `[a-A]?`
               (a0.kind in {rxkCharset}) or
               # Single-char text also does not require any escape
               # wrapping, even if it is escaped (e.g. `\.?` is fine)
               (a0.kind in {rxkText} and len(a0.text) == 1)
             ) and
             # Explicitly check for wrapper parenthesis (might be necessary
             # to check for balanced too.)
             not (result[0] == '(' and result[^1] == ')'):
            result = "(" & result & ")"

          result &= toStr(rx.kind)

          case rx.kind:
            of rxkRepeatNTimes:
              result &= "{" & $rx.repeatMin & "}"

            of rxkRepeatNtoMTimes:
              result &= "{" & $rx.repeatMin & ","
              if rx.repeatMax < high(int):
                result &= $rx.repeatMax & "}"

            else:
              discard

        of rxkSpecial:
          result = toStr(rx.special, flavor)

        of rxkCharset:
          result &= "["
          for elem in rx.charElems:
            result &= toStr(elem)

          result &= "]"

        of rxkRepeatedArgKinds:
          case rx.kind:
            of rxkGroup: result = "("
            of rxkNonCapturingGroup: result = "(?:"
            else: discard


          result &= mapIt(rx.args, toStr(it, flavor)).join(toStr(rx.kind))

          if rx.kind in rxkGroupKinds:
            result = result & ")"

          elif rx.kind == rxkAlt:
            # TODO generate more optimized matching, avoid creating
            # unnecessary groups
            result = "(" & result & ")"

template toConstStr*(rx: static[Rx], flavor: RxFlavor = rxfPerl): untyped =
  const rxRes = toStr(rx, flavor)
  rxRes

func toRegex*(rx: static[Rx]): Regex =
  re(toConstStr(rx, rxfPerl))

func toRegex*(rx: Rx): Regex = re(toStr(rx, rxfPerl))
func toRegex*(regex: Regex): Regex {.inline.} = regex


template `=~`*(s: string, rx: Rx): untyped {.dirty.} =
  s =~ toRegex(rx)

{.push inline.}

func nrx*(text: string): Rx =
  result = Rx(kind: rxkText)
  result.text = text

func nrx*(ch: char): Rx = Rx(kind: rxkText, text: $ch)

func nrx*(special: RxSpecialKind): Rx =
  Rx(kind: rxkSpecial, special: special)

func nrx*(ch1, ch2: char): Rx =
  Rx(kind: rxkCharset, charElems: @[
    RxSetElem(start: ch1, finish: ch2, kind: rseRange)
  ])

func nrx*(charset: set[char]): Rx =
  result = Rx(kind: rxkCharset)
  for elem in charset:
    result.charElems.add RxSetElem(item: elem, kind: rseItem)



func `+`*(rx: Rx): Rx = initRx(rxkOneOrMoreGreedy, @[rx])
func `+?`*(rx: Rx): Rx = initRx(rxkOneOrMoreLAzy, @[rx])
func `*`*(rx: Rx): Rx = initRx(rxkZeroOrMoreGreedy, @[rx])
func `*?`*(rx: Rx): Rx = initRx(rxkZeroOrMoreLAzy, @[rx])
func `?`*(rx: Rx): Rx = initRx(rxkOptional, @[rx])

func `?`*(rxItems: openarray[Rx]): Rx =
  initRx(rxkOptional, @[initRx(rxkConcat, rxItems)])

func `|`*(rhs, lhs: Rx): Rx = initRx(rxkAlt, @[rhs, lhs])
func `|`*(rxItems: openarray[Rx]): Rx = initRx(rxkAlt, rxItems)

func `&`*(rhs, lhs: Rx): Rx = initRx(rxkConcat, @[rhs, lhs])
func `&`*(rxItems: openarray[Rx]): Rx = initRx(rxkConcat, rxItems)

func group*(rxSeq: varargs[Rx]): Rx =
  initRx(rxkNonCapturingGroup, rxSeq)

func fullLine*(rx: Rx): Rx =
  initRx(rxkConcat, @[nrx(rskLineStart), rx, nrx(rskLineEnd)])

func capture*(rxSeq: varargs[Rx]): Rx = initRx(rxkGroup, rxSeq)

func repeat*(rx: Rx, repeatMin: int): Rx =
  result = initRx(rxkRepeatNTimes, @[rx])
  result.repeatMin = repeatMin

func repeat*(rx: Rx, repeatMin, repeatMax: int): Rx =
  result = initRx(rxkRepeatNtoMTimes, @[rx])
  result.repeatMin = repeatMin
  result.repeatMax = repeatMax

func atLeast*(rx: Rx, repeatMin: int): Rx =
  repeat(rx, repeatMin, high(int))

{.pop.}
