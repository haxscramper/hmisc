import std/[
  math, strutils, sequtils, random, macros, options, strformat,
  parseutils, algorithm, sugar, wordwrap, tables
]

import htemplates
export htemplates

import hseq_distance
import hstring_algo
export hstring_algo

import ../types/hprimitives
import ../hdebug_misc

import hmath
export hmath

import ../base_errors

#=======================  small helper templates  ========================#

func add*[A, B](s: var seq[(A, B)], a: A, b: B) =
  s.add((a, b))

template expectEqualTypes(a, b: untyped): untyped =
  assert (a is typeof(b)), "Mismatch between types: first is `" &
    $typeof(a) & "` and second is `" & $typeof(b) & "`"

macro disjointIterImpl(x: typed): untyped =
  var values: seq[NimNode]
  for value in x.getTypeImpl[1..^1]:
    values.add newIdentNode($value.tostrlit)

  result = nnkStmtList.newTree(
    nnkPrefix.newTree(
      newIdentNode("@"),
      nnkBracket.newTree(values)))

macro disjointIter*(x: typed): untyped =
  nnkBracket.newTree(x.getType[1][1..^1])


#========================  sequence operations  ==========================#

type
  LenIndexable*[T] = concept x
    x.len is int
    x[0] is T

iterator rmpairs*[T](s: var LenIndexable[T]): (int, var T) =
  ## Iterate over mutable sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield (idx, s[idx])

iterator ritems*[T](s: var LenIndexable[T]): var T =
  ## Iterate over mutable sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield s[idx]

iterator rpairs*[T](s: T): auto =
  ## Iterate over sequence starting from the rightk
  for idx in countdown(s.len - 1, 0):
    yield (idx, s[idx])

iterator ritems*[T](s: openarray[T]): T =
  ## Iterate over sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield s[idx]

iterator rmitems*[T](s: var seq[T]): var T =
  ## Iterate over sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield s[idx]


iterator itemsIsFirst*[T](s: T): auto =
  mixin `[]`
  for idx in 0 .. s.len - 1:
    yield (idx == 0, s[idx])

iterator itemsIsLast*[T](s: T): auto =
  let sLen = s.len - 1
  var idx = 0

  mixin items
  for item in items(s):
    yield (idx == sLen, item)
    inc idx


iterator itemsIsFirstLast*[T](
  s: LenIndexable[T]): tuple[isFirst, isLast: bool, val: T] =
  let sLen = s.len - 1
  mixin `[]`
  for idx in 0 .. sLen:
    yield (idx == 0, idx == sLen, s[idx])

iterator zip*[T1, T2, T3, T4, T5](
    s1: LenIndexable[T1],
    s2: LenIndexable[T2],
    s3: LenIndexable[T3],
    s4: LenIndexable[T4],
    s5: LenIndexable[T5]
  ): tuple[v1: T1, v2: T2, v3: T3, v4: T4, v5: T5] =

  for idx in 0 ..< min([s1.len, s2.len, s3.len, s4.len, s5.len]):
    yield (s1[idx], s2[idx], s3[idx], s4[idx], s5[idx])


func emptySeq*[T](): seq[T] = discard
proc enumerate*[T](s: openArray[T]): seq[(int, T)] =
  ## Return enumerated sequence of items
  for idx, item in s:
    result.add((idx, item))

func splitList*[T](s: openarray[T]): (T, seq[T]) =
  ## Return head and tail of the list
  if s.len == 0:
    raiseArgumentError("Cannot split empty list")

  (s[0], s[1..^1])


template last*[T](stack: var seq[T]): var T = stack[^1]
template last*[T](stack: seq[T]): T = stack[^1]

template last2*[T](stack: var seq[seq[T]]): var T = stack[^1][^1]
template last2*[T](stack: seq[seq[T]]): T = stack[^1][^1]

# TODO use static hashtable instead of searching whole list each time.
proc matchWith*[K, V](
  val: K,
  tbl: seq[tuple[k: seq[K], v: V]]): Option[V] =
  ## Search `seq[seq[Key], Val]` for entry that has matching `Key` and
  ## return corresponding `Val`. If nothing found return `none(V)`
  runnableExamples:
    import options
    let lookup = @[
      (@["one", "two", "three"], "number"),
      (@["cat", "dog", "mole"], "animal")
    ]

    doAssert "one".matchWith(lookup) == some("number")
    doAssert "dog".matchWith(lookup) == some("animal")
    doAssert "number".matchWith(lookup).isNone()


  for tupl in tbl:
    if val in tupl.k:
      return some(tupl.v)

    result = none(V)

#=======================  single item operations  ========================#

#=========================  string operations  ===========================#

func join*[T](obj: T, sep: string, wrap: (string, string)): string =
  var first: bool = true
  for elem in obj:
    if not first:
      result &= sep
    else:
      first = false

    result &= wrap[0] & $elem & wrap[1]




func dashedWords*(
  str: string,
  toDash: set[char] = {'-', '_', ' ', '.', ',', ';', ':'},
  toLower: set[char] = {'a'..'z', 'A'..'Z', '0'..'9'}): string =

  for ch in str:
    if ch in toDash:
      result &= '-'
    elif ch in toLower:
      result &= ch.toLowerAscii()

func makeCommentSection*(str: string, level: range[0..2]): string =
  ## Generate separation comment
  case level:
    of 2:
      &"# ~~~~ {str} ~~~~ #"
    of 1:
      "#" & center(" " & str.strip() & " ", 73, '=') & "#"
    of 0:
      "#" & "*".repeat(73) & "#\n" &
      "#" & center(" " & str.strip() & " ", 73, '*') & "#\n" &
      "#" & "*".repeat(73) & "#"


func isSubseq*[T](subseq, inseq: openarray[T]): bool =
  var inPos, subPos: int
  while subPos < subseq.len:
    # debugecho inseq[inPos .. ^1], " ", subseq[subPos]
    let inPos = inseq[inPos .. ^1].find(subseq[subPos])
    # debugecho inPos
    if inPos == -1:
      return false
    else:
      inc subPos
      result = true


func dropSubseq*[T](inseq, subseq: openarray[T]): seq[T] =
  ## Drop all non-overlapping occurencies of `subseq` in `inseq`
  var i = 0
  if subseq.len == 0:
    result = toSeq(inseq)
    return


  var prev = -1
  while i < inseq.len:
    # # debugecho i, " ", inseq[i..^1], " ", subseq
    # if prev == i:
    #   raiseAssert("#[ IMPLEMENT ]#")
    var matches: bool = true
    for shift in 0 ..< subseq.len:
      if (i + shift < inseq.len):
        if (inseq[i + shift] != subseq[shift]):
          matches = false
      else:
        matches = false


    prev = i
    # debugecho "@ ", inseq[i], " matches"
    if not matches:
      result.add inseq[i]
      inc i
    else:
      i += subseq.len


func dropLongestSubseq*[T](inseq: seq[T], subseqs: seq[seq[T]]): seq[T] =
  ## Sort `subseq` by lenght and try to drop each from `inseq`. First
  ## first drop attempt that changes result length is returned.
  let subseqs = subseqs.sortedByIt(-it.len)
  result = inseq
  for sub in subseqs:
    let dropped = inseq.dropSubseq(sub)
    if dropped.len != inseq.len:
      result = dropped
      break


func dropLongestSubseq*(inseq: string, inseqs: seq[string]): string =
  ## Sort `subseq` by lenght and try to drop each from `inseq`. First
  ## first drop attempt that changes result length is returned.
  runnableExamples:
    doAssert "CXX_CX".dropLongestSubseq(@["CXX", "CX"]) == "_CX"

  let inseqs = collect(newSeq):
    for str in inseqs:
      str.mapIt(it)

  dropLongestSubseq(inseq.mapIt(it), inseqs).join("")

func dropSubstr*(instr, substr: string): string =
  ## Drop all occurencies of `substr` in `instr`
  runnableExamples:
    doAssert "CX_CX_EEECX".dropSubstr("CX") == "__EEE"

  instr.dropSubseq(substr).join("")

func dropLowerPrefix*(str: sink string): string =
  result = str[str.skipWhile({'a' .. 'z'}) .. ^1]

func dropCommonPrefix*(
  strs: seq[string], dropSingle: bool = true): seq[string] =
  ## Drop common prefix from sequence of strings. If `dropSingle` is
  ## false sequences with `len == 1` are returned as-is.
  runnableExamples:
    doAssert @["--", "-="].dropCommonPrefix() == @["-", "="]
    doAssert @["---"].dropCommonPrefix(false) == @["---"]

  if not dropSingle and strs.len == 1:
    return strs

  let pref = strs.commonPrefix()
  for str in strs:
    result.add str.dropPrefix(pref)

func splitTokenize*(str: string, seps: seq[string]): seq[string] =
  var prev = 0
  var curr = 0
  # var cnt = 0
  while curr < str.len:
    # inc cnt
    # if cnt > 20:
    #   break

    # debugecho curr, result, str[prev .. curr]
    block nextSep:
      for sep in seps:
        if str.continuesWith(sep, curr):
          if prev != curr:
            result.add str[prev ..< curr]
            prev = curr

          curr += sep.len
          result.add str[prev ..< curr]
          prev = curr
          break nextSep

      inc curr


func splitTokenize*(str: string, seps: set[char]): seq[string] =
  var prev = 0
  var curr = 0
  while curr < str.len:
    if str[curr] in seps:
      if prev != curr:
        result.add str[prev ..< curr]

      result.add $str[curr]
      inc curr
      prev = curr
    else:
      inc curr


func splitCamel*(
    str: string,
    dropUnderscore: bool = true,
    splitUnderscores: bool = true,
    mergeCapitalized: bool = true,
    adaptiveMerge: bool = true
  ): seq[string] =
  ##[

Split abbreviation as **camelCase** identifier

- @arg{dropUnderscore} :: Drop all `_` characters if found
- @arg{splitUnderscores} :: Split on `_` characters
- @arg{mergeCapitalized} :: Do not split consecutive capitalized
- @arg{adaptiveMerge} :: Employ additional heuristics to make
  capitalized chunk splits more 'logical'. `DBManager -> DB + Manager`,
  but `FILE -> FILE`

  ]##
  # TODO handle `kebab-style-identifiers`
  # TODO Split things like `ABBRName` into either `ABBR, Name` or
  #      `A, B, B ...`
  var pos = 0


  var dropSet: set[char]
  if splitUnderscores:
    dropset.incl '_'

  const capital = {'A' .. 'Z'}

  var splitset = capital + dropset

  while pos < str.len:
    var start = pos
    var next: int
    if  str[pos] in capital and mergeCapitalized:
      next = start + str.skipWhile(capital, start + 1)
      if adaptiveMerge:
        if next == start:
          next = next + str.skipUntil(splitset, next + 1)

        elif next > start + 1 and
             next < str.high and
             str[next + 1] notin splitset
          :
          dec next

      else:
        next = next + str.skipUntil(splitset, next + 1)

    else:
      next = start + str.skipUntil(splitset, start + 1)

    if str[start] == '_' and dropUnderscore:
      inc start

    # echov str[start..next]
    # echov (start, next)

    if str[start..next].allOfIt(it in {'_'}) and dropUnderscore:
      discard
    else:
      result.add str[start..next]

    pos = next + 1

func toSnakeCase*(str: string): string =
  str.splitCamel().mapIt(it.toLowerAscii()).join("_")

func toSnakeCamelCase*(str: string): string =
  str.split("_").filterIt(it.len > 0).mapIt(
    it.toLowerAscii().capitalizeAscii()).join("")

func abbrevCamel*(
    abbrSplit: seq[string],
    splitWords: seq[seq[string]],
    getExact: bool = false
  ): seq[string] =
  ## Split abbreviation and all worlds as **camelCase** identifiers.
  ## Find all worlds that contains `abbrev` as subsequence.
  let abbr = abbrSplit.join("")
  for word in splitWords:
    let lcs = longestCommonSubsequence(
      abbrSplit, word,
      itemCmp = proc(lhs, rhs: string): bool =
                    # debugecho lhs, rhs
                    # lhs == rhs
                    rhs.startsWith(lhs)
    )

    if lcs.len > 0:
      if lcs[0].matches.len == abbrSplit.len:
        let word = word.join("")
        if getExact and word == abbr:
          return @[word]
        else:
          result.add word

func abbrevCamel*(
  abbrev: string,
  words: seq[string],
  getExact: bool = false): seq[string] =
  ## Split abbreviation and all worlds as **camelCase** identifiers.
  ## Find all worlds that contains `abbrev` as subsequence. `getExact`
  ## - if any of the alternatives fully matches input word return it
  ## as only result
  ##
  ## To avoid ambiguous returns on tests like `"Else", @["Else",
  ## "ElseBlock"]`)
  abbrevCamel(abbrev.splitCamel(), words.mapIt(it.splitCamel()))

func posString*(node: NimNode): string =
  let info = node.lineInfoObj()
  return "on line " & $info.line

func mismatchStart*(str1, str2: string): int =
  ## Find position where two strings mismatch first
  # TODO implement mismatch with support for multiple
  # matching/mismatching sections - use larges common subsequence to
  # determine differences

  # NOTE can use annotation highlighter from code error reporting
  # `hmisc/defensive`

  # TODO support multiline strings (as sequence of strigns and as
  # single multiline strings)
  for i in 0 ..< min(str1.len(), str2.len()):
    if str1[i] != str2[i]:
      return i

  if str1.len() != str2.len():
    # Have common prefix but second one is longer
    return min(str1.len(), str2.len()) + 1
  else:
    # No mismatch found
    return -1

func joinCamel*(ins: openarray[string]): string =
  for elem in ins:
    result.add elem.capitalizeAscii()

  result[0] = result[0].toLowerAscii()

func joinl*(inseq: openarray[string]): string =
  ## Join items using newlines
  runnableExamples:
    assert @["as", "bn"].joinl == "as\nbn"
  inseq.join("\n")

func joinql*(
  inseq: openarray[string], ident: int = 1,
  wrap: string = "\"", identStr: string = "  "): string =

  inseq.mapIt(identStr.repeat(ident) & wrap & it & wrap).join("\n")

func joinkv*[K, V](
  t: openarray[(K, V)], eqTok: string = "="): seq[string] =
  ## Join table values as key-value pairs
  for k, v in t:
    result.add &"{k} {eqTok} {v}"

proc joinw*(inseq: openarray[string], sep = " "): string =
  ## Join items using spaces
  runnableExamples:
    assert @["as", ";;"].joinw == "as ;;"
  inseq.join(sep)

func joinq*(inseq: openarray[string], sep: string = " ", wrap: string = "\""): string =
  ## Join items using spaces and quote each item
  runnableExamples:
    assert @["as", "qq"].joinq == "\"as\" \"qq\""

  inseq.mapIt(wrap & it & wrap).join(sep)

func replaceN*(str: string, n: int, subst: char = ' '): string =
  ## Replace first `n` characters in string with `subst`
  runnableExamples:
    assert "123".replaceN(1) == " 23"
    assert "0--".replaceN(3, '-') == "---"

  result = str
  for i in 0..<min(str.len, n):
    result[i] = subst

func wrapTwoColumns*(
  text: seq[(string, string)],
  padding: (int, int) = (0,0),
  widthColLimits: (int, int) = (30, -1),
  maxWidthTotal: int = 80): seq[(string, string)] =

  var wrapped: seq[(seq[string], seq[string])] =
    text.mapIt(
      (it[0].wrapWords(widthColLimits[0]).split("\n"), @[it[1]]))

  let maxWidth1: int =
    wrapped
    .mapIt(
      it[0]
      .mapIt(it.len)
      .max()
    )
    .max()

  let maxWidth2: int =
    if widthColLimits[1] > 0:
      widthColLimits[1]
    else:
      maxWidthTotal - min(maxWidth1, widthColLimits[0])


  wrapped = wrapped.mapIt(
    (it[0],
     it[1][0].wrapWords(maxWidth2).split("\n")))

  for entry in wrapped:
    let lines1 = entry[0]
    let lines2 = entry[1]

    let lineCount = max(lines1.len, lines2.len)
    for idx in 0 ..< lineCount:
      # echo " $# $#" % [
        result.add (
          (idx < lines1.len)
          .tern(
            lines1[idx].alignLeft(maxWidth1 + 1),
            " ".repeat(maxWidth1 + 1)
          ),
                  (idx < lines2.len).tern(lines2[idx], "")
        )
      # ]


proc printTwoColumns*(
  text: seq[(string, string)],
  padding: (int, int) = (0,0),
  widthColLimits: (int, int) = (30, -1),
  maxWidthTotal: int = 80): void =
  ## Print two columns of text side by side
  ##
  ## :params:
  ##   :padding: amount of spaces from left and right
  ##   :maxWidthNotal: max width of two columns plus padding
  ##   :widthColLimits: limit of each column width
  ##   :text: sequence of string pairs. Each pair will be printed on new
  ##          row
  ##
  ## .. code-block::
  ##     @[
  ##       ("=first line=", "=second="),
  ##       ("=aaaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
  ##       ("=a d d d aaaaaaaaaaaaaaaaa=", "=sd d fd fd ="),
  ##       ("q", "=sd d fd fd =")
  ##     ].printTwoColumns()
  ##
  ## .. code-block:: text
  ##     =first line=                 =second=
  ##     =aaaaaaaaaaaaaaaaaa=         =sd d fd fd =
  ##     =a d d d aaaaaaaaaaaaaaaaa=  =sd d fd fd =
  ##     q                            =sd d fd fd =

  for (lhs, rhs) in wrapTwoColumns(text, padding, widthColLimits, maxWidthTotal):
    echo " $# $#" % [lhs, rhs]

func join*(text: openarray[(string, string)], sep: string = " "): string =
  text.mapIt(it[0] & it[1]).join(sep)

func join*(text: openarray[string], sep: char = ' '): string =
  text.join($sep)

func wrap*(
  str: string,
  delim: tuple[left, right: string]): string =
  ## Check if string starts and ends with strings.
  return delim.left & str & delim.right

func wrap*(str, left, right: string): string =
  wrap(str, (left, right))


func wrap*(str: string, delim: string): string =
  ## Split `delim` in two, use wrap `str` in left and right halves.
  let left = delim.len div 2
  return delim[0 ..< left] & str & delim[left .. ^1]

func wrap*(str: string, left, right: char): string {.inline.} =
  $left & str & $right


func escapeHTML*(input: string): string =
  input.multiReplace([
    (">", "&gt;"),
    ("<", "&lt;"),
    ("&", "&amp;"),
    ("\"", "&quot;")
  ])

func enclosedIn*(s: string, delim: string): bool =
  s.enclosedIn((delim, delim))

proc getRandomBase64*(length: int): string =
  ## Return random base 64 string with `length` characters
  newSeqWith(
    length,
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".
    sample()).join("")

when (NimMajor, NimMinor, NimPatch) <= (1, 2, 6):
  func dedent*(multiline: string): string =
    ## Uniformly deindent multiline string
    let seplines = multiline.split('\n')
    var indent = 0
    for c in seplines[0]:
      if c == ' ': inc indent
      else: break

    seplines.mapIt(
      if it.len == 0:
        it
      else:
        assert it[0..<indent].allOfIt(it == ' '),
          "Cannot unindent non-whitespace character"

        it[indent..^1]
    ).join("\n")
else:
  export dedent

#===============================  options  ===============================#
proc `==`*[T](opt: Option[T],val: T): bool =
  ## Compare option with value for equatilty
  if opt.isNone: false
  else: opt.get() == val

proc `==`*[A, B](tpl: (Option[A], Option[B]), tpl1: (A, B)): bool =
  ## Compare tuple of optional values for equality
  tpl[0] == tpl1[0] and tpl[1] == tpl1[1]

template ifSomeIt*[T](opt: Option[T], predicate: untyped): bool =
  when not compiles(opt.isSome()):
    static: error "ifSomeIt denends on options module. " &
      "Add `import std/options` to fix this error"
  else:
    ((
      block:
      (opt.isSome() and ((let it {.inject.} = opt.get(); predicate)))
    ))


template getSomeIt*[T](opt: Option[T], value, default: untyped): untyped =
  if opt.isSome():
    let it {.inject.} = opt.get()
    value
  else:
    default

template getSome*[T](opt: Option[T], injected: untyped): bool =
  let expr = opt

  expr.isSome() and (
    let injected {.inject.} = expr.get();
    mixin injected;
    true
  )


template mapSomeIt*[T](opt: Option[T], expr: untyped): untyped =
  var result: Option[typeof(
    block:
      var it {.inject.}: T
      expr
  )]

  if opt.isSome():
    let it {.inject.} = opt.get()
    result = some(expr)

  result


#================================  tests  ================================#

import unittest
proc testEq*[A, B](lhs: A, rhs: B) =
  # TODO use LCS to highlight only parts that are different in red
  # static:
  #   assert compiles(lhs == rhs),
  #    "Cannot directly compare objects of type" & $typeof(lhs) &
  #      " and " & $typeof(rhs)

  mixin fmt
  if lhs != rhs:
    let
      lhsStr = ($lhs).replace("\e", "\\e")
      rhsStr = ($rhs).replace("\e", "\\e")

    # testEnded(
    #   ConsoleOutputFormatter(colorOutput: true, isInSuite: true),
    #   TestResult(testName: "Equality comparison", status: FAILED)
    # )

    let diffPos = mismatchStart(lhsStr, rhsStr)
    if '\n' in lhsStr or '\n' in rhsStr:
      let
        linesA = lhsStr.split('\n')
        linesB = rhsStr.split('\n')

      var hadAny = false
      for idx, line in zip(linesA, linesB):
        if line[0] != line[1]:
          echo fmt("LHS #{idx}: '{line[0]}'")
          echo fmt("RHS #{idx}: '{line[1]}'")
          hadAny = true
          break

      if not hadAny:
        echo fmt("LHS: '{lhsStr}'")
        echo fmt("RHS: '{rhsStr}'")
        # else:
        #   echo &"#{idx}: '{line[0]}' == '{line[1]}'"

    else:
      if (lhsStr.len > 50 or rhsStr.len > 50):
        let start = max(diffPos - 20, 0)
        let diffPos = max(diffPos, 1)
        echo "LHS: ...\e[32m", lhsStr[start ..< diffPos], "\e[39m",
          "\e[31m", lhsStr[diffPos ..< min(diffPos + 40, lhsStr.len)],
          "\e[39m..."

        echo "RHS: ...\e[32m", rhsStr[start ..< diffPos], "\e[39m",
          "\e[31m", rhsStr[diffPos ..< min(diffPos + 40, rhsStr.len)],
          "\e[39m..."

        echo " ".repeat(28), "^".repeat(10)
      else:
        echo fmt("LHS: {lhsStr}")
        echo fmt("RHS: {rhsStr}")

        echo "    ", " ".repeat(diffPos + 1),
                 "^".repeat(rhsStr.len() - diffPos + 1)

    echo ""

template assertEq*(lhs, rhs: untyped): untyped =
  let lhsVal = lhs
  let rhsVal = rhs
  testEq(lhsVal, rhsVal)
  let lInfo = instantiationInfo()
  if not (lhsVal == rhsVal):
    echo lhs.astToStr(), " == ", rhs.astToStr()
    raiseAssert("Comparison failed on line " & $lInfo.line)

func pop*[E](s: var set[E]): E =
  if len(s) == 0:
    raise newArgumentError("Cannot pop from empty set")

  for val in s:
    result = val
    s.excl result
    return

type
  MarkTable*[K, M] = object
    used: set[M]
    table: Table[K, M]

func nextVal*[E](used: var set[E]): E =
  var allowed: set[E] = { low(E) .. high(E) } - used
  result = pop(allowed)
  used.incl result

func getMark*[K, E](marks: var MarkTable[K, E], value: K): E =
  if value notin marks.table:
    marks.table[value] = nextVal(marks.used)

  result = marks.table[value]

proc nextRandVal*[E](used: var set[E]): E =
  var allowed: set[E] = { low(E) .. high(E) } - used
  result = sample(allowed)
  used.incl result

proc getRandMark*[K, E](marks: var MarkTable[K, E], value: K): E =
  if value notin marks.table:
    marks.table[value] = nextRandVal(marks.used)

  result = marks.table[value]


func toMapArray*[K, V](map: openarray[(K, V)]): array[K, V] =
  for (k, v) in map:
    result[k] = v

func toRevMapArray*[K, V](map: openarray[(K, V)]): array[V, K] =
  for (k, v) in map:
    result[v] = k

func toMapArray*[K, V](map: openarray[(set[K], V)]): array[K, V] =
  for (keySet, v) in map:
    for k in items(keySet):
      result[k] = v

func toKeySet*[K, V](map: openarray[(K, V)]): set[K] =
  for (k, v) in map:
    result.incl k

func toValSet*[K, V](map: openarray[(K, V)]): set[V] =
  for (k, v) in map:
    result.incl v

func mapChar*[Cat: enum](
  ch: char, map: static[openarray[tuple[key: char, val: Cat]]]): Cat =

  const
    chars = toKeySet(map)
    map = toMapArray(map)

  if ch notin chars:
    raiseArgumentError(
      &"Unexpected input char: got '{ch}', but expected {chars}")

  return map[ch]

type
  StrNormalizationKind* = enum
    snkNoNormalization
    snkNimNormalize
    snkFullNormalize
    snkCaseNormalize

  EnumParseError* = object of ParseError



func normalize*(str: string, kind: StrNormalizationKind): string =
  case kind:
    of snkNoNormalization:
      result = str

    of snkNimNormalize, snkFullNormalize:
      var start = 0
      if kind == snkNimNormalize:
        result.add str[0]
        inc start

      for ch in str[start ..^ 1]:
        case ch:
          of '_', '-': discard
          of 'a'..'z': result.add ch
          of 'A'..'Z': result.add char(ch.uint8 - 32)
          else: result.add ch

    of snkCaseNormalize:
      for ch in str:
        case ch:
          of 'A'..'Z': result.add char(ch.uint8 - 32)
          else: result.add ch


func parseEnum*[E: enum](
    map: array[E, string],
    str: string,
    normalize: StrNormalizationKind = snkNimNormalize,
    optionalPrefix: bool = true
  ): E =
  if optionalPrefix:
    let normalized = str.dropLowerPrefix().normalize(normalize)

    for (key, val) in map:
      if val == normalized:
        return key

    raise newException(
      EnumParseError,
      &"Could not parse enum value for {typeof(E)} from '{str}' (normalized to {normalized})")

  let normalized = str.normalize(normalize)

  for (key, val) in map:
    if val == normalized:
      return key

  raise newException(
    EnumParseError,
    &"Could not parse enum value for {typeof(E)} from '{str}'")



# func `&`*[T](s: seq[T], v: T): seq[T] = s & @[v]

#=========================  functional helpers  ==========================#

func curry1*[A, B, C](arg: proc(a: A, b: B): C, a: A): proc(b: B): C =
  return proc(b: B): C = arg(a, b)

func curry2*[A, B, C](arg: proc(a: A, b: B): C, b: B): proc(a: A): C =
  return proc(a: A): C = arg(a, b)

template matchProc1*[A, B](pr: untyped): proc(a: A): B =
  block:
    proc tmp(a: A): B = pr(a)
    tmp

template matchProc2*[A, B, C](pr: untyped): proc(a: A, b: B): C =
  block:
    proc tmp(a: A, b: B): C = pr(a, b)
    tmp

template matchCurry2*[B](tA: typed, val: B, pr: untyped): untyped =
  block:
    type ResT = typeof((var a: tA; pr(a, val)))
    proc tmp(a: tA): ResT = pr(a, val)
    tmp


when isMainModule:
  proc t(a, b: int): string = $a & $b
  proc t(a, b: string): string = a & b
  assert matchProc2[int, int, string](t).curry1(9)(3) == "93"
  assert matchProc2[int, int, string](t).curry2(9)(3) == "39"
  assert matchCurry2(int, 9, t)(3) == "39"
