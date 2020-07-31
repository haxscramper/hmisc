# TODO account for control codes in stings

# TODO allow to discard certain elements after they have been
# generated. NOTE to not overcomplicate interface it should be done
# using `isAllowed(val: ObjTree)` instead of generic predicate.

# TODO allow to compress elements back if necessary (rst node
# represents each field as separate leaf)

## Universal pretty-printer

import types/[hnim_ast, hprimitives]
import helpers

import strformat, tables, strutils, sequtils, unicode
import with, gara # TODO remove gara from dependencies


#==========================  type declaration  ===========================#

type
  Delim* = object
    ## Block delimiters
    content*: string ## String for delimiter
    preferMultiline*: bool ## Don't try to put delimiter on the same
    ## line with content - always prefer new chunks

  DelimPair* = tuple[
    start: Delim,
    final: Delim
  ]

  PPrintConf* = object
    ##[

Pretty print configuration

    ]##

    maxWidth*: int ## Max allowed width
    identStr*: string ## String to use for indentaion
    # wrapLargerThan*: int ##

    kvSeparator*: string ## String to use when separating key-value
    ## pairs.
    tblWrapper*: DelimPair ## Pair of delimiter around table instance

    objWrapper*: DelimPair ## Pair of delimiters around object instance
    fldNameWrapper*: DelimPair ## Pair of delimiter around table key
    fldSeparator*: string ## String to place between key-value pairs in list
    nowrapMultiline*: bool ## Do not wrap mutliline objects in delimiters
    alignFieldsRight*: bool ## Use right align on fields (default - left)

    seqSeparator*: string ## String to place between items in sequence
    seqPrefix*: string ## Prefix to use for multiline sequece
    ## instance. If empty multiline string will be wrapped in regular
    ## delimiters
    seqWrapper*: DelimPair ## Delimiters for sequence instance
    hideEmptyFields*: bool ## Hide empty fields (seq of zero length,
    ## `nil` references etc.).


func isKVpairs(obj: ObjTree): bool =
  ## Check if entry should be printed as list of key-value pairs
  obj.kind == okTable or (obj.kind == okComposed and obj.namedFields)

import json

proc dedicatedConvertMatcher*[Obj](
  val: Obj, conv: proc(obj: Obj): ObjTree): ObjTree =
  ## Helper proc to correctly resolve overloading for pretty print
  ## converters
  return conv(val)

proc prettyPrintConverter(val: JsonNode, path: seq[int] = @[0]): ValObjTree =
  ## Dedicated pretty-print converter implementation for `JsonNode`
  case val.kind:
    of JNull:
      return ValObjTree(
        constType: "nil", kind: okConstant, strLit: "null")
    of JBool:
      return ValObjTree(
        constType: "bool", kind: okConstant, strLit: $val.getBool())
    of JInt:
      return ValObjTree(
        constType: "int", kind: okConstant, strLit: $val.getInt())
    of JFloat:
      return ValObjTree(
        constType: "string", kind: okConstant, strLit: $val.getFloat())
    of JString:
      return ValObjTree(
        constType: "string", kind: okConstant, strLit: &"\"{val.getStr()}\"")
    of JArray:
      return ValObjTree(
        kind: okSequence,
        valItems: val.getElems().mapPairs(
          prettyPrintConverter(rhs, path = path & @[idx])
        )
      )
    of JObject:
      return ValObjTree(
        kind: okComposed,
        namedFields: true,
        namedObject: false,
        sectioned: false,
        fldPairs: val.getFields().mapPairs((
          name: lhs,
          value: prettyPrintConverter(rhs, path = path & @[idx])
        )))

proc prettyPrintConverter(val: seq[Rune], path: seq[int] = @[0]): ValObjTree =
  ValObjTree(
    kind: okConstant,
    constType: "seq[Rune]",
    strLit: &"\"{val}\""
  )

template unref(val: untyped): untyped =
  when val is ref: val[]
  else: val

func checkPrimitive(tree: ObjTree): bool =
  ##[

Check if tree can be considered primitive.

NOTE: values are not checked for primitivenes recursively. Instead
`isPrimitive` field is used.

  ]##
  case tree.kind:
    of okConstant:
      tree.constType in @["string", "int", "float"]
    of okSequence:
      (tree.valItems.len < 5) and
      tree.valItems.allOfIt(it.isPrimitive)
    of okTable:
      (tree.valPairs.len < 5) and
      tree.valPairs.allOfIt(it.val.isPrimitive)
    of okComposed:
      if tree.sectioned:
        # NOTE IMPLEMENT objects with sectioned case fields are not
        # currently supported.
        tree.kindBlocks.len < 5 and
        true
      else:
        (tree.fldPairs.len < 5) and
        tree.fldPairs.allOfIt(it.value.isPrimitive)

type
  IdCounter = object
    now: int

func next(cnt: var IdCounter): int =
  result = cnt.now
  inc cnt.now

proc toSimpleTree*[Obj](
  entry: Obj,
  idCounter: var IdCounter,
  conf: PPrintConf = PPrintConf(),
  path: seq[int] = @[0]): ValObjTree =
  ## Top-level dispatch for pretty-printing
  ##
  ## Generic implementation for pretty-print conveter for types not
  ## implementing dedicated `prettyPrintConverter`
  mixin prettyPrintConverter
  defer:
    result.isPrimitive = result.checkPrimitive()

  when compiles(prettyPrintConverter(entry, path = path)):
    # If dedicated implementation exists, use it
    return prettyPrintConverter(entry, path = path)
  elif not (
      (entry is seq) or
      (entry is array) or
      (entry is openarray) or
      (entry is string)
    ) and compiles(for k, v in pairs(entry): discard):

    # IMPLEMENT Insert values in sorted order to give the same layout
    # for unordered containers
    result = ValObjTree(
      kind: okTable,
      keyType: $typeof((pairs(entry).nthType1)),
      valType: $typeof((pairs(entry).nthType2)),
      path: path,
      objId: idCounter.next()
    )

    for key, val in pairs(entry):
      result.valPairs.add(($key, toSimpleTree(val,
          idCounter
      )))

  elif not (
      (entry is string)
    ) and (
    (compiles(for i in items(entry): discard)) or
    (compiles(for i in items(entry[]): discard))
  ):
    result = ValObjTree(
      kind: okSequence,
      itemType: $typeof(items(unref entry)),
      objId: (entry is ref).tern(
        cast[int](unsafeAddr entry),
        idCounter.next()
      )
    )

    var idx: int = 0
    for it in items(unref entry):
      result.valItems.add(toSimpleTree(
        it, path = path & @[idx],
        idCounter = idCounter
      ))
      inc idx

  elif (entry is object) or
       (entry is ref object) or
       (entry is tuple):
    let id: int =
      when entry is ref:
        cast[int](unsafeAddr entry)
      else:
        idCounter.next()
    when (entry is object) or (entry is ref object):
      result = ValObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: true,
        namedFields: true,
        objId: id
      )
    elif isNamedTuple(Obj):
      result = ValObjTree(
        kind: okComposed,
        name: $typeof(Obj),
        sectioned: false,
        namedObject: false,
        namedFields: true,
        objId: id
      )
    else:
      result = ValObjTree(
        kind: okComposed,
        sectioned: false,
        namedFields: false,
        namedObject: false,
        objId: id
      )

    result.path = path

    when (entry is ref object):
      if entry == nil:
        result = ValObjTree(
          kind: okConstant,
          constType: $(typeof(Obj)),
          strLit: "nil",
          objId: idCounter.next()
        )
      else:
        var idx: int = 0
        for name, value in entry[].fieldPairs():
          result.fldPairs.add((name, toSimpleTree(
            value, path = path & @[idx],
            idCounter = idCounter
          )))
          inc idx
    else:
      var idx: int = 0
      for name, value in entry.fieldPairs():
        result.fldPairs.add((name, toSimpleTree(
          value,
          path = path & @[idx],
          idCounter = idCounter
        )))
        inc idx


  elif (entry is proc):
    result = ValObjTree(
      kind: okConstant,
      constType: $(typeof(Obj)),
      strLit: $(typeof(Obj)),
      path: path,
      objId: idCounter()
    )
  else:
    when entry is string:
      let val = "\"" & entry & "\""
    elif entry is pointer:
      let val = "<pointer>"
    elif entry is void:
      let val = "<void>"
    elif entry is Rune:
      let val = "\'" & $(@[entry]) & "\'"
    else:
      let val = $entry

    result = ValObjTree(
      kind: okConstant,
      constType: $typeof(Obj),
      strLit: val,
      path: path,
      objId: idCounter.next()
    )



type
  Chunk* = object
    content: seq[string] ## Lines for chunk
    maxWidth: int ## Max line lenght in chunk



type
  RelPos* = enum
    ## Relative position of label to chunk
    rpBottomRight
    # [|||||||]
    # [|||||||] <label>
    rpTopLeftAbove
    # <label>
    #   [|||||||]
    #   [|||||||]
    rpTopLeftLeft
    # <label> [|||||||]
    #         [|||||||]
    rpBottomLeft
    #   [|||||||]
    #   [|||||||]
    # <label>
    rpPrefix
    # <label>[|||||||]
    # <label>[|||||||]

proc lineCount(c: Chunk): int = c.content.len()
proc `$`*(c: Chunk): string = c.content.join("\n")

func multiline(chunk: Chunk): bool = chunk.content.len > 1
func empty(conf: Delim): bool = conf.content.len == 0
func makeDelim*(str: string, multiline: bool = false): Delim =
  Delim(
    # appendNew: str.startsWith('\n'),
    # prependNew: str.endsWith('\n'),
    content: str.strip(chars = {'\n'}),
    preferMultiline: multiline
  )

proc makeChunk*(content: seq[string]): Chunk =
  ## Create chunk from list of lines
  Chunk(
    content: content,
    maxWidth: content.mapIt(it.len()).max(0)
  )

proc makeChunk*(content: string): Chunk =
  ## Create chunk from string
  Chunk(content: @[content], maxWidth: content.len)

proc makeChunk*(other: seq[Chunk]): Chunk =
  ## Create chunk from lines in other chunks
  result = Chunk(
    content: sequtils.concat(other.mapIt(it.content))
  )

  result.maxWidth = result.content.mapIt(it.len()).max(0)

type
  ChunkLabels* = Table[RelPos, tuple[text: string, offset: int]]


proc relativePosition*(
  chunk: Chunk,
  labelsIn:
    ChunkLabels |
    seq[(RelPos, tuple[text: string, offset: int])],
  ignored: set[RelPos] = {}): Chunk =
  ## Position mutliple labels arounc chunk. Labels on `ignored`
  ## positions will be ingored

  let labels: ChunkLabels =
    when labelsIn is Table:
      labelsIn
    else:
      labelsIn.toTable()

  var
    leftPad = 0
    rightPad = 0
    topPad = 0
    bottomPad = 0

  let prefixOverride = (rpPrefix in labels)

  if prefixOverride and (rpTopLeftLeft in labels or rpBottomRight in labels):
    raiseAssert("Incompatible chunk labels - prefix&top-left-left or prefix&bottom-right")

  for pos, label in labels:
    with label:
      match(pos): # XXX remove `match`, use `case`
        rpBottomRight:
          rightPad = text.len
        rpTopLeftAbove:
          topPad = 1
          leftPad = max(offset, leftPad)
        rpTopLeftLeft:
          leftPad = max(leftPad, text.len)
        rpBottomLeft:
          bottomPad = 1
          leftPad = max(leftPad, offset)
        rpPrefix:
          leftPad = text.len

  let resWidth = chunk.maxWidth + leftPad + rightPad
  let resHeight = chunk.lineCount + topPad + bottomPad

  var resLines: seq[string]
  if rpTopLeftAbove in labels:
    resLines.add labels[rpTopLeftAbove].text

  let pref =
    if prefixOverride:
      labels[rpPrefix].text
    else:
      " ".repeat(leftPad)

  for idx, line in chunk.content:
    if idx == 0 and (rpTopLeftLeft in labels):
      resLines.add(labels[rpTopLeftLeft].text & line)
    else:
      resLines.add(pref & line)

  if (rpBottomRight in labels) and
     (rpBottomRight notin ignored) and
     (resLines.len > 0):
    resLines[^1] &= labels[rpBottomRight].text

  if rpBottomLeft in labels:
    resLines.add labels[rpBottomLeft].text

  return makeChunk(resLines.mapIt(strutils.strip(it, leading = false)))


template echov(variable: untyped, other: varargs[string, `$`]): untyped =
  let pref = "  ".repeat(getStackTraceEntries().len)

  when variable is string:
    echo pref, astToStr(variable), ": \"", variable, "\" ", other.join(" ")
  else:
    echo pref, astToStr(variable), ": ", variable, " ", other.join(" ")

proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): Chunk

proc getWrapperConf(current: ObjTree, conf: PPrintConf): tuple[start, final: Delim] =
  let (wrapBeg, wrapEnd) = # Delimiters at the start/end of the block
    case current.kind:
      of okComposed:
        if current.namedObject:
          var objWrap = conf.objWrapper
          objWrap.start.content = &"{current.name}{objWrap.start.content}"
          objWrap
        else:
          conf.objWrapper
      of okSequence:
        conf.seqWrapper
      of okTable:
        # TODO use configurable wrapper begin/end
        conf.tblWrapper
      else:
        assert false, "Cannot arrange kv pair in constant"
        (makeDelim("."), makeDelim("."))

  return (wrapBeg, wrapEnd)


proc getLabelConfiguration(
  conf: PPrintConf, current: ValObjTree, ident: int): tuple[
  item, blocks: ChunkLabels,
  widthconf: (int, int)] =
  ## Get label configuration

  let (wrapBeg, wrapEnd) = getWrapperConf(current, conf)
  var subIdent = 0 # Required right margin for field values
  var maxWidth = 0 # Max allowed withd
  var itemLabels: ChunkLabels
  var blockLabels: ChunkLabels
  let offset = conf.identStr.len # Internal offset between prefix
                                 # delimiter and chunk body
  let prefixOverride = (current.kind == okSequence) and (conf.seqPrefix.len > 0)
  match((prefixOverride, wrapBeg.preferMultiline, wrapEnd.preferMultiline)):
    (true, _, _):
      subIdent = conf.seqPrefix.len
      maxWidth = conf.maxWidth
      itemLabels[rpTopLeftLeft] = (text: conf.seqPrefix, offset: 0)
    (_, true, true):
      # (prefix delimiter)
      # <field> [ block block
      #           block block ]
      # (suffix delimiter)
      subIdent = ident
      maxWidth = conf.maxWidth
      itemLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: offset)
      itemLabels[rpBottomLeft] = (text: wrapEnd.content, offset: offset)
    (_, true, false):
      # (prefix delimiter)
      # <field> [ block block
      #           block block ] (suffix delimiter)
      subIdent = ident
      maxWidth = conf.maxWidth - wrapEnd.content.len()
      itemLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: offset)
      itemLabels[rpBottomRight] = (text: wrapEnd.content, offset: 0)
    (_, false, true):
      # (prefix delimiter) <field> [ block block
      #                              block block ]
      # (suffix delimiter)
      subIdent = ident + wrapBeg.content.len()
      # IMPLEMENT account for sequence prefix, kvSeparator etc.
      maxWidth = conf.maxWidth
      itemLabels[rpTopLeftLeft] = (text: wrapBeg.content, offset: 0)
      itemLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 0)
    (_, false, false):
      # (prefix delimiter) <field> [ block block
      #                              block block ] (suffix delimiter)
      subIdent = ident + wrapBeg.content.len()
      maxWidth = conf.maxWidth - wrapEnd.content.len()
      case current.kind:
        of okSequence:
          itemLabels[rpBottomRight] = (text: conf.seqSeparator, offset: 0)
          blockLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: 2)
          blockLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 2)
        of {okTable, okComposed}:
          itemLabels[rpBottomRight] = (text: conf.fldSeparator, offset: 0)
          # TODO more fine-grained configuration for different
          # wrapping settings.
          if not conf.nowrapMultiline:
            blockLabels[rpTopLeftAbove] = (text: wrapBeg.content, offset: 2)
            blockLabels[rpBottomLeft] = (text: wrapEnd.content, offset: 2)
          elif current.kind == okComposed and current.namedObject:
            blockLabels[rpTopLeftAbove] = (text: current.name, offset: 2)
        else:
          discard

  return (
    item: itemLabels,
    blocks: blockLabels,
    widthconf: (subIdent, offset)
  )



proc arrangeKVPairs(
  input: seq[tuple[name: string, val: Chunk]],
  conf: PPrintConf, current: ValObjTree, ident: int): Chunk =
  ## Layout sequence of key-value pairs. `name` field in tuple items
  ## might be empty if `current.kind` is `okSequence`.

  let (wrapBeg, wrapEnd) = getWrapperConf(current, conf)

  let trySingleLine = (not input.anyOfIt(it.val.multiline()))

  if trySingleLine:
    var singleLine =
      if current.kind == okSequence or (
        current.kind == okComposed and (not current.namedFields)):
        input.mapIt(&"{it.val.content[0]}").join(conf.seqSeparator)
      else:
        input.mapIt(&"{it.name}{conf.kvSeparator}{it.val.content[0]}").join(
          conf.seqSeparator)

    singleLine = wrapBeg.content & singleLine & wrapEnd.content

    if singleLine.len < (conf.maxWidth - ident):
      return makeChunk(content = @[singleLine])

  # Try positioning on multiple lines
  let fldsWidth =
    # Width of the larges field
    if current.isKVPairs():
      input.mapIt(it.name.len()).max() + conf.kvSeparator.len()
    else:
      0

  proc makeFldName(it: tuple[name: string, val: Chunk]): (RelPos, (string, int)) =
    let pos = case current.kind:
      of okSequence: rpTopLeftLeft
      of okTable, okComposed: (it.val.multiline()).tern(rpTopLeftAbove, rpTopLeftLeft)
      of okConstant: rpPrefix

    let text = case current.kind:
      of okComposed, okTable:
        if conf.alignFieldsRight:
          strutils.align(it.name & conf.kvSeparator, fldsWidth)
        else:
          strutils.alignLeft(it.name & conf.kvSeparator, fldsWidth)
      of okSequence:
        ""
      of okConstant:
        raiseAssert("Invalid current kind: constant")

    return (pos, (text, conf.identStr.len()))

  let (itemLabels, blockLabels, widthConf) =
    getLabelConfiguration(conf = conf, current = current, ident = ident)

  return input.enumerate().mapIt(
    relativePosition(it[1].val, (@[makeFldName(it[1])])).
    relativePosition(
      itemLabels,
      ignored = (it[0] == input.len() - 1).tern(
        {rpBottomRight},
        {})))
  .makeChunk()
  .relativePosition(blockLabels)



proc pstringRecursive(
  current: ObjTree, conf: PPrintCOnf, ident: int = 0): Chunk =
  case current.kind:
    of okConstant:
      return makeChunk(content = @[ current.strLit ])
    of okComposed:
      if not current.sectioned:
        let maxFld = current.fldPairs.mapIt(
          it.name.len() + conf.fldNameWrapper.start.content.len() +
          conf.fldNameWrapper.start.content.len()
        ).max()

        result = current.fldPairs.mapIt(
          (
            conf.fldNameWrapper.start.content & it.name &
              conf.fldNameWrapper.final.content,
            pstringRecursive(it.value, conf, maxFld + ident)
          )
        ).arrangeKVPairs(conf, current, ident + maxFld)
    of okTable:
      let maxFld = current.valPairs.mapIt(it.key.len()).max(0)
      return current.valPairs.mapIt(
        (it.key, pstringRecursive(it.val, conf, maxFld + ident))
      ).arrangeKVPairs(conf, current, ident + maxFld)
    of okSequence:
      let tmp = current.valItems.mapIt(
        ("", pstringRecursive(it, conf, ident + conf.seqPrefix.len()))
      )
      result = tmp.arrangeKVPairs(conf, current, ident)

proc prettyString*(tree: ObjTree, conf: PPrintConf, ident: int = 0): string =
  ## Convert object tree to pretty-printed string using configuration `conf`
  var newConf = conf
  newConf.maxWidth = conf.maxWidth - ident
  pstringRecursive(tree, newConf, ident = ident).content.mapIt(
    " ".repeat(ident) & it).join("\n")


const objectPPrintConf = PPrintConf(
  maxWidth: 80,
  identStr: "  ",
  seqSeparator: ", ",
  seqPrefix: "- ",
  seqWrapper: (makeDelim("["), makeDelim("]")),
  objWrapper: (makeDelim("("), makeDelim(")")),
  tblWrapper: (makeDelim("{"), makeDelim("}")),
  kvSeparator: ": ",
  nowrapMultiline: true
)


func getSubnodes*(it: ObjTree): seq[ObjTree] =
  debugecho &"get subnodes for tree id {it.objId}"
  case it.kind:
    of okConstant: raiseAssert("No sub for `okConstant`")
    of okSequence: it.valItems
    of okTable: it.valPairs.mapIt(it.val)
    of okComposed:
      case it.sectioned:
        of false: it.fldPairs.mapIt(it.value)
        of true: raiseAssert(
          "IMPLEMENT Sectioned objects are not supported RN")


func makeCounter*(): IdCounter = IdCounter()
proc pstring*[Obj](obj: Obj, ident: int = 0, maxWidth: int = 80): string =
  var counter = makeCounter()
  var conf = objectPPrintConf
  conf.maxWidth = maxWidth
  prettyString(toSimpleTree(obj, counter), conf, ident)

proc pprint*[Node](tree: ObjTree[Node]): void =
  var conf = objectPPrintConf
  conf.maxWidth = 80
  echo prettyString(tree, conf)


proc toValObjTree*[Obj](obj: Obj): ValObjTree =
  var counter = makeCounter()
  toSimpleTree(obj, counter)

proc pprintAtPath*[Obj](obj: Obj, path: TreePath): void =
  var counter = makeCounter()
  pprint toSimpleTree(obj, counter).getAtPath(path)

proc pprint*[Obj](obj: Obj, ident: int = 0, maxWidth: int = 80): void =
  echo pstring(obj, ident,  maxWidth)
  # var conf = objectPPrintConf
  # conf.maxWidth = maxWidth
  # echo prettyString(toSimpleTree(obj), conf, ident)

func debugPPrint*[Obj](obj: Obj): void =
  {.noSideEffect.}:
    pprint(obj)
