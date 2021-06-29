import
  ./blockfmt,
  std/[
    typetraits, options, unicode, sets,
    json, strformat, tables, strutils, sequtils
  ],
  ".."/[
    hexceptions,
    hdebug_misc,
    types/colorstring,
    macros/introspection,
    algo/hseq_mapping,
    algo/hseq_distance
  ]

type
  PprintType* = object
    ## Simplified representation of generic types
    head*: string
    parameters*: seq[PPrintType]


  PprintTreeKind* = enum
    ptkIgnored
    ptkVisited

    ptkConst
    ptkTuple
    ptkList
    ptkMapping
    ptkObject
    ptkNamedTuple

  PPrintPathKind = enum
    ppkField
    ppkObject
    ppkIndex
    ppkKey

  PPrintPathElem = object
    case kind*: PPrintPathKind
      of ppkIndex:
        idx*: int

      else:
        name*: string

  PPrintPath* = seq[PPrintPathElem]

  PprintTree* = object
    path*: PPrintPath
    treeId*: int
    styling* {.requiresinit.}: PrintStyling
    treeType*: PPrintType
    size*: int
    height*: int
    forceLayout*: Option[tuple[line, stack: bool]]


    case kind*: PprintTreeKind
      of ptkIgnored, ptkVisited:
        discard

      of ptkConst:
        strVal*: string

      of ptkObject, ptkNamedTuple, ptkTuple, ptkList:
        elements*: seq[tuple[key: string, value: PPrintTree]]

      of ptkMapping:
        mappings*: seq[tuple[key, val: PPrintTree]]

  PPrintGlobPart = object
    case kind*: GenGlobPartKind
      of ggkWord:
        name*: string

      else:
        discard

  PPrintLytChoice = tuple[line, stack: bool]
  PPrintGlob = seq[PPrintGlobPart]

  PPrintMatch = object
    globs*: seq[PPrintGlob]

  PPrintConf* = object
    idCounter: int
    visited: HashSet[int]

    ignorePaths*: PPrintMatch
    forceLayouts*: seq[tuple[match: PprintMatch,
                             force: PPrintLytChoice]]

    maxStackHeightChoice*: int
    minLineHeightChoice*: int
    minLineSizeChoice*: int
    formatOpts*: LytOptions
    colored*: bool
    sortBySize*: bool
    alignSmallFields*: bool
    alignSmallGrids*: bool

proc globKind*(pprintGlob: PPrintGlobPart): GenGlobPartKind = pprintGlob.kind
proc globKind*(pprintGlob: PPrintPathElem): GenGlobPartKind = ggkWord

func pglob*(): PPrintGlob = discard


func star*(main: sink PPrintGlob): PPrintGlob =
  result = main
  result.add @[PPrintGlobPart(kind: ggkAnyStar)]


func field*(main: sink PPrintGlob, field: string): PPrintGlob =
  result = main
  result.add PPrintGlobPart(kind: ggkWord, name: field)

func match*(globs: varargs[PPrintGlob]): PPrintMatch =
  PPrintMatch(globs: toSeq(globs))

func matchField*(name: varargs[string]): PPrintMatch =
  mapIt(name, pglob().star().field(it).star()).match()

func forceLine*(): PPrintLytChoice = (true, false)
func forceStack*(): PPrintLytChoice = (false, true)
func forceChoice*(): PPrintLytChoice = (true, true)

proc pathElem*(kind: PPrintPathKind, name: string): PPrintPathElem =
  result = PprintPathElem(kind: kind)
  result.name = name

proc pathElem*(idx: int): PPrintPathElem =
  PPrintPathElem(kind: ppkIndex, idx: idx)

func `$`*(part: PPrintGlobPart): string =
  "[" & ($part.kind)[3..^1] & " " & (if part.kind == ggkWord: part.name else: "") & "]"

func `$`*(part: PPrintGlob): string =
  for p in part:
    result &= $p


func globEqCmp*(path: PPrintPathElem, glob: PPrintGlobPart): bool =
  let k = (path.kind, glob.kind)
  if k == (ppkField, ggkWord):
    result = path.name == glob.name

func matches*(match: PPrintMatch, path: PPrintPath): bool =
  for glob in match.globs:
    if gitignoreGlobMatch(path, glob, globEqCmp):
      return true


proc newPPrintTree*(
    kind: PPrintTreeKind, id: int, path: PPrintPath,
    conf: PprintConf,
    styling: PrintStyling = initPrintStyling()): PPrintTree =

  result = PPrintTree(kind: kind, path: path, treeId: id, styling: styling)

  for (patt, force) in conf.forceLayouts:
    if patt.matches(path):
      result.forceLayout = some force


func updateCounts*(tree: var PPrintTree, conf: PPrintConf) =
  case tree.kind:
    of ptkConst, ptkVisited, ptkIgnored:
      tree.height = 1

    of ptkObject, ptkNamedTuple, ptkTuple, ptkList:
      for (_, val) in tree.elements:
        tree.height = max(tree.height, val.height)
        tree.size += val.size + 1

      inc tree.height

      if conf.sortBySize:
        sortIt(tree.elements, it.value.size)

    of ptkMapping:
      for (key, val) in tree.mappings:
        tree.height = max(max(tree.height, key.height), val.height)
        tree.size += val.size + key.size

      inc tree.height

      if conf.sortBySize:
        sortIt(tree.mappings, max(it[0].size, it[1].size))




func updateCounts*(tree: sink PPrintTree, conf: PPrintConf): PPrintTree =
  result = tree
  updateCounts(result, conf)

proc newPPrintConst*(
    value, constType: string, id: int,
    styling: PrintStyling, path: PPrintPath
  ): PprintTree =

  PPrintTree(
    kind: ptkConst, strVal: value, treeId: id,
    styling: styling, path: path, treeType: PPrintType(head: constType))


proc ignoredBy*(conf: PPrintConf, path: PPrintPath): bool =
  conf.ignorePaths.matches(path)

proc isVisited*[T](conf: PPrintConf, obj: T): bool =
  when obj is ref or obj is ptr:
    cast[int](obj) in conf.visited

  else:
    false

proc visit*[T](conf: var PprintConf, obj: T) =
  when obj is ref or obj is ptr:
    conf.visited.incl cast[int](obj)

proc getId*[T](
    conf: var PPrintConf, entry: T, forceCounter: bool = false): int =

  if forceCounter:
    result = conf.idCounter
    inc conf.idCounter

  when entry is ref or entry is ptr:
    when nimvm:
      when compiles(hash(entry)):
        hash(entry)

      else:
        result = conf.idCounter
        inc conf.idCounter

    else:
      result = cast[int](entry)

  else:
    result = conf.idCounter
    inc conf.idCounter


proc toPprintTree*(
    val: JsonNode, conf: var PPrintConf, path: PPrintPath): PPrintTree =
  ## Dedicated pretty-print converter implementation for `JsonNode`
  let nilval = newPprintConst("nil", "nil", conf.getId(val), fgCyan + bgCyan, path)

  if isNil(val):
    result = nilval

  else:
    case val.kind:
      of JNull:
        result = nilval

      of JBool:
        result = newPPrintConst(
          $val.getBool(), "bool",
          conf.getId(val), fgBlue + bgDefault, path)

      of JInt:
        result = newPPrintConst(
          $val.getInt(), "int",
          conf.getId(val), fgBlue + bgDefault, path)

      of JFloat:
        result = newPPrintConst(
          $val.getInt(), "float",
          conf.getId(val), fgMagenta + bgDefault, path)

      of JString:
        result =newPPrintConst(
          &"\"{val.getStr()}\"", "string",
          conf.getId(val), fgYellow + bgDefault, path)

      of JArray:
        result = newPPrintTree(ptkList, conf.getId(val), path, conf)
        for idx, item in val.getElems():
          result.elements.add(($idx, toPPrintTree(
            item, conf, path & pathElem(idx))))

      of JObject:
        result = newPPrintTree(ptkObject, conf.getId(val), path, conf)
        for name, item in pairs(val.getFields()):
          result.elements.add(($name, toPPrintTree(
            item, conf, path & pathElem(ppkField, name))))

  result.updateCounts(conf)

proc toPprintTree*[T](
    entry: T, conf: var PPrintConf, path: PPrintPath): PPrintTree =

  if conf.isVisited(entry):
    return newPPrintTree(
      ptkVisited, conf.getId(entry), path,
      conf, fgRed + bgDefault).updateCounts(conf)

  elif conf.ignoredBy(path):
    return newPPrintTree(
      ptkIgnored, conf.getId(entry), path,
      conf, fgYellow + bgDefault).updateCounts(conf)

  else:
    conf.visit(entry)

    var entryT: PPrintType

    when entry is typeof(nil):
      return newPPrintConst(
        "nil", "nil", conf.getId(entry), fgBlue + bgDefault, path).updateCounts(conf)

    elif not ( # key-value pairs (tables etc.)
        (entry is seq) or
        (entry is array) or
        (entry is openarray) or
        (entry is string)
      ) and compiles(for k, v in pairs(entry): discard):

      result = newPPrintTree(
        ptkMapping, conf.getId(entry), path, conf)

      for key, val in pairs(entry):
        let res = toPPrintTree(val, conf, path)
        if res.kind notin {ptkIgnored}:
          result.mappings.add((toPPrintTree(key, conf, path), res))

    elif (entry is array) and
         (
           when compiles(genericParams(typeof entry)):
             get(genericParams(typeof entry), 0) is (
               StaticParam[char] or static[char] or char or
               StaticParam[enum] or static[enum] or enum
             )
           else:
             false
         )
      :
      type
        ArrKey = get(genericParams(typeof entry), 0)
        ArrValue = get(genericParams(typeof entry), 1)

      mixin items

      result = newPPrintTree(
        ptkMapping, conf.getId(entry), path, conf)

      for key, val in pairs(entry):
        when ArrKey is (StaticParam[enum] or static[enum] or enum):
          when key is range:
            # FIXME get underlying enum type instead of `range[]`.
            # `directEnumName` uses `getTypeImpl`, which cannot handle ranges
            # correctly. It can be fixed here, or in
            # `hmisc/macros/introspection`, but I would prefer to figure out
            # the way to implement it with `std/typetraits` if possible.

            # The question is: how to get type of `array` range using
            # `typetraits` (or somehow else)? - I can get to `range
            # 0..3(int)` using `genericParams` and `get()`, but I cannot use
            # them repeatedly - `echo genericParams(array[0 .. 3,
            # int]).get(0).genericParams()` fails to compile with
            #
            # ```nim
            # Error: type expected, but got:
            # typeof(default:
            #   type
            #     T2`gensym5 = array[0 .. 3, int]
            #   (range[0 .. 3], int)[0])
            # ```

            let keyName = directEnumName(key)

          else:
            let keyName = directEnumName(key)

        else:
          let keyName = $key

        let res = toPPrintTree(
          val, conf, path & pathElem(ppkKey, keyName))

        if res.kind notin {ptkIgnored}:
          result.mappings.add((
            newPPrintConst(
              keyName, $typeof(key),
              conf.getId(key), fgGreen + bgDefault, path), res))


    elif not ( # sequences but not strings
        (entry is string) or
        (entry is char)
      ) and (
      (
        (compiles(for i in items(entry): discard)) or
        (compiles(for i in items(entry[]): discard))
      ) and (not compiles(entry.kind))
      # Iterable objects with `.kind` field are most likely to be some form
      # of AST
    ):
      mixin items
      const directItems = compiles(for i in items(entry): discard)

      result = newPPrintTree(
        ptkList, conf.getId(entry), path, conf)

      var idx: int = 0
      for it in (
        when directItems:
          items(entry)

        else:
          items(entry[])
      ):
        let res = toPPrintTree(it, conf, path & pathElem(idx))
        if res.kind notin { ptkIgnored }:
          result.elements.add(($idx, res))

        inc idx

    elif not (entry is Option) and
         (
           (entry is object) or # objects
           (entry is tuple) or
           (entry is ref object) or
           (entry is ref tuple) or
           (entry is ptr object) or
           (entry is ptr tuple)
         ):
      entryT.head = $typeof(entry)

      let id = conf.getId(entry)

      when (entry is object) or
           (entry is ref object) or
           (entry is ptr object):
        let kind = ptkObject

        let path = path & pathElem(ppkObject, $typeof(entry))

      elif isNamedTuple(T):
        let kind = ptkNamedTuple

      else:
        let kind = ptkTuple

      when entry is ptr or entry is ref:
        if isNil(entry):
          result = newPPrintTree(
            kind, conf.getId(entry, true), path, conf)

          result.styling = fgRed + bgDefault

          return

        else:
          result = newPPrintTree(
            kind, conf.getId(entry), path, conf)


      else:
        result = newPPrintTree(
          kind, conf.getId(entry), path, conf)

      result.treeType = entryT

      when (entry is ref object) or
           (entry is ref tuple) or
           (entry is ptr object) or
           (entry is ptr tuple):
        for name, value in fieldPairs(entry[]):
          let res = toPPrintTree(
            value, conf, path & pathElem(ppkField, name))

          if res.kind notin {ptkIgnored}:
            result.elements.add((name, res))

      else:
        for name, value in fieldPairs(entry):
          let res = toPPrintTree(
            value, conf, path & pathElem(ppkField, name))

          if res.kind notin {ptkIgnored}:
            result.elements.add((name, res))


    elif (entry is proc): # proc type
      result = newPPrintConst(
        $(typeof(T)), $(typeof("T")), conf.getId(entry),
        fgMagenta + bgDefault, path)

    elif entry is Option:
      if entry.isSome():
        result = toPPrintTree(entry.get(), conf, path)

      else:
        result = newPPrintConst(
          "none", $typeof(entry), conf.getId(entry),
          fgCyan + bgDefault, path)

    else:
      entryT.head = $typeof(entry)
      var style = initPrintStyling()

      when entry is string:
        let val = "\"" & entry & "\""
        style = fgYellow + bgDefault

      elif entry is pointer:
        let val = "<pointer>"
        style = fgRed + bgDefault

      elif entry is void:
        let val = "<void>"
        style = fgRed + bgDefault + styleItalic

      elif entry is Rune:
        let val = "\'" & $(@[entry]) & "\'"
        style = fgYellow + bgDefault

      elif entry is NimNode:
        let val = entry.treeRepr()
        style = fgDefault + styleItalic

      elif entry is (SomeNumber | bool):
        let val = $entry
        style = bgDefault + fgBlue

      elif entry is enum:
        let val = $entry
        style = fgGreen + bgDefault

      else:
        when entry is distinct and not compiles($entry):
          let val = $distinctBase(entry)

        else:
          let val = $entry

      result = newPPrintCOnst(
        val,
        $typeof(entry),
        conf.getId(entry),
        style,
        path
      )

  updateCounts(result, conf)

proc toPPrintTree*[T](obj: T): PPrintTree =
  var conf = PPrintConf()
  return toPPrintTree(obj, conf, @[])

initBlockFmtDSL()

proc layouts(tree: PPrintTree, conf: PPrintConf): PprintLytChoice =
  if tree.forceLayout.isSome():
    result = tree.forceLayout.get()

  else:
    result =
      if tree.height >= conf.maxStackHeightChoice:
        (false, true)

      elif tree.height < conf.minLineHeightChoice and
           tree.size < conf.minLineSizeChoice:
        (true, false)

      else:
        (true, true)

proc toPPrintBlock*(tree: PPrintTree, conf: PPrintConf): LytBlock =
  let (hasLine, hasStack) = tree.layouts(conf)

  case tree.kind:
    of ptkConst:
      result = T[toColored(tree.strVal, tree.styling, conf.colored)]

    of ptkTuple, ptkNamedTuple, ptkObject:
      var
        hasName = tree.kind == ptkObject
        hasFields = tree.kind in {ptkNamedTuple, ptkObject}

        line = H[]
        stack = V[]


      var maxName = 0
      for idx, (name, value) in tree.elements:
        if conf.alignSmallFields and value.size < 6:
          maxName = max(name.len, maxName)


      for idx, (name, value) in tree.elements:
        var valueBlock = toPPrintBlock(value, conf)
        # echov  valueBlock.treeRepr()
        if hasLine:
          line.add H[
            T[", "] ?? idx > 0,
            T[name & ": "] ?? hasFields, valueBlock]

        if hasStack:
          let name = strutils.alignLeft(name & ": ", maxName + 2)
          let toAdd =
            if value.height > 3 or valueBlock.minWidth > 20:
              V[T[name] ?? hasFields, I[2, valueBlock]]

            else:
              H[T[name] ?? hasFields, valueBlock]

          stack.add toAdd

      let open = (T[@[
        toColored(tree.treeType.head, fgGreen + bgDefault, conf.colored),
        toColored("(")]], T["("]) ?? hasName

      if hasLine:
        line = H[open, line, T[")"]]

      if hasStack:
        stack = V[open, I[2, stack],
          # T[")"] # NOTE potentially configurable
        ]

      # line = H[T[$line.minWidth], line]
      # stack = H[T[$stack.minWidth], stack]

      if hasLine and hasStack:
        result = C[line, stack]

      elif hasLine:
        result = line

      else:
        result = stack

    of ptkVisited:
      result = T[toColored("<visited>", fgRed + bgDefault, conf.colored)]

    of ptkList:
      var
        line = H[T["["]]
        stack = V[]

      for idx, (name, value) in tree.elements:
        if hasLine:
          line.add T[", "] ?? idx > 0
          line.add toPprintBlock(value, conf)

        if hasStack:
          stack.add H[T["- "], toPPrintBlock(value, conf)]

      if hasLine:
        line.add T["]"]

      if hasLine and hasStack:
        result = C[line, stack]

      elif hasLine:
        result = line

      else:
        result = stack

    of ptkMapping:
      var
        line = H[T["{"]]
        stack = V[]

      for idx, (key, val) in tree.mappings:
        if hasLine:
          line.add H[
            T[", "] ?? idx > 0,
            toPPrintBlock(key, conf),
            T[": "],
            toPPrintBlock(val, conf)
          ]

        if hasStack:
          stack.add V[
            H[toPPrintBlock(key, conf), T[" ="]],
            I[2, toPPrintBlock(val, conf)]
          ]

      if hasLine:
        line.add T["}"]

      if hasLine and hasStack:
        result = C[line, stack]

      elif hasLine:
        result = line

      else:
        result = stack


    else:
      raise newImplementKindError(tree)

const defaultPPrintConf* = PPrintConf(
  formatOpts: defaultFormatOpts,
  maxStackHeightChoice: 5,
  minLineHeightChoice: 2,
  minLineSizeChoice: 6,
  colored: true,
  sortBySize: true,
  alignSmallFields: true,
  alignSmallGrids: true
)

proc pptree*[T](obj: T, conf: PPrintConf = defaultPPrintConf): PPrintTree =
  var conf = conf
  return toPPrintTree(obj, conf, @[])

proc ppblock*[T](obj: T, conf: PPrintConf = defaultPPrintConf): LytBlock =
  var conf = conf
  return toPPrintTree(obj, conf, @[]).toPPrintBlock(conf)

proc pstring*[T](
    obj: T, rightMargin: int = 80,
    force: openarray[(PPrintMatch, PPrintLytChoice)] = @[],
    ignore: PPrintMatch = PPrintMatch(),
    conf: PPrintConf = defaultPPrintConf,
  ): string =

  var conf = conf
  if conf.formatOpts.rightMargin ==
     defaultPPrintConf.formatOpts.rightMargin:

    conf.formatOpts.rightMargin = rightMargin

  conf.forceLayouts = toSeq(force)
  conf.ignorePaths = ignore

  return toPPrintTree(obj, conf, @[]).
    toPPrintBlock(conf).
    toString(conf.formatOpts.rightMargin, opts = conf.formatOpts)


proc pprint*[T](
    obj: T, rightMargin: int = 80,
    force: openarray[(PPrintMatch, PPrintLytChoice)] = @[],
    ignore: PPrintMatch = PPrintMatch(),
    conf: PPrintConf = defaultPPrintConf
  ) =

  echo pstring(obj, rightMargin, force, ignore, conf = conf)
