# - TODO :: runtime switching for `pairs`/`items` usee for objects
# - TODO :: rewrite `treeRepr` from hpprint_repr

import
  ./blockfmt,
  std/[
    typetraits, options, unicode, sets, segfaults,
    json, strformat, tables, strutils, sequtils
  ],
  ".."/[
    core/all,
    core/algorithms,
    core/code_errors,
    types/colorstring,
    macros/introspection,
    macros/argpass,
    algo/hseq_mapping,
    algo/hseq_distance,
    algo/halgorithm,
    algo/clformat,
    algo/procbox
  ]

type
  PPrintType* = object
    ## Simplified representation of generic types
    head*: string
    isVariant*: bool
    parameters*: seq[PPrintType]


  PPrintTreeKind* = enum
    ptkIgnored
    ptkVisited
    ptkNil
    ptkAnnotation

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

      of ppkObject:
        typeName*: PPrintType

      else:
        name*: string

  PPrintPath* = seq[PPrintPathElem]
  PPrintLytChoice* = tuple[line, stack: bool]
  PPrintLytForce* = tuple[match: PprintMatch, force: PPrintLytChoice]
  PPrintExtraField* = tuple[match: PPrintMatch, impl: ProcBox]


  PPrintGlobPart = object
    case kind*: GenGlobPartKind
      of ggkWord:
        name*: string

      else:
        discard

  PPrintGlob = seq[PPrintGlobPart]

  PPrintMatch = object
    globs*: seq[PPrintGlob]

  PPrintConf* = object
    idCounter: int
    visited: CountTable[int]

    ignorePaths*: PPrintMatch
    stringPaths*: PPrintMatch
    forceLayouts*: seq[PPrintLytForce]
    extraFields*: seq[PPrintExtraField]
    idFields*: seq[(PPrintMatch, string)] ## `path pattern` -> `id field`
    ## name. Object id fields are highlighed differently in the result and
    ## can be used to select particular values.

    showTypes*: bool
    maxStackHeightChoice*: int
    minLineHeightChoice*: int
    minLineSizeChoice*: int
    formatOpts*: LytOptions
    colored*: bool
    sortBySize*: bool
    alignSmallFields*: bool
    alignSmallGrids*: bool


const
  ptkObjectKinds* = {
    ptkObject, ptkNamedTuple, ptkTuple, ptkList,
    ptkAnnotation
  }

type
  PPrintTree* = object
    annotation*: ColoredText
    path*: PPrintPath
    treeId*: int
    styling* {.requiresinit.}: PrintStyling
    treeType*: PPrintType
    size*: int
    height*: int
    forceLayout*: Option[PPrintLytChoice]
    visitedAt*: int


    case kind*: PprintTreeKind
      of ptkIgnored, ptkNil, ptkVisited:
        discard

      of ptkConst:
        strVal*: string

      of ptkObjectKinds:
        elements*: seq[tuple[key: string, value: PPrintTree]]

      of ptkMapping:
        mappings*: seq[tuple[key, val: PPrintTree]]



proc globKind*(pprintGlob: PPrintGlobPart): GenGlobPartKind = pprintGlob.kind
proc globKind*(pprintGlob: PPrintPathElem): GenGlobPartKind = ggkWord

func pglob*(): PPrintGlob = discard


func star*(main: sink PPrintGlob): PPrintGlob =
  result = main
  result.add @[PPrintGlobPart(kind: ggkAnyStar)]


func field*(main: sink PPrintGlob, field: string): PPrintGlob =
  result = main
  result.add PPrintGlobPart(kind: ggkWord, name: field)

func typeName*(main: sink PPrintGlob, name: string): PPrintGlob =
  result = main
  result.add PPrintGlobPart(kind: ggkWord, name: name)

func match*(globs: varargs[PPrintGlob]): PPrintMatch =
  PPrintMatch(globs: toSeq(globs))

func matchField*(name: varargs[string]): PPrintMatch =
  mapIt(name, pglob().star().field(it).star()).match()

func matchType*(name: varargs[string]): PPrintMatch =
  mapIt(name, pglob().star().typeName(it).star()).match()

func matchAll*(): PPrintMatch =
  match(pglob().star())

func forceLine*(): PPrintLytChoice = (true, false)
func forceStack*(): PPrintLytChoice = (false, true)
func forceChoice*(): PPrintLytChoice = (true, true)

proc pathElem*(kind: PPrintPathKind, name: string): PPrintPathElem =
  result = PprintPathElem(kind: kind)
  result.name = name

proc pathElem*(typename: PPrintType): PPrintPathElem =
  result = PprintPathElem(kind: ppkObject, typeName: typeName)

proc pathElem*(idx: int): PPrintPathElem =
  PPrintPathElem(kind: ppkIndex, idx: idx)

template pprintExtraField*(
    typename: untyped, fieldName: string, body: untyped): untyped =
  (
    matchType(astToStr(typename)),
    toProcBox(
      proc(it {.inject.}: typename): (string, PPrintTree) = (fieldName, body)
    )
  )


template pprintExtraField*(
    typename: string, argtype: untyped,
    fieldName: string, body: untyped): untyped =
  (
    matchType(typename),
    toProcBox(
      proc(it {.inject.}: argtype): (string, PPrintTree) = (fieldName, body)
    )
  )


func `$`*(part: PPrintGlobPart): string =
  "[" & ($part.kind)[3..^1] & " " & (if part.kind == ggkWord: part.name else: "") & "]"

func `$`*(part: PPrintGlob): string =
  for p in part:
    result &= $p


func globEqCmp*(path: PPrintPathElem, glob: PPrintGlobPart): bool =
  let (pk, gk) = (path.kind, glob.kind)
  if pk == ppkField and gk == ggkWord:
    result = path.name == glob.name

  elif pk == ppkIndex and gk == ggkWord:
    result = false

  elif pk == ppkObject and gk == ggkWord:
    result = path.typeName.head.startsWith(glob.name)

  else:
    raise newImplementKindError(glob,
      &"Compare glob of kind {glob.kind} and path of kind {path.kind}")

func matches*(match: PPrintMatch, path: PPrintPath): bool =
  for glob in match.globs:
    if gitignoreGlobMatch(path, glob, globEqCmp):
      return true


proc newPPrintType*(head: PprintType, _: bool = false): PPrintType =
  head

proc newPPrintType*(head: string, isVariant: bool = false): PPrintType =
  result.head = head
  result.isVariant = isVariant

proc newPPrintType*[T](obj: T): PPrintType =
  let tof = $typeof(obj)
  let split = tof.splitTokenize({'.', ',', '[', ']', ' '})

  var idx = 0
  var buf: string
  while idx < split.len:
    if idx == split.high:
      buf.add split[idx]
      inc idx

    else:
      if split[idx + 1] == ".":
        inc idx
        inc idx

      buf.add split[idx]
      inc idx

  result.head = buf

proc newPPrintTree*(
    kind: PPrintTreeKind, id: int, path: PPrintPath,
    conf: PprintConf,
    styling: PrintStyling = initPrintStyling()): PPrintTree =

  result = PPrintTree(kind: kind, path: path, treeId: id, styling: styling)

  for (patt, force) in conf.forceLayouts:
    if patt.matches(path):
      result.forceLayout = some force

proc newPPrintNil*(id: int, path: PPrintPath, conf: PPrintConf): PPrintTree =
  newPPrintTree(ptkNil, id, path, conf)

proc isCommonType*(t: PprintType): bool =
  t.head in ["string", "int", "float", "char", "bool"] or
  t.head.startsWith("proc")


proc `$`*(t: PPrintType): string =
  result = t.head
  if t.parameters.len > 0:
    result &= "["
    for idx, param in t.parameters:
      if idx > 0: result &= ", "
      result &= $param

    result &= "]"

proc `$`*(elem: PPrintPathElem): string =
  case elem.kind:
    of ppkIndex:
      result = &"[{elem.idx}]"

    of ppkObject:
      result = &"<{elem.typeName}>"

    else:
      result = &"[{elem.name}]"

proc `$`*(path: PPrintPath): string =
  for idx, elem in pairs(path):
    if idx > 0: result &= "/"
    result.add "["
    result.add $elem
    result.add "]"



func updateCounts*(
    tree: var PPrintTree,
    sortBySize: bool = false
  ) =
  case tree.kind:
    of ptkConst, ptkVisited, ptkIgnored, ptkNil, ptkAnnotation:
      tree.height = 1

    of ptkObject, ptkNamedTuple, ptkTuple, ptkList:
      for (_, val) in tree.elements:
        tree.height = max(tree.height, val.height)
        tree.size += val.size + 1

      inc tree.height

      if sortBySize:
        sortIt(tree.elements, it.value.size)

    of ptkMapping:
      for (key, val) in tree.mappings:
        tree.height = max(max(tree.height, key.height), val.height)
        tree.size += val.size + key.size

      inc tree.height

      if sortBySize:
        sortIt(tree.mappings, max(it[0].size, it[1].size))




func updateCounts*(
    tree: sink PPrintTree,
    sortBySize: bool = false
  ): PPrintTree =

  result = tree
  updateCounts(result, sortBySize)

proc newPPrintConst*(
    value, constType: string, id: int,
    styling: PrintStyling, path: PPrintPath
  ): PprintTree =

  PPrintTree(
    kind: ptkConst, strVal: value, treeId: id,
    styling: styling, path: path, treeType: PPrintType(head: constType))


proc newPPrintConst*(
    value: string, constType: PPrintType, id: int,
    styling: PrintStyling, path: PPrintPath
  ): PprintTree =

  PPrintTree(
    kind: ptkConst, strVal: value, treeId: id,
    styling: styling, path: path, treeType: constType)

proc newPPrintNil*[T](
    value: T, path: PPrintPath, msg: string, conf: var PPrintConf
  ): PPrintTree =
    newPPrintConst(
      "[!" & msg & "!]",
      $typeof(value),
      conf.getId(value),
      fgRed + bgDefault,
      path)


proc newPPrintError*[T](
    value: T, path: PPrintPath, msg: string, conf: var PPrintConf
  ): PPrintTree =
    newPPrintConst(
      "error [!" & msg & "!]",
      $typeof(value),
      0, fgRed + bgDefault, path)



proc add*(this: var PPrintTree, other: PPrintTree) =
  case this.kind:
    of ptkObjectKinds:
      this.elements.add(("", other))

    of ptkMapping:
      this.mappings.add((
        newPPrintConst("", "", 0, defaultPrintStyling, @[]),
        other
      ))

    else:
      raise newUnexpectedKindError(this)

  updateCounts(this)


proc add*(this: var PPrintTree, key: string, other: PPrintTree) =
  case this.kind:
    of ptkObjectKinds:
      this.elements.add((key, other))

    of ptkMapping:
      this.mappings.add((
        newPPrintConst(key, "", 0, defaultPrintStyling, @[]),
        other
      ))

    else:
      raise newUnexpectedKindError(this)

  updateCounts(this)


proc add*(this: var PPrintTree, key, other: PPrintTree) =
  case this.kind:
    of ptkObjectKinds:
      this.elements.add(("???", other))

    of ptkMapping:
      this.mappings.add((key, other))

    else:
      raise newUnexpectedKindError(this)

  updateCounts(this)

proc annotate*(this: var PPrintTree, colored: ColoredText) =
  this.annotation.add colored

proc newPPrintObject*(
    head: string | PPrintType,
    elements: seq[(string, PPrintTree)] = @[],
    styling: PrintSTyling = defaultPrintStyling,
    isVariant: bool = false
  ): PPrintTree =

  result = PPrintTree(
    styling: styling,
    kind: ptkObject,
    elements: elements,
    treeType: newPPrintType(head, isVariant)
  )

  updateCounts(result)

proc newPPrintVariant*(
    kind: string,
    elements: seq[(string, PprintTree)],
    styling: PrintStyling = defaultPrintStyling
  ): PPrintTree =

  newPPrintObject(kind, elements, styling, true)


proc newPPrintMap*(
    elements: seq[(PPrintTree, PPrintTree)],
    styling: PrintStyling = defaultPrintStyling
  ): PPrintTree =

  result = PprintTree(
    styling: styling,
    kind: ptkMapping,
    mappings: elements
  )

  updateCounts(result)

proc newPPrintConst*(
    value: string,
    styling: PrintStyling = defaultPrintStyling
  ): PPrintTree =

  result = PPrintTree(
    styling: styling,
    kind: ptkConst,
    strVal: value,
  )

  updateCounts(result)

proc newPPrintSeq*(
    values: seq[PPrintTree],
    styling: PrintStyling = defaultPrintStyling
  ): PPrintTree =

  result = PPrintTree(kind: ptkList, styling: styling)
  for item in values:
    result.elements.add(("", item))

  updateCounts(result)

proc newPPrintAnnotation*(
    text: ColoredText,
    elements: seq[PPrintTree] = @[]
  ): PPrintTree =
  result = PPrintTree(
    kind: ptkAnnotation,
    annotation: text,
    styling: defaultPrintStyling,
  )

  for elem in elements:
    result.elements.add(("", elem))

  updateCounts(result)

proc getField*(tree: PPrintTree, field: string): PPrintTree =
  tree.elements.findItFirst(it.key == field).value

proc ignoredBy*(conf: PPrintConf, path: PPrintPath): bool =
  conf.ignorePaths.matches(path)

proc isVisited*[T](conf: PPrintConf, obj: T): bool =
  when obj is ref or obj is ptr:
    not isNil(obj) and cast[int](obj) in conf.visited

  else:
    false

proc visit*[T](conf: var PprintConf, obj: T) =
  when obj is ref or obj is ptr:
    conf.visited.inc cast[int](obj)

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

  result.updateCounts(conf.sortBySize)

proc isNilEntry*[T](entry: T): bool =
  when entry is ref or
       entry is ptr or
       compiles(isNil(entr)):
    isNil(entry)

  else:
    false

proc isErrorDeref*[T](entry: T, err: var string): bool =
  when entry is ref or entry is ptr:
    try:
      discard entry[]
      return false

    except:
      err.add "Could not dereference "
      err.add $typeof(T)
      err.add getCurrentExceptionMsg()
      return true

  # elif entry is seq:
  #   when defined(c):
  #     static: echo "C backend"



  else:
    return false


proc toPprintTree*[T](
    entry: T, conf: var PPrintConf, path: PPrintPath): PPrintTree =

  if conf.stringPaths.matches(path) and
     not isNilEntry(entry):
    when compiles($entry):
      result = newPPrintConst(
        $entry,
        newPPrintType(entry),
        conf.getId(entry),
        fgDefault + bgDefault,
        path)

    else:
      result = newPPrintConst(
        "[[No `$` defined for " & $typeof(entry) & "]]",
        newPPrintType(entry),
        conf.getId(entry),
        fgDefault + bgDefault,
        path)

  elif conf.isVisited(entry):
    result = newPPrintTree(
      ptkVisited, conf.getId(entry), path,
      conf, fgRed + bgDefault).updateCounts(conf.sortBySize)

    conf.visit(entry)

  elif conf.ignoredBy(path):
    result = newPPrintTree(
      ptkIgnored, conf.getId(entry), path,
      conf, fgYellow + bgDefault).updateCounts(conf.sortBySize)

  else:
    var err: string
    conf.visit(entry)
    if isNilEntry(entry):
      result = newPPrintNil(conf.getId(entry), path, conf)
      result.treeType = newPprintType(entry)
      updateCounts(result, conf.sortBySize)
      return

    elif isErrorDeref(entry, err):
      result = newPPrintNil(entry, path, err, conf)
      result.treeType = newPPrintType(entry)
      updateCounts(result, conf.sortBySize)
      return

    when entry is typeof(nil):
      return newPPrintConst(
        "nil", "nil", conf.getId(entry),
        fgBlue + bgDefault, path).updateCounts(conf.sortBySize)

    elif not ( # key-value pairs (tables etc.)
        (entry is seq) or
        (entry is array) or
        (entry is openarray) or
        (entry is string) or
        (entry is cstring)
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
        (entry is char) or
        (entry is cstring) or
        (entry is ptr string) or
        (entry is ref string)
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
      try:
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

      except:
        result = newPPrintError(
          entry, path, getCurrentExceptionMsg(), conf)


    elif not (entry is Option) and
         (
           (entry is object) or
           (entry is tuple) or
           (entry is ref object) or
           (entry is ref tuple) or
           (entry is ptr object) or
           (entry is ptr tuple)
         ):

      let id = conf.getId(entry)

      when (entry is object) or
           (entry is ref object) or
           (entry is ptr object):
        let
          kind = ptkObject
          path = path & pathElem(newPprintType(entry))

      elif isNamedTuple(T):
        let kind = ptkNamedTuple

      else:
        let kind = ptkTuple

      when entry is ptr or entry is ref:
        if isNilEntry(entry):
          result = newPPrintTree(
            kind, conf.getId(entry, true), path, conf)

          result.styling = fgRed + bgDefault
          result.treeType = newPprintType(entry)

          return

        else:
          result = newPPrintTree(
            kind, conf.getId(entry), path, conf)


      else:
        result = newPPrintTree(
          kind, conf.getId(entry), path, conf)

      # result.treeType = entryT

      when (entry is ref object) or
           (entry is ref tuple) or
           (entry is ptr object) or
           (entry is ptr tuple):
        for name, value in fieldPairs(entry[]):
          try:
            var err: string
            let res =
              if isErrorDeref(value, err):
                newPPrintNil(value, path, err, conf)

              else:
                toPPrintTree(
                  value, conf, path & pathElem(ppkField, name))

            if res.kind notin {ptkIgnored}:
              result.elements.add((name, res))

          except:
            result.elements.add((name, newPPrintError(
              entry, path, getCurrentExceptionMsg(), conf)))



      else:
        for name, value in fieldPairs(entry):
          let res = toPPrintTree(
            value, conf, path & pathElem(ppkField, name))

          if res.kind notin {ptkIgnored}:
            result.elements.add((name, res))

      for (pattern, impl) in conf.extraFields:
        if pattern.matches(path):
          result.elements.add impl.callAs(
            proc(e: T): (string, PPrintTree), entry)


    elif (entry is proc): # proc type
      result = newPPrintConst(
        $(typeof(T)), $(typeof("T")), conf.getId(entry),
        fgMagenta + bgDefault, path)

    elif entry is Option:
      if entry.isSome():
        if isNilEntry(entry.get()):
          result = newPPrintNil(conf.getId(entry), path, conf)

        else:
          result = toPPrintTree(entry.get(), conf, path)

      else:
        result = newPPrintConst(
          "none", $typeof(entry), conf.getId(entry),
          fgCyan + bgDefault, path)

    else:
      # entryT.head = $typeof(entry)
      var style = initPrintStyling()

      when entry is string:
        let val = "\"" & entry & "\""
        style = fgYellow + bgDefault

      elif (entry is ptr string) or (entry is ref string):
        style = fgYellow + bgDefault
        let val =
          if isNil(entry):
            when entry is ptr:
              "<ptr to nil string>"

            else:
              "<ref to nil string>"

          else:
            try:
              "\"" & entry[] & "\""

            except:
              style = fgRed + bgDefault
              "[!" & getCurrentExceptionMsg() & "!]"


      elif entry is cstring:
        let val = "\"" & $entry & "\""
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
        let val = entry.repr
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
          when compiles($entry):
            let val = $entry

          else:
            style = fgRed + bgDefault
            let val = "<not convertible " & $typeof(entry) & ">"

      result = newPPrintConst(
        val,
        $typeof(entry),
        conf.getId(entry),
        style,
        path
      )

  updateCounts(result, conf.sortBySize)
  result.treeType = newPPrintType(entry)
  when entry is ref or entry is ptr:
    result.visitedAt = cast[int](entry)

  for (match, force) in conf.forceLayouts:
    if match.matches(path):
      result.forceLayout = some force

proc toPPrintTree*[T](obj: T): PPrintTree =
  var conf = PPrintConf()
  return toPPrintTree(obj, conf, @[])

initBlockFmtDSL()

proc layouts(tree: PPrintTree, conf: PPrintConf): PprintLytChoice =
  if tree.forceLayout.isSome():
    result = tree.forceLayout.get()

  else:
    result =
      if tree.kind == ptkAnnotation:
        (line: false, stack: true)

      elif tree.height >= conf.maxStackHeightChoice:
        (line: false, stack: true)

      elif tree.height < conf.minLineHeightChoice and
           tree.size < conf.minLineSizeChoice:
        (line: true, stack: false)

      else:
        (line: true, stack: true)

proc toPPrintBlock*(tree: PPrintTree, conf: PPrintConf): LytBlock =
  let (hasLine, hasStack) = tree.layouts(conf)
  let visitedAt =
    if (tree.visitedAt in conf.visited and
        conf.visited[tree.visitedAt] > 1):
      " @ " & $tree.visitedAt

    else:
      ""

  case tree.kind:
    of ptkConst:
      result = T[toColored(tree.strVal, tree.styling, conf.colored)]

    of ptkTuple, ptkNamedTuple, ptkObject, ptkAnnotation:
      var
        hasName = tree.kind == ptkObject
        hasFields = tree.kind in {ptkNamedTuple, ptkObject}
        isAnnotation = tree.kind == ptkAnnotation

        line = H[]
        stack = V[]


      var maxName = 0
      for idx, (name, value) in tree.elements:
        if conf.alignSmallFields and value.size < 6:
          maxName = max(maxName, name.len + (
            if value.treeType.isCommonType() or not conf.showTypes:
              0

            else:
              len($value.treeType) + 3
          ))


      for idx, (name, value) in tree.elements:
        var valueBlock =
          if value.kind == ptkNil:
            T[toColored("<nil>", fgMagenta + bgDefault, conf.colored)]

          else:
            toPPrintBlock(value, conf)


        var resName: ColoredLine
        if hasFields:
          resName.add toColored(name)

        if value.treeType.isCommonType() or not conf.showTypes:
          if hasFields:
            resName.add toColored(": ")

        else:
          if hasFields:
            resName.add toColored(
              ": " & $value.treeType & " ", fgRed + bgDefault)

        if hasLine:
          line.add H[
            T[", "] ?? idx > 0,
            T[resName] ?? hasFields, valueBlock]

        if hasStack:
          stack.add (
            if hasFields:
              if value.height > 3 or valueBlock.minWidth > 20:
                V[T[resName], I[2, valueBlock]]

              else:
                resName.add toColored(repeat(" ", clamp(
                  maxName - resName.textLen(), 0, high(int))))
                H[T[resName], valueBlock]

            else:
              valueBlock
          )

      let open = condOr(
        hasName,
        T[@[
          toColored(
            tree.treeType.head & visitedAt, fgGreen + bgDefault, conf.colored),
          toColored("(")]],
        T["("]
      )

      let forceStack = false

      if hasLine and not isAnnotation:
        line = H[open.deepCopy(), line, T[")"]]

      if (hasStack or forceStack) and not isAnnotation:
        stack = V[open, I[2, stack]]

      if hasLine and (hasStack or forceStack):
        result = C[line, stack]

      elif hasLine:
        result = line

      else:
        result = stack

      if tree.annotation.len > 0:
        if tree.elements.len == 0:
          result = T[tree.annotation]

        else:
          result = V[T[tree.annotation], I[2, result]]

    of ptkVisited:
      result = T[
        toColored(
          "<visited>" & visitedAt & tern(
            conf.showTypes and not tree.treeType.isCommonType(),
            " " & $tree.treeType,
            ""),
          fgRed + bgDefault, conf.colored
      )]

    of ptkNil:
      result = T[
        toColored(
          "<nil> " & $tree.treeType.head, fgMagenta + bgDefault, conf.colored)
      ]

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
        line = H[T["{ "]]
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

      # if hasLine:
      #   line.add T["}"]

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
  alignSmallGrids: true,
  showTypes: false
)

proc pptree*[T](obj: T, conf: PPrintConf = defaultPPrintConf): PPrintTree =
  var conf = conf
  return toPPrintTree(obj, conf, @[])

proc ppblock*[T](obj: T, conf: PPrintConf = defaultPPrintConf): LytBlock =
  var conf = conf
  return toPPrintTree(obj, conf, @[]).toPPrintBlock(conf)

proc ppblock*(
    obj: PPrintTree,
    conf: PPrintConf = defaultPPrintConf): LytBlock =
  var conf = conf
  return obj.toPPrintBlock(conf)


proc pstring*(
    tree: PPrintTree,
    conf: PPrintConf = defaultPPrintConf,
  ): string =

  return tree.toPPrintBlock(conf).
    toString(conf.formatOpts.rightMargin, opts = conf.formatOpts)

proc pstring*[T](
    obj: T, rightMargin: int = 80,
    force: openarray[(PPrintMatch, PPrintLytChoice)] = @[],
    ignore: PPrintMatch = PPrintMatch(),
    conf: PPrintConf = defaultPPrintConf,
    extraFields: seq[PPrintExtraField] = @[]
  ): string =

  var conf = conf
  if conf.formatOpts.rightMargin ==
     defaultPPrintConf.formatOpts.rightMargin:

    conf.formatOpts.rightMargin = rightMargin

  conf.forceLayouts.add toSeq(force)
  conf.extraFields.add toSeq(extraFields)
  conf.ignorePaths = ignore

  let pblock = toPPrintTree(obj, conf, @[]).toPPrintBlock(conf)

  return pblock.toString(conf.formatOpts.rightMargin, opts = conf.formatOpts)


proc pprint*[T](
    obj: T,
    rightMargin: int = 80,
    force: openarray[(PPrintMatch, PPrintLytChoice)] = @[],
    ignore: PPrintMatch = PPrintMatch(),
    conf: PPrintConf = defaultPPrintConf,
    extraFields: seq[PPrintExtraField] = @[]
  ) =

  echo pstring(
    obj, rightMargin, force, ignore,
    conf = conf,
    extraFields = extraFields
  )

import std/macros


macro pconf*(body: varargs[untyped]): untyped =
  withFieldAssignsTo(
    ident("defaultPPrintConf"), body,
    asExpr = true, withTmp = true)


func debugpprint*[T](
    obj: T, rightMargin: int = 80,
    force: openarray[(PPrintMatch, PPrintLytChoice)] = @[],
    ignore: PPrintMatch = PPrintMatch(),
    conf: PPrintConf = defaultPPrintConf
  ) =

  {.cast(noSideEffect).}:
    echo pstring(obj, rightMargin, force, ignore, conf = conf)

proc treeDiff*(t1, t2: PPrintTree): Option[PPrintTree] =
  var hasDiff: bool = false
  proc aux(t1: var PprintTree, t2: PPrintTree) =
    if t1.kind != t2.kind:
      hasDiff = true
      t1 = newPPrintAnnotation(
        colored(
          "Different tree kinds - ",
          hshow(t1.kind), ", ",
          hshow(t2.kind)))

    else:
      case t1.kind:
        of ptkObject:
          var
            diffObject = newPPrintObject(t1.treeType)
            diffs = newPPrintAnnotation("" + fgDefault)

          let
            lhsFields = toHashSet t1.elements.mapIt(it.key)
            rhsFields = toHashSet t2.elements.mapIt(it.key)

          for field in lhsFields - rhsFields:
            hasDiff = true
            diffs.add newPPrintAnnotation(
              "del " & (field + fgRed), @[t1.getField(field)])

          for field in rhsFields - lhsFields:
            hasDiff = true
            diffs.add newPPrintAnnotation(
              "add " & (field + fgGreen), @[t2.getField(field)])

          for field in sorted(toSeq rhsFields * lhsFields):
            var lhsField = t1.getField(field)
            let rhsField = t2.getField(field)

            aux(lhsField, rhsField)
            diffs.add(field, lhsField)

          t1 = diffObject
          t1.add diffs

        of ptkConst:
          if t1.strVal != t2.strVal:
            hasDiff = true
            t1 = newPPrintAnnotation(colored(
              "value mismatch - \n",
              "lhs: ", t1.strVal + fgGreen, "\n",
              "rhs: ", t2.strVal + fgRed, "\n"
            ))


        of ptkIgnored, ptkVisited, ptkNil, ptkAnnotation,
           ptkTuple, ptkList, ptkMapping, ptkNamedTuple:
          raise newImplementKindError(t1)





  var diff = t1
  aux(diff, t2)
  if hasDiff:
    return some diff
