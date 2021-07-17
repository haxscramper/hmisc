import std/[unicode, sequtils]
export unicode

import
  ../base_errors,
  ../helpers

{.pragma: apiStruct, importc, incompleteStruct,
  header: "<tree_sitter/api.h>".}

{.pragma: apiProc, importc, cdecl,
  header: "<tree_sitter/api.h>".}

{.pragma: apiEnum, importc,
  header: "<tree_sitter/api.h>".}

{.pragma: parserStruct, importc, incompleteStruct,
  header: "<tree_sitter/parser.h>".}

{.pragma: parserProc, importc, cdecl,
  header: "<tree_sitter/parser.h>".}

type
  TSTree* {.apiStruct.} = object
  PtsTree* = ptr TSTree

  TSLanguage* {.apiStruct.} = object
    version*: uint32
    symbol_count*: uint32
    alias_count*: uint32
    token_count*: uint32
    external_token_count*: uint32
    state_count*: uint32
    large_state_count*: uint32
    production_id_count*: uint32
    field_count*: uint32
    max_alias_sequence_length*: uint16
    parse_table*: ptr uint16
    small_parse_table*: ptr uint16
    small_parse_table_map*: ptr uint32
    # parse_actions*: ptr TSParseActionEntry
    symbol_names*: cstringArray
    field_names*: cstringArray
    field_map_slices*: ptr UncheckedArray[TSFieldMapSlice]
    field_map_entries*: ptr UncheckedArray[TSFieldMapEntry]
    symbol_metadata*: ptr TSSymbolMetadata
    public_symbol_map*: ptr TSSymbol
    alias_map*: ptr uint16
    alias_sequences*: ptr TSSymbol
    # lex_modes*: ptr TSLexMode
    # lex_fn*: proc (a1: ptr TSLexer; a2: TSStateId): bool
    # keyword_lex_fn*: proc (a1: ptr TSLexer; a2: TSStateId): bool
    # keyword_capture_token*: TSSymbol

  PtsLanguage* = ptr TSLanguage

  TSParser* {.apiStruct.} = object
  PtsParser* = ptr TSParser

  TSInput* {.apiStruct.} = object
  PtsInput* = ptr TSInput
  TSFieldId* = distinct uint16
  TSSymbol* = distinct uint16

  PTsTreeCursor* = ptr TsTreeCursor
  TsTreeCursor* {.apiStruct.} = object
    tree*: pointer
    id*: pointer
    context*: array[2, uint32]

  TSNode* {.apiStruct.} = object
    context*: array[4, uint32]
    id*: pointer
    tree*: PtsTree

  PtsRange* = ptr TSRange
  TSRange* {.apiStruct.} = object
    start_point*: TSPoint
    end_point*: TSPoint
    start_byte*: uint32
    end_byte*: uint32

  TSInputEncoding* {.apiEnum.} = enum
    TSInputEncodingUTF8
    TSInputEncodingUTF16


  TSPoint* {.apiStruct.} = object
    row*: uint32
    column*: uint32

  TSLexer* {.parserStruct.} = object
    lookahead: cint
    result_symbol: TSSymbol
    advance: proc(lex: ptr TSLexer, skip: bool) {.cdecl.}
    mark_end: proc(lex: ptr TSLexer) {.cdecl.}
    get_column: proc(lex: ptr TSLexer): cuint {.cdecl.}
    is_at_included_range_start: proc(lex: ptr TSLexer) {.cdecl.}

  TSLogType {.apiEnum.} = enum
    TSLogTypeParse
    TSLogTypeLex

  TSLogger* {.apiStruct.} = object
    payload*: pointer
    log*: proc(payload: pointer, logType: TSLogType, text: cstring) {.cdecl.}

  TSInputEdit* {.apiStruct.} = object
    start_byte*: uint32
    old_end_byte*: uint32
    new_end_byte*: uint32
    start_point*: TSPoint
    old_end_point*: TSPoint
    new_end_point*: TSPoint

  TSQueryCapture* {.apiStruct.} = object
    node*: TSNode
    index*: uint32


  TSQuery* {.apiStruct.} = object
  TSQueryCursor* {.apiStruct.} = object
  TSQueryMatch* {.apiStruct.} = object
    id*: uint32
    pattern_index*: uint16
    capture_count*: uint16
    captures*: UncheckedArray[TSQueryCapture]
    # const TSQueryCapture *captures;

  TSSymbolType* {.apiEnum.} = enum
    TSSymbolTypeRegular
    TSSymbolTypeAnonymous
    TSSymbolTypeAuxiliary

  TSQueryPredicateStepType* {.apiEnum.} = enum
    TSQueryPredicateStepTypeDone
    TSQueryPredicateStepTypeCapture
    TSQueryPredicateStepTypeString

  TSQueryPredicateStep* {.apiStruct.} = object
    stepType* {.importc: "type".}: TSQueryPredicateStepType
    value_id*: uint32

  TSQueryError* {.apiEnum.} = enum
    TSQueryErrorNone
    TSQueryErrorSyntax
    TSQueryErrorNodeType
    TSQueryErrorField
    TSQueryErrorCapture
    TSQueryErrorStructure

  TSFieldMapEntry* {.parserStruct.} = object
    field_id*: TSFieldId
    child_index*: uint8
    inherited*: bool

  TSFieldMapSlice* {.parserStruct.} = object
    index*: uint16
    length*: uint16

  TSSymbolMetadata* {.parserStruct.} = object
    visible*: bool
    named*: bool
    supertype*: bool


proc currRune*(lex: var TsLexer): Rune =
  Rune(lex.lookahead)

proc `[]`*(lex: var TSLexer): Rune =
  Rune(lex.lookahead)

proc `[]`*(lex: var TSLexer, ch: char): bool =
  lex.lookahead.int16 == ch.int16

# proc `[]`*(lex: TSLexer): Rune = Rune(lex.lookahead)


proc advance*(lex: var TsLexer) =
  lex.advance(addr lex, false)

proc skip*(lex: var TSLexer) =
  lex.advance(addr lex, true)

proc markEnd*(lex: var TsLexer) =
  lex.mark_end(addr lex)

proc setTokenKind*(lex: var TsLexer, kind: enum) =
  lex.result_symbol = cast[TSSymbol](kind)

proc finished*(lex: var TsLexer): bool =
  lex.lookahead == 0

proc column*(lex: var TSLexer): int =
  lex.get_column(addr lex).int

proc `==`*(rune: Rune, ch: char): bool =
  rune.int16 == ch.int16


proc ts_parser_parse(
  self: PtsParser, oldTree: PtsTree, input: PtsInput): PtsTree {.apiProc.}

proc ts_parser_new*(): PtsParser {.apiProc.}
proc ts_parser_delete*(parser: PtsParser) {.apiProc.}
proc ts_parser_set_language*(
  self: PtsParser; language: PtsLanguage): bool {.apiProc.}

proc ts_parser_language*(self: PtsParser): PtsLanguage {.apiProc.}
proc ts_parser_set_included_ranges*(
  self: PtsParser; ranges: PtsRange; length: uint32) {.apiProc.}

proc ts_parser_included_ranges*(
  self: PtsParser; length: ptr uint32): ptr TSRange {.apiProc.}

proc ts_parser_parse*(
  self: PtsParser; old_tree: PtsTree; input: TSInput): TSTree {.apiProc.}

proc ts_parser_parse_string*(
  self: PtsParser; old_tree: PtsTree;
  inString: cstring; length: uint32): PtsTree {.apiProc.}

proc ts_parser_parse_string_encoding*(
  self: PtsParser; old_tree: PtsTree;
  string: cstring; length: uint32; encoding: TSInputEncoding): PtsTree {.apiProc.}
proc ts_parser_reset*(self: PtsParser) {.apiProc.}
proc ts_parser_set_timeout_micros*(self: PtsParser; timeout: uint64) {.apiProc.}
proc ts_parser_timeout_micros*(self: PtsParser): uint64 {.apiProc.}
proc ts_parser_cancellation_flag*(self: PtsParser): ptr uint {.apiProc.}
proc ts_parser_set_logger*(self: PtsParser; logger: TSLogger) {.apiProc.}
proc ts_parser_logger*(self: PtsParser): TSLogger {.apiProc.}
proc ts_parser_print_dot_graphs*(self: PtsParser; file: cint) {.apiProc.}
proc ts_parser_halt_on_error*(self: PtsParser; halt: bool) {.apiProc.}
proc ts_tree_copy*(self: PtsTree): PtsTree {.apiProc.}
proc ts_tree_delete*(self: PtsTree) {.apiProc.}
proc ts_tree_root_node*(self: PtsTree): TSNode {.apiProc.}
proc ts_tree_language*(a1: PtsTree): ptr TSLanguage {.apiProc.}
proc ts_tree_edit*(self: PtsTree; edit: ptr TSInputEdit) {.apiProc.}
proc ts_tree_get_changed_ranges*(old_tree: PtsTree; new_tree: PtsTree;
                                length: ptr uint32): ptr TSRange {.apiProc.}

proc ts_tree_print_dot_graph*(a1: PtsTree; a2: File) {.apiProc.}
proc ts_node_type*(a1: TSNode): cstring {.apiProc.}
proc ts_node_symbol*(a1: TSNode): TSSymbol {.apiProc.}
proc ts_node_start_byte*(a1: TSNode): uint32 {.apiProc.}
proc ts_node_start_point*(a1: TSNode): TSPoint {.apiProc.}
proc ts_node_end_byte*(a1: TSNode): uint32 {.apiProc.}
proc ts_node_end_point*(a1: TSNode): TSPoint {.apiProc.}
proc ts_node_string*(a1: TSNode): cstring {.apiProc.}
proc ts_node_is_null*(a1: TSNode): bool {.apiProc.}
proc ts_node_is_named*(a1: TSNode): bool {.apiProc.}
proc ts_node_is_missing*(a1: TSNode): bool {.apiProc.}
proc ts_node_is_extra*(a1: TSNode): bool {.apiProc.}
proc ts_node_has_changes*(a1: TSNode): bool {.apiProc.}
proc ts_node_has_error*(a1: TSNode): bool {.apiProc.}
proc ts_node_parent*(a1: TSNode): TSNode {.apiProc.}
proc ts_node_child*(a1: TSNode; a2: uint32): TSNode {.apiProc.}
proc ts_node_child_count*(a1: TSNode): uint32 {.apiProc.}
proc ts_node_named_child*(a1: TSNode; a2: uint32): TSNode {.apiProc.}
proc ts_node_named_child_count*(a1: TSNode): uint32 {.apiProc.}
proc ts_node_child_by_field_name*(
  self: TSNode; field_name: cstring;
  field_name_length: uint32): TSNode {.apiProc.}

proc ts_node_child_by_field_id*(
  a1: TSNode; a2: TSFieldId): TSNode {.apiProc.}

proc ts_node_field_name_for_child*(
    node: TSNode, idx: uint32): cstring {.apiProc.}

proc childName*(node: TsNode, idx: int): string =
  if idx.uint32 <= ts_node_child_count(node):
     # not isNil(node.tree) and
     # not isNil(node.id) and
    let name = ts_node_field_name_for_child(node, idx.uint32)
    if not isNil(name):
      result = $name

proc fieldNames*(node: TsNode): seq[string] =
  for idx in 0 ..< ts_node_child_count(node):
    result.add childName(node, idx.int)

proc ts_node_next_sibling*(a1: TSNode): TSNode {.apiProc.}
proc ts_node_prev_sibling*(a1: TSNode): TSNode {.apiProc.}
proc ts_node_next_named_sibling*(a1: TSNode): TSNode {.apiProc.}
proc ts_node_prev_named_sibling*(a1: TSNode): TSNode {.apiProc.}
proc ts_node_first_child_for_byte*(
  a1: TSNode; a2: uint32): TSNode {.apiProc.}

proc ts_node_first_named_child_for_byte*(
  a1: TSNode; a2: uint32): TSNode {.apiProc.}

proc ts_node_descendant_for_byte_range*(
  a1: TSNode; a2: uint32; a3: uint32): TSNode {.apiProc.}
proc ts_node_descendant_for_point_range*(
  a1: TSNode; a2: TSPoint; a3: TSPoint): TSNode {.apiProc.}

proc ts_node_named_descendant_for_byte_range*(
  a1: TSNode; a2: uint32; a3: uint32): TSNode {.apiProc.}

proc ts_node_named_descendant_for_point_range*(
  a1: TSNode; a2: TSPoint; a3: TSPoint): TSNode {.apiProc.}


proc ts_node_edit*(a1: ptr TSNode; a2: ptr TSInputEdit) {.apiProc.}
proc ts_node_eq*(a1: TSNode; a2: TSNode): bool {.apiProc.}
proc ts_tree_cursor_new*(a1: TSNode): TSTreeCursor {.apiProc.}
proc ts_tree_cursor_delete*(a1: PtsTreeCursor) {.apiProc.}
proc ts_tree_cursor_reset*(a1: PtsTreeCursor; a2: TSNode) {.apiProc.}
proc ts_tree_cursor_current_node*(a1: PtsTreeCursor): TSNode {.apiProc.}
proc ts_tree_cursor_current_field_name*(a1: PtsTreeCursor): cstring {.apiProc.}
proc ts_tree_cursor_current_field_id*(a1: PtsTreeCursor): TSFieldId {.apiProc.}
proc ts_tree_cursor_goto_parent*(a1: PtsTreeCursor): bool {.apiProc.}
proc ts_tree_cursor_goto_next_sibling*(a1: PtsTreeCursor): bool {.apiProc.}
proc ts_tree_cursor_goto_first_child*(a1: PtsTreeCursor): bool {.apiProc.}
proc ts_tree_cursor_goto_first_child_for_byte*(a1: PtsTreeCursor; a2: uint32): int64 {.apiProc.}
proc ts_tree_cursor_copy*(a1: PtsTreeCursor): TSTreeCursor {.apiProc.}
proc ts_query_new*(language: ptr TSLanguage; source: cstring; source_len: uint32;
                  error_offset: ptr uint32; error_type: ptr TSQueryError): ptr TSQuery {.
    apiProc.}
proc ts_query_delete*(a1: ptr TSQuery) {.apiProc.}
proc ts_query_pattern_count*(a1: ptr TSQuery): uint32 {.apiProc.}
proc ts_query_capture_count*(a1: ptr TSQuery): uint32 {.apiProc.}
proc ts_query_string_count*(a1: ptr TSQuery): uint32 {.apiProc.}
proc ts_query_start_byte_for_pattern*(a1: ptr TSQuery; a2: uint32): uint32 {.apiProc.}
proc ts_query_predicates_for_pattern*(self: ptr TSQuery; pattern_index: uint32;
                                     length: ptr uint32): ptr TSQueryPredicateStep {.
    apiProc.}
proc ts_query_capture_name_for_id*(a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.
    apiProc.}
proc ts_query_string_value_for_id*(a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.
    apiProc.}
proc ts_query_disable_capture*(a1: ptr TSQuery; a2: cstring; a3: uint32) {.apiProc.}
proc ts_query_cursor_new*(): ptr TSQueryCursor {.apiProc.}
proc ts_query_cursor_delete*(a1: ptr TSQueryCursor) {.apiProc.}
proc ts_query_cursor_exec*(a1: ptr TSQueryCursor; a2: ptr TSQuery; a3: TSNode) {.apiProc.}
proc ts_query_cursor_set_byte_range*(a1: ptr TSQueryCursor; a2: uint32; a3: uint32) {.apiProc.}
proc ts_query_cursor_set_point_range*(a1: ptr TSQueryCursor; a2: TSPoint; a3: TSPoint) {.apiProc.}
proc ts_query_cursor_next_match*(a1: ptr TSQueryCursor; match: ptr TSQueryMatch): bool {.apiProc.}
proc ts_query_cursor_remove_match*(a1: ptr TSQueryCursor; id: uint32) {.apiProc.}
proc ts_query_cursor_next_capture*(a1: ptr TSQueryCursor; match: ptr TSQueryMatch;
                                  capture_index: ptr uint32): bool {.apiProc.}
proc ts_language_symbol_count*(a1: ptr TSLanguage): uint32 {.apiProc.}
proc ts_language_symbol_name*(a1: ptr TSLanguage; a2: TSSymbol): cstring {.apiProc.}
proc ts_language_symbol_for_name*(self: ptr TSLanguage; string: cstring;
                                 length: uint32; is_named: bool): TSSymbol {.apiProc.}
proc ts_language_field_count*(a1: ptr TSLanguage): uint32 {.apiProc.}
proc ts_language_field_name_for_id*(a1: ptr TSLanguage; a2: TSFieldId): cstring {.apiProc.}
proc ts_language_field_id_for_name*(a1: ptr TSLanguage; a2: cstring; a3: uint32): TSFieldId {.apiProc.}
proc ts_language_symbol_type*(a1: ptr TSLanguage; a2: TSSymbol): TSSymbolType {.apiProc.}
proc ts_language_version*(a1: ptr TSLanguage): uint32 {.apiProc.}


import ./wraphelp

type
  TsFieldMap = object
    fieldArray*: ptr UncheckedArray[TsFieldMapEntry]
    maxIdx*: int



proc fieldMap*(lang: PtsLanguage, productionId: uint32): TsFieldMap =
  TsFieldMap(
    fieldArray: lang.fieldMapEntries.subArrayPtr(productionId),
    maxIdx: lang.fieldMapSlices[productionId].length.int)

iterator pairs*(map: TsFieldMap): (int, TsFieldMapEntry) =
  var idx = 0
  while idx < map.maxIdx:
    yield (idx, map.fieldArray[idx])


iterator items*(map: TsFieldMap): TsFieldMapEntry =
  for _, it in pairs(map):
    yield it


import std/[macros, options, unicode, strutils]
export options

macro tsInitScanner*(
  langname: untyped, scannerType: typed = void): untyped =
  result = newStmtList()

  var scannerType = scannerType

  if scannerType.repr == "void":
    scannerType = ident("int8")

  let
    initCall = ident("init" & scannerType.repr)
    kindType = ident(langname.strVal().capitalizeAscii() & "ExternalTok")

  result.add quote do:
    when `scannerType` is int8:
      var scanner {.inject.}: int8

    else:
      var scanner {.inject.} = `initCall`()

  let
    scanCreate = ident(
      "tree_sitter_" & langname.strVal() & "_external_scanner_create")
    scanDestroy = ident(
      "tree_sitter_" & langname.strVal() & "_external_scanner_destroy")
    scanScan = ident(
      "tree_sitter_" & langname.strVal() & "_external_scanner_scan")
    scanSerialize = ident(
      "tree_sitter_" & langname.strVal() & "_external_scanner_serialize")
    scanDeserialize = ident(
        "tree_sitter_" & langname.strVal() & "_external_scanner_deserialize")

  result.add quote do:
    proc `scanCreate`(): pointer {.exportc.} =
      addr scanner

  result.add quote do:
    proc `scanDestroy`(inScanner: `scannerType`) {.exportc.} =
      discard

  result.add quote do:
    proc `scanScan`(
        payload: pointer, lexer: ptr TsLexer, valid_symbols: ptr bool
      ): bool {.exportc.} =

      let res = scan(
        cast[ptr `scannerType`](payload)[],
        lexer[],
        cast[ptr UncheckedArray[bool]](valid_symbols)
      )

      when res isnot Option:
        static:
          error "`scan` must return `Option[<external token kind>]`"

      if res.isSome():
        lexer[].setTokenKind(res.get())
        return true

      else:
        return false

  result.add quote do:
    proc `scanSerialize`(payload: pointer, buffer: cstring) {.exportc.} =
      discard

  result.add quote do:
    proc `scanDeserialize`(payload: pointer, buffer: cstring, length: cuint) {.exportc.} =
      discard

  result = quote do:
    block:
      `result`
      scanner

import
  ../types/colorstring,
  ../algo/halgorithm

type
  TsBaseNodeKind* = enum
    tskDefault
    tskKeyword
    tskComment
    tskIdent
    tskPrefixOp
    tskInfixOp
    tskPrimitiveType

  TsKindMap*[TsKind: enum] = array[TsKind, TsBaseNodeKind]
  TsColorMap* = array[TsBaseNodeKind, PrintStyling]

let baseColorMap* = toMapArray {
  tskKeyword: bgDefault + fgCyan,
  tskComment: bgDefault + fgBlue,
  tskIdent, tskPrefixOp: bgDefault + styleItalic,
  tskPrimitiveType: bgDefault + fgBlue
}

import std/[with]

type
  HtsNode[N, K] = object
    base: ptr string
    case isGenerated*: bool
      of true:
        original: Option[N]
        isNamed: bool
        nodeKind: K
        subnodes: seq[HtsNode[N, K]]
        tokenVal: string

      of false:
        node: N

func kind*[N, K](node: HtsNode[N, K]): K =
  if node.isGenerated:
    node.nodeKind

  else:
    node.node.kind

func strVal*[N, K](node: HtsNode[N, K]): K =
  if node.isGenerated:
    node.tokenVal

  else:
    node.base[][node.slice()]


func toHtsNode*[N, K](node: N, base: ptr string, generated: bool = false): HtsNode[N, K] =
  ## Convert single tree-sitter node to HtsNode
  assertRef base
  assertRef node
  if generated:
    HtsNode(
      isGenerated: true,
      original: some node,
      base: base,
      nodeKind: node.kind)

  else:
    HtsNode(isGenerated: false, node: node, base: base)

func toGenerated*[N, K](
    node: HtsNode[N, K], doConvert: bool = true): HtsNode[N, K] =

  if node.isGenerated or not doConvert:
    node

  else:
    toHtsNode(node.node, node.base, generated = true)

func toHtsNode*[N, K](
    node: N, base: var string, generated: bool = false): HtsNode[N, K] =
  toHtsNode(node, addr base, generated)


func newTree*[N, K](
    kind: K, subnodes: varargs[HtsNode[N, K]]): HtsNode[N, K] =

  HtsNode[N, K](
    isGenerated: true,
    nodeKind: kind,
    subnodes: toSeq(subnodes))

func add*[N, K](
    node: var HtsNode[N, K],
    other: HtsNode[N, K] | seq[HtsNode[N, K]]) =

  node.subnodes.add other

func len*[N, K](
    node: HtsNode[N, K], unnamed: bool = false): int =

  if node.isGenerated:
    node.subnodes.len

  else:
    node.node.len(unnamed)

func `[]`*[N, K](
    node: HtsNode[N, K],
    idx: IndexTypes,
    unnamed: bool = false,
    generated: bool = false
  ): HtsNode[N, K] =

  if node.isGenerated:
    node.subnodes[idx].toGenerated(generated)

  else:
    toHtsNode(node.node[idx], node.base, generated = generated)

func `[]`*[N, K](node: var HtsNode[N, K], slice: SliceTypes): var seq[HtsNode[N, K]] =
  node.subnodes[slice]

func `[]`*[N, K](
    node: HtsNode[N, K],
    slice: SliceTypes,
    generated: bool = false,
    unnamed: bool = false
  ): seq[HtsNode[N, K]] =

  if node.isGenerated:
    assertArg(
      unnamed, not unnamed,
      "cannot iterate over generated tree using unnamed nodes")

    result = node.subnodes[slice].toGenerated(generated)

  else:
    let
      start = slice.startFor(node.len(unnamed))
      finish = slice.endFor(node.len(unnamed))

    for idx in start .. finish:
      result.add toHtsNode(node[idx], node.base, generated)

func `[]=`*[N, K](
    node: var HtsNode[N, K], idx: IndexTypes, other: HtsNode[N, K]) =
  node.subnodes[idx] = other

iterator pairs*[N, K](
    node: HtsNode[N, K],
    slice: SliceTypes = 0 .. high(int),
    generated: bool = false,
    unnamed: bool = false
  ): (int, HtsNode[N, K]) =

  if node.isGenerated:
    assertArg(
      unnamed, not unnamed, "cannot iterate over generated tree using unnamed nodes")

    for idx in clamp(slice, node.high):
      yield (idx, node.subnodes[idx].toGenerated(generated))
      inc idx

  else:
    for idx in clamp(slice, node.high):
      yield (idx, toHtsNode(node[idx, unnamed], node.base, generated))
      inc idx

iterator items*[N, K](
    node: HtsNode[N, K],
    slice: SliceTypes = 0 .. high(int),
    generated: bool = false,
    unnamed: bool = false
  ): HtsNode[N, K] =

  for _, node in pairs(node, slice, generated, unnamed):
    yield node


func toHtsTree*[N, K](node: N, base: var string): HtsNode[N, K] =
  ## Fully convert tree-sitter tree to `HtsNode[N, K]`.
  result = toHtsNode(node, base, generated = true)

func newHtsTree*[N, K](kind: K, val: string): HtsNode[N, K] =
  HtsNode[N, K](isGenerated: true, nodeKind: kind, tokenVal: val)

proc treeRepr*[N, K](
    node: N,
    base: string = "",
    langLen: int = 0,
    kindMap: TsKindMap[K],
    unnamed: bool = false,
    indexed: bool = false,
    maxdepth: int = high(int),
    pathIndexed: bool = false
  ): string =

  mixin kind
  const numStyle = tcDefault.bg + tcGrey54.fg
  proc aux(
      node: N or HtsNode[N, K],
      level: int,
      res: var string,
      name: string,
      idx: int,
      path: seq[int],
      nodeIdx: int
    ) =

    let startPoint = ts_node_start_point(TsNode(node))
    if node.isNil():
      with res:
        add "  ".repeat(level)
        add "<nil>", fgRed + bgDefault

    else:
      if pathIndexed:
        for item in path:
          with res:
            add "["
            add $item
            add "]"

        res.add " "

      else:
        res.add "  ".repeat(level)

        if indexed:
          if level > 0:
            with res:
              add "["
              add $idx
              add "] "

          else:
            res.add "   "

        with res:
          add ($node.kind())[langLen ..^ 1], tern(
            ts_node_is_named(TsNode(node)),
            fgGreen + bgDefault,
            fgYellow + bgDefault
          )

          add " "

        if name.len > 0:
          with res:
            add "<"
            add name, fgCyan + bgDefault
            add "("
            add $nodeIdx
            add ")> "

        let
          startPoint = ts_node_start_point(TsNode(node))
          endPoint = ts_node_end_point(TsNode(node))

        with res:
          add $startPoint.row, numStyle
          add ":"
          add $startPoint.column, numStyle

        if endPoint.row == startPoint.row:
          with res:
            add ".."
            add $endPoint.column, numStyle
            add " "

        else:
          with res:
            add "-"
            add $endPoint.row, numStyle
            add ":"
            add $endPoint.column, numStyle


        if node.len(unnamed) == 0:
          let
            text = base[node.slice()].split("\n")
            style = baseColorMap[`kindMap`[node.kind]]

          if text.len > 1:
            for idx, line in text:
              res.add "\n"
              res.add getIndent(level + 1)
              res.add line, style

          else:
            res.add text[0], style


        else:
          if level + 1 < `maxDepth`:
            var namedIdx = 0
            for idx, subn in pairs(node, unnamed = true):
              if ts_node_is_named(TsNode(subn)) or unnamed:
                res.add "\n"
                subn.aux(
                  level + 1, res, TsNode(node).childName(idx),
                  namedIdx, path & namedIdx, idx)

                inc namedIdx

          else:
            with res:
              add " ... ("
              add $node.len()
              add " subnodes)"

  aux(node, 0, result, "", 0, @[], 0)



macro repassArgs*(other: varargs[untyped]): untyped =
  let head = other[0]
  if head.kind == nnkCall:
    result = head

  else:
    result = newCall(head)

  for arg in other[1..^1]:
    if arg.kind == nnkIdent:
      result.add nnkExprEqExpr.newTree(arg, arg)

    else:
      result.add arg

  echo result.repr
