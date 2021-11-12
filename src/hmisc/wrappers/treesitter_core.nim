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
  TSTreeCursor* {.apiStruct, importc: "TSTreeCursor".} = object
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
    lookahead*: cint
    result_symbol*: TSSymbol
    advance*: proc(lex: ptr TSLexer, skip: bool) {.cdecl.}
    mark_end*: proc(lex: ptr TSLexer) {.cdecl.}
    get_column*: proc(lex: ptr TSLexer): cuint {.cdecl.}
    is_at_included_range_start*: proc(lex: ptr TSLexer) {.cdecl.}

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

proc ts_parser_parse*(
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


proc ts_node_edit*(
  a1: ptr TSNode; a2: ptr TSInputEdit) {.apiProc.}

proc ts_node_eq*(
  a1: TSNode; a2: TSNode): bool {.apiProc.}

proc ts_tree_cursor_new*(
  a1: TSNode): TSTreeCursor {.apiProc.}

proc ts_tree_cursor_delete*(
  a1: PtsTreeCursor) {.apiProc.}

proc ts_tree_cursor_reset*(
  a1: PtsTreeCursor; a2: TSNode) {.apiProc.}

proc ts_tree_cursor_current_node*(
  a1: PtsTreeCursor): TSNode {.apiProc.}

proc ts_tree_cursor_current_field_name*(
  a1: PtsTreeCursor): cstring {.apiProc.}

proc ts_tree_cursor_current_field_id*(
  a1: PtsTreeCursor): TSFieldId {.apiProc.}

proc ts_tree_cursor_goto_parent*(
  a1: PtsTreeCursor): bool {.apiProc.}

proc ts_tree_cursor_goto_next_sibling*(
  a1: PtsTreeCursor): bool {.apiProc.}


proc ts_tree_cursor_goto_first_child*(
  a1: PtsTreeCursor): bool {.apiProc.}

proc ts_tree_cursor_goto_first_child_for_byte*(
  a1: PtsTreeCursor; a2: uint32): int64 {.apiProc.}

proc ts_tree_cursor_copy*(
  a1: PtsTreeCursor): TSTreeCursor {.apiProc.}

proc ts_query_new*(
  language: ptr TSLanguage; source: cstring;
  source_len: uint32; error_offset: ptr uint32;
  error_type: ptr TSQueryError): ptr TSQuery {.apiProc.}

proc ts_query_delete*(
  a1: ptr TSQuery) {.apiProc.}

proc ts_query_pattern_count*(
  a1: ptr TSQuery): uint32 {.apiProc.}

proc ts_query_capture_count*(
  a1: ptr TSQuery): uint32 {.apiProc.}

proc ts_query_string_count*(
  a1: ptr TSQuery): uint32 {.apiProc.}

proc ts_query_start_byte_for_pattern*(
  a1: ptr TSQuery; a2: uint32): uint32 {.apiProc.}

proc ts_query_predicates_for_pattern*(
  self: ptr TSQuery; pattern_index: uint32;
  length: ptr uint32): ptr TSQueryPredicateStep {.apiProc.}

proc ts_query_capture_name_for_id*(
  a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.apiProc.}

proc ts_query_string_value_for_id*(
  a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.apiProc.}

proc ts_query_disable_capture*(
  a1: ptr TSQuery; a2: cstring; a3: uint32) {.apiProc.}

proc ts_query_cursor_new*(): ptr TSQueryCursor {.apiProc.}

proc ts_query_cursor_delete*(
  a1: ptr TSQueryCursor) {.apiProc.}

proc ts_query_cursor_exec*(
  a1: ptr TSQueryCursor; a2: ptr TSQuery; a3: TSNode) {.apiProc.}

proc ts_query_cursor_set_byte_range*(
  a1: ptr TSQueryCursor; a2: uint32; a3: uint32) {.apiProc.}


proc ts_query_cursor_set_point_range*(
  a1: ptr TSQueryCursor; a2: TSPoint; a3: TSPoint) {.apiProc.}

proc ts_query_cursor_next_match*(
  a1: ptr TSQueryCursor; match: ptr TSQueryMatch): bool {.apiProc.}

proc ts_query_cursor_remove_match*(
  a1: ptr TSQueryCursor; id: uint32) {.apiProc.}

proc ts_query_cursor_next_capture*(
  a1: ptr TSQueryCursor; match: ptr TSQueryMatch;
  capture_index: ptr uint32): bool {.apiProc.}

proc ts_language_symbol_count*(
  a1: ptr TSLanguage): uint32 {.apiProc.}

proc ts_language_symbol_name*(
  a1: ptr TSLanguage; a2: TSSymbol): cstring {.apiProc.}

proc ts_language_symbol_for_name*(
  self: ptr TSLanguage; string: cstring;
  length: uint32; is_named: bool): TSSymbol {.apiProc.}

proc ts_language_field_count*(
  a1: ptr TSLanguage): uint32 {.apiProc.}

proc ts_language_field_name_for_id*(
  a1: ptr TSLanguage; a2: TSFieldId): cstring {.apiProc.}

proc ts_language_field_id_for_name*(
  a1: ptr TSLanguage; a2: cstring; a3: uint32): TSFieldId {.apiProc.}

proc ts_language_symbol_type*(
  a1: ptr TSLanguage; a2: TSSymbol): TSSymbolType {.apiProc.}

proc ts_language_version*(
  a1: ptr TSLanguage): uint32 {.apiProc.}

func nodeString*[N: distinct](node: N): string =
  $ts_node_string(TSNode(node))

func isNamed*[N: distinct](node: N): bool =
  ts_node_is_named(TSNode(node))

func isMissing*[N: distinct](node: N): bool =
  ts_node_is_missing(TSNode(node))

func isExtra*[N: distinct](node: N): bool =
  ts_node_is_extra(TSNode(node))

func hasChanges*[N: distinct](node: N): bool =
  ts_node_has_changes(TSNode(node))

func hasError*[N: distinct](node: N): bool =
  ts_node_has_error(TSNode(node))

func parent*[N: distinct](node: N): N =
  N(ts_node_parent(TSNode(node)))

func child*[N: distinct](node: N; a2: int): N =
  N(ts_node_child(TSNode(node), a2.uint32))

func childCount*[N: distinct](node: N): int =
  ## Number of subnodes (including tokens) for a tree
  ts_node_child_count(TSNode(node)).int

func namedChild*[N: distinct](node: N; a2: int): N =
  ## named child at index
  N(ts_node_named_child(TSNode(node), a2.uint32))

func namedChildCount*[N: distinct](node: N): int =
  ## Number of named (non-token) subnodes for a triee
  assert not isNil(node)
  ts_node_named_child_count(TSNode(node)).int

func startPoint*[N: distinct](node: N): TSPoint =
  ## Return start point for AST node (line and column)
  assert not isNil(node)
  ts_node_start_point(TSNode(node))

func startByte*[N: distinct](node: N): int =
  ## Return start point for AST node (line and column)
  assert not isNil(node)
  ts_node_start_byte(TsNode(node)).int

func endByte*[N: distinct](node: N): int =
  ## Return start point for AST node (line and column)
  assert not isNil(node)
  ts_node_end_byte(TsNode(node)).int

func slice*[N: distinct](node: N): Slice[int] =
  assert not isNil(node)
  {.cast(noSideEffect).}:
    ## Get range of source code **bytes** for the node
    startByte(node) ..< endByte(node)

func endPoint*[N: distinct](node: N): TSPoint =
  ## Return end point for AST node (line and column)
  assert not isNil(node)
  ts_node_end_point(TSNode(node))

func startLine*[N: distinct](node: N): int =
  assert not isNil(node)
  node.startPoint().row.int

func endLine*[N: distinct](node: N): int =
  node.endPoint().row.int

func startColumn*[N: distinct](node: N): int =
  node.startPoint().column.int

func endColumn*[N: distinct](node: N): int =
  node.endPoint().column.int


proc childName*[N: distinct](node: N, idx: int): string =
  if idx.uint32 <= ts_node_child_count(node.TsNode()):
    let name = ts_node_field_name_for_child(node.TsNode(), idx.uint32)
    if not isNil(name):
      result = $name

proc fieldNames*[N: distinct](node: N): seq[string] =
  for idx in 0 ..< ts_node_child_count(node.TsNode()):
    result.add childName(node, idx.int)


func `[]`*[N: distinct](
    node: N,
    idx: int | BackwardsIndex, unnamed: bool = false
  ): N =

  when idx is BackwardsIndex:
    let idx = node.len(unnamed = unnamed) - idx.int + 1

  assert 0 <= idx and idx < node.len(unnamed = unnamed),
    "Cannot get subnode at index " & $idx & " - len is " &
      $node.len(unnamed = unnamed)


  if unnamed:
    N(ts_node_child(TSNode(node), uint32(idx)))
  else:
    N(ts_node_named_child(TSNode(node), uint32(idx)))

func `[]`*[N: distinct](
  node: N, slice: Slice[int],
  unnamed: bool = false): seq[N] =

  for i in slice:
    result.add node[i, unnamed]

func `[]`*[N: distinct](
  node: N, slice: HSlice[int, BackwardsIndex],
  unnamed: bool = false): seq[N] =

  let maxIdx = node.len() - slice.b.int
  for i in slice.a .. maxIdx:
    result.add node[i, unnamed]

func `{}`*[N: distinct](node: N, idx: int): N =
  ## Retun node positioned at `idx` - count includes unnamed (token)
  ## subnodes.
  assert 0 <= idx and idx < node.len(unnamed = true)
  N(ts_node_child(TSNode(node), uint32(idx)))


func `[]`*[N: distinct](
    node: N, name: string, check: bool = true): N =
  result = N(ts_node_child_by_field_name(
    TsNode(node), name.cstring, name.len.uint32))

  if check and isNil(result):
    raise newException(KeyError,
      "No field named '" & name & "' for AST node type",
      typeof(N), ", kind",
      $node.kind() & ". Possible field names:",
      $fieldNames(node)
    )

func has*[N: distinct](node: N, name: string): bool =
  ## Check if node contains field with `name`
  not(ts_node_is_null(TsNode(node[name, false])))

func contains*[N: distinct](node: N, name: string): bool =
  ## Check if node contains field with `name`
  not(ts_node_is_null(TsNode(node[name, check = false])))


iterator items*[N: distinct](node: N,
               unnamed: bool = false): N =
  ## Iterate over subnodes. `unnamed` - also iterate over unnamed
  ## nodes (usually things like punctuation, braces and so on).
  for i in 0 ..< node.len(unnamed):
    yield node[i, unnamed]

iterator pairs*[N: distinct](node: N, unnamed: bool = false): (int, N) =
  ## Iterate over subnodes. `unnamed` - also iterate over unnamed
  ## nodes.
  for i in 0 ..< node.len(unnamed):
    yield (i, node[i, unnamed])

func `[]`*[N: distinct](s: string, node: N): string =
  s[node.slice()]

func childByFieldName*[N: distinct](
  self: N; fieldName: string; fieldNameLength: int
): TSNode =
  ts_node_child_by_field_name(
    TSNode(self), fieldName.cstring, fieldNameLength.uint32)
