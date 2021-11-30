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
  ## Use the parser to parse some source code and create a syntax tree.
  ##
  ## If you are parsing this document for the first time, pass `NULL` for
  ## the `old_tree` parameter. Otherwise, if you have already parsed an
  ## earlier version of this document and the document has since been
  ## edited, pass the previous syntax tree so that the unchanged parts of
  ## it can be reused. This will save time and memory. For this to work
  ## correctly, you must have already edited the old syntax tree using the
  ## `ts_tree_edit` function in a way that exactly matches the source code
  ## changes.
  ##
  ## The `TSInput` parameter lets you specify how to read the text. It has the
  ## following three fields:
  ## 1. `read`: A function to retrieve a chunk of text at a given byte offset
  ##    and (row, column) position. The function should return a pointer to the
  ##    text and write its length to the `bytes_read` pointer. The parser does
  ##    not take ownership of this buffer; it just borrows it until it has
  ##    finished reading it. The function should write a zero value to the
  ##    `bytes_read` pointer to indicate the end of the document.
  ## 2. `payload`: An arbitrary pointer that will be passed to each invocation
  ##    of the `read` function.
  ## 3. `encoding`: An indication of how the text is encoded. Either
  ##    `TSInputEncodingUTF8` or `TSInputEncodingUTF16`.
  ##
  ## This function returns a syntax tree on success, and `NULL` on failure. There
  ## are three possible reasons for failure:
  ## 1. The parser does not have a language assigned. Check for this using the
  ##     `ts_parser_language` function.
  ## 2. Parsing was cancelled due to a timeout that was set by an earlier call to
  ##    the `ts_parser_set_timeout_micros` function. You can resume parsing from
  ##    where the parser left out by calling `ts_parser_parse` again with the
  ##    same arguments. Or you can start parsing from scratch by first calling
  ##    `ts_parser_reset`.
  ## 3. Parsing was cancelled using a cancellation flag that was set by an
  ##    earlier call to `ts_parser_set_cancellation_flag`. You can resume parsing
  ##    from where the parser left out by calling `ts_parser_parse` again with
  ##    the same arguments.



proc ts_parser_new*(): PtsParser {.apiProc.}
  ## Create a new parser.

proc ts_parser_delete*(parser: PtsParser) {.apiProc.}
  ## Delete the parser, freeing all of the memory that it used.
proc ts_parser_set_language*(
  self: PtsParser; language: PtsLanguage): bool {.apiProc.}
  ## Set the language that the parser should use for parsing.
  ##
  ## Returns a boolean indicating whether or not the language was successfully
  ## assigned. True means assignment succeeded. False means there was a version
  ## mismatch: the language was generated with an incompatible version of the
  ## Tree-sitter CLI. Check the language's version using `ts_language_version`
  ## and compare it to this library's `TREE_SITTER_LANGUAGE_VERSION` and
  ## `TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION` constants.

proc ts_parser_language*(self: PtsParser): PtsLanguage {.apiProc.}
  ## Get the parser's current language.

proc ts_parser_set_included_ranges*(
  self: PtsParser; ranges: PtsRange; length: uint32) {.apiProc.}
  ## Set the ranges of text that the parser should include when parsing.
  ##
  ## By default, the parser will always include entire documents. This function
  ## allows you to parse only a *portion* of a document but still return a syntax
  ## tree whose ranges match up with the document as a whole. You can also pass
  ## multiple disjoint ranges.
  ##
  ## The second and third parameters specify the location and length of an array
  ## of ranges. The parser does *not* take ownership of these ranges; it copies
  ## the data, so it doesn't matter how these ranges are allocated.
  ##
  ## If `length` is zero, then the entire document will be parsed. Otherwise,
  ## the given ranges must be ordered from earliest to latest in the document,
  ## and they must not overlap. That is, the following must hold for all
  ## `i` < `length - 1`:
  ##
  ##     ranges[i].end_byte <= ranges[i + 1].start_byte
  ##
  ## If this requirement is not satisfied, the operation will fail, the ranges
  ## will not be assigned, and this function will return `false`. On success,
  ## this function returns `true`

proc ts_parser_included_ranges*(
  self: PtsParser; length: ptr uint32): ptr TSRange {.apiProc.}
  ## Get the ranges of text that the parser will include when parsing.
  ##
  ## The returned pointer is owned by the parser. The caller should not free it
  ## or write to it. The length of the array will be written to the given
  ## `length` pointer.

proc ts_parser_parse_string*(
  self: PtsParser; old_tree: PtsTree;
  inString: cstring; length: uint32): PtsTree {.apiProc.}
  ## Use the parser to parse some source code stored in one contiguous buffer.
  ## The first two parameters are the same as in the `ts_parser_parse` function
  ## above. The second two parameters indicate the location of the buffer and its
  ## length in bytes.



proc ts_parser_parse_string_encoding*(
  self: PtsParser; old_tree: PtsTree;
  string: cstring; length: uint32; encoding: TSInputEncoding): PtsTree {.apiProc.}
  ## Use the parser to parse some source code stored in one contiguous buffer with
  ## a given encoding. The first four parameters work the same as in the
  ## `ts_parser_parse_string` method above. The final parameter indicates whether
  ## the text is encoded as UTF8 or UTF16.


proc ts_parser_reset*(self: PtsParser) {.apiProc.}
  ## Instruct the parser to start the next parse from the beginning.
  ##
  ## If the parser previously failed because of a timeout or a cancellation, then
  ## by default, it will resume where it left off on the next call to
  ## `ts_parser_parse` or other parsing functions. If you don't want to resume,
  ## and instead intend to use this parser to parse some other document, you must
  ## call `ts_parser_reset` first.



proc ts_parser_set_timeout_micros*(self: PtsParser; timeout: uint64) {.apiProc.}
  ## Set the maximum duration in microseconds that parsing should be allowed to
  ## take before halting.
  ##
  ## If parsing takes longer than this, it will halt early, returning NULL.
  ## See `ts_parser_parse` for more information.



proc ts_parser_timeout_micros*(self: PtsParser): uint64 {.apiProc.}
  ## Get the duration in microseconds that parsing is allowed to take.


proc ts_parser_cancellation_flag*(self: PtsParser): ptr uint {.apiProc.}
  ## Set the parser's current cancellation flag pointer.
  ##
  ## If a non-null pointer is assigned, then the parser will periodically read
  ## from this pointer during parsing. If it reads a non-zero value, it will
  ## halt early, returning NULL. See `ts_parser_parse` for more information.


proc ts_parser_set_logger*(self: PtsParser; logger: TSLogger) {.apiProc.}
  ## Get the parser's current cancellation flag pointer.



proc ts_parser_logger*(self: PtsParser): TSLogger {.apiProc.}
  ## Set the logger that a parser should use during parsing.
  ##
  ## The parser does not take ownership over the logger payload. If a logger was
  ## previously assigned, the caller is responsible for releasing any memory
  ## owned by the previous logger.



proc ts_parser_print_dot_graphs*(self: PtsParser; file: cint) {.apiProc.}
  ## Get the parser's current logger.


proc ts_parser_halt_on_error*(self: PtsParser; halt: bool) {.apiProc.}
  ## Set the file descriptor to which the parser should write debugging graphs
  ## during parsing. The graphs are formatted in the DOT language. You may want
  ## to pipe these graphs directly to a `dot(1)` process in order to generate
  ## SVG output. You can turn off this logging by passing a negative number.



proc ts_tree_copy*(self: PtsTree): PtsTree {.apiProc.}
  ## Create a shallow copy of the syntax tree. This is very fast.
  ##
  ## You need to copy a syntax tree in order to use it on more than one thread at
  ## a time, as syntax trees are not thread safe.



proc ts_tree_delete*(self: PtsTree) {.apiProc.}
  ## Delete the syntax tree, freeing all of the memory that it used.


proc ts_tree_root_node*(self: PtsTree): TSNode {.apiProc.}
  ## Get the root node of the syntax tree.


proc ts_tree_language*(a1: PtsTree): ptr TSLanguage {.apiProc.}
  ## Get the language that was used to parse the syntax tree.


proc ts_tree_edit*(self: PtsTree; edit: ptr TSInputEdit) {.apiProc.}
  ## Edit the syntax tree to keep it in sync with source code that has been
  ## edited.
  ##
  ## You must describe the edit both in terms of byte offsets and in terms of
  ## (row, column) coordinates.


proc ts_tree_get_changed_ranges*(old_tree: PtsTree; new_tree: PtsTree;
                                length: ptr uint32): ptr TSRange {.apiProc.}
  ## Compare an old edited syntax tree to a new syntax tree representing the same
  ## document, returning an array of ranges whose syntactic structure has changed.
  ##
  ## For this to work correctly, the old syntax tree must have been edited such
  ## that its ranges match up to the new tree. Generally, you'll want to call
  ## this function right after calling one of the `ts_parser_parse` functions.
  ## You need to pass the old tree that was passed to parse, as well as the new
  ## tree that was returned from that function.
  ##
  ## The returned array is allocated using `malloc` and the caller is responsible
  ## for freeing it using `free`. The length of the array will be written to the
  ## given `length` pointer.



proc ts_tree_print_dot_graph*(a1: PtsTree; a2: File) {.apiProc.}
  ## Write a DOT graph describing the syntax tree to the given file.


proc ts_node_type*(a1: TSNode): cstring {.apiProc.}
  ## Get the node's type as a null-terminated string.


proc ts_node_symbol*(a1: TSNode): TSSymbol {.apiProc.}
  ## Get the node's type as a numerical id.


proc ts_node_start_byte*(a1: TSNode): uint32 {.apiProc.}
  ## Get the node's start byte.


proc ts_node_start_point*(a1: TSNode): TSPoint {.apiProc.}
  ## Get the node's start position in terms of rows and columns.


proc ts_node_end_byte*(a1: TSNode): uint32 {.apiProc.}
  ## Get the node's end byte.


proc ts_node_end_point*(a1: TSNode): TSPoint {.apiProc.}
  ## Get the node's end position in terms of rows and columns.


proc ts_node_string*(a1: TSNode): cstring {.apiProc.}
  ## Get an S-expression representing the node as a string.
  ##
  ## This string is allocated with `malloc` and the caller is responsible for
  ## freeing it using `free`.


proc ts_node_is_null*(a1: TSNode): bool {.apiProc.}
  ## Check if the node is null. Functions like `ts_node_child` and
  ## `ts_node_next_sibling` will return a null node to indicate that no such node
  ## was found.


proc ts_node_is_named*(a1: TSNode): bool {.apiProc.}
  ## Check if the node is *named*. Named nodes correspond to named rules in the
  ## grammar, whereas *anonymous* nodes correspond to string literals in the
  ## grammar.


proc ts_node_is_missing*(a1: TSNode): bool {.apiProc.}
  ## Check if the node is *missing*. Missing nodes are inserted by the parser in
  ## order to recover from certain kinds of syntax errors.


proc ts_node_is_extra*(a1: TSNode): bool {.apiProc.}
  ## Check if the node is *extra*. Extra nodes represent things like comments,
  ## which are not required the grammar, but can appear anywhere.


proc ts_node_has_changes*(a1: TSNode): bool {.apiProc.}
  ## Check if a syntax node has been edited.


proc ts_node_has_error*(a1: TSNode): bool {.apiProc.}
  ## Check if the node is a syntax error or contains any syntax errors.


proc ts_node_parent*(a1: TSNode): TSNode {.apiProc.}
  ## Get the node's immediate parent.


proc ts_node_child*(a1: TSNode; a2: uint32): TSNode {.apiProc.}
  ## Get the node's child at the given index, where zero represents the first
  ## child.


proc ts_node_child_count*(a1: TSNode): uint32 {.apiProc.}
  ## Get the node's number of children.



proc ts_node_named_child*(a1: TSNode; a2: uint32): TSNode {.apiProc.}
  ## Get the node's *named* child at the given index.
  ##
  ## See also `ts_node_is_named`.


proc ts_node_named_child_count*(a1: TSNode): uint32 {.apiProc.}
  ## Get the node's number of *named* children.
  ##
  ## See also `ts_node_is_named`.



proc ts_node_child_by_field_name*(
  self: TSNode; field_name: cstring;
  field_name_length: uint32): TSNode {.apiProc.}
  ## Get the node's child with the given field name.


proc ts_node_child_by_field_id*(
  a1: TSNode; a2: TSFieldId): TSNode {.apiProc.}
  ## Get the node's child with the given numerical field id.
  ##
  ## You can convert a field name to an id using the
  ## `ts_language_field_id_for_name` function.


proc ts_node_field_name_for_child*(
    node: TSNode, idx: uint32): cstring {.apiProc.}

proc ts_node_next_sibling*(a1: TSNode): TSNode {.apiProc.}
  ## Get the node's next / previous sibling.

proc ts_node_prev_sibling*(a1: TSNode): TSNode {.apiProc.}
  ## Get the node's next / previous sibling.

proc ts_node_next_named_sibling*(a1: TSNode): TSNode {.apiProc.}
  ## Get the node's next / previous *named* sibling.

proc ts_node_prev_named_sibling*(a1: TSNode): TSNode {.apiProc.}
  ## Get the node's next / previous *named* sibling.


proc ts_node_first_child_for_byte*(
  a1: TSNode; a2: uint32): TSNode {.apiProc.}
  ## Get the node's first child that extends beyond the given byte offset.


proc ts_node_first_named_child_for_byte*(
  a1: TSNode; a2: uint32): TSNode {.apiProc.}
  ## Get the node's first named child that extends beyond the given byte offset.

proc ts_node_descendant_for_byte_range*(
  a1: TSNode; a2: uint32; a3: uint32): TSNode {.apiProc.}
  ## Get the smallest node within this node that spans the given range of bytes
  ## or (row, column) positions.

proc ts_node_descendant_for_point_range*(
  a1: TSNode; a2: TSPoint; a3: TSPoint): TSNode {.apiProc.}
  ## Get the smallest node within this node that spans the given range of bytes
  ## or (row, column) positions.

proc ts_node_named_descendant_for_byte_range*(
  a1: TSNode; a2: uint32; a3: uint32): TSNode {.apiProc.}
  ## Get the smallest named node within this node that spans the given range of
  ## bytes or (row, column) positions.

proc ts_node_named_descendant_for_point_range*(
  a1: TSNode; a2: TSPoint; a3: TSPoint): TSNode {.apiProc.}
  ## Get the smallest named node within this node that spans the given range of
  ## bytes or (row, column) positions.


proc ts_node_edit*(
  a1: ptr TSNode; a2: ptr TSInputEdit) {.apiProc.}
  ## Edit the node to keep it in-sync with source code that has been edited.
  ##
  ## This function is only rarely needed. When you edit a syntax tree with the
  ## `ts_tree_edit` function, all of the nodes that you retrieve from the tree
  ## afterward will already reflect the edit. You only need to use `ts_node_edit`
  ## when you have a `TSNode` instance that you want to keep and continue to use
  ## after an edit.

proc ts_node_eq*(
  a1: TSNode; a2: TSNode): bool {.apiProc.}
  ## Check if two nodes are identical.

proc ts_tree_cursor_new*(
  a1: TSNode): TSTreeCursor {.apiProc.}
  ## Create a new tree cursor starting from the given node.
  ##
  ## A tree cursor allows you to walk a syntax tree more efficiently than is
  ## possible using the `TSNode` functions. It is a mutable object that is always
  ## on a certain syntax node, and can be moved imperatively to different nodes.

proc ts_tree_cursor_delete*(
  a1: PtsTreeCursor) {.apiProc.}
  ## Delete a tree cursor, freeing all of the memory that it used.

proc ts_tree_cursor_reset*(
  a1: PtsTreeCursor; a2: TSNode) {.apiProc.}
  ## Re-initialize a tree cursor to start at a different node.

proc ts_tree_cursor_current_node*(
  a1: PtsTreeCursor): TSNode {.apiProc.}
  ## Get the tree cursor's current node.

proc ts_tree_cursor_current_field_name*(
  a1: PtsTreeCursor): cstring {.apiProc.}
  ## Get the field name of the tree cursor's current node.
  ##
  ## This returns `NULL` if the current node doesn't have a field.
  ## See also `ts_node_child_by_field_name`.

proc ts_tree_cursor_current_field_id*(
  a1: PtsTreeCursor): TSFieldId {.apiProc.}
  ## Get the field name of the tree cursor's current node.
  ##
  ## This returns zero if the current node doesn't have a field.
  ## See also `ts_node_child_by_field_id`, `ts_language_field_id_for_name`.

proc ts_tree_cursor_goto_parent*(
  a1: PtsTreeCursor): bool {.apiProc.}
  ## Move the cursor to the parent of its current node.
  ##
  ## This returns `true` if the cursor successfully moved, and returns `false`
  ## if there was no parent node (the cursor was already on the root node).

proc ts_tree_cursor_goto_next_sibling*(
  a1: PtsTreeCursor): bool {.apiProc.}
  ## Move the cursor to the next sibling of its current node.
  ##
  ## This returns `true` if the cursor successfully moved, and returns `false`
  ## if there was no next sibling node.


proc ts_tree_cursor_goto_first_child*(
  a1: PtsTreeCursor): bool {.apiProc.}
  ## Move the cursor to the first child of its current node.
  ##
  ## This returns `true` if the cursor successfully moved, and returns `false`
  ## if there were no children.

proc ts_tree_cursor_goto_first_child_for_byte*(
  a1: PtsTreeCursor; a2: uint32): int64 {.apiProc.}
  ## Move the cursor to the first child of its current node that extends beyond
  ## the given byte offset or point.
  ##
  ## This returns the index of the child node if one was found, and returns -1
  ## if no such child was found.

proc ts_tree_cursor_copy*(
  a1: PtsTreeCursor): TSTreeCursor {.apiProc.}

proc ts_query_new*(
  language: ptr TSLanguage; source: cstring;
  source_len: uint32; error_offset: ptr uint32;
  error_type: ptr TSQueryError): ptr TSQuery {.apiProc.}
  ## Create a new query from a string containing one or more S-expression
  ## patterns. The query is associated with a particular language, and can
  ## only be run on syntax nodes parsed with that language.
  ##
  ## If all of the given patterns are valid, this returns a `TSQuery`.
  ## If a pattern is invalid, this returns `NULL`, and provides two pieces
  ## of information about the problem:
  ## 1. The byte offset of the error is written to the `error_offset` parameter.
  ## 2. The type of error is written to the `error_type` parameter.

proc ts_query_delete*(
  a1: ptr TSQuery) {.apiProc.}
  ## Delete a query, freeing all of the memory that it used.

proc ts_query_pattern_count*(
  a1: ptr TSQuery): uint32 {.apiProc.}
  ## Get the number of patternsliterals in the query.

proc ts_query_capture_count*(
  a1: ptr TSQuery): uint32 {.apiProc.}
  ## Get the number of captures in the query.

proc ts_query_string_count*(
  a1: ptr TSQuery): uint32 {.apiProc.}
  ## Get the number of string literals in the query.
  ##
proc ts_query_start_byte_for_pattern*(
  a1: ptr TSQuery; a2: uint32): uint32 {.apiProc.}
  ## Get the byte offset where the given pattern starts in the query's source.
  ##
  ## This can be useful when combining queries by concatenating their source
  ## code strings.

proc ts_query_predicates_for_pattern*(
  self: ptr TSQuery; pattern_index: uint32;
  length: ptr uint32): ptr TSQueryPredicateStep {.apiProc.}
  ## Get all of the predicates for the given pattern in the query.
  ##
  ## The predicates are represented as a single array of steps. There are three
  ## types of steps in this array, which correspond to the three legal values for
  ## the `type` field:
  ## - `TSQueryPredicateStepTypeCapture` - Steps with this type represent names
  ##    of captures. Their `value_id` can be used with the
  ##   `ts_query_capture_name_for_id` function to obtain the name of the capture.
  ## - `TSQueryPredicateStepTypeString` - Steps with this type represent literal
  ##    strings. Their `value_id` can be used with the
  ##    `ts_query_string_value_for_id` function to obtain their string value.
  ## - `TSQueryPredicateStepTypeDone` - Steps with this type are *sentinels*
  ##    that represent the end of an individual predicate. If a pattern has two
  ##    predicates, then there will be two steps with this `type` in the array.

proc ts_query_capture_name_for_id*(
  a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.apiProc.}
  ## Get the name and length of one of the query's captures, or one of the
  ## query's string literals. Each capture and string is associated with a
  ## numeric id based on the order that it appeared in the query's source.

proc ts_query_string_value_for_id*(
  a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.apiProc.}
  ## Get the name and length of one of the query's captures, or one of the
  ## query's string literals. Each capture and string is associated with a
  ## numeric id based on the order that it appeared in the query's source.

proc ts_query_disable_capture*(
  a1: ptr TSQuery; a2: cstring; a3: uint32) {.apiProc.}
  ## Disable a certain capture within a query.
  ##
  ## This prevents the capture from being returned in matches, and also avoids
  ## any resource usage associated with recording the capture. Currently, there
  ## is no way to undo this.

proc ts_query_disable_pattern*(
  a1: ptr TSQuery; a3: uint32) {.apiProc.}
  ## Disable a certain pattern within a query.
  ##
  ## This prevents the pattern from matching and removes most of the overhead
  ## associated with the pattern. Currently, there is no way to undo this.


proc ts_query_cursor_new*(): ptr TSQueryCursor {.apiProc.}
  ## Create a new cursor for executing a given query.
  ##
  ## The cursor stores the state that is needed to iteratively search
  ## for matches. To use the query cursor, first call `ts_query_cursor_exec`
  ## to start running a given query on a given syntax node. Then, there are
  ## two options for consuming the results of the query:
  ## 1. Repeatedly call `ts_query_cursor_next_match` to iterate over all of the
  ##    *matches* in the order that they were found. Each match contains the
  ##    index of the pattern that matched, and an array of captures. Because
  ##    multiple patterns can match the same set of nodes, one match may contain
  ##    captures that appear *before* some of the captures from a previous match.
  ## 2. Repeatedly call `ts_query_cursor_next_capture` to iterate over all of the
  ##    individual *captures* in the order that they appear. This is useful if
  ##    don't care about which pattern matched, and just want a single ordered
  ##    sequence of captures.
  ##
  ## If you don't care about consuming all of the results, you can stop calling
  ## `ts_query_cursor_next_match` or `ts_query_cursor_next_capture` at any point.
  ##  You can then start executing another query on another node by calling
  ##  `ts_query_cursor_exec` again.


proc ts_query_cursor_delete*(
  a1: ptr TSQueryCursor) {.apiProc.}
  ## Delete a query cursor, freeing all of the memory that it used.

proc ts_query_cursor_exec*(
  a1: ptr TSQueryCursor; a2: ptr TSQuery; a3: TSNode) {.apiProc.}
  ## Start running a given query on a given node.

proc ts_query_cursor_set_byte_range*(
  a1: ptr TSQueryCursor; a2: uint32; a3: uint32) {.apiProc.}
  ## Set the range of bytes or (row, column) positions in which the query
  ## will be executed.


proc ts_query_cursor_set_point_range*(
  a1: ptr TSQueryCursor; a2: TSPoint; a3: TSPoint) {.apiProc.}
  ## Set the range of bytes or (row, column) positions in which the query
  ## will be executed.

proc ts_query_cursor_next_match*(
  a1: ptr TSQueryCursor; match: ptr TSQueryMatch): bool {.apiProc.}
  ## Advance to the next match of the currently running query.
  ##
  ## If there is a match, write it to `*match` and return `true`.
  ## Otherwise, return `false`.

proc ts_query_cursor_remove_match*(
  a1: ptr TSQueryCursor; id: uint32) {.apiProc.}

proc ts_query_cursor_next_capture*(
  a1: ptr TSQueryCursor; match: ptr TSQueryMatch;
  capture_index: ptr uint32): bool {.apiProc.}
  ## Advance to the next capture of the currently running query.
  ##
  ## If there is a capture, write its match to `*match` and its index within
  ## the matche's capture list to `*capture_index`. Otherwise, return `false`.

proc ts_language_symbol_count*(
  a1: ptr TSLanguage): uint32 {.apiProc.}
  ## Get the number of distinct node types in the language.


proc ts_language_symbol_name*(
  a1: ptr TSLanguage; a2: TSSymbol): cstring {.apiProc.}
  ## Get a node type string for the given numerical id.

proc ts_language_symbol_for_name*(
  self: ptr TSLanguage; string: cstring;
  length: uint32; is_named: bool): TSSymbol {.apiProc.}
  ## Get the numerical id for the given node type string.

proc ts_language_field_count*(
  a1: ptr TSLanguage): uint32 {.apiProc.}
  ## Get the number of distinct field names in the language.

proc ts_language_field_name_for_id*(
  a1: ptr TSLanguage; a2: TSFieldId): cstring {.apiProc.}
  ## Get the field name string for the given numerical id.

proc ts_language_field_id_for_name*(
  a1: ptr TSLanguage; a2: cstring; a3: uint32): TSFieldId {.apiProc.}
  ## Get the numerical id for the given field name string.

proc ts_language_symbol_type*(
  a1: ptr TSLanguage; a2: TSSymbol): TSSymbolType {.apiProc.}
  ## Check whether the given node type id belongs to named nodes, anonymous nodes,
  ## or a hidden nodes.
  ##
  ## See also `ts_node_is_named`. Hidden nodes are never returned from the API.

proc ts_language_version*(
  a1: ptr TSLanguage): uint32 {.apiProc.}
  ## Get the ABI version number for this language. This version number is used
  ## to ensure that languages were generated by a compatible version of
  ## Tree-sitter.
  ##
  ## See also `ts_parser_set_language`.


func nodeString*[N: distinct](node: N): string =
  $ts_node_string(TSNode(node))

func isNamed*[N: distinct](node: N): bool =
  ts_node_is_named(TSNode(node))

func isMissing*[N: distinct](node: N): bool =
  ts_node_is_missing(TSNode(node))

func isExtra*[N: distinct](node: N): bool =
  ts_node_is_extra(TSNode(node))

func isNil*(node: TsNode): bool =
  ts_node_is_null(node)

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
  assert not isNil(TsNode(node))
  ts_node_named_child_count(TSNode(node)).int

func startPoint*[N: distinct](node: N): TSPoint =
  ## Return start point for AST node (line and column)
  assert not isNil(TsNode(node))
  ts_node_start_point(TSNode(node))

func startByte*[N: distinct](node: N): int =
  ## Return start point for AST node (line and column)
  assert not isNil(TsNode(node))
  ts_node_start_byte(TsNode(node)).int

func endByte*[N: distinct](node: N): int =
  ## Return start point for AST node (line and column)
  assert not isNil(TsNode(node))
  ts_node_end_byte(TsNode(node)).int

func slice*[N: distinct](node: N): Slice[int] =
  assert not isNil(TsNode(node))
  {.cast(noSideEffect).}:
    ## Get range of source code **bytes** for the node
    startByte(node) ..< endByte(node)

func endPoint*[N: distinct](node: N): TSPoint =
  ## Return end point for AST node (line and column)
  assert not isNil(TsNode(node))
  ts_node_end_point(TSNode(node))

func startLine*[N: distinct](node: N): int =
  assert not isNil(TsNode(node))
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

  if unnamed:
    result = N(ts_node_child(TSNode(node), uint32(idx)))
    if isNil(TsNode(result)):
      raise newException(ValueError,
        "Cannot get subnode at index " & $idx &
          " - `ts_node_child` returned `nil`")

  else:
    result = N(ts_node_named_child(TSNode(node), uint32(idx)))
    if isNil(TsNode(result)):
      raise newException(ValueError,
        "Cannot get subnode at index " & $idx &
          " - `ts_node_named_child` returned `nil`")



func `[]`*[N: distinct](
  node: N, slice: Slice[int],
  unnamed: bool = false): seq[N] =

  for i in slice:
    result.add node[i, unnamed]

func `[]`*[N: distinct](
  node: N, slice: HSlice[int, BackwardsIndex],
  unnamed: bool = false): seq[N] =

  let maxIdx = len(node) - slice.b.int
  for i in slice.a .. maxIdx:
    result.add node[i, unnamed]

func `{}`*[N: distinct](node: N, idx: int): N =
  ## Retun node positioned at `idx` - count includes unnamed (token)
  ## subnodes.
  result = N(ts_node_child(TSNode(node), uint32(idx)))
  if isNil(TsNode(result)):
    raise newException(IndexError,
      "Node index is out of range - `ts_node_child` returned `nil` for " &
        $idx)


func `[]`*[N: distinct](
    node: N, name: string, check: bool = true): N =
  result = N(ts_node_child_by_field_name(
    TsNode(node), name.cstring, name.len.uint32))

  if check and isNil(TsNode(result)):
    raise newException(KeyError,
      "No field named '" & name & "' for AST node type" &
      $typeof(N) & " kind" &
      $node.kind() & ". Possible field names:" &
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

type
  HtsRuleKind* = enum
    hrkBlank
    hrkSeq
    hrkRepeat
    hrkRepeat1
    hrkString
    hrkRegex
    hrkChoice
    hrkSymbol

  HtsRule*[K] = ref object
    case kind*: HtsRuleKind
      of hrkString:
        strVal*: string

      of hrkSymbol:
        symbol*: K

      of hrkRepeat, hrkRepeat1:
        content*: HtsRule[K]

      of hrkBlank:
        discard

      of hrkRegex:
        rxValue*: string

      of hrkChoice, hrkSeq:
        members*: seq[HtsRule[K]]

  HtsRegexKind*[K] = enum
    hrxAlt

  HtsRegex*[K] = ref object

proc tsChoice*[K](args: varargs[HtsRule[K]]): HtsRule[K] =
  HtsRule[K](kind: hrkChoice, members: @args)

proc tsSeq*[K](args: varargs[HtsRule[K]]): HtsRule[K] =
  HtsRule[K](kind: hrkSeq, members: @args)


func tsSymbol*[K](arg: K): HtsRule[K] =
  HtsRule[K](kind: hrkSymbol, symbol: arg)

func tsString*[K](arg: string): HtsRule[K] =
  HtsRule[K](kind: hrkString, strVal: arg)

func tsRegex*[K](arg: string): HtsRule[K] =
  HtsRule[K](kind: hrkRegex, rxValue: arg)

func tsRepeat*[K](arg: HtsRule[K]): HtsRule[K] =
  HtsRule[K](kind: hrkRepeat, content: arg)

func tsRepeat1*[K](arg: HtsRule[K]): HtsRule[K] =
  HtsRule[K](kind: hrkRepeat1, content: arg)

func tsBlank*[K](): HtsRule[K] = HtsRule[K](kind: hrkBlank)
