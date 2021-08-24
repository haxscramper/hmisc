import
  ../other/[
    hshell, # execute shell applications
    hlogger, # log program running
    hargparse, # parse command-line arguments
    hpprint, # pretty-print data
    oswrap # os interactions
  ],
  ../types/[
    colorstring # Colored text formatting
  ],
  ../algo/[
    htemplates, # `with*It` templates
    hstring_algo, # String manipulations
    hlex_base, # Parsing input data
    clformat # Simplified custom data pretty-printing
  ],
  ../core/all # Debug misc helpers etc.

import
  std/[strutils, tables, macros, strformat, parseutils]


export
  strutils, tables, macros, strformat, parseutils,
  hshell,
  hlogger,
  hargparse,
  hpprint,
  oswrap,
  colorstring,
  htemplates,
  hstring_algo,
  hlex_base,
  clformat,
  all


proc just_having_file_in_project_is_not_enough_for_nim_doc_so_I_defined_public_symbol_here*() =
  discard
