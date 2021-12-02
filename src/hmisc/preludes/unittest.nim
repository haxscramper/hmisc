import ../other/hcoverage

## Unittest prelude file - enable compile-time coverage, `startHax()` on
## import. Exports all the necessary modules for unit testing -
## `hunittest`, `hcoverage` and pretty-printe.

hcoverageEnable()

import
  ../core/all,
  ../other/[hunittest, hpprint, oswrap]

const hmiscUnittestOut {.strdefine.}: string = ""

when hmiscUNittestOut == "xml":
  import ../other/hunittest_xml
  setTestContext(newXUnitContext())

elif hmiscUnittestOut == "json":
  import ../other/hunittest_json
  setTestContext(newJsonContext())

else:
  startHax()

export all, hunittest, hcoverage, hpprint, oswrap

proc nim_doc_project_does_not_consider_this_file_to_be_a_part_of_the_project_unless_I_declare_at_least_one_public_symbol_so_I_have_this_proc_here*() =
  discard
