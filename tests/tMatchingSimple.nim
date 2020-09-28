import hmisc/macros/matching
{.experimental: "caseStmtMacros".}

if [1,2,3].matches [@head, all @tail]:
  assert head == 1
  assert tail == @[2,3]
else:
  raiseAssert("#[ IMPLEMENT ]#")
