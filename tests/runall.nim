import hmisc/scripts/nim_test

startHax()

let dir = AbsDir(relToSource"")
runTestDir(dir, getCwdNimDump(), 1)
