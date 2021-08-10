import std/macros

func blockCall*(arg: NimNode): NimNode =
  nnkBlockStmt.newTree(newEmptyNode(), arg)
