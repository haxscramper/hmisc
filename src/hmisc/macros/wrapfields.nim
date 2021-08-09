import
  std/[macros, sequtils]

import
  ../core/all


func maybeExport(str: string, cond: bool): NimNode =
  if cond:
    nnkPostfix.newTree(ident"*", ident(str))

  else:
    ident(str)

macro wrapKindAst*(main: typed, kind: typed): untyped =
  let
    eqTil = ident("=~")

  result = quote do:
    func has*(n: `main`, idx: int): bool =
      0 <= idx and idx < len(n)

    func `eqTil`*(n: `main`, k: `kind`): bool = n.kind == k
    func `eqTil`*(n: `main`, k: set[`kind`]): bool = n.kind in k
    func `eqTil`*(n: `main`, sub: openarray[`kind`]): bool =
      if len(n) != len(sub):
        result = false

      else:
        for idx, val in pairs(sub):
          if n[idx].kind != val:
            return false

macro wrapSeqContainer*(
    main: typed,
    fieldType: typed,
    isRef: static[bool] = false,
    withIterators: static[bool] = true,
    ignore: openarray[string]{nkBracket} = [""],
    exported: static[bool] = true
  ) =

  ## - TODO :: Generate kind using `assertKind`

  let
    mainType = main[0]
    field = main[1]
    mutType = if isRef: mainType else: nnkVarTy.newTree(mainType)
    ignore = mapIt(ignore, it.strVal)

  let
    indexOp = ident("[]")
    indexAsgn = ident("[]=")

  result = newStmtList()

  if "len" notin ignore:
    let lenId = maybeExport("len", exported)
    result.add quote do:
      proc `lenId`(main: `mainType`): int = len(main.`field`)

  if "high" notin ignore:
    let highId = maybeExport("high", exported)
    result.add quote do:
      proc `highId`(main: `mainType`): int = high(main.`field`)

  if "add" notin ignore:
    let addId = maybeExport("add", exported)
    result.add quote do:
      proc `addId`(main: `mutType`, other: `fieldType` | seq[`fieldType`]) =
        add(main.`field`, other)


  if "[]" notin ignore:
    let indexOp = maybeExport("[]", exported)
    result.add quote do:
      proc `indexOp`(main: `mainType`, index: IndexTypes): `fieldType` =
        main.`field`[index]

      proc `indexOp`(main: `mainType`, slice: SliceTypes): seq[`fieldType`] =
        main.`field`[slice]

  if "[]=" notin ignore:
    let indexAsgn = maybeExport("[]=", exported)
    result.add quote do:
      proc `indexAsgn`(
          main: `mutType`, index: IndexTypes, value: `fieldType`) =

        main.`field`[index] = value

  if withIterators:
    let
      pairsId = maybeExport("pairs", exported)
      itemsId = maybeExport("items", exported
      )
    result.add quote do:
      iterator `pairsId`(main: `mainType`): (int, `fieldType`) =
        for item in pairs(main.`field`):
          yield item

      iterator `itemsId`(main: `mainType`): `fieldType` =
        for item in items(main.`field`):
          yield item

      iterator `pairsId`(main: `mainType`, slice: SliceTypes):
        (int, `fieldType`) =
        let slice = clamp(slice, main.`field`.high)
        var resIdx = 0
        for idx in slice:
          yield (resIdx, main.`field`[idx])
          inc resIdx

      iterator `itemsId`(main: `mainType`, slice: SliceTypes): `fieldType` =
        for idx, item in pairs(main, slice):
          yield item

macro wrapStructContainer*(
    main: untyped,
    fieldList: untyped,
    isRef: static[bool] = false
  ): untyped =

  assertKind(main, {nnkDotExpr})

  let
    mainType = main[0]
    structField = main[1]
    mutType = if isRef: mainType else: nnkVarTy.newTree(mainType)

  result = newStmtList()

  var prev: seq[NimNode]
  for field in fieldList:
    if field.kind != nnkExprColonExpr:
      prev.add field

    else:
      for name in prev & field[0]:
        assertKind(name, {nnkIdent})
        let fieldType = field[1]
        assertKind(fieldType, {nnkIdent, nnkBracketExpr})

        let asgn = ident(name.strVal() & "=")

        result.add quote do:
          func `name`*(n: `mainType`): `fieldType` =
            n.`structField`.`name`

          func `asgn`*(n: `mutType`, value: `fieldType`) =
            n.`structField`.`name` = value

      prev = @[]
