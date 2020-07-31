import strutils, sequtils, strformat
import ../types/hnim_ast
import ../algo/[halgorithm, hseq_mapping, clformat]

func pptConst*(val: string): ValObjTree =
  ValObjTree(kind: okConstant, strlit: val)

func pptSeq*(vals: varargs[ValObjTree]): ValObjTree =
  ValObjTree(kind: okSequence, valItems: toSeq(vals))

func pptSeq*(valType: string, vals: varargs[ValObjTree]): ValObjTree =
  ValObjTree(kind: okSequence, valItems: toSeq(vals), itemType: valType)

func pptMap*(kvTypes: (string, string),
             vals: varargs[tuple[
               key: string,
               val: ValObjTree]]): ValObjTree =

  ValObjTree(kind: okTable,
             keyType: kvTypes[0],
             valType: kvTypes[1],
             valPairs: toSeq(vals))

func pptObj*(name: string,
             flds: varargs[tuple[
               name: string,
               value: ValObjTree]]): ValObjTree =

  ValObjTree(kind: okComposed,
             sectioned: false,
             namedObject: true,
             namedFields: true,
             name: name,
             fldPairs: toSeq(flds))

func pptObj*(name: string,
             flds: varargs[ValObjTree]): ValObjTree =
  ValObjTree(kind: okComposed,
             sectioned: false,
             namedObject: true,
             namedFields: false,
             name: name,
             fldPairs: flds.mapIt(("", it)))

type
  TreeReprParams* = object
    maxDepth: int
    outWidth: int

#==============================  Lisp repr  ==============================#

func lispReprImpl*(tree: ValObjTree,
                   params: TreeReprParams,
                   level: int): string =

  if level >= params.maxDepth:
    return "..."

  case tree.kind:
    of okConstant:
      return tree.strLit
    of okSequence:
      return tree.valItems.
        mapIt(lispReprImpl(it, params, level + 1)).
        joinw().wrap(("'(", ")"))
    of okTable:
      return tree.valPairs.
        mapPairs(fmt("(:{lhs} {rhs.lispReprImpl(params, level)})")).
        joinw().wrap("()")
    of okComposed:
      case tree.sectioned:
        of true:
          raiseAssert("#[ IMPLEMENT ]#")
        of false:
          return tree.fldPairs.
            mapPairs(
              tree.namedFields.tern(&":{lhs} ", "") &
              rhs.lispReprImpl(params, level + 1)).
            joinw().
            wrap do:
              if tree.namedObject:
                if tree.name.validIdentifier():
                  (&"({tree.name} ", ")")
                else:
                  (&"(`{tree.name}` ", ")")
              else:
                (("(", ")"))


func lispRepr*(tree: ValObjTree, maxlevel: int = 60): string =
  lispReprImpl(tree, TreeReprParams(
    maxDepth: maxlevel,
  ), level = 0)

#==============================  Tree repr  ==============================#
func treeReprImpl*(tree: ValObjTree,
                   params: TreeReprParams,
                   pref: seq[bool],
                   parentMaxIdx, currIdx: int,
                   parentKind: ObjKind): seq[string] =

  let arrow =
    case parentKind:
      of okComposed: "+-> "
      of okConstant: "+-> "
      of okSequence: "+-- "
      of okTable: "+-: "

  let prefStr =
    if pref.len > 0:
      if parentKind == okSequence and pref.len == 1:
        arrow
      else:
        pref.mapIt(it.tern("|   ", "    ")).join("") & arrow
    else:
      ""

  let prefStrNoarrow = pref.mapIt(it.tern("|   ", "    ")).join("")
  case tree.kind:
    of okConstant:
      return @[prefStr & tree.strLit]
    of okSequence:
      if pref.len + 1 > params.maxdepth:
        return @[prefStr & (tree.itemType.len > 0).tern(
          &"seq[{tree.itemType}] ", "") & "... (" &
          toPluralNoun("item", tree.valItems.len) & ")"
        ]

      for idx, item in tree.valItems:
        result &= treeReprImpl(
          item,
          params,
          pref & @[currIdx != parentMaxIdx],
          parentMaxIdx = tree.valItems.len - 1,
          currIdx = idx,
          parentKind = tree.kind
        )
    of okTable:
      let name = (tree.keyType.len > 0 and tree.valType.len > 0).tern(
          &"[{tree.keyType} -> {tree.valType}] ", "")
      if pref.len + 1 > params.maxdepth:
        result &= prefStr & name & "... (" &
          toPluralNoun("pair", tree.valPairs.len) & ")"
        return
      else:
        result &= prefStr & name

      result &= concat mapPairs(tree.valPairs) do:
         @[prefStrNoarrow & (currIdx < parentMaxIdx).tern("|", " ") &
           "   +-: " & lhs] &
         treeReprImpl(
           rhs,
           params,
           pref & @[currIdx != parentMaxIdx] &
             (rhs.kind == okConstant).tern(
               @[idx < tree.valPairs.len - 1], @[]),
           parentMaxIdx = tree.valPairs.len - 1,
           currIdx = idx,
           parentKind = rhs.kind
         )
    of okComposed:
      if tree.sectioned:
        raiseAssert("#[ IMPLEMENT ]#")
      else:
        let
          name = tree.name.validIdentifier.tern(
            tree.name, tree.name.wrap("``"))

        if pref.len + 1 > params.maxdepth:
          result &= prefStr & name & " ... (" &
            toPluralNoun("field", tree.fldPairs.len) & ")"
          return
        else:
          result &= prefStr & name & ":"

        result &= concat mapPairs(tree.fldPairs) do:
          tree.namedFields.tern(@[prefStr & lhs], @[]) &
          treeReprImpl(
            rhs,
            params,
            pref & @[currIdx != parentMaxIdx],
            parentMaxIdx = tree.fldPairs.len - 1,
            currIdx = idx,
            parentKind = tree.kind
          )

func treeRepr*(tree: ValObjTree,
               maxlevel: int = 60): string =
  treeReprImpl(
    tree,
    TreeReprParams(
      maxDepth: maxlevel
    ),
    @[], 0, 0,
    parentKind = tree.kind
  ).join("\n")
