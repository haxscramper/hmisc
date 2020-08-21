import sugar, strutils, sequtils, strformat, macros, options
import ../src/hmisc/types/hnim_ast
import ../src/hmisc/helpers
import ../src/hmisc/macros/obj_field_macros

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "HNimAst":
  test "{enumPref} :macro:":
    type
      En = enum
        en1 = 2

    assertEq enumPref(En), "en"
    let val = en1
    assertEq enumPref(val), "en"
    assertEq enumPref(en1), "en"

    proc gen[T](a: T, pr: string): void = assertEq enumPref(a), pr

    gen(en1, "en")

    type Alias = En

    var alias: Alias
    gen(alias, "en")

  test "{enumNames} :macro:":
    type
      En = enum
        en1 = "hello"
        en2 = "world"

    assertEq enumNames(En), @["en1", "en2"]
    assertEq enumNames(en1), @["en1", "en2"]
    let val = en1
    assertEq enumNames(val), @["en1", "en2"]

  test "{parseObject} parse nim pragma":
    macro parse(body: untyped): untyped =
      for stmt in body:
        for obj in stmt:
          if obj.kind == nnkTypeDef:
            let obj = obj.parseObject(parseNimPragma)
            for call in obj.annotation.get().elements:
              echo call.toStrLit()

            for field in obj.flds:
              for call in field.annotation.get().elements:
                echo call.toStrLit()

    parse:
      type
        Type {.zzz(Check).} = object
          f1 {.check(it < 10).}: float = 12.0

  test "{parseObject} filter pragma annotations":
    macro parse(body: untyped): untyped =
      echo "filter pragma annotations"
      var obj = body[0][0].parseObject(parseNimPragma)
      for call in obj.annotation.get().elements:
        echo call.toStrLit()

      obj.annotation = none(NPragma)

      obj.eachField do(fld: var ObjectField[NimNode, NPragma]):
        fld.annotation = none(NPragma)

      result = nnkTypeSection.newTree obj.toNimNode()

    parse:
      type
        Type {.zz(C), ee: "333", ee.} = object
          f1 {.check(it < 2).}: float = 32.0
