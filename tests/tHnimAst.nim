import sugar, strutils, sequtils, strformat, macros, options
import ../src/hmisc/types/hnim_ast
import ../src/hmisc/helpers
import ../src/hmisc/macros/obj_field_macros

import compiler/renderer

import hpprint

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
            if obj.annotation.isSome():
              for call in obj.annotation.get().elements:
                discard

            for field in obj.flds:
              if field.annotation.isSome():
                for call in field.annotation.get().elements:
                  discard

    parse:
      type
        Type {.zzz(Check).} = object
          f1 {.check(it < 10).}: float = 12.0

        Type*[A] {.ss.} = object
          f1: int

        Type* {.ss.} = object
          f23: int

        Type[A] {.ss.} = object
          f33: int

        Type[B] {.ss.} = object
          case a: bool
            of true:
              b: int
            of false:
              c: float

        Hhhhh = object
          f1: float
          f3: int
          case f5: bool
            of false:
              f2: char
            else:
              f4: float


  test "{parseObject} filter pragma annotations":
    macro parse(body: untyped): untyped =
      var obj = body[0][0].parseObject(parseNimPragma)
      for call in obj.annotation.get().elements:
        discard

      obj.annotation = none(NPragma)

      obj.eachFieldMut do(fld: var ObjectField[NimNode, NPragma]):
        fld.annotation = none(NPragma)

      result = nnkTypeSection.newTree obj.toNimNode()

    parse:
      type
        Type {.zz(C), ee: "333", ee.} = object
          f1 {.check(it < 2).}: float = 32.0

  test "{mkProcDeclNode}":
    macro mcr(): untyped =
      result = newStmtList()
      result.add mkProcDeclNode(
        ident "hello",
        { "world" : mkNType("int") },
        newCall(
          "echo", newLit("value is: "), ident "world"
        ),
        exported = false
      )

    mcr()

    hello(12)

  test "{eachCase}":
    macro mcr(body: untyped): untyped =
      let obj = body[0][0].parseObject(parseNimPragma)
      let objid = ident "hjhh"
      let impl = objid.eachCase(obj) do(fld: NField[NPragma]) -> NimNode:
        let fld = ident fld.name
        quote do:
          echo `objid`.`fld`

      result = newStmtList(body)

      result.add quote do:
        let hjhh {.inject.} = Hello()

      result.add impl

    mcr:
      type
        Hello = object
          ewre: char
          case a: uint8:
            of 0:
              zee: float
            of 2:
              eee: string
            of 4:
              eee3: int
              eee24: int
              eee2343: int
              eee321344: int
            else:
              eee23: string


  test "{eachParallelCase}":
    ## Automatically generate comparison proc for case objects.
    macro mcr(body: untyped): untyped =
      let
        obj = body[0][0].parseObject(parseNimPragma)
        lhs = ident "lhs"
        rhs = ident "rhs"

      let impl = (lhs, rhs).eachParallelCase(obj) do(
        fld: NField[NPragma]) -> NimNode:
        let fld = ident fld.name
        quote do:
          if `lhs`.`fld` != `rhs`.`fld`:
            return false


      let eqcmp = [ident "=="].mkProcDeclNode(
        mkNType("bool"),
        { "lhs" : obj.name, "rhs" : obj.name },
        pragma = mkNPragma("noSideEffect"),
        impl = (
          quote do:
            `impl`
            return true
        ),
        exported = false
      )

      result = nnkStmtList.newTree(body, eqcmp)

    mcr:
      type
        A = object
          fdl: seq[float]
          case b: char
            of '0':
              qw: char
              wer: float
            of '-':
              ee: float
            else:
              eeerw: char
              # nil # TODO

    echo A() == A()

  test "{eachPath}":
    macro mcr(body: untyped): untyped =
      let obj = body[0][0].parseObject(parseNimPragma)
      let self = ident "self"
      let impl = self.eachPath(obj) do(
        path: NPath[NPragma], flds: seq[NField[NPragma]]) -> NimNode:
        discard

    mcr:
      type
        A = object
          c: int
          f: int
          case a: char
            of 'e':
              e: int
            of 'q':
              q: char
            else:
              hello: seq[seq[int]]

suite "working with PNode":
  test "Core":
    echo newPIdent("hello")
    echo newReturn(newPIdent("qqqq"))
    echo newPrefix("!", newPIdent("eee"))
