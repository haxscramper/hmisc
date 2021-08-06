import
  hmisc/other/[hunittest, hpprint, blockfmt],
  hmisc/types/[colorstring],
  hmisc/core/all

startHax()

suite "main suite":
  test "test suite":
    check(stringdiff("a", "b"))

  test "parametrize":
    parametrizeOnConst N, [1, 3, 4]:
      echo N
      echo N + 1

suite "structdiff":
  test "different fields":
    type
      TestStruct = object
        f: string

    let
      s1 = TestStruct(f: "struct-1")
      s2 = TestStruct(f: "struct-2")

    echo pptree(s1).pstring()
    echo pptree(s2).pstring()

    check structdiff(s1, s2)

  test "Mismatched fields":
    let blc = ppblock newPPrintAnnotation(
      "head" + fgBlue,
      @[
        newPPrintAnnotation(
          "deleted field" + fgRed, @[newPPrintConst("12")]),

        newPPrintAnnotation(
          "added field" + fgGreen, @[newPPrintConst("12")]),
      ]
    )

    echo blc.toString()

    check structdiff(
      newPPrintObject("head", @{"field1": newPPrintConst("12")}),
      newPPrintObject("head", @{"field2": newPPrintConst("12")})
    )
