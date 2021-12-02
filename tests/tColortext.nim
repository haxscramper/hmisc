import hmisc/preludes/unittest

testFileStarted()


import
  hmisc/types/[colortext, colorstring],
  hmisc/algo/[halgorithm, clformat],
  hmisc/preludes/unittest

import
  std/[strformat, strutils, sequtils, unicode, macros]

startHax()

suite "Colortext c++":
  show colorizeToStr(
    "int main () { /* Long comment */ }",
    cppStyleMap,
    "c++"
  )

suite "Full printouts":
  test "Named colors":
    var buf: string
    for base in 0 .. (ord(high(TermColor8Bit)) -
                      ord(low(TermColor8Bit))) div 4:
      for color in 0 .. 3:
        let color = TermColor8Bit(color + base * 4)
        buf.add $to8BitBg("##", color)
        buf.add " "
        buf.add $to8Bit(strutils.alignLeft($color, 20), color)

      buf.add "\n"

    show buf

  test "Color table":
    var buf: string
    for gray in 0 .. 23:
      buf.add $to8BitBg(&"[{gray}]", gray)

    buf.add "\n"

    for r in 0 .. 5:
      for g in 0 .. 5:
        for b in 0 .. 5:
          buf.add $to8BitBg(&"[{r} {g} {b}]", r, g, b)
        buf.add "\n"
      buf.add "\n"

    show buf

suite "Close colors":
  test "Complementary":
    var buf: string
    for c in 0 .. 5:
      let
        r = 5 - c
        g = c
        b = 5 - c
        col = term8Bit(r, g, b)
      buf.add &"{col.int:<3} "
      buf.add $to8Bit(&"{r} {g} {b}", col)
      buf.add " -> "
      buf.add $to8Bit("??", col.complement())
      buf.add "; "
      buf.add $col.toHSV()
      buf.add "\n"

    show buf


  test "Analog/square/triad/gradient":
    for r in 0 .. 5:
      for g in 0 .. 5:
        for b in 0 .. 5:
          let col = term8Bit(r, g, b)
          let (r1, g1, b1) = col.toRGB()
          assert r1 == r, &"{r1}, {r}"
          assert g1 == g, &"{g1}, {g}"
          assert b1 == b, &"{b1}, {b}"

    show:
      interpolGradient(tcBlue3, tcGreen3)
      interpolGradient(tcRed3, tcWhite)

    var buf: string
    buf.add "analog            square            triad\n"
    for c in 0 .. 5:
      let
        r = 5 - c
        g = c
        b = 5 - c
        col = term8Bit(r, g, b)

      buf.add $col.analog()
      buf.add $col.square()
      buf.add $col.triad()
      buf.add &" {r} {g} {b}\n"

    show buf

suite "Colored string":
  test "Colored string wrapping":
    check $toRed("999") == "\e[31m999\e[39m"
    check $toDefault("999") == "999"

  test "{termLen}":
    check termLen("hhh") == 3
    check termLen("\e[41mhhh\e[49m") == 3
    check termLen("ᛀᛀᛀᛀ") == 4
    check termLen("\e[42mᛀᛀ\e[49m\e[44mᛀᛀ\e[49m") == 4

  test "{splitSGR}":
    func lispRepr(strs: seq[ColoredString]): string =
      strs.mapIt(it.lispRepr()).join(" ").wrap("()")

    # TEST TODO test multiple consecutive control codes
    check "hello \e[31mworld\e[39m".splitSGR() == @[
      "hello ".initColoredString(),
      "world".initColoredString(fg = fgRed)
    ]

    check "--\e[31mAA\e[39m--".splitSGR() == @[
      "--".initColoredString(),
      "AA".initColoredString(fg = fgRed),
      "--".initColoredString()
    ]


    check "\e[92m000\e[39m-\e[94m000\e[39m".splitSGR() == @[
      initColoredString("000", fg = fgGreen, style = {styleBright}),
      initColoredString("-"),
      initColoredString("000", fg = fgBlue, style = {styleBright})
    ]

    check "###---\e[31m\e[44m\e[4m\e[3m###\e[23m\e[24m\e[49m\e[39m".
      splitSGR() == @[
        initColoredString("###---"),
        initColoredString(
          "###", fg = fgRed, bg = bgBlue, style = {
            styleItalic, styleUnderscore})
      ]

  test "{split} ColorString":
    check splitSGR($"Hello-World".toRed())[0].split("-") == @[
      initColoredString("Hello", fg = fgRed),
      initColoredString("World", fg = fgRed)
    ]

    check initColoredString("--").split("-").mapIt(it.str) == "--".split("-")


  test "{splitColor}":
    check "\e[31mHello\nworld\e[39m".splitColor("\n") == @[
      $"Hello".toRed(),
      $"world".toRed()]

    check:
      "".split("\n")[0] == ""
      "".splitColor("\n")[0] == ""

  test "{toString} colored runes":
    check @[
      initColoredRune(uc"¤", initPrintStyling(fg = fgRed)),
      initColoredRune(uc"¤", initPrintStyling(bg = bgGreen, fg = fgRed)),
    ].toString() == "\e[31m¤\e[42m¤\e[39m\e[49m"

  test "{splitSGR_sep}":
    block:
      let tt = splitSGR_sep($("test" & toRed("colored") & "12312"))[0]
      check tt.len == 3
      check tt == @[
        initColoredString("test"),
        initColoredString("colored", fg = fgRed),
        initColoredString("12312")
      ]

    check "*=*=*".splitSGR_sep("=").mapIt(it[0]) ==
      initColoredString("*=*=*").split("=")

    check "hello\n\e[31msfadf\e[39m\nsf".splitSGR_sep("\n") == @[
      @[ initColoredString("hello") ],
      @[ initColoredString("sfadf", fg = fgRed) ],
      @[ initColoredString("sf") ],
    ]

    check "\e[43mhello\e[49m\n-\n\e[41mworld\e[49m".splitSGR_sep() == @[
      @[ initColoredString("hello", bg = bgYellow) ],
      @[ initColoredString("-") ],
      @[ initColoredString("world", bg = bgRed) ]
    ]

    check "\n\nee\n".splitSGR_sep() == @[
      @[ initColoredString("") ],
      @[ initColoredString("") ],
      @[ initColoredString("ee") ],
      @[ initColoredString("") ]
    ]

    check "-\e[31m--\n--\e[39m-".splitSGR() == @[
      initColoredString("-"),
      initColoredString("--\n--", fg = fgRed),
      initColoredString("-")
    ]

    check "-\e[31m--\e[39m\n\e[31m--\e[39m-".splitSGR() == @[
      initColoredString("-"),
      initColoredString("--", fg = fgRed),
      initColoredString("\n"),
      initColoredString("--", fg = fgRed),
      initColoredString("-")
    ]

    check "-\e[31m--\n--\e[39m-".splitSGR_sep() == @[
      @[ initColoredString("-"), initColoredString("--", fg = fgRed) ],
      @[ initColoredString("--", fg = fgRed), initColoredString("-") ]
    ]

    check "-\e[31m--\e[39m\n\e[31m--\e[39m-".splitSGR_sep() == @[
      @[ initColoredString("-"), initColoredString("--", fg = fgRed) ],
      @[ initColoredString("--", fg = fgRed), initColoredString("-") ]
    ]


  test "{splitSGR_sep} to runes":
    check "-\e[31m$\n$\e[39m-".splitSGR_sep().toRuneGrid() == @[
      @[ initColoredRune(Rune('-')),
         initColoredRune(Rune('$'), initPrintStyling(fg = fgRed))],
      @[ initColoredRune(Rune('$'), initPrintStyling(fg = fgRed)),
         initColoredRune(Rune('-'))]
    ]

  test "{exception printout}":
    let iinfo = instantiationInfo()
    let err = CodeError(
      msg: "Toplevel \e[31mannotation\e[39m ",
      annots: @[
      ErrorAnnotation(
        errpos: LineInfo(
          line: iinfo.line,
          column: iinfo.column,
          filename: currentSourcePath()
        ),
        fromString: false,
        annotation: "Hell \e[32masdfas\e[39md o",
        linerange: -2,
        expr: "errpos: currLineInf()")])

  test "Link printing":
    show:
      "google.com".toLink()
      instantiationInfo().toLink("here")

    # echo err.toColorString()

suite "clformat":
  test "Describe chars":
    show:
      describeChar('.')
      describeChar("б"[0])
      describeChar("б"[1])

  test "не-ASCII":
    show:
      hshow("юникод\n\n")
      toColoredText("юникод")
      toColoredText("юникод\n")
      toYellow("векторная диаграмма")
      toYellow("векторная диаграмма\n")
      "Расчет смещения" + fgYellow
      "Расчет смещения\n" + fgGreen

    block:
      var c: ColoredText
      c.add toYellow("___AAA___")
      c.add toYellow("___ЖЖЖ___")

      show:
        c
        c.runes

      c.add toYellow("___QQQ___\n")
      c.add toYellow("___ГГГ___\n")

      show:
        c
        c.runes

testFileEnded()
