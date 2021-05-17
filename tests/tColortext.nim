import
  hmisc/types/[colortext, colorstring],
  hmisc/algo/halgorithm,
  hmisc/[hdebug_misc, base_errors]

import std/[
  strformat, unittest, strutils, sequtils, unicode, macros
]

startHax()

suite "Colortext c++":
  echo colorizeToStr(
    "int main () { /* Long comment */ }",
    cppStyleMap,
    "c++"
  )

suite "Full printouts":
  test "Named colors":
    for base in 0 .. (ord(high(TermColor8Bit)) -
                      ord(low(TermColor8Bit))) div 4:
      for color in 0 .. 3:
        let color = TermColor8Bit(color + base * 4)
        stdout.write to8BitBg("##", color), " "
        stdout.write to8Bit(strutils.alignLeft($color, 20), color)

      stdout.write "\n"

    echo "done"


  test "Color table":
    for gray in 0 .. 23:
      stdout.write to8BitBg(&"[{gray}]", gray)

    stdout.write("\n")

    for r in 0 .. 5:
      for g in 0 .. 5:
        for b in 0 .. 5:
          stdout.write to8BitBg(&"[{r} {g} {b}]", r, g, b)
        stdout.write("\n")
      stdout.write("\n")


suite "Close colors":
  test "Complementary":
    for c in 0 .. 5:
      let
        r = 5 - c
        g = c
        b = 5 - c
        col = term8Bit(r, g, b)
      echo &"{col.int:<3} ", to8Bit(&"{r} {g} {b}", col), " -> ",
            to8Bit("??", col.complement()), "; ", col.toHSV()


  test "Analog/square/triad/gradient":
    for r in 0 .. 5:
      for g in 0 .. 5:
        for b in 0 .. 5:
          let col = term8Bit(r, g, b)
          let (r1, g1, b1) = col.toRGB()
          assert r1 == r, &"{r1}, {r}"
          assert g1 == g, &"{g1}, {g}"
          assert b1 == b, &"{b1}, {b}"


    for col in interpolGradient(tcBlue3, tcGreen3):
      echo col

    echo "analog            square            triad"
    for c in 0 .. 5:
      let
        r = 5 - c
        g = c
        b = 5 - c
        col = term8Bit(r, g, b)

      stdout.write(
        col.analog(), col.square(), col.triad(),&" {r} {g} {b}\n")

suite "Colored string":
  test "Colored string wrapping":
    assertEq "999".toRed(), "\e[31m999\e[39m"
    assertEq "999".toDefault(), "999"

  test "{termLen}":
    assertEq termLen("hhh"), 3
    assertEq termLen("\e[41mhhh\e[49m"), 3
    assertEq termLen("ᛀᛀᛀᛀ"), 4
    assertEq termLen("\e[42mᛀᛀ\e[49m\e[44mᛀᛀ\e[49m"), 4

  test "{splitSGR}":
    func lispRepr(strs: seq[ColoredString]): string =
      strs.mapIt(it.lispRepr()).join(" ").wrap("()")

    # TEST TODO test multiple consecutive control codes
    assertEq "hello \e[31mworld\e[39m".splitSGR(), @[
      "hello ".initColoredString(),
      "world".initColoredString(fg = fgRed)
    ]

    assertEq "--\e[31mAA\e[39m--".splitSGR(), @[
      "--".initColoredString(),
      "AA".initColoredString(fg = fgRed),
      "--".initColoredString()
    ]


    assertEq "\e[92m000\e[39m-\e[94m000\e[39m".splitSGR(), @[
      initColoredString("000", fg = fgGreen, style = {styleBright}),
      initColoredString("-"),
      initColoredString("000", fg = fgBlue, style = {styleBright})
    ]

    assertEq "###---\e[31m\e[44m\e[4m\e[3m###\e[23m\e[24m\e[49m\e[39m".
      splitSGR(), @[
        initColoredString("###---"),
        initColoredString(
          "###", fg = fgRed, bg = bgBlue, style = {
            styleItalic, styleUnderscore})
      ]

  test "{split} ColorString":
    assertEq "Hello-World".toRed().splitSGR()[0].split("-"), @[
      initColoredString("Hello", fg = fgRed),
      initColoredString("World", fg = fgRed)
    ]

    assertEq initColoredString("--").split("-").mapIt(it.str),
         "--".split("-")


    # echo initColoredString("\n\n\n").split("\n")

  test "{splitColor}":
    assertEq "\e[31mHello\nworld\e[39m".splitColor("\n"), @[
      "Hello".toRed(),
      "world".toRed()
    ]

    assertEq "".split("\n")[0], ""
    assertEq "".splitColor("\n")[0], ""

  test "{toString} colored runes":
    assertEq @[
      initColoredRune(uc"¤", initPrintStyling(fg = fgRed)),
      initColoredRune(uc"¤", initPrintStyling(bg = bgGreen, fg = fgRed)),
    ].toString(), "\e[31m¤\e[42m¤\e[39m\e[49m"

  test "{splitSGR_sep}":
    # echo "==".splitSGR_sep("=")
    # echo "==".splitSGR().mapIt(it.split("="))

    block:
      let tt = splitSGR_sep("test" & toRed("colored") & "12312")[0]
      assertEq tt.len, 3
      assertEq tt, @[
        initColoredString("test"),
        initColoredString("colored", fg = fgRed),
        initColoredString("12312")
      ]

    assertEq "*=*=*".splitSGR_sep("=").mapIt(it[0]),
      initColoredString("*=*=*").split("=")

    assertEq "hello\n\e[31msfadf\e[39m\nsf".splitSGR_sep("\n"), @[
      @[ initColoredString("hello") ],
      @[ initColoredString("sfadf", fg = fgRed) ],
      @[ initColoredString("sf") ],
    ]

    assertEq "\e[43mhello\e[49m\n-\n\e[41mworld\e[49m".splitSGR_sep(), @[
      @[ initColoredString("hello", bg = bgYellow) ],
      @[ initColoredString("-") ],
      @[ initColoredString("world", bg = bgRed) ]
    ]

    assertEq "\n\nee\n".splitSGR_sep(), @[
      @[ initColoredString("") ],
      @[ initColoredString("") ],
      @[ initColoredString("ee") ],
      @[ initColoredString("") ]
    ]
    # echo "\e[41m*=========\e[49m  eee  \e[41m==========*\e[49m"
    assertEq "-\e[31m--\n--\e[39m-".splitSGR(), @[
      initColoredString("-"),
      initColoredString("--\n--", fg = fgRed),
      initColoredString("-")
    ]

    assertEq "-\e[31m--\e[39m\n\e[31m--\e[39m-".splitSGR(), @[
      initColoredString("-"),
      initColoredString("--", fg = fgRed),
      initColoredString("\n"),
      initColoredString("--", fg = fgRed),
      initColoredString("-")
    ]

    assertEq "-\e[31m--\n--\e[39m-".splitSGR_sep(), @[
      @[ initColoredString("-"), initColoredString("--", fg = fgRed) ],
      @[ initColoredString("--", fg = fgRed), initColoredString("-") ]
    ]

    assertEq "-\e[31m--\e[39m\n\e[31m--\e[39m-".splitSGR_sep(), @[
      @[ initColoredString("-"), initColoredString("--", fg = fgRed) ],
      @[ initColoredString("--", fg = fgRed), initColoredString("-") ]
    ]


  test "{splitSGR_sep} to runes":
    assertEq "-\e[31m$\n$\e[39m-".splitSGR_sep().toRuneGrid(), @[
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
        expr: "errpos: currLineInf()"
      )
    ])

    # echo err.toColorString()
