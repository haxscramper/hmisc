# {.define(plainStdout).}

import
  std/[strutils, sequtils, strformat, options, terminal, unittest]

import
  hmisc/extra/hdrawing/hdrawing,
  hmisc/types/[seq2d, colorstring],
  hmisc/algo/halgorithm

converter toSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  makeSeq2D(s)

suite "Drawing":
  test "test":
    block:
      withBufEcho:
        newTermText((0, 0), @["222"]).render(buf)

    if false:
      var buf = newBuf()
      newTermText((0,0), @["* (0, 0)".toRunes()]).render(buf)
      newTermText((8, 5), @["* (5, 5)"]).render(buf)
      # newTermPoint((39, 19)).render(buf)
      newTermVLine((0, 0), 18).render(buf)
      # newTermHLine((0, 0), 38).render(buf)
      newTermPoint((0, 0), '#').render(buf)
      newBoxedTermText(
        (0, 0), @["Hello world", "Some text", "to render"]
      ).render(buf)

      let w = 10
      newTermRect((8, 6), w, 5, makeTwoLineRectBorder()).render(buf)

      for p in 6 ..+ 5:
        newTermText((8 + w, p), @[&"* ({8 + w}, {p})"]).render(buf)

      newTermText((8, 11), @["12345"]).render(buf)

      newBoxedTermText(
        (15, 15), @["Text inside", "of unicode box"],
        makeTwoLineRectBorder()
      ).render(buf)
      echo buf.toString()


    block:
      var buf = newBuf()
      newTermGrid(
        (0, 0),
        @[
          @["HEllo", "world"],
          @["wer", "2333"],
          @[
            "1222",
            newTermGrid(
              (0, 0),
              @[
                @["HEllo", "world"],
                @["wer", "2333"],
                @["1222", "Hello"]
              ].toTermBufGrid(),
              makeThinLineGridBorders(initPrintStyling(fg = fgRed))
            ).toTermBuf().toString(),
            newTermGrid(
              (0, 0),
              @[
                @["HEllo", "world"],
                @["wer", "2333"],
                @["1222", "Hello"]
              ].toTermBufGrid(),
              makeEmptyGridBorders()
            ).toTermBuf().toString()
          ]
        ].toTermBufGrid(),
        makeThinLineGridBorders()
      ).render(buf)
      echo buf.toString()

  test "Multicell grid":
    proc ms(a, b: int): auto = makeArrSize(a, b)
    let nn = none((ArrSize, TermBuf))
    proc sb(s: string): TermBuf = s.toTermBuf()
    let res = newTermMultiGrid(
      (0, 0),
      @[
        @[
          some((ms(2, 3), sb("Hello\nWorld\nreallyu long str\ning"))),
          nn,
          some((ms(2, 3), sb(newTermMultiGrid(
            (0, 0),
            @[
              @[
                some((ms(1, 1), sb("222"))),
                some((ms(1, 1), sb("(((---)))"))),
                some((ms(1, 1), sb("***)"))),
                some((ms(1, 1), sb("***")))
              ],
              @[
                some((ms(1, 1), sb("222"))),
                some((ms(1, 1), sb("((()))"))),
                some((ms(1, 1), sb("***"))),
                some((ms(1, 1), sb("***")))
              ],
            ],
            makeAsciiGridBorders(initPrintStyling(fg = fgRed)),
          ).toTermBuf().toString() & "\nSome annotation"))),
          nn
        ],
        @[nn, nn, nn, nn],
        @[nn, nn, nn, nn],
        @[
          some((ms(1, 1), sb("222"))),
          some((ms(1, 1), sb("(((---)))"))),
          some((ms(1, 1), sb("***\n((()))"))),
          some((ms(1, 1), sb("***")))
        ],
        @[
          some((ms(1, 1), sb("222"))),
          some((ms(1, 1), sb("((()))"))),
          some((ms(1, 1), sb("***"))),
          some((ms(1, 1), sb("***")))
        ],
      ],
      makeAsciiGridBorders(),
    ).toTermBuf().toString()
    echo res

  if true:
    let buf1 = newTermGridVert(@[
      "eeeeaf asdf as\nasdfd\ndff",
      "eeee\nsdfa\ndddd"
    ]).toStringBlock()


    let buf2 = newTermGridVert(@[
      "eeeeaf asdf as\nasdfd\ndff",
      "eeee\nsdfa\ndddd"
    ]).toStringBlock()

    let shape = newTermGridHoriz(@[
      buf1, buf2
    ], ' ')

    # echo shape.nthShape().toStringBlock.join("\n")
    echo shape.toStringBlock().join("\n")
