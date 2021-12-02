import hmisc/preludes/unittest

testFileStarted()

import
  std/[sugar, strutils, sequtils, strformat, options]

import
  hmisc/core/[all, code_errors],
  hmisc/other/[blockfmt],
  hmisc/types/colorstring

initBlockFmtDSL()

suite "Block formatting":
  test "Vertical layouts":
    check V[T["a"], T["b"]].toString() == clt("a\nb")
    check V[T["-"], T["-"], T["-"]].toString() == clt("-\n-\n-")
    check V[T["*"], C[T["a"], T["b"]]].toString() == clt("*\na")

  test "Aligned grid":
    show makeAlignedGrid(
      @[
        @[T["test"], T[" = "], T["value"]],
        @[T["tes123t"], T[" = "], T["value"]]
      ],
      [
        sadRight, sadCenter, sadLeft
      ]
    ).toString()

    show makeAlignedGrid(
      @[
        @[T["version*:"], T["cuint"], T[""]],
        @[
          T["show*:"],
          T["git_status_show_t"],
          V[T["## The version"], T["## Documentation comment"]]
        ],
        @[T["flags*:"], T["cuint"], T["## Flags"]]
      ], [
        sadLeft, sadLeft, sadLeft
      ]
    ).toString()


    show toString(T["1231" + fgRed]).toString(true)
    show toString(T["1231" + fgRed]).toString(false)


  # test "Choice":
  #   assertEq @["0000".txb, "00".txb].choice().lyt(3), "00"

  #   let bl = @[
  #     @["hello".txb, " ".txb, "world".txb].vsb,
  #     @["hello".txb, " ".txb, "world".txb].hsb
  #   ]

  #   assertEq choice(bl).lyt(), "hello world"

  # test "Wrap blocks":
  #   assertEq makeTextBlocks(@["1", "2", "3"]).wrapBlocks(margin = 2), "12\n3"

  # test "Python implementation conmparison":
  #   assertEq(str(txb("hello")), "hello")
  #   assertEq(str(vsb([txb("he"), txb("llo")])), "he\nllo")

  #   echo str(hsb([txb("proc"), txb("hello*"), nl(), txb("world")]))

  #   echo str(hsb([
  #     txb("proc"),
  #     txb("nice*"),
  #     txb("("),
  #     ind(wrap([
  #       hsb([txb("arg:"), txb("Type"), txb(", ")]),
  #       hsb([txb("arg:"), txb("Type")]),
  #     ]), 4),
  #     txb(")")
  #   ]))

  # test "Function argument wrap":
  #   echo str hsb([
  #     txb("    "),
  #     hsb([
  #       txb("similarityTreshold"),
  #       txb(": "),
  #       txb("ScoreCmpProc"),
  #       txb(",")
  #     ])
  #   ])

  #   echo str(hsb([
  #     txb("proc "),
  #     txb("hello*"),
  #     txb("("),
  #     choice([
  #       hsb([
  #         hsb([
  #           hsb([
  #             txb("similarityTreshold"),
  #             txb(": "),
  #             txb("ScoreCmpProc"),
  #             txb(",")
  #           ])
  #         ]),
  #         hsb([
  #           hsb([
  #             txb("secondArgument"),
  #             txb(": "),
  #             txb("StdCxx11BasicStringSizeType"),
  #             txb(",")
  #           ])
  #         ]),
  #         txb(")")
  #       ])
  #     ])
  #   ]))

suite "Block compaction":
  let r = codegenRepr

  show r H[T["qwe"], T["1111"]]

  show r H[
    H[T["name: "]],
    T["nim_gprof"],
    T[", "],
    H[T["altNames: "]]]

  show r H[T["123"]]


suite "Line layouts":
  test "1":
    # echo H[T["//"], V[H[T["--"], V[T["()"], T["<>"]]]]].toString()
    # let bl = H[
    #   V[T["[1.1]\n[1.2]"], T["[1.3]\n[1.4]"]],
    #   V[T["[2.1]\n[2.2]"], T["[2.3]\n[2.4]"]],
    #   # V[T["[3.1]\n[3.2]"], T["[3.3]\n[3.4]"]],
    # ]

    let sep = "\n"

    let bl =
      V[H[V[H[V[
        T["Value must be one of the following: "],
        H[V[H[T["info    "],
              V[T["Anything associated with\nnormal " &
                "operation and without\nany particular importance"]]]]]]]]]]

    show bl.toString()



suite "Paper examples":
  test "2":
    let bl =
      V[H[V[H[
        T["    "],
        V[T["Value must be one of the following: "],
          H[V[H[T["info    "],
                V[T["Anything with\nnormal and without\nany importance"]]],
              H[T["notice  "],
                V[T["More information that\nusers"]]]]],
          T[" "]]]]]]

    show bl.toString()


  test "6.2 function name wrap":
    show toString(H[
      H[T["FnName"], T["("]],
      W[mapIt(1 .. 10, T[&"argument{it}"])],
      T[")"]
    ], 50)

    show toString(H[
      H[T["FnName"], T["("]],
      W[mapIt(1 .. 10, T[&"argument{it}"])],
      T[")"]
    ], 30)

    show toString(H[
      H[T["AVeryLongAndDescriptiveFunctionName"], T["("]],
      W[mapIt(1 .. 10, T[&"argument{it}"])],
      T[")"]
    ], 50)

    show toString(C[
      H[
        H[T["AVeryLongAndDescriptiveFunctionName"], T["("]],
        W[mapIt(1 .. 10, T[&"argument{it}"])],
        T[")"]
      ],
      V[
        H[T["AVeryLongAndDescriptiveFunctionName"], T["("]],
        I[4, W[mapIt(1 .. 10, T[&"argument{it}"])]],
        T[")"]
      ]
    ], 50)

suite "Edge case layouts":
  test "Stack of lines in braces":
    let bl = H[
      T["proc ("],
      V[
        T["line 1"],
        T["line 2"],
        T["line 3"],
      ],
      T[" = "],
      V[
        T["line 4"],
        T["line 5"],
        T["line 6"],
      ],
      T[")"]
    ]

    show toString(bl)


  test "Choice stack vs line":

    if true:
      show toString(
        H[
          T["proc ("],
          V[T["arg1: int"], T["arg2: int"], T["arg3: int"]].join(T[", "]),
          T[")"]
        ]
      )

    if true:
      show toString(
        H[
          T["proc ("],
          C[
            V[@[T["arg1: int"], T["arg2: int"],]].join(T[", "])
          ],
          T[")"]
        ]
      )


    if true:
      show toString(
        H[
          T["proc ("],
          C[
            H[@[T["arg1: int"], T["arg2: int"],]].join(T[", "]),
            V[@[T["arg1: int"], T["arg2: int"],]].join(T[", "]),
          ],
          T[")"]
        ],
        40
      )

    if true:
      for i in [1, 5, 10]:
        var blocks = mapIt(0 .. i, T["arg: int" & $i])
        let bl = H[
          T["proc ("],
          C[
            H[blocks].join(T[", "]),
            V[blocks].join(T[", "])
          ],
          T[")"]
        ]

        show toString(bl)

suite "WIP testing":
  test "A:":
    var line = makeLineBlock([
        makeLineBlock([makeVerbBlock(["222"]), makeTextBlock("333")]),
        makeTextBlock("444"),
      ])


    # wipeNewlined()
    show pyCodegenRepr(line)
    show treeRepr(line)
    show toString(line)

testFileEnded()
