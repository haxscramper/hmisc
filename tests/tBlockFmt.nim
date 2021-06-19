import std/[sugar, strutils, sequtils, strformat, options]
import hmisc/helpers
import hmisc/hdebug_misc

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest
import hmisc/other/blockfmt

initBlockFmtDSL()

suite "Block formatting":
  test "Vertical layouts":
    assertEq V[T["a"], T["b"]].toString(), "a\nb"
    assertEq V[T["-"], T["-"], T["-"]].toString(), "-\n-\n-"

    assertEq V[T["*"], C[T["a"], T["b"]]].toString(), "*\na"


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


suite "Line layouts":
  # echo H[T["//"], V[H[T["--"], V[T["()"], T["<>"]]]]].toString()
  # let bl = H[
  #   V[T["[1.1]\n[1.2]"], T["[1.3]\n[1.4]"]],
  #   V[T["[2.1]\n[2.2]"], T["[2.3]\n[2.4]"]],
  #   # V[T["[3.1]\n[3.2]"], T["[3.3]\n[3.4]"]],
  # ]

  let sep = "\n"

  let bl = H[
    V[T[&"[1.1]{sep}[1.2]"], T[&"[1.3]{sep}[1.4]"]],
    V[T[&"[2.1]{sep}[2.2]"], T[&"[2.3]{sep}[2.4]"]],
    # V[T["[3.1]\n[3.2]"], T["[3.3]\n[3.4]"]],
  ]

  startHax()
  echo bl.treeRepr()
  echo bl

  echo bl.toString()
  echo bl.treeRepr()
  stopHax()


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

    echo toString(deepCopy(bl), fixLyt = false)
    echo toString(bl)


  test "Choice stack vs line":

    if true:
      echo toString(
        H[
          T["proc ("],
          V[T["arg1: int"], T["arg2: int"], T["arg3: int"]].join(T[", "]),
          T[")"]
        ]
      )

    if true:
      echo toString(
        H[
          T["proc ("],
          C[
            V[@[T["arg1: int"], T["arg2: int"],]].join(T[", "])
          ],
          T[")"]
        ]
      )


    if true:
      echo toString(
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

        echo toString(bl)
