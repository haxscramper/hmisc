This module implements combinator-based text layout algorithm, adapted from
google's rfmt `implementation <https://github.com/google/rfmt>`_. It
provides convenient primitves for building custom pretty-printers without
worrying about choosing optimal layout - you describe all possible layouts
and then get the most optimal one, given constraints on right and left
margins, line break and several others.

To construct block tree for layoyt you can either use `makeLineBlock`,
`makeStackBlock` and other functions or DSL.

DSL for tree construction is implemented in form of operator overloading,
allowing for easy splicing of custom logic. `LytBuilderKind` describes
different forms of layout and has overloaded `[]` operator.
`initBlockFmtDSL` template creates one-letter shortcuts for different
layout kinds -

- ``H`` for **H** orizontal
- ``V`` for **V** ertical
- ``T`` for **T** ext
- ``I`` for **I** ndent
- ``S`` for **S** pace
- ``C`` for **C** hoice

And should be used like this:

.. code-block:: nim

    H[
      T["proc ("],
      C[ # Choice combinator
        # Put arguments horizontally
        H[@[T["arg1: int"], T["arg2: int"],]].join(T[", "]),

        # Put arguments vertically
        V[@[T["arg1: int"], T["arg2: int"],]].join(T[", "]),
      ],
      T[")"]
    ]

This describes layut for nim proc declarations, and provides to
alternatives - put arguments either horizontally or veritcally.

**Note**

Horizontal layout combinator attaches topmost line in the block to the
lowest part of the preceding block, so arrangement ``H[V[T["[#]"],
T["[#]"]], V[T["[#]"], T["[#]"]]]`` would result in

.. code-block ::

  [#]
  [#][#]
     [#]
