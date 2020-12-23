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
alternatives - put arguments either horizontally or veritcally. By default
necessary spacing is inserted for stacked layots, meaning `I[2, S[T["line
1"], T["line 2"]]]` will yield following output, as one might normally
expect -

.. code-block::

   | column 0
     | column 2
     line1
     line2

But layout algorithm does not operate on multiline blocks, and originally,
(without any fixes), you would get following output:

.. code-block::

   | column 0
     | column 2
     line1
   line2

Layout correction is enabled by default, and you should not be concerned
about it most of the time, but if needed it can be turned off via
`fixLayout = false` argument to variuous `make<block-kind>` procedures.
