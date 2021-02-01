import hmisc/macros/introspection


import unittest

suite "Enum introspection":
  test "Underlying names":
    echo 1

  test "Named subnodes":
    type
      AstKind = enum
        askFirst
        askSecond
        askThird

    echo namedSubnode(askFirst, 1, @{
      askFirst : @["head", "tail"]
    })
