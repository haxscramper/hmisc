discard """
joinable: false
"""

import hmisc/preludes/unittest

testFileStarted()

import
  hmisc/hasts/mustache_template,
  hmisc/types/[colorstring],
  hmisc/preludes/unittest

import std/[streams, db_sqlite]

startHax()

suite "Simple template instantiation":
  test "Multiline interpolate":
    let tree = mustacheParse(lit3"""
      <h2>Names</h2>
      {{#names}}
        <strong>{{name}}</strong>
      {{/names}}
      """)

    echo treeRepr(tree)


testFileEnded()
