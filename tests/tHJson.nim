import hmisc/other/hjson
import std/unittest

suite "Json pretty printing":
  test "test":
    echo toPretty(parseJson("""
{
  "plainName": "hhh",
  "doctext": {
    "subnodes": [
      {
        "subnodes": [
          {
            "subnodes": [
              {
                "kind": "Word"
              },
              {
                "kind": "Word"
              },
              {
                "kind": "Word"
              }
            ],
            "kind": "Paragraph"
          },
          {
            "subnodes": [
              {
                "subnodes": [
                  {
                    "kind": "RawText"
                  },
                  {
                    "kind": "EmptyNode"
                  },
                  {
                    "kind": "EmptyNode"
                  },
                  {
                    "kind": "EmptyNode"
                  },
                  {
                    "kind": "EmptyNode"
                  }
                ],
                "kind": "ListItem"
              }
            ],
            "kind": "List"
          }
        ],
        "kind": "StmtList"
      }
    ],
    "kind": "Document"
  },
  "kind": "dekProc",
  "prReturn": 1
}
"""))

  test "Enum serialization":
    type
      En = enum
        en1 = "hello"
        en2

    echo toJson(en1)
    echo toJson(en2)
