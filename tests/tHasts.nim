import hmisc/preludes/unittest

testFileStarted()

import
  std/[terminal, strformat, strutils, colors]

import
  hmisc/hasts/[
    html_ast,
    graphviz_ast,
    svg_ast,
    latex_ast,
    json_ast,
    pandoc,
    xml_ast,
    html_ast2,
    openscad_ast
  ],
  hmisc/types/colorstring,
  hmisc/preludes/unittest,
  hmisc/algo/halgorithm

startHax()

func cs(str: string, fg: ForegroundColor): ColoredString =
  initColoredString(str, fg = fg)

func cs(str: ColoredString, bg: BackgroundColor): ColoredString =
  str.withIt:
    it.styling.bg = bg

func cs(str: ColoredString, ss: Style): ColoredString =
  str.withIt: it.styling.style.incl ss

let styling = """
div.termline { display:flex; }
span.term { display:inline-block; }
span.term-fgred { color: red; }
span.term-fggreen { color: green; }
span.term-bgblue { background-color: blue; }
"""


suite "HTML ast":
  test "Convert from colored string chunks":
    let strs = @[
      "Hello world".cs(fgRed),
      "Hello world 2".cs(fgGreen),
      "Hello world 2".cs(fgYellow).cs(styleBright),
      "Hello world 2".cs(fgGreen).cs(styleItalic),
    ]

    let doc = @[
      strs.toHTML(),
      newHtmlText("--- === ---").wrap("p"),
      strs.toHtml(false)
    ].toDocument(styling)

    # echo doc
    # "/tmp/page.html".writeFile(doc)

  test "Convert colored chuinks":
      let text = &"""
   hello
wer asdf {"werwer".toRed()} 2
      as
asd
  fa
sdfas
"""
      # echo text
      let colored = text.splitSGR_sep()
      let doc = colored.toHTML(false).toDocument(styling)

      # echo doc
      "/tmp/page.html".writeFile(doc)


suite "Graphviz generation":
  var topGraph = makeDotGraph()

  test "Colored note text":
    var graph = makeDotGraph()
    graph.idshift = 1

    for color in ForegroundColor:
      var str = splitCamel($color)
        .toUpperAscii()
        .join("-")
        .center(20)
        .wrap("<<<<", ">>>>")
        .initColoredString()

      str.fg = color
      str.style.incl styleItalic

      graph.add makeColoredDotNode(
        color.int, $str,
        tableAttrs = {"bgcolor" : $colLightSlateGray}
      )

      if color != low(ForegroundColor):
        graph.add makeDotEdge(color.int, pred(color).int)

    try:
      graph.toPng(AbsFile"/tmp/res.png")

    except:
      discard

    topGraph.add graph

  test "Record nodes":
    var record = makeDotGraph()
    record.idshift = 2
    record.add makeRecordDotNode(0, @[
      makeDotRecord(1, "test-0:1"),
      makeDotRecord(2, "test-0:2", @[
        makeDotRecord(3, "test-0:3"),
        makeDotRecord(4, "test-0:4")
      ]),
    ])
    record.add makeRecordDotNode(1, @[
      makeDotRecord(1, "test-1:1"),
      makeDotRecord(2, "test-1:2", @[
        makeDotRecord(3, "test-1:3"),
        makeDotRecord(4, "test-1:4"),
        makeDotRecord(5, "test-1:5", @[
          makeDotRecord(6, "test-1:6"),
          makeDotRecord(7, "test-1:7")])])])

    record.add makeDotEdge(toDotPath(0, 1), toDotPath(1, 1))
    record.add makeDotEdge(
      toDotPath(0, 3, dppRight),
      toDotPath(1, 4, dppRight))

    record.add makeDotEdge(
      toDotPath(0, 3, dppRight),
      toDotPath(1, 7, dppRight))


    topGraph.add record

  try:
    topGraph.toPng(
      AbsFile "/tmp/res.png",
      tmpfile = AbsFile "/tmp/dot-1.dot")

  except:
    discard

suite "graphiz terminal node styling":
  test "All color combinations":
    var topGraph = makeDotGraph()
    topGraph["bgcolor"] = "\"#27212E\""

    type
      F = ForegroundColor
      B = BackgroundColor

    proc toId(fg: F, bg: B): DotNodeId =
      toDotNodeId(int(fg) * 100 + int(bg))

    proc hasSucc(c: F | B): bool =
      c < high(typeof(c))

    for fg in F:
      for bg in B:
        topGraph.add makeDotNode(
          int(fg) * 100 + int(bg), $fg & "-" & $bg, fg, bg)

    for fg in F:
      for bg in B:
        if hasSucc(fg):
          topGraph.add makeDotEdge(toId(fg, bg), toId(succ(fg), bg))

        if hasSucc(bg):
          topGraph.add makeDotEdge(toId(fg, bg), toId(fg, succ(bg)))



    try:
      topGraph.toPng(
        AbsFile"/tmp/res-1.png",
        tmpfile = AbsFile"/tmp/dot-2.dot")

    except:
      discard

import
    hmisc/hasts/graphviz_ast,
    hmisc/other/[oswrap, hshell, hjson],
    hmisc/core/all,
    std/[strformat, parsesql, hashes],
    fusion/matching

const sqlExample = """
CREATE TABLE entries (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    parentId INTEGER references entries(id),
    depth INTEGER NOT NULL,
    name TEXT NOT NULL,
    kind INTEGER,
    docRaw TEXT,
    docParsed BLOB,
    -- Optional type uses in different entries
    typeUse INTEGER REFERENCES typeInstances(id),
    declHead INTEGER REFERENCES locations(id),
    -- procType INTEGER REFERENCES procedureSignatures(id)
);

CREATE TABLE identParts (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    entry INTEGER NOT NULL REFERENCES entries(id),
    partKind INTEGER NOT NULL
);

CREATE TABLE files (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    content TEXT
);

CREATE TABLE uses(
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    kind INTEGER NOT NULL,
    location INTEGER REFERENCES locations(id)
);

CREATE TABLE locations (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    fileId INTEGER REFERENCES files(id),
    line INTEGER NOT NULL,
    startCol INTEGER NOT NULL,
    endCol INTEGER NOT NULL,
    endLine INTEGER
);

CREATE TABLE typeInstances (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    -- Can be NULL when generating from `--fastdoc` or for DocType rows like
    -- 'value'
    usedType INTEGER REFERENCES entries(id),
    -- Different variants of indirect type uses
    kind INTEGER,
    -- Unwrapping layers for indirection for pointers, references and other
    --  aux helpers
    useKind INTEGER REFERENCES indirectUses(id)
);

CREATE TABLE genericSpecializations (
    baseType INTEGER NOT NULL REFERENCES entries(id),
    paramPos INTEGER NOT NULL,
    type INTEGER NOT NULL REFERENCES typeInstances(id)
);

CREATE TABLE argLists (
    id INTEGER NOT NULL,                             -- Arg list id
    pos INTEGER NOT NULL,                         -- Argument positin
    name TEXT NOT NULL,                              -- Argument name
    type INTEGER NOT NULL REFERENCES typeInstances(id) -- Arg type
);

CREATE TABLE pragmaLists (
    id INTEGER UNIQUE PRIMARY KEY NOT NULL,
    pragma INTEGER NOT NULL REFERENCES entries(id)
);

CREATE TABLE procedureSignatures (
    id INTEGER UNIQUE PRIMARY KEY NOT NULL,
    returnType INTEGER REFERENCES typeInstances(id),
    argList INTEGER REFERENCES argLists(id),
    pragmaList INTEGER REFERENCES pragmaLists(id)
);

CREATE TABLE indirectUses (
    id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    serialized TEXT
);

CREATE TABLE nested (
    parent INTEGER REFERENCES entries(id),
    sub INTEGER REFERENCES entries(id)
);
"""

suite "SQL schema visualization":
  var graph = makeDotGraph(dgpRecords)

  iterator items(sql: SqlNode): SqlNode =
    for i in 0 ..< sql.len:
      yield sql[i]

  for parsed in parseSql(sqlExample):
    parsed.assertMatch(CreateTable[
      (strVal: @tableName),
      all @columns
    ])

    let name = &"[[<b> {tableName} </b>]]"

    var record = makeTableDotNode(hash(tableName), name)

    let w = name.len

    for col in columns:
      var id = col[0].strVal.hash()
      record.add makeDotNode(
        id,
        RawHtml(
          strutils.alignLeft(
            col[0].strVal & ":",
            w - col[1].strVal.len - 1,
          ) & &"<i>{col[1].strVal}</i>"
        )
      )

      for sub in col:
        if sub.kind == nkReferences:
          graph.add makeDotEdge(
            (hash(tableName), id),
            (hash(sub[0][0].strVal), hash(sub[0][1].strVal))
          )

    graph.add record

  if hasCmd(shellCmd(dot)):
    graph.toPng(getAppTempFile("sqlExample.png"))


testFileEnded()