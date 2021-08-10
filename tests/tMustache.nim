import
  hmisc/hasts/mustache_template,
  hmisc/types/[ptree, colorstring],
  hmisc/preludes/unittest

import std/[streams, db_sqlite]

startHax()

suite "Simple template instantiation":
  test "Multiline interpolate":
    let tree = mustacheParse("""
<h2>Names</h2>
{{#names}}
  <strong>{{name}}</strong>
{{/names}}
""")

    echo treeRepr(tree)

    var ctx = newPTree({
      "names": newPTree([
        newPTree({"name": "test-name"}),
        newPTree({"name": "test-name"}),
        newPTree({"name": "test-name"})
      ])
    })

    let s = newFileStream(stdout)
    s.writeTemplate(tree, ctx)

suite "Data providers":
  test "SQlite provider":
    type EKind = enum ekModule, ekProc, ekType

    var db = open(":memory:", "", "", "")

    db.exec(sql"""
CREATE TABLE entries (
  id INT PRIMARY KEY UNIQUE,
  name TEXT,
  kind INT,
  category STRING
)""")

    db.exec(sql("CREATE TABLE nested (id INT, parent INT)"))

    for (id, name, kind, category) in [
      (1, "io", ekModule, "#top"),
    ]:
      db.exec(sql("""
INSERT INTO entries
(id, name, kind, category)
VALUES
(?, ?, ?, ?)
"""), id, name, kind.int, category)

    for x in db.fastRows(sql"""
SELECT name
FROM sqlite_master
WHERE type ='table' AND name NOT LIKE 'sqlite_%';
"""):
      echo x

    const base = """
{{#kind.ekModule}}
  <h2>{{name}}</h2>
  <table>
    <tr><th>Category</th><th>Functions</th></tr>
    {{#category}}
       <tr><th>{{name}}</th><th>{{kind}}</th></tr>
    {{/category}}
  </table>
{{/kind.ekModule}}
"""


    let tree = mustacheParse(base)
    echo treeRepr(tree)

    var ptree = newPTree({
      "kind": newPTree({
        "ekModule": newPTree([
          newPTree({
            "name": newPTree[string, string]("test"),
            "category": newPTree([newPTree({
              "name": "Category-value",
              "kind": "Category-name"
            })])})])})})

    let s = newFileStream(stdout)
    s.writeTemplate(tree, ptree)

    db.close()
