import hmisc/preludes/unittest
import hmisc/algo/tree/tree_selector

import std/[htmlparser, streams, parsesql, xmltree]

iterator items(node: SqlNode): SqlNode =
  for idx in 0 ..< len(node):
    yield node[idx]


suite "XML selectors":
  test "nth-child":
    let html = lit3"""
      <!DOCTYPE html>
      <html>
        <head><title>Example</title></head>
        <body>
          <p>1</p>
          <p>2</p>
          <p>3</p>
          <p>4</p>
        </body>
      </html>
      """

    let xml = parseHtml(newStringStream(html))

    var query = initQuery(@[
      initQueryPart(@[
        initElementDemand[XmlNode, string]("p"),
        initNthChildDemand[XmlNode, string](tkPseudoNthChild, 2, 1)
      ], cmLeaf)])

    let res = query.exec(xml,
      initQueryCtx(
        proc(node: XmlNode): XmlNode = <>"wrap"(xml),
        proc(node: XmlNode, elem: string): bool =
          node.kind == xnElement and node.tag == elem,
        proc(node1, node2: XmlNode): bool =
          node1.kind == xnElement and
          node2.kind == xnElement and
          node1.tag == node2.tag))

    echo res


suite "SQL selectors":
  let ctx = initQueryCtx(
    proc(node: SqlNode): SqlNode = newNode(nkStmtList, @[node]),
    proc(node: SqlNode, elem: SqlNodeKind): bool = node.kind == elem,
    proc(node1, node2: SqlNode): bool = node1.kind == node2.kind)

  test "nth-child":
    let sql = parseSql("SELECT * FROM table_name;")

    var query = initQuery(@[
      initQueryPart(@[
        initElementDemand[SqlNode, SqlNodeKind](nkSelect),
        initNthChildDemand[SqlNode, SqlNodeKind](tkPseudoNthChild, 2, 1)
      ], cmLeaf)])

    let res = query.exec(sql, ctx)
    echo res[0].treeRepr()

  test "Predicate selector":
    let sql = parseSql("SELECT * FROM table;")
    let res = ctx.predicate(it.kind == nkIdent).query().exec(sql, ctx)
    echo res

    echo sql.execWithCtx(ctx, ctx.predicate(it.kind == nkIdent))
