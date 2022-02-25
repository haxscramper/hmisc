import std/[db_sqlite, sqlite3, options, sequtils,
            strutils, sugar, strformat]

import ./oswrap

proc sqliteOpen*(file: AnyFile): DbConn = open(file.getStr(), "", "", "")
proc sqliteOpenNew*(file: AnyFile): DbConn =
  if exists(file):
    rmFile file

  return sqliteOpen(file)

proc toSqlite*(t: typedesc[int]): string = "integer"
proc toSqlite*(t: typedesc[bool]): string = "integer"

proc toSqlite*(t: typedesc[string]): string = "text"
proc toSqlite*(t: typedesc[enum]): string = "integer"

proc toSqlite*[T](t: typedesc[Option[T]]): string =
  toSqlite(typeof Option[T]().get())

template sq*(expr: untyped): untyped =
  toSqlite(typeof expr)

const
  sqPrimaryKey* = " primary key unique not null"

proc sqlite3_expanded_sql(sqlite3_stmt: PStmt): cstring {.
  importc, dynlib: "libsqlite3.so(|.0)", cdecl.}


proc `$`*(pstmt: SqlPrepared): string =
  $sqlite3_expanded_sql(pstmt.PStmt)

proc connError*(conn: DbConn): string =
  &"connection error: {errmsg(conn)}, code: {errcode(conn)}"

proc bindParam*[T](ps: SqlPrepared, idx: int, opt: Option[T]) =
  if opt.isSome():
    bindParam(ps, idx, opt.get())

  else:
    bindNull(ps, idx)

proc newInsert*(table: string, columns: openarray[(string, int)]): string =
  var r = "insert into " & table & " (\n  "
  r.add columns.mapIt(it[0]).join(", ")
  r.add "\n ) values (\n"
  for idx, (name, val) in columns:
   r.add "  ?" & $val
   if idx < columns.high:
     r.add ","

   else:
     r.add " "

   r.add " -- " & name & "\n"

  r.add ");"
  return r

proc newTable*(
    table: string, columns: openarray[(string, string)]): SqlQuery =

  var r: string

  r.add "create table "
  r.add table
  r.add "(\n"
  for idx, (name, format) in columns:
    if idx > 0: r.add ",\n"
    r.add "  "
    r.add name
    r.add " "
    r.add format

  r.add "\n);"

  return sql(r)

proc newTableWithInsert*(
    db: DbConn,
    table: string,
    columns: openArray[((string, int), string)],
    extra: string = ""
  ): string =

  var cols: seq[(string, string)]
  var insert: seq[(string, int)]
  for (key, val) in columns:
    cols.add((key[0], val))
    insert.add(key)

  db.exec sql(&"drop table if exists {table}")

  db.exec newTable(table, cols, extra)
  result = newInsert(table, insert)

proc beginTransaction*(db: DbConn) = db.exec(sql"begin transaction")
proc endTransaction*(db: DbConn) = db.exec(sql"end transaction")
proc disableSync*(db: DbConn) = db.exec(sql"PRAGMA synchronous = OFF")
proc journalMemory*(db: DbConn) = db.exec(sql"PRAGMA journal_mode = MEMORY")



proc bindParam*[E: enum](ps: SqlPrepared, idx: int, opt: E) =
  bindParam(ps, idx, opt.int)

proc reset*(p: SqlPrepared) =
  discard reset(p.PStmt)

proc doExec*(sq: DbConn, prep: SqlPrepared) =
  sq.exec(prep)
  reset(prep)

export db_sqlite


template withPrepared(conn: DbConn, prepCode: SqlPrepared, body: untyped): untyped =
  block:
    var prep {.inject.} = prepCode
    body
    finalize(prep)

template withPrepared(
    conn: DbConn, prepCode: string,
    prepName: untyped,
    body: untyped
  ): untyped =

  block:
    var prepName {.inject.} = conn.prepare(prepCode)
    body
    finalize(prepName)

template withPrepared(conn: DbConn, prepCode: string, body: untyped): untyped =
  withPrepared(conn, prepCode, prep, body)

proc writeTable*[K, V](conn: DbConn, table: Table[K, V], tabname, keyname, valname: string) =
  withPrepared(conn, conn.newTableWithInsert(tabname, {
    (keyname, 1): sq(K),
    (valname, 2): sq(V)
  })):
    for key, val in pairs(table):
      prep.bindParam(1, key)
      prep.bindParam(2, val)
      conn.doExec(prep)

proc readTable*[K, V, T](
    conn: DbConn, table: var Table[K, V], tabname: string, rows: typedesc[T]
  ) =

  for (key, val) in conn.typedRows(tabname, T):
    table[key] = val


proc writeTable*[K, V](
    conn: DbConn, table: Table[K, seq[V]],
    tabname, keyname, valname: string
  ) =

  withPrepared(conn, conn.newTableWithInsert(tabname, {
    (keyname, 1): sq(K),
    (valname, 2): sq(V)
  })):
    for key, val in pairs(table):
      for item in val:
        prep.bindParam(1, key)
        prep.bindParam(2, val)
        conn.doExec(prep)

proc readTable*[K, V, T](
    conn: DbConn, table: var Table[K, seq[V]], tabname: string, rows: typedesc[T]
  ) =
  for (key, val) in conn.typedRows(tabname, T):
    table.mgetOrPut(key).add val
