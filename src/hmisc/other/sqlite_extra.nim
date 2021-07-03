import std/[db_sqlite, sqlite3, options, sequtils, strutils, sugar]

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
