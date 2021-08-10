import
  hmisc/hasts/json_ast,
  hmisc/preludes//unittest

suite "Generate JSON":
  test "1":
    var w = stdout.newJsonWriter()
    w.writeJson(123)

suite "Lex JSON":
  test "Lex json simple":
    let s = "{\"test\": 123}"
    echo s, " ", jsonTokens(s)

suite "Load json":
  test "Load integer":
    var r = newJsonReader("1")
    var num: int
    r.loadJson(num)
