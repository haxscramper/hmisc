import
  std/[sugar, strutils, sequtils, strformat]

import hmisc/preludes/unittest

testFileStarted()


import
  hmisc/other/[hshell, oswrap, pathwrap]

startHax()


suite "Pathwrap":
  test "test":
    check AbsDir("/a/b/c") /../ 2 /../ RelDir("hello") == AbsDir("/hello")

  test "A":
    show getNewTempDir()

    for path in toAbsDir("/tmp").walkDir():
      case path.kind:
        of pcDir:
          show path
        else:
          discard

  test "dirs":
    for dir in parentDirs(cwd()):
      show dir

  test "Os errros":
    try:
      discard newPathError(toAbsDir("/tmp"), pekExpectedRel):
        "Expected relative directory"

      let path = "12"
      raise newPathError(AbsFile("12"), pekExpectedAbs):
        &"Input path {path} has type {$typeof(path)}, but contains " &
        &"invalid string - expected absolute path"

    except:
      discard

  test "Extension parts":
    check RelFile("hello").withExt("nim") == RelFile("hello.nim")
    check AbsFile("/tmp/test.nim").withoutPrefix(
      AbsDir("/tmp")) == RelFile("test.nim")

  test "Relative directories":
    check AbsDir("/a/b/c/d").relativePath(AbsDir("/a/b/c")) == RelDir("d")

  test "Relative depth":
    check:
      relativeUpCount(AbsFile("/a/b.txt"), AbsDir("/a")) == 0
      relativeUpCount(AbsFile("/a/b/b.txt"), AbsDir("/a")) == 1
      relativeUpCount(AbsFile("/a.txt"), AbsFile("/b.txt")) == 0
      relativeUpCount(AbsFile("/tmp/a.txt"), AbsFile("/tmp/b.txt")) == 0

    expect PathError:
      check relativeUpCount(AbsDir("/a/b"), AbsFile("/b.txt")) == -1

  test "Import split":
    let base = AbsDir("/tmp")
    template rs(p1, p2: string): untyped =
      importSplit(base / RelFile(p1), base / RelFile(p2))

    check:
      rs("tmp/a.nim", "tmp/b.nim") == (0, @["b.nim"])
      rs("tmp/a.nim", "b.nim") == (1, @["b.nim"])
      rs("a.nim", "tmp/b.nim") == (0, @["tmp", "b.nim"])
      rs("a.nim", "b.nim") == (0, @["b.nim"])
      rs("a", "b") == (0, @["b"])
      rs("a", "a") == (0, @["a"])
    # let (d, p) = relativeSplit(
    #   base / RelFile("tmp/a.nim"),
    #   base / RelFile("tmp/b.nim"))

    # check d == 0
    # check p == @["b.nim"]

  test "Mkdir structure":
    let name = "hello"
    proc generateText(): string = "input text test"
    withTempDir(false):
      mkDirStructure:
        file "hello", "content"
        file "test-1", generateText()
        file &"{name}.nimble":
          "author = haxscramper\n"
          "second line"

        file "loop":
          for i in 0 ..< 10: i

        dir "src":
          file &"{name}.nim"
          dir &"{name}":
            file "make_wrap.nim"
            file "make_build.nim"

        dir "tests":
          file "multiline-test":
            """
Multiline string as file content
Not that is looks particulatly pretty though
"""

          file "config.nims":
            """switch("path", "$projectDir/../src")"""

      check:
        readFile(&"{name}.nimble") == "author = haxscramper\nsecond line"
        readFile("loop") == "0123456789"


suite "Shell":
  test "shell":
    expect ShellError:
      discard runShell(ShellExpr "hello")

  test "Options":
    var cmd = makeGnuShellCmd("cat")
    # cmd["hello"] = "world"
    # cmd["nice"]

  test "OS":
    show getCurrentOs()
    const os = getCurrentOs()
    show os

    # when not isPackageInstalled("hunspell"):
    #   show "Install hunspell using ", getInstallCmd("hunspell")

    # static:
    #   let missing = getMissingDependencies({
    #     { Distribution.ArchLinux } : @["hunspell-12"]
    #   })

    #   for (pkg, cmd) in missing:
    #     show "Missing ", pkg, ", install it using ", cmd


suite "User directories":
  test "xdg":
    show:
      getUserConfigDir()
      getAppConfigDir()

      getUserCacheDir()
      getAppCacheDir()

      getUserDataDir()
      getAppDataDir()

      getUserRuntimeDir()
      getAppRuntimeDir()

suite "Env wrap":
  test "get/set/exists":
    let env = $$TEST_ENV1

    doAssert not exists(env)
    set(env, "hello")
    doAssert get(env) == "hello"
    del(env)

  test "Getting typed vars":
    let env = $$TEST_ENV2
    env.put(false)
    doAssert not env.get(bool)

    env.put(100)
    doAssert env.get(int) == 100

    del(env)

  test "Equality comparison for variables":
    put($$CI, "true")
    discard $$CI == true
    discard $$CI == 1
    discard $$IC == "1"

    static:
      put($$III, true)
      if $$III == 1:
        discard


  test "Assign variables":
    static:
      put($$ZZZ, false)

      doAssert $$ZZZ == false
      doAssert get($$ZZZ, string) == "false"


    when $$ZZZ == true:
      fail()

    else:
      discard

    put($$CI, true)
    if $$CI == true:
      discard

    else:
      fail()


testFileEnded()
