#!/usr/bin/env nim

import strformat, strutils, sugar, sequtils, xmltree, macros
import hshell, oswrap
import colorlogger

func format*(str: string, kvalues: openarray[(string, string)]): string =
  str % kvalues.mapIt(@[it[0], it[1]]).concat()

export AbsDir, RelDir, AbsFile, RelFile, ShellExpr, ShellVar
export get, set, del, exists

import ../algo/htemplates

## Helper utilities for running nimble tasks

type
  TaskRunConfig* = object
    hrefPref*: string
    outDir*: AbsDir
    nimdocCss*: AbsFile
    testRun*: bool

    logFile*: AbsFile

    projectDir*: AbsDir

    packageName*: string
    version*: string
    author*: string
    description*: string
    license*: string
    srcDir*: string
    binDir*: string
    cmdOptions*: seq[ShellCmdPart]

func switch*(conf: var TaskRunConfig, val: string) =
  conf.cmdOptions.add initCmdFlag(val)

func switch*(conf: var TaskRunConfig, key, val: string) =
  conf.cmdOptions.add initCmdOption(key, val)

proc envOrParm*(
  conf: var TaskRunConfig, key: string, env: ShellVar,
  allowEmpty: bool = false) =
  if env.exists and (env.get.len > 0 or allowEmpty):
    conf.cmdOptions.add initCmdOption(key, env.get)
  else:
    conf.cmdOptions.add initCmdOption(key, paramVal(key)[0])

proc envOrParam*(
  conf: var TaskRunConfig, key: string, interpol: ShellExpr,
  allowEmpty: bool = false) =
  let interp = interpolateShell(interpol)
  if interp.isSome():
    conf.cmdOptions.add initCmdOption(key, interp.get())
  else:
    conf.cmdOptions.add initCmdOption(key, paramVal(key)[0])

func makeBodyToc*(linkList: string): string =
  """
  <div class="row">
    <div class="three columns">
    <div class="theme-switch-wrapper">
      <label class="theme-switch" for="checkbox">
        <input type="checkbox" id="checkbox" />
        <div class="slider round"></div>
      </label>
     &nbsp;&nbsp;&nbsp; <em>Dark Mode</em>
    </div>
    <div id="global-links">
  """ &
    linklist &
  """
    </div>
    <div id="searchInputDiv">
      Search: <input type="text" id="searchInput"
        onkeyup="search()" />
    </div>
    $tableofcontents
    </div>
    <div class="nine columns" id="content">
    <div id="tocRoot"></div>
    $deprecationMsg
    <p class="module-desc">$moduledesc</p>
    $content
    </div>
  </div>
  """

func wrap3QuoteNL*(str: string): string =
  "\"\"\"" & str & "\"\"\"\n\n"

func newTree*(tag: string,
             subitems: openarray[XmlNode],
             attrs: openarray[(string, string)] = @[]): XmlNode =

  result = newElement(tag)
  for it in subitems:
    result.add it

  # for attr in attrs:
  result.attrs = attrs.toXmlAttributes()



func newTree*(subitems: seq[XmlNode], tag: string): XmlNode =
  newTree(tag, subitems)

func toHtmlList*(tree: FsTree,
                 hrefPref: string,
                 dropnref: int = 0,
                 dropnames: seq[string] = @[]): XmlNode =
  if tree.isDir:
    if tree.basename notin dropnames:
      let sub = tree.sub.mapIt(it.toHtmlList(
        hrefPref = hrefPref,
        dropnref = dropnref,
        dropnames = dropnames
      )).filterIt(it != nil)
      if sub.len > 0:
        return newTree("li", @[newText(tree.basename), newTree("ol", sub)])
  else:
    let url = hrefPref & "/" &
      tree.parent[min(tree.parent.len, dropnref) .. ^1].join("/") & "/" &
        &"{tree.basename}.html"

    return newTree(
      "li",
      @[ newTree("a", @[newText(tree.basename)], {"href" : url}) ])


proc makeDocConf*(linklist: string): string =
  &"""
--hints:off
--verbosity:0
doc.body_toc = {makeBodyToc(linklist).wrap3QuoteNL()}
doc.body_toc_group = {makeBodyToc(linkList).wrap3QuoteNL()}
"""


proc docgenBuild(conf: TaskRunConfig) =
  let curr = conf.projectDir.toFsTree()
  var rstfiles: seq[FsTree]
  var errMsg: seq[string]


  let
    files = buildFsTree(allowExts = @["nim", "rst"])
    docConf = cwd() /. "nimdoc.cfg"
    tree = $files.mapIt(it.toHtmlList(
      dropnref = curr.pathLen(),
      dropnames = @["tests"],
      hrefPref = conf.hrefPref
    )).filterIt(it != nil).newTree("ol")

  if not conf.testRun:
    docConf.writeFile makeDocConf(tree)

  # debug makeDocConf(tree)

  notice "Wrote nimdoc configuration to", docConf

  for file in files.mapIt(it.flatFiles()).concat():
    let dir = conf.outdir / file.parent[curr.pathLen() .. ^1]
    if conf.testRun:
      logcall mkDir(dir), true
    else:
      mkDir(dir)

    let outfile = dir /. $file.withoutParent().withExt("html")
    if (file.ext in @["nim", "rst"]) and ("tests" notin $file):
      var cmd: ShellCmd
      case file.ext:
        of "rst":
          cmd = makeNimShellCmd("nim").withIt do:
            it.cmd "rst2html"
            it - ("o", outfile)
            it.arg file

        of "nim":
          cmd = makeNimShellCmd("nim").withIt do:
            it.cmd "doc"
            # it - ("cc", "tcc")
            it - ("o", outfile)
            it - ("hints", "off")
            it.arg file

      if not cmd.isEmpty():
        if conf.testRun:
          notice cmd.toLogStr()
        else:
          let res = shellResult(cmd)
          if res.resultOk:
            info $file & "\n" & $outfile
          #   debug "ok:", cmd.toLogStr()
          #   notice cmd.toLogStr()
          else:
            warn file
            debug res.exception.outstr
            errMsg.add @["-".repeat(80)].repeat(3).concat()
            errMsg.add cmd.toLogStr()
            errMsg.add res.exception.outstr

  if (errMsg.len > 0) and (conf.logFile.len > 0):
    warn "Errors during documentation compilation"
    conf.logFile.writeFile(errMsg.join("\n"))
    notice &"Log file saved to {conf.logFile}"
  else:
    notice "Documentation buid ok, no errors detected"
    info &"Saved documentation at path {conf.outdir}"
    # logcall cpFile(conf.nimdocCss, dir /. "nimdoc.out.css"), conf.testRun

  # rmFile docConf

template initBuildConf*(): TaskRunConfig {.dirty.} =
  block:
    var tmp = TaskRunConfig(
      packageName: packageName,
      version: version,
      author: author,
      description: description,
      license: license,
      srcDir: srcDir,
      binDir: binDir,
      projectDir: AbsDir thisDir(),
    )

    tmp.hrefPref = ("https://" & author & ".github.io/" & packageName)
    tmp

proc runDocGen*(conf: TaskRunConfig): void =
  logIdented:
    docgenBuild(conf)



func makeSeparator*(msg: string, col: string): string =
  "\e[" & col & "m" & (
    "  " & msg.alignLeft(46, ' ') & "  ").center(80, '~') & "\e[39m"

proc thisAbsDir*(): AbsDir =
  when compiles(thisDir()):
    AbsDir thisDir()
  else:
    cwd()

proc runDockerTest*(
  projDir, tmpDir: AbsDir, cmd: string,
  runCb: proc() = (proc() = discard)): void =
  ## Copy project directory `projDir` into temporary `tmpDir` and
  ## execute command `cmd` inside new docker container based on
  ## `nim-base` image.
  mkDir tmpDir

  let mainDir = (tmpDir / "main")
  if mainDir.dirExists:
    rmDir (tmpDir / "main")

  cpDir projDir, (tmpDir / "main")

  echo "copied ", projDir, " to ", mainDir

  withDir tmpDir / "main":
    runCb()

  let dockerCmd = makeGnuShellCmd("docker").withIt do:
    it.cmd "run"
    it - "i"
    it - "t"
    it - "rm"
    it - ("v", "", $tmpDir & ":/project")
    it.arg "nim-base"
    it.arg "sh"
    it - "c"
    it.raw &"'cd /project/main && {cmd}'"

  # echo dockerCmd.toStr()

  execShell(dockerCmd)


func `&&`*(lhs, rhs: string): string =
  if lhs.len == 0:
    rhs
  elif rhs.len == 0:
    lhs
  else:
    # TODO check for `||` ending to prevent incorrect string construction
    if lhs.strip().endsWith("&&") or rhs.strip.startsWith("&&"):
      # if rhs.strip().startsWith("&&"): # IMPLEMENT
      lhs & " " & rhs
    else:
      lhs & " && " & rhs

proc pkgVersion*(pkg: string): string =
  let (stdout, stderr, code) = runShell(ShellExpr "nimble dump " & pkg)
  for line in stdout.split("\n"):
    if line.startsWith("version: "):
      return line["version: \"".len() .. ^2]


proc makeLocalDevel*(testDir: AbsDir, pkgs: seq[string]): string =
  info "Copying local development versions"
  let home = getHomeDir()
  let dirs = collect(newSeq):
    for pkg in pkgs:
      let dir = home / ".nimble/pkgs" / (pkg & "-#head")

      assert dirExists(dir),
          &"Could not find {dir} - run " &
            "`nimble develop` to make it available"

      dir

  mkDir testDir
  for pkg in pkgs:
    let meta = home / &".nimble/pkgs/{pkg}-#head" /
      RelFile(pkg & ".nimble-link")

    for nimble in meta.readFile().split("\n"):
      let nimble = AbsDir nimble
      if nimble.endsWith(&"{pkg}.nimble"): # XXX
        let dir = parentDir(nimble)
        cpDir dir, (testDir / pkg)

  for pkg in pkgs:
    result = result && &"cd /project/{pkg}" && "nimble develop"

proc writeTestConfig*(str: string): void =
  "tests/nim.cfg".writeFile(str.unindent())

proc testAllImpl*(): void =
  try:
    execShell(ShellExpr "choosenim stable")
    execShell(ShellExpr "nimble test")
    info "Stable test passed"
  except:
    err "Stable test failed"

  try:
    execShell(ShellExpr "choosenim devel")
    execShell(ShellExpr "nimble test")
    info "Devel test passed"
  except:
    err "Devel test failed"
  finally:
    execShell(ShellExpr "choosenim stable")

  try:
    execShell(ShellExpr "nimble install")
    info "Installation on stable OK"
  except:
    err "Installation on stable failed"

proc runDockerTestDevel*(
  startDir, testDir: AbsDir, localDevel: seq[string],
  cmd: string, cb: proc()) =
  let develCmd = makeLocalDevel(testDir, localDevel)
  let cmd = develCmd && ("cd " & "/project/main") && cmd

  info "executing docker container"
  debug "command is"
  echo cmd

  runDockerTest(thisAbsDir(), testDir, cmd) do:
    cb()
