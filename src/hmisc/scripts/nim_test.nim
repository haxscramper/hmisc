import ../preludes/cli_app
import ../other/[hjson, hlogger]
import ../algo/[hlex_base, lexcast]
import std/[sequtils, streams, parsecfg, sets]

type
  NimFlag = enum
    nrfStrictFuncs = "strictFuncs"

  NimGc = enum
    ngcRefc = "refc"
    ngcArc = "arc"
    ngcOrc = "orc"

  NimBackend = enum
    nbC = "c"
    nbJs = "js"
    nbCpp = "cpp"
    nbObjc = "objc"

  NimRun* = object
    flags: set[NimFlag]
    gc: NimGc
    backend: NimBackend
    file: AbsFile
    outFile: AbsFile
    tmpDir: AbsDir

    spec: Option[NimTestSpec]

  NimHint* = enum
    nfSuccess                    = "Success"
    nfSuccessX                   = "SuccessX"
    nfCC                         = "CC"
    nfLineTooLong                = "LineTooLong"
    nfXDeclaredButNotUsed        = "XDeclaredButNotUsed"
    nfDuplicateModuleImport      = "DuplicateModuleImport"
    nfXCannotRaiseY              = "XCannotRaiseY"
    nfConvToBaseNotNeeded        = "ConvToBaseNotNeeded"
    nfConvFromXtoItselfNotNeeded = "ConvFromXtoItselfNotNeeded"
    nfExprAlwaysX                = "ExprAlwaysX"
    nfQuitCalled                 = "QuitCalled"
    nfProcessing                 = "Processing"
    nfProcessingStmt             = "ProcessingStmt"
    nfCodeBegin                  = "CodeBegin"
    nfCodeEnd                    = "CodeEnd"
    nfConf                       = "Conf"
    nfPath                       = "Path"
    nfCondTrue                   = "CondTrue"
    nfCondFalse                  = "CondFalse"
    nfName                       = "Name"
    nfPattern                    = "Pattern"
    nfExec                       = "Exec"
    nfLink                       = "Link"
    nfDependency                 = "Dependency"
    nfSource                     = "Source"
    nfPerformance                = "Performance"
    nfStackTrace                 = "StackTrace"
    nfGCStats                    = "GCStats"
    nfGlobalVar                  = "GlobalVar"
    nfExpandMacro                = "ExpandMacro"
    nfUser                       = "User"
    nfUserRaw                    = "UserRaw"
    nfExtendedContext            = "ExtendedContext"
    nfMsgOrigin                  = "MsgOrigin"
    nfDeclaredLoc                = "DeclaredLoc"

  NimWarning* = enum
    nwCannotOpenFile        = "CannotOpenFile"
    nwOctalEscape           = "OctalEscape"
    nwXIsNeverRead          = "XIsNeverRead"
    nwXmightNotBeenInit     = "XmightNotBeenInit"
    nwDeprecated            = "Deprecated"
    nwConfigDeprecated      = "ConfigDeprecated"
    nwDotLikeOps            = "DotLikeOps"
    nwSmallLshouldNotBeUsed = "SmallLshouldNotBeUsed"
    nwUnknownMagic          = "UnknownMagic"
    nwRedefinitionOfLabel   = "RedefinitionOfLabel"
    nwUnknownSubstitutionX  = "UnknownSubstitutionX"
    nwBrokenLink            = "BrokenLink"
    nwLanguageXNotSupported = "LanguageXNotSupported"
    nwFieldXNotSupported    = "FieldXNotSupported"
    nwWarnRstStyle          = "warnRstStyle"
    nwCommentXIgnored       = "CommentXIgnored"
    nwTypelessParam         = "TypelessParam"
    nwUseBase               = "UseBase"
    nwWriteToForeignHeap    = "WriteToForeignHeap"
    nwUnsafeCode            = "UnsafeCode"
    nwUnusedImport          = "UnusedImport"
    nwInheritFromException  = "InheritFromException"
    nwEachIdentIsTuple      = "EachIdentIsTuple"
    nwUnsafeSetLen          = "UnsafeSetLen"
    nwUnsafeDefault         = "UnsafeDefault"
    nwProveInit             = "ProveInit"
    nwProveField            = "ProveField"
    nwProveIndex            = "ProveIndex"
    nwUnreachableElse       = "UnreachableElse"
    nwUnreachableCode       = "UnreachableCode"
    nwIndexCheck            = "IndexCheck"
    nwGcUnsafe              = "GcUnsafe"
    nwGcUnsafe2             = "GcUnsafe2"
    nwUninit                = "Uninit"
    nwGcMem                 = "GcMem"
    nwDestructor            = "Destructor"
    nwLockLevel             = "LockLevel"
    nwResultShadowed        = "ResultShadowed"
    nwSpacing               = "Spacing"
    nwCaseTransition        = "CaseTransition"
    nwCycleCreated          = "CycleCreated"
    nwObservableStores      = "ObservableStores"
    nwStrictNotNil          = "StrictNotNil"
    nwResultUsed            = "ResultUsed"
    nwCannotOpen            = "CannotOpen"
    nwFileChanged           = "FileChanged"
    nwEnumConv              = "EnumConv"
    nwAnyEnumConv           = "AnyEnumConv"
    nwHoleEnumConv          = "HoleEnumConv"
    nwCStringConv           = "CStringConv"
    nwEffect                = "Effect"
    nwUser                  = "User"

  NimTestAction* = enum
    actionRun = "run"
    actionCompile = "compile"
    actionReject = "reject"

  NimOutputCheck* = enum
    ocIgnore = "ignore"
    ocEqual = "equal"
    ocSubstr = "substr"

  NimResultKind* = enum
    reNimcCrash,       # nim compiler seems to have crashed
    reMsgsDiffer,      # error messages differ
    reFilesDiffer,     # expected and given filenames differ
    reLinesDiffer,     # expected and given line numbers differ
    reOutputsDiffer,
    reExitcodesDiffer, # exit codes of program or of valgrind differ
    reTimeout,
    reInvalidPeg,
    reCodegenFailure,
    reCodeNotFound,
    reExeNotFound,
    reInstallFailed    # package installation failed
    reBuildFailed      # package building failed
    reDisabled,        # test is disabled
    reJoined,          # test is disabled because it was joined into the megatest
    reSuccess          # test was successful
    reInvalidSpec      # test had problems to parse the spec

  NimTestSpec* = object
    # xxx make sure `isJoinableSpec` takes into account each field here.
    action*: NimTestAction
    file*: AbsFile
    msg*: string
    exitCode*: int
    cmd*: string
    input*: string
    outputCheck*: NimOutputCheck
    sortoutput*: bool
    output*: string
    line*, column*: int
    ccodeCheck*: seq[string]
    maxCodeSize*: int
    err*: NimResultKind
    inCurrentBatch*: bool
    targets*: set[NimBackend]
    matrix*: seq[string]
    nimout*: string
    nimoutFull*: bool
    joinable*: bool
    unbatchable*: bool
    timeout*: float
    debugInfo*: string

  NimState = object
    paths: seq[AbsDir]
    hints: set[NimWarning]
    flags: seq[NimFlag]

  NimReportKind = enum
    nrHint
    nrWarning
    nrError

  NimReportPart = object
    file: string
    line: int
    column: int
    text: string

  NimReport = object
    parts: seq[NimReportPart]

    case kind*: NimReportKind
      of nrHint:
        hint: NimHint

      of nrWarning:
        warning: NimWarning

      of nrError:
        discard


proc parseSpec*(
    spec: string,
    file: AbsFile,
    skipFiles: seq[AbsFile] = @[]
  ): NimTestSpec =

  result.joinable = true
  result.file = file
  var ss = newStringStream(spec)
  var p: CfgParser
  open(p, ss, file.string, 1)
  var flags: HashSet[string]
  var nimoutFound = false
  while true:
    var e = next(p)
    case e.kind:
      of cfgKeyValuePair:
        let key = e.key.normalize
        const whiteListMulti = ["disabled", "ccodecheck"]
          ## list of flags that are correctly handled when passed multiple times
          ## (instead of being overwritten)
        if key notin whiteListMulti:
          doAssert key notin flags, $(key, file.string)

        flags.incl key
        case key:
          of "action":
            case e.value.normalize:
              of "compile": result.action = actionCompile
              of "run":     result.action = actionRun
              of "reject":  result.action = actionReject
              else:
                raise newParseError("cannot interpret as action: ", e.value)

          of "file":
            if result.msg.len == 0 and result.nimout.len == 0:
              raise newParseError("errormsg or msg needs to be specified before file")

            result.file = AbsFile(e.value)

          of "line":
            if result.msg.len == 0 and result.nimout.len == 0:
              raise newParseError("errormsg, msg or nimout needs to be specified before line")

            discard parseInt(e.value, result.line)

          of "column":
            if result.msg.len == 0 and result.nimout.len == 0:
              raise newParseError("errormsg or msg needs to be specified before column")

            discard parseInt(e.value, result.column)

          of "output":
            if result.outputCheck != ocSubstr:
              result.outputCheck = ocEqual

            result.output = e.value

          of "input":
            result.input = e.value

          of "outputsub":
            result.outputCheck = ocSubstr
            result.output = strip(e.value)

          of "sortoutput":
            result.sortoutput = lexcast[bool](e.value)

          of "exitcode":
            result.exitCode = lexcast[int](e.value) # parseInt(e.value, result.exitCode)
            result.action = actionRun

          of "errormsg":
            result.msg = e.value
            result.action = actionReject

          of "nimout":
            result.nimout = e.value
            nimoutFound = true

          of "nimoutfull":
            result.nimoutFull = lexcast[bool](e.value) #  parseCfgBool(e.value)

          of "batchable":
            result.unbatchable = not lexcast[bool](e.value)

          of "joinable":
            result.joinable = lexcast[bool](e.value)

          of "disabled":
            case e.value.normalize:
              of "y", "yes", "true", "1", "on": result.err = reDisabled
              of "n", "no", "false", "0", "off": discard
              of "win", "windows": (when defined(windows): result.err = reDisabled)
              of "linux":          (when defined(linux):   result.err = reDisabled)
              of "bsd":            (when defined(bsd):     result.err = reDisabled)
              of "osx", "macosx":  (when defined(osx):     result.err = reDisabled)
              of "unix":           (when defined(unix):    result.err = reDisabled)
              of "posix":          (when defined(posix):   result.err = reDisabled)
              of "travis":         (if exists($$TRAVIS):   result.err = reDisabled)
              of "appveyor":       (if exists($$APPVEYOR): result.err = reDisabled)
              of "azure":          (if exists($$TF_BUILD): result.err = reDisabled)
              of "32bit":          (if sizeof(int) == 4:   result.err = reDisabled)
              of "freebsd":        (when defined(freebsd): result.err = reDisabled)
              of "arm64":          (when defined(arm64):   result.err = reDisabled)
              of "i386":           (when defined(i386):    result.err = reDisabled)
              of "openbsd":        (when defined(openbsd): result.err = reDisabled)
              of "netbsd":         (when defined(netbsd):  result.err = reDisabled)
              else:
                raise newParseError("cannot interpret as a bool: ", e.value)

          of "cmd":
            discard
            # if e.value.startsWith("nim "):
            #   result.cmd = compilerPrefix & e.value[3..^1]

            # else:
            #   result.cmd = e.value

          of "ccodecheck":
            result.ccodeCheck.add e.value

          of "maxcodesize":
            discard parseInt(e.value, result.maxCodeSize)

          of "timeout":
            result.timeout = lexcast[float](e.value)

          of "targets", "target":
            for v in e.value.normalize.splitWhitespace:
              case v:
                of "c":          result.targets.incl(nbC)
                of "cpp", "c++": result.targets.incl(nbCpp)
                of "objc":       result.targets.incl(nbObjC)
                of "js":         result.targets.incl(nbJS)
                else:
                  raise newParseError("invalid target: '", v, "'")

          of "matrix":
            for v in e.value.split(';'):
              result.matrix.add(v.strip)

          else:
            raise newParseError("invalid key for test spec: ", e.key)

      of cfgSectionStart:
        raise newParseError("section ignored: ", e.section)

      of cfgOption:
        raise newParseError("command ignored: ", e.key & ": " & e.value)

      of cfgError:
        raise newParseError(e.msg)

      of cfgEof:
        break

  close(p)

  if result.file in skipFiles:
    result.err = reDisabled

  if nimoutFound and result.nimout.len == 0 and not result.nimoutFull:
    raise newParseError(
      "Empty content is not allowed to for `nimout` - use `nimoutFull:true` if intentional")

  # result.inCurrentBatch = isCurrentBatch(testamentData0, filename) or result.unbatchable
  # if not result.inCurrentBatch:
  #   result.err = reDisabled

proc getCwdNimDump*(): NimState =
  let j = shellCmd(nim, dump, "dump.format" = "json", "-").
    evalShellStdout().
    parseJson()

  for path in j["lib_paths"]:
    result.paths.add AbsDir(path.asStr())

proc makeCmd(conf: NimRun, dump: NimState): ShellCmd =
  var cmd = shellCmd(nim)

  cmd.arg "compile"
  for flag in conf.flags:
    cmd.flag $flag

  with cmd:
    opt("gc", $conf.gc)
    opt("backend", $conf.backend)
    opt("out", $conf.outfile)
    opt("unitsep", "on")

    opt("verbosity", "0")

    flag("skipUserCfg")
    flag("skipParentCfg")
    flag("skipProjCfg")

    opt("d", "hmiscUnittestOut=json")

  cmd.opt("nimcache", $getNewTempDir(conf.file.name(), getAppTempDir()))

  for p in dump.paths:
    cmd.opt("path", p)

  cmd.arg conf.file


  return cmd


func hshow(run: NimRun, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  with result:
    add hshow(run.gc)
    add ", "
    add hshow(run.backend)
    add " "
    add hshow(run.flags)

func `$`*(run: NimRun): string = $hshow(run)

proc extractSpec(s: string, filename: AbsFile): string =
  const
    tripleQuote = "\"\"\""
    specStart = "discard " & tripleQuote

  var i = 0
  var a = -1
  var b = -1
  var line = 1
  var col = 1

  while i < s.len:
    if (i == 0 or s[i-1] != ' ') and s.continuesWith(specStart, i):
      # `s[i-1] == '\n'` would not work because of `tests/stdlib/tbase64.nim` which contains BOM (https://en.wikipedia.org/wiki/Byte_order_mark)
      const lineMax = 10
      if a != -1:
        raise newException(
          ValueError, "testament spec violation: duplicate `specStart` found: " & $(filename, a, b, line))
      elif line > lineMax:
        # not overly restrictive, but prevents mistaking some `specStart` as spec if deeep inside a test file
        raise newException(
          ValueError,
          "testament spec violation: `specStart` should be before line $1, or be indented; info: $2" % [$lineMax, $(filename, a, b, line)])
      i += specStart.len
      a = i

    elif a > -1 and b == -1 and s.continuesWith(tripleQuote, i):
      b = i
      i += tripleQuote.len
    elif s[i] == '\n':
      inc line
      inc i
      col = 1

    else:
      inc col
      inc i

  if a >= 0 and b > a:
    result = s.substr(a, b-1).multiReplace({"'''": tripleQuote, "\\31": "\31"})
  elif a >= 0:
    raise newException(
      ValueError,
      "testament spec violation: `specStart` found but not trailing `tripleQuote`: $1" % $(filename, a, b, line))
  else:
    result = ""

proc runsFromDir*(dir: AbsDir, first: seq[AbsFile] = @[]): seq[NimRun] =
  for file in first & toSeq(walkDir(dir, AbsFile, exts = @["nim"])):
    result.add NimRun(
      file: file,
      outfile: file.withExt("out")
    )

    let text = file.readFile()
    if text.startsWith("discard"):
      result[^1].spec = some text.extractSpec(file).parseSpec(file)

proc getJoinable*(runs: seq[NimRun]): tuple[joinable, standalone: seq[NimRun]] =
  for run in runs:
    if (?run.spec and run.spec.get().joinable) or not(?run.spec):
      result.joinable.add run

    else:
      result.standalone.add run

proc makeJoinedCode*(runs: seq[NimRun]): string =
  for run in runs:
    result.add &"import \"{run.file}\"\n"

proc parseReport(report: string): NimReport =
  var str = initPosStr(report)
  while ?str:
    var part: NimReportPart
    str.skipWhile({'\n'})
    part.file = str.asSlice(str.skipTo('(')).strVal()
    str.skip('(')

    part.line = str.asSlice(str.skipWhile(Digits)).strVal().parseInt()
    str.skip(", ")

    part.column = str.asSlice(str.skipWhile(Digits)).strVal().parseInt()
    str.skip(')')

    str.space()

    if str["Hint:"] or str["Warning:"] or str["Error:"]:
      var kind = str.asSlice(str.skipWhile(IdentChars)).strVal()
      str.skip(':')
      str.skip(' ')

      case kind:
        of "Hint":
          part.text = str.asSlice(str.skipToEol()).strVal()
          result.kind.setKind(nrHint)

        of "Warning":
          part.text = str.asSlice(str.skipToEol()).strVal()
          result.kind.setKind(nrWarning)

        of "Error":
          part.text = str.asSlice((str.goToEof(); str.next())).strVal()
          result.kind.setKind(nrError)

        else:
          raise newUnexpectedKindError(kind)

    else:
      part.text = str.asSlice(str.skipToEol()).strVal()

    result.parts.add part





proc getReports(res: ShellResult, run: NimRun): seq[NimReport] =
  for part in res.getStderr().split("\31"):
    if part.strip().len() > 0:
      result.add parseReport(part)


proc formatShellCmd*(cmd: ShellCmd): ColoredText =
  result = clt(cmd.bin)
  for arg in cmd:
    result.add "  \\\n  "
    result.add arg.toStr(cmd.conf, true)

proc reportError*(l: HLogger, report: NimReport) =
  for part in report.parts:
    l.err part.file, part.line, part.column
    l.err part.text
    l.logLines(AbsFile(part.file), part.line, "nim", part.column)


proc runTestDir*(
    dir: AbsDir,
    dump: NimState,
    maxfail: int = high(int),
    first: seq[AbsFile] = @[],
    l: HLogger = newTermLogger()
  ) =

  let runs = runsFromDir(dir, first)
  let (joined, standalone) = runs.getJoinable()

  block joined_items:
    let
      tmp = getAppTempDir()
      res = tmp /. "joined.nim"

    let code = makeJoinedCode(joined)
    res.writeFile(code)

    let
      run = NimRun(file: res, outfile: res.withExt("out"))
      cmd = makeCmd(run, dump)

    l.info "Running joined tests"
    let
      runRes = shellResult(cmd)
      reports = getReports(runRes, run)


    if reports.anyIt(it of nrError):
      for report in reports:
        if report of nrError:
          l.reportError(report)

      raise newImplementError()

    else:
      l.success "No errors detected"


  block split_items:
    l.info "Running separate files"
    var failCount = maxfail
    for (res, run) in runShellResult(
      standalone.mapIt((makeCmd(it, dump), it)), maxPool = 2):

      let reports = getReports(res, run)
      var hasError = false
      for report in reports:
        if report of nrError:
          l.reportError(report)
          inc failCount

          if maxFail < failCount:
            break

          hasError = true

      if not hasError:
        l.success run.file

export oswrap, all

when isMainModule:
  startHax()
  runTestDir(
    AbsDir(relToSource"../../../tests"),
    getCwdNimDump(),
    1,
    @[AbsFile("/mnt/workspace/github/hmisc/tests/tMacros.nim")])