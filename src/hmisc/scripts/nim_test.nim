import ../preludes/cli_app
import ../other/[hjson, hlogger]
import ../algo/[hlex_base, lexcast]
import std/[sequtils, streams, parsecfg, sets]
import pkg/jsony

export hlogger


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

  NimError* = enum
    neNone
    neException
    neCannotOpen

    neOverloadFail
    neInvalidIndentation
    neAmbiguousCall
    neLdFail
    neGccFail

  NimTestAction* = enum
    actionRun = "run"
    actionCompile = "compile"
    actionReject = "reject"

  NimOutputCheck* = enum
    ocIgnore = "ignore"
    ocEqual = "equal"
    ocSubstr = "substr"

  NimResultKind* = enum
    reNimcCrash        ## nim compiler seems to have crashed
    reMsgsDiffer       ## error messages differ
    reFilesDiffer      ## expected and given filenames differ
    reLinesDiffer      ## expected and given line numbers differ
    reOutputsDiffer
    reExitcodesDiffer  ## exit codes of program or of valgrind differ
    reTimeout
    reInvalidPeg
    reCodegenFailure
    reCodeNotFound
    reExeNotFound
    reInstallFailed    ## package installation failed
    reBuildFailed      ## package building failed
    reDisabled         ## test is disabled
    reJoined           ## test is disabled because it was joined into the megatest
    reSuccess          ## test was successful
    reInvalidSpec      ## test had problems to parse the spec

  NimTestSpec* = object
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

  NimState* = object
    paths*: seq[AbsDir]
    hints*: set[NimWarning]
    flags*: seq[NimFlag]
    nimblePath*: AbsDir

  NimReportKind* = enum
    nrHint
    nrWarning
    nrError

  NimReportPartKind* = enum
    nrpNone
    nrpInstOf
    nrpException
    nrpTracePart

    nrpProcessingImports
    nrpLdFail

  NimReportPart* = object
    file*: string
    line*: int
    column*: int
    text*: string
    case kind*: NimReportPartKind
      of nrpProcessingImports:
        imports*: string
        target*: AbsFile
        level*: int
        isInclude*: bool
        isToplevel*: bool

      else:
        discard



  GccPoint* = object
    byteColumn*: int
    displayColumn*: int
    line*: int
    file*: string
    column*: int

  GccReportChild = object
    locations*: seq[GccLocation]
    message*: string
    kind*: string

  GccLocation* = object
    caret*: GccPoint
    finish*: Option[GccPoint]

  GccReport* = object
    kind*: string
    columnOrigin*: int
    children*: seq[GccReportChild]
    locations*: seq[GccLocation]
    message*: string
    options*: seq[string]

  NimOverloadAlt* = object
    signature*: string
    argumentFail*: string
    isOfType*: Option[string]

  NimFixit* = object
    file*: Option[AbsFile]
    line*: Option[int]
    column*: Option[int]
    message*: string

  NimReport* = object
    parts*: seq[NimReportPart]
    fixit*: seq[NimFixit]

    case kind*: NimReportKind
      of nrHint:
        hint*: NimHint

      of nrWarning:
        warning*: NimWarning

      of nrError:
        case error*: NimError
          of neAmbiguousCall:
            definedAlts*: seq[tuple[
              kind: string,
              name: string,
              definedIn: AbsFile,
              line: int,
              column: int
            ]]

            matchFor*: string

          of neLdFail:
            ldReport*: tuple[
              message: string,
              compile: seq[string]
            ]

          of neOverloadFail:
            overloadContext*: tuple[
              gotType: string,
              alts: seq[NimOverloadAlt],
              expression: string
            ]

          of neGccFail:
            gccReport*: tuple[
              diags: seq[seq[GccReport]],
              compile: seq[string]
            ]

          else:
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


proc getCwdNimDump*(file: string = "-"): NimState =
  let j = shellCmd(nim, dump, "dump.format" = "json", $file).
    evalShellStdout().
    parseJson()

  for path in j["lib_paths"]:
    result.paths.add AbsDir(path.asStr())

  let code = "import std/[compilesettings, json]; " &
    "echo %*{\"nimble\": %querySettingSeq(nimblePaths)}"

  let query = shellCmd(nim, "eval" = $code).
    evalShellStdout().
    parseJson()

  result.nimblePath = query["nimble"][0].asStr().AbsDir()


proc makeCmd(
    conf: NimRun,
    dump: NimState,
    hints: bool = on
  ): ShellCmd =

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

    opt("nimblePath", dump.nimblePath)

    flag("skipUserCfg")
    flag("skipParentCfg")
    flag("skipProjCfg")
    opt("showAllMismatches", "on")
    opt("cc", "gcc")
    opt("passc", "-fdiagnostics-format=json")

  if hints:
    cmd.opt("hints", "on")
    cmd.opt("processing", "filenames")

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

proc runFromFile*(file: AbsFile): NimRun =
  result = NimRun(
    file: file,
    outfile: file.withExt("out")
  )

  let text = file.readFile()
  if text.startsWith("discard"):
    result.spec = some text.extractSpec(file).parseSpec(file)



proc runsFromDir*(dir: AbsDir, first: seq[AbsFile] = @[]): seq[NimRun] =
  for file in first & toSeq(walkDir(dir, AbsFile, exts = @["nim"])):
    if file.name().startsWith("t"):
      result.add runFromFile(file)

proc getJoinable*(runs: seq[NimRun]): tuple[joinable, standalone: seq[NimRun]] =
  for run in runs:
    if (?run.spec and run.spec.get().joinable) or not(?run.spec):
      result.joinable.add run

    else:
      result.standalone.add run

proc makeJoinedCode*(runs: seq[NimRun]): string =
  for run in runs:
    result.add &"import \"{run.file}\"\n"

proc skipFileLineCol*(str: var PosStr): tuple[file: AbsFile, line, column: int] =
  result.file = str.asStrSlice(str.skipTo('(')).AbsFile()
  str.skip("(")
  echov str
  result.line = str.asStrSlice(str.skipTo(',')).parseInt()
  str.skip(", ")
  echov str
  result.column = str.asStrSlice(str.skipTo(')')).parseInt()
  str.skip(")")

proc skipKindDeclaredIn*(str: var PosStr):
  tuple[kind: string, file: AbsFile, line, column: int] =

  str.skip("[")
  echov str
  str.space()
  echov str

  result.kind = str.asStrSlice(str.skipTo(' '))
  str.skip(" declared in ")

  (result.file, result.line, result.column) = str.skipFileLineCol()

  str.skip("]")

proc parseAmbiguousCall*(text: string): NimReport =
  result = NimReport(kind: nrError, error: neAmbiguousCall)
  var msg = initPosStr(text)

  msg.skip("ambiguous call; both")
  msg.space()
  for i in 0 .. 1:
    if i == 1:
      msg.skip(" and ")

    echov msg
    let name = msg.asStrSlice():
      echov msg
      msg.skipTo('[')
      echov msg
      if msg["[]"]:
        echov msg
        msg.next(2)
        msg.skipUntil('[')

      echov msg
      msg.back()

    msg.skip(' ')

    let (kind, file, line, column) = msg.skipKindDeclaredIn()

    result.definedAlts.add((
      kind: kind,
      name: name,
      definedIn: file,
      line: line,
      column: column
    ))

  msg.space()
  msg.skip("match for: ")
  result.matchFor = msg.asStrSlice(msg.skipToEof(), 0)


proc parseHintProcessingImports(str: var PosStr): NimReportPart =
  str.skip("Hint: ")
  result.kind.setKind(nrpProcessingImports)
  result.level = str.asSlice(str.skipWhile({'>'})).strVal().len()
  str.space()
  if str["(toplevel)"]:
    result.isToplevel = true
    result.level = 0

  result.imports = str.asSlice(str.skipUntil({':'})).strVal()
  str.skip(": ")
  if str["include"]:
    str.skip("include: ")
    result.isInclude = true

  else:
    str.skip("import: ")

  str.space()

  result.target = AbsFile(str.asSlice(str.skipToEol()).strVal()[
    0 ..< ^len(" [Processing]")])


proc parseCmdCommand(str: var PosStr): seq[string] =
  str.pushSlice()
  while ?str:
    case str[]:
      of ' ':
        result.add str.popSlice().strVal()
        str.space()
        str.pushSlice()

      of '"': str.skipStringLit()
      else: str.next()

proc parseExecutionOf(str: var PosStr): seq[string] =
  str.skip("Error: execution of an external ")
  if str["compiler"]:
    str.skip("compiler program '")
    let slice = str.asSlice():
      str.skipToEol()
      while str[Digits]:
        str.back()

      str.back(len("' failed with exit code: "))

    result = slice.strVal().initPosStr().asVar().parseCmdCommand()

  else:
    str.skip("program failed: '")
    result = str.asStrSlice(str.skipToEol(), -2).
      initPosStr().
      asVar().
      parseCmdCommand()


proc parseLdReport(str: var PosStr): NimReport =
  result = NimReport(kind: nrError, error: neLdFail)
  str.skip("/bin/ld: ")
  result.ldReport.message = str.asStrSlice(str.skipPastEol(), -2)
  str.skip("collect2: ")
  str.skipPastEol()
  result.ldReport.compile = parseExecutionOf(str)

proc parseGccReport(report: string): NimReport =
  var
    idx = 0
    diags: seq[seq[GccReport]]

  while report[idx] in {' ', '\n'}: inc idx

  while report[idx] == '[':
    if report[idx .. idx + 1] == "[]":
      inc idx, 3

    else:
      var tmp: seq[GccReport]
      parseHook(report, idx, tmp)
      inc idx
      diags.add tmp

  if report[idx .. ^1].startsWith("/bin/ld"):
    result = initPosStr(report[idx .. ^1]).asVar().parseLdReport()

  else:
    result = NimReport(kind: nrError, error: neGccFail)
    result.gccReport.diags = diags

    while idx < report.len and report[idx] in {' ', '\n'}: inc idx

    if idx < report.len:
      result.gccReport.compile = report[idx .. ^1].
        initPosStr().
        asVar().
        parseExecutionOf()


proc parseOverloadAlts(text: string): NimReport =
  result = NimReport(kind: nrError, error: neOverloadFail)
  var str = initPosStr(text)
  str.skip("type mismatch: got <")
  result.overloadContext.gotType = str.asStrSlice():
    str.skipToEol()
    str.back()


  str.skip(">\nbut expected one of:\n")

  var expression: string
  while str["proc"]:
    let signature = str.asSlice():
      if str.trySkipTo("first type mismatch at position"):
        str.back(3)

      else:
        raise newParseError(
          "Could not find correct finish of the proc signature range " &
            text)

    var argumentFail, isOfType: string
    str.next(3)
    if str["first type mismatch"]:
      str.skipPastEol()
      str.skip("  ")
      argumentFail = str.asStrSlice(str.skipPastEol(), -2)
      str.skip("  ")
      str.skip("but expression '")
      expression = str.asStrSlice():
        assert str.trySkipTo("' is of type: ")

      str.skip("' is of type: ")
      isOfType = str.asStrSlice(str.skipPastEol(), -2)

    result.overloadContext.alts.add NimOverloadAlt(
      signature: signature.strVal(),
      argumentFail: argumentFail,
      isOfType: if isOfType.len > 0: some(isOfType) else: none(string)
    )

  result.overloadContext.expression = expression





proc parseReportImpl(report: string): NimReport =
  var str = initPosStr(report)

  str.skipWhile({'\n'})
  if str["/bin/ld"]:
    result = parseLdReport(str)

  elif str["["]:
    result = parseGccReport(report)

  else:
    while ?str:
      str.skipWhile({'\n'})
      if str["stack trace"]:
        str.skipToEol()
        str.skipWhile({'\n'})

      var part: NimReportPart

      if str["Hint: >"]:
        part = parseHintProcessingImports(str)
        result.kind.setKind(nrHint)

      else:
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
              part.text = str.asSlice((str.skipToEof(); str.next())).strVal()
              result.kind.setKind(nrError)

              if part.text.startsWith("unhandled exception:"):
                result.error.setKind neException
                part.kind.setKind nrpException

              elif part.text.startsWith("cannot open file:"):
                result.error.setKind neCannotOpen

              elif part.text.startsWith("ambiguous call;"):
                result = parseAmbiguousCall(part.text)

              elif part.text.startsWith("invalid indentation"):
                result.error.setKind neInvalidIndentation
                if part.text.startsWith("invalid indentation, maybe"):
                  result.fixit.add NimFixit(
                    message: part.text[len("invalid indentation, ") .. ^1])

              elif part.text.startsWith("type mismatch"):
                result = parseOverloadAlts(part.text)

            else:
              raise newUnexpectedKindError(kind)

        else:
          part.text = str.asSlice(str.skipToEol()).strVal()

      const genInst = "template/generic instantiation of"
      if part.text.startsWith(genInst):
        part.text = part.text[genInst.len .. ^len("from here")]
        part.kind.setKind nrpInstOf

      if result of nrError and result.error == neOverloadFail:
        discard

      else:
        result.parts.add part

    if result.kind == nrError and result.error == neException:
      for part in mitems(result.parts):
        if part.kind == nrpNone:
          part.kind.setKind nrpTracePart


proc parseReport(report: string): NimReport =
  try:
    result = parseReportImpl(report)

  except UnexpectedCharError as err:
    var tmp = newParseError(
      "Failed to parse nim compilation report. Full report text is - ",
      mblock(report),
      "Failure ocurred while parsing text as position - ",
      describeStrPos(report, err.pos),
      ". Error message was \"", $err.msg, "\"")

    tmp.parent = err

    raise tmp

proc getReports(res: ShellResult, run: NimRun): seq[NimReport] =
  for part in res.getStderr().split("\31"):
    if part.strip().len() > 0:
      var report = parseReport(part)
      if report of nrError and
         report.error == neGccFail and
         report.gccReport.diags.len == 0:
        discard

      else:
        result.add report


proc formatShellCmd*(cmd: ShellCmd): ColoredText =
  result = clt(cmd.bin)
  for arg in cmd:
    result.add "  \\\n  "
    result.add arg.toStr(cmd.conf, true)

proc logLines*(l: HLogger, part: NimReportPart) =
  if not(part of {nrpException}):
    l.err part.file, part.line, part.column

  if not(part of {nrpInstOf, nrpTracePart}):
    l.err part.text

  if not(part of {nrpException}):
    l.logLines(AbsFile(part.file), part.line, "nim", part.column)


type
  NimReportState = object
    importParts: seq[NimReportPart]



proc register(state: var NimReportState, report: NimReport) =
  for part in report.parts:
    if report of nrHint and part of nrpProcessingImports:
      if len(state.importParts) == 0:
        doAssert part.level == 0
        state.importParts.add part

      else:
        if part.level > state.importParts.last().level:
          state.importParts.add part

        elif part.level < state.importParts.last().level:
          discard state.importParts.pop()




proc reportError*(
    l: HLogger, report: NimReport,
    dump: NimState, state: NimReportState
  ) =

  var hadInstOf = false
  for part in report.parts:
    hadInstOf = true
    if part of {nrpInstOf}:
      l.logLines(part)

  if hadInstOf:
    l.info "---------------"

  for part in report.parts:
    if not(part of {nrpInstOf}):
      l.logLines(part)

  if report of nrError:
    case report.error:
      of neCannotOpen:
        l.pdump dump.paths
        l.pdump dump.nimblePath

        if false:
          for idx, part in state.importParts:
            if not part.isToplevel:
              echo "  ".repeat(idx - 1), "↳", part.target

      of neAmbiguousCall:
        let (k1, k2) = (report.definedAlts[0], report.definedAlts[1])
        let m = max(k1.name.len, k2.name.len)
        l.err "ambiguous call"
        l.info k1.name
        l.info k2.name
        l.info " match for ", report.matchFor

      else:
        discard


proc getCompileReportFor*(
    file: AbsFile,
    dump: NimState,
    hints: bool = on
  ): seq[NimReport] =
  let run = runFromFile(file)
  let cmd = makeCmd(run, dump, hints = hints)
  let res = shellResult(cmd)
  return res.getReports(run)

proc skipKinds*(
    reports: sink seq[NimReport],
    kind: set[NimReportKind] = {nrHint, nrWarning}
  ): seq[NimReport] =

  filterIt(reports, it.kind notin kind)

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
    var state: NimReportState
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
      l.err "Errors during compilation"
      for report in reports:
        state.register(report)

        if report of nrError:
          l.reportError(report, dump, state)

    else:
      l.success "No errors detected during compilation"
      for run in joined:
        l.debug run.file

      execShell shellCmdGnu(run.outfile.string)


  if standalone.len == 0:
    l.info "No standalone files"

  else:
    l.info "Running separate files"
    var failCount = maxfail
    for (res, run) in runShellResult(
      standalone.mapIt((makeCmd(it, dump), it)), maxPool = 2):

      var state: NimReportState

      let reports = getReports(res, run)
      var hasError = false
      for report in reports:
        state.register(report)
        if report of nrError:
          l.reportError(report, dump, state)
          inc failCount

          if maxFail < failCount:
            break

          hasError = true

      if not hasError:
        l.success run.file
        l.execShell shellCmdGnu(run.outfile.string)

export oswrap, all

when isMainModule:
  startHax()
  runTestDir(
    AbsDir(relToSource"../../../tests"),
    getCwdNimDump(),
    1,
    @[AbsFile("/mnt/workspace/github/hmisc/tests/tMacros.nim")])
