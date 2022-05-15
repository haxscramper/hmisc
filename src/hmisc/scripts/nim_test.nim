import
  hmisc/preludes/cli_app,
  hmisc/other/[hjson, hlogger, hunittest],
  hmisc/algo/[hlex_base, lexcast, clformat_interpolate, hseq_distance],
  hmisc/hasts/[json_serde, json_serde_extra]

import std/[sequtils, streams, parsecfg, sets, algorithm]

export hlogger, colorstring


type
  NimFlag* = enum
    ## Extra nim configuration options
    nrfStrictFuncs = "strictFuncs"
    nrfHints = "hints"
    nrfThreads = "threads" ## Enable or disable threads
    nrfPanics = "panics" ## Enable or disable panics in the code

  NimGc* = enum
    ## Nim garbage collectors
    ngcRefc = "refc"
    ngcArc = "arc"
    ngcOrc = "orc"
    ngcNone = "none"

  NimBackend* = enum
    ## Target backends to compile code to
    nbC = "c"
    nbJs = "js"
    nbCpp = "cpp"
    nbObjc = "objc"

  NimMatrixCell* = object
    ## Single configuration cell in CI matrix
    backend*: NimBackend ## Target backend to compile to
    gc*: NimGc ## Garbage collector to use when testing
    flags*: array[NimFlag, Option[bool]] ## Additional compilation flags
    defines*: seq[tuple[key: string, value: Option[string]]] ## Additional
    ## compilation defines. `value` is added as `key=<value>` to the passed
    ## compiler flag. Value is not quoted.

    extraFlags*: seq[string]

    # hints*: set[NimHint]
    # warnings*: set[NimWarning]

  NimRun* = object
    ## Configuration of the single execution
    cell*: NimMatrixCell ## Full configuration of the runner matrix cell.
                         ## Contains all necessary options for configurin
                         ## nim compiler to produce `outFile`
    # flags: set[NimFlag]
    # gc: NimGc
    # backend: NimBackend
    file: AbsFile
    joinedFrom: seq[AbsFile]
    outFile: AbsFile
    tmpDir: AbsDir

    spec: Option[NimTestSpec] ## Optional testament test configuration
    runFlags*: seq[ShellCmdPart] ## Command-line flags for the program
                                 ## execution that were either deduced from
                                 ## the test specification or provided via
                                 ## some other means

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
    nwNone = "none"

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

  NimDump* = object
    ## Current configuration of the nim compiler
    paths*: seq[AbsDir] ## Dependency paths
    hints*: set[NimHint]
    flags*: seq[NimFlag]
    nimblePath*: AbsDir ## Nimble directory path

  NimRunConf* = object
    ## Global test configuration
    dump*: NimDump ## Compiler environment configuration state

    fullSeal*: bool
    hints*: seq[NimHint] ## Set of hints to enable during compilation
    flags*: array[NimFlag, Option[bool]]
    warnings*: seq[NimWarning] ## Set of warnings to enable during compilation
    globalMatrix*: seq[NimMatrixCell] ## List of global CI matrix cells.
    extraFlags*: seq[string]
    megatest*: bool

    defines*: seq[tuple[key: string, value: Option[string]]]
    tempDir*: AbsDir ## Temporary directory to write compiled test
                     ## binaries, cache and other intermediate files to.

    parallelCompile*: int ## Number of parallel compilation jobs
    parseCompilation*: bool ## Parse compiler messages or pass the output directly
    parseExecution*: bool ## Parse unit test execution result or pass the output directly

    randomPattern*: string ## Pattern for random file name generation. Uses
    ## `?` as a character replacement pattern. Note that this is only a
    ## part of a file name - full one might be constructed based on the
    ## multiple parameters.

    reportEvent*: proc(event: NimRunnerEvent)

    fileGlobs*: seq[GitGlob]
    testArguments*: seq[string]


  NimReportKind* = enum
    ## Type of the single nim compile entry report
    nrHint ## Compilation hint
    nrWarning ## Compilation warning
    nrError ## Compilation error
    nrUnparsed ## Report could not be parsed
    nrSkip ## Empty report. Right now only produced by gcc diagnostics data
           ## (it always runs, but most of the time succesfully).

  NimReportPartKind* = enum
    ## Single part of the nim compile report
    nrpNone
    nrpInstOf ## `template/generic instantiation of`
    nrpException ## Final exception message section
    nrpTracePart ## Part of the exception stacktrace

    nrpProcessingImports

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

  NimRunnerEventKind* = enum
    nrekNone

    nrekCompiledFile
    nrekRunningFile
    nrekFinishedExecution
    nrekJoined
    nrekIgnoreFile

  NimRunnerEvent* = object
    case kind*: NimRunnerEventKind
      of nrekCompiledFile,
         nrekRunningFile,
         nrekFinishedExecution,
         nrekIgnoreFile:
        file*: AbsFile

      of nrekJoined:
        joined*: seq[AbsFile]

      else:
        discard


  NimReport* = object
    parts*: seq[NimReportPart]
    fixit*: seq[NimFixit]
    original*: string
    case kind*: NimReportKind
      of nrSkip:
        discard

      of nrUnparsed:
        reportText*: string
        parseFail*: Option[ParseError]

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


proc getCwdNimDump*(file: string = "-"): NimDump =
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

func initNimRunConf*(dump: NimDump, tempDir: AbsDir): NimRunConf =
  NimRunConf(
    dump: dump,
    tempDir: tempDir,
    megatest: true,
    parallelCompile: 6,
    randomPattern: "????_????_????",
    parseCompilation: true,
    parseExecution: true,
    reportEvent: proc(event: NimRunnerEvent) = discard
  )

func incl*(conf: var NimRunConf, flag: NimFlag) =
  conf.flags[flag] = some true

func excl*(conf: var NimRunConf, flag: NimFlag) =
  conf.flags[flag] = some false

func define*(conf: var NimRunConf, value: string) =
  conf.defines.add(value, none string)

proc getRandomFile*(
    conf: NimRunConf,
    dir: AbsDir,
    prefix, suffix: string
  ): AbsFile =

  result = dir.getTempFile(prefix & conf.randomPattern & suffix)

proc getRandomFile*(conf: NimRunConf, prefix, suffix: string): AbsFile =
  getRandomFile(conf, conf.tempDir, prefix, suffix)


proc makeCmd(run: NimRun, conf: NimRunConf): ShellCmd =
  let dump = conf.dump
  var cmd = shellCmd(nim)

  cmd.arg "compile"
  for flag in run.cell.flags:
    if flag.canGet(state):
      cmd.opt($flag, if state: "on" else: "off")

  for (key, value) in run.cell.defines:
    if value.canGet(value):
      cmd.opt("define", key & "=" & value)

    else:
      cmd.opt("define", key)

  if conf.parseExecution:
    cmd.opt("define", "hmiscUnittestOut=json")

  with cmd:
    opt("gc", $run.cell.gc)
    opt("backend", $run.cell.backend)
    opt("out", $run.outfile)
    opt("unitsep", "on")

    opt("verbosity", "0")

  with cmd:
    opt("showAllMismatches", "on")
    opt("cc", "gcc")
    opt("passc", "-fdiagnostics-format=json")

  # if hints:
  #   cmd.opt("hints", "on")
  #   cmd.opt("processing", "filenames")

  cmd.opt("nimcache", $getNewTempDir(run.file.name(), getAppTempDir()))

  if conf.fullSeal:
    with cmd:
      opt("nimblePath", dump.nimblePath)
      flag("skipUserCfg")
      flag("skipParentCfg")
      flag("skipProjCfg")

    for p in dump.paths:
      cmd.opt("path", p)

  cmd.arg run.file

  return cmd


func hshow(run: NimRun, opts: HDisplayOpts = defaultHDisplay): ColoredText =
  with result:
    add hshow(run.cell.gc)
    add ", "
    add hshow(run.cell.backend)
    add " "
    add hshow(run.cell.flags)

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

proc runsFromFile*(file: AbsFile, conf: NimRunConf): seq[NimRun] =
  var spec: Option[NimTestSpec]
  let text = file.readFile()
  if text.startsWith("discard"):
    spec = some text.extractSpec(file).parseSpec(file)

  var run = NimRun(
    file: file,
    outfile: file.withExt("out"),
    spec: spec
  )

  for define in conf.defines:
    run.cell.defines.add define

  run.cell.extraFlags.add conf.extraFlags

  result.add run




proc runsFromDir*(
    dir: AbsDir,
    conf: NimRunConf,
    first: seq[AbsFile] = @[]
  ): seq[NimRun] =

  var files: seq[AbsFile]
  for file in first & toSeq(walkDir(dir, AbsFile, exts = @["nim"])):
    if file.name().startsWith("t"):
      let fileRel = file.string.dropPrefix(dir.string).string
      if conf.fileGlobs.accept(fileRel):
        files.add file

      else:
        conf.reportEvent(NimRunnerEvent(
          kind: nrekIgnoreFile, file: file))


  for file in sortedByIt(files, string(it)):
    result.add runsFromFile(file, conf)


proc getJoinable*(runs: seq[NimRun], conf: NimRunConf):
    tuple[joinable, standalone: seq[NimRun]] =

  for run in runs:
    if conf.megatest and (
       (?run.spec and run.spec.get().joinable) or
       not(?run.spec)
    ):
      result.joinable.add run

    else:
      result.standalone.add run

proc makeJoinedCode*(runs: seq[NimRun]): string =
  for run in runs:
    result.add &"import \"{run.file}\"\n"

proc skipFileLineCol*(str: var PosStr): tuple[file: AbsFile, line, column: int] =
  result.file = str.asStrSlice(str.skipTo('(')).AbsFile()
  str.skip("(")
  result.line = str.asStrSlice(str.skipTo(',')).parseInt()
  str.skip(", ")
  result.column = str.asStrSlice(str.skipTo(')')).parseInt()
  str.skip(")")

proc skipKindDeclaredIn*(str: var PosStr):
  tuple[kind: string, file: AbsFile, line, column: int] =

  str.skip("[")
  str.space()

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

    let name = msg.asStrSlice():
      msg.skipTo('[')
      if msg["[]"]:
        msg.next(2)
        msg.skipUntil('[')

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
      if str.atEof(): str.back()
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

const ldNames = ["/bin/ld", "/usr/bin/ld"]

proc parseLdReport(str: var PosStr): NimReport =
  result = NimReport(kind: nrError, error: neLdFail)
  str.skip(ldNames)
  str.skip(": ")
  result.ldReport.message = str.asStrSlice(str.skipPastEol(), -2)
  str.skip("collect2: ")
  str.skipPastEol()
  result.ldReport.compile = parseExecutionOf(str)

proc parseGccReport(report: string): NimReport =
  var
    idx = 0
    diags: seq[seq[GccReport]]

  while report[idx] in {' ', '\n'}: inc idx

  var other: seq[string]

  for line in splitLines(report):
    if line == "[]":
      discard

    elif line.startsWith("["):
      diags.add fromJson(line, seq[GccReport])

    elif line == "":
      discard

    else:
      other.add line

  let post = other.join("\n")
  if post.startsWith(ldNames):
    result = initPosStr(post).asVar().parseLdReport()

  else:
    if 0 < len(diags) or 0 < len(post):
      result = NimReport(kind: nrError, error: neGccFail)
      result.gccReport.diags = diags

      if 0 < len(post):
        if idx < report.len:
          result.gccReport.compile = post.
            initPosStr().
            asVar().
            parseExecutionOf()


    else:
      result = NimReport(kind: nrSkip)


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





proc parseNimReportImpl(report: string): NimReport =
  var str = initPosStr(report)

  str.skipWhile({'\n'})
  if str[ldNames]:
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



proc parseNimReport*(report: string): NimReport =
  try:
    result = parseNimReportImpl(report)
    result.original = report

  except UnexpectedCharError as err:
    result = NimReport(
      kind: nrUnparsed,
      reportText: report,
      parseFail: some newParseError(
        "Failed to parse nim compilation report. Full report text is - ",
        mblock(report),
        "Failure ocurred while parsing text as position - ",
        describeStrPos(report, err.pos),
        ". Error message was \"", $err.msg, "\"")[])

proc getCompileReports(res: ShellResult, run: NimRun): seq[NimReport] =
  for part in res.getStderr().split("\31"):
    if part.strip().len() > 0:
      var report = parseNimReport(part)
      if report of nrSkip:
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
  NimReportState* = object
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



proc skipKinds*(
    reports: sink seq[NimReport],
    kind: set[NimReportKind] = {nrHint, nrWarning}
  ): seq[NimReport] =

  filterIt(reports, it.kind notin kind)

type
  NimRunResultKind* = enum
    nrrkNone

    nrrkFailedCompilation
    nrrkFailedExecution
    nrrkSuccess
    nrrkFailedTests

  NimRunResult* = object
    run*: NimRun
    compileRes*: ShellResult
    compileReports*: seq[NimReport]
    msg*: string
    kind*: NimRunResultKind
    runReports*: seq[TestReport]


proc createJoined*(runs: seq[NimRun], conf: NimRunConf): seq[NimRun] =
  let
    (joined, standalone) = runs.getJoinable(conf)

  if 0 < len(joined):
    let
      res = conf.getRandomFile("tmp_joined", ".nim")
      code = makeJoinedCode(joined)

    res.writeFile(code)

    var merged = runsFromFile(res, conf)
    for it in mitems(merged):
      for run in joined:
        it.joinedFrom.add run.file

    conf.reportEvent(NimRunnerEvent(
      kind: nrekJoined, joined: merged[0].joinedFrom))

    result.add merged

  result.add standalone


proc joinedRunsFromDir*(
    dir: AbsDir,
    conf: NimRunConf,
    first: seq[AbsFile] = @[],
  ): seq[NimRun] =

  runsFromDir(dir, conf, first).createJoined(conf)

iterator compileRuns*(
    runs: seq[NimRun],
    conf: NimRunConf
  ): tuple[run: NimRun, compile: ShellResult] =

  var cmds: seq[(ShellCmd, NimRun)]
  for it in runs:
    let cmd = makeCmd(it, conf)
    cmds.add(cmd, it)

  for (res, run) in runShellResult(
    cmds,
    maxPool = conf.parallelCompile
  ):
    conf.reportEvent(NimRunnerEvent(
      kind: nrekCompiledFile,
      file: run.file))

    yield (run, res)

proc makeRunCmd*(run: NimRun): ShellCmd =
  result = makeFileShellCmd(run.outfile)
  for flag in run.runFlags:
    result.add flag

proc parseRunReports*(res: ShellResult): seq[TestReport] =
  var activeLocation: TestLocation
  for line in res.getStdout().splitLines():
    if 0 < len(line):
      echo line
      result.add fromJson(line, TestReport)
      activeLocation = result.last().location


proc hasFail*(reports: seq[TestReport]): bool =
  var skippedTest = false
  for rep in items(reports):
    case rep.kind:
      of {trkTestSkip}: skippedTest = true
      of {trkTestStart, trkTestEnd}: skippedTest = false
      of {trkTestFail, trkCheckFail}:
        if not skippedTest:
          return true

      of {trkSuiteFail}:
        return true

      else:
        discard

proc invokeCompiled*(
    run: NimRun,
    compile: ShellResult,
    conf: NimRunConf
  ): NimRunResult =

  result = NimRunResult(run: run, compileRes: compile)
  if conf.parseCompilation:
    result.compileReports = compile.getCompileReports(run)


  if not compile.isOk(@[-1, 0]) or
     anyIt(result.compileReports, it of nrError):
    result.kind = nrrkFailedCompilation

  else:
    conf.reportEvent(NimRunnerEvent(
      kind: nrekRunningFile, file: run.file))

    let cmd = makeRunCmd(run)
    if conf.parseExecution:
      let execResult = shellResult(cmd)

      conf.reportEvent(NimRunnerEvent(
        kind: nrekFinishedExecution, file: run.file))

      result.kind =
        if execResult.isOk():
          nrrkSuccess
        else:
          nrrkFailedExecution

      result.runReports = parseRunReports(execResult)

    else:
      try:
        execShell(cmd)
        result.kind = nrrkSuccess

      except ShellError:
        result.kind = nrrkFailedExecution

      conf.reportEvent(NimRunnerEvent(
        kind: nrekFinishedExecution, file: run.file))

  if hasFail(result.runReports):
    result.kind = nrrkFailedTests


proc getRunReportsFor*(file: AbsFile, conf: NimRunConf): seq[NimRunResult] =
  for (run, compile) in runsFromFile(file, conf).compileRuns(conf):
    result.add invokeCompiled(run, compile, conf)

proc getCompileReportsFor*(
    file: AbsFile,
    conf: NimRunConf
  ): seq[tuple[run: NimRun, reports: seq[NimReport]]] =

  for (run, compile) in runsFromFile(file, conf).compileRuns(conf):
    result.add(run, compile.getCompileReports(run))


when false:
  proc processRunResult(
      run: NimRun,
      runRes: ShellResult,
      dump: NimDump,
      l: HLogger,
      parseRun: bool = true
    ): NimRunResult =

    var state: NimReportState

    if parseRun:
      let reports = getCompileReports(runRes, run)

      if reports.anyIt(it of nrError):
        result = some NimRunFailDesc(msg: "Compilation failed")
        l.err "Errors during compilation"
        for report in reports:
          state.register(report)

          if report of nrError:
            l.reportError(report, dump, state)


    elif not runRes.isOk():
      result = some NimRunFailDesc(msg: "Compilation failed")
      l.err runRes.getStderr()
      l.err runRes.getStdout()

    if runRes.isOk():
      try:
        execShell shellCmdGnu(run.outfile.string)

      except ShellError as err:
        l.err err.getStdout()
        l.err err.getStderr()
        result = some NimRunFailDesc(msg: "Script file execution failed")

    if result.isSome():
      result.get().run = run
      result.get().compileRes = runRes

proc formatPart*(part: NimReportPart): ColoredText =
  coloredResult()

  add hshow(part.file)
  add ":"
  add hshow(part.line)
  add ":"
  add hshow(part.column)
  add " "
  add part.text

  endResult()

proc formatReport*(
    report: NimReport,
    dump: NimDump,
    state: NimReportState,
  ): ColoredText =

  coloredResult()

  case report.kind:
    of nrSkip:
      discard

    of nrUnparsed:
      add "Unparsed compiled output:\n"
      add indent(report.reportText, 2)
      add "\n"
      if report.parseFail.canGet(err):
        add "Parse failed due to an error\n  "
        add err.msg
        add "\n"

    of nrHint:
      add "Hint ...\n"

    of nrWarning:
      add "Warning " + fgYellow
      add "\n"

      for part in report.parts:
        add formatPart(part)
        add "\n"

    of nrError:
      add report.original

      if false:
        case report.error:
          of neCannotOpen:
            add "Cannot open imported path\n"
            addi 1, "List of known import paths:\n"
            for path in dump.paths:
              addi 2, hshow(path)
              add "\n"

            addi 1, "Nimble path\n"
            addi 2, hshow(dump.nimblePath)
            add "\n"


          else:
            add $report.error

  endResult()

const
  nimTestAlign = (18, '[', ']')


proc formatEvent*(event: NimRunnerEvent): ColoredText =
  coloredResult()

  let ind = 12

  case event.kind:
    of nrekNone:
      raise newUnexpectedKindError(event)

    of nrekCompiledFile:
      add ("compiled" |<> nimTestAlign) + fgYellow
      add " "
      add $event.file

    of nrekIgnoreFile:
      add ("ignore" |<> nimTestAlign) + fgCyan
      add " "
      add $event.file

    of nrekRunningFile:
      add ("running" |<> nimTestAlign) + fgCyan
      add " "
      add $event.file

    of nrekFinishedExecution:
      add ("finished" |<> nimTestAlign) + fgGreen
      add " "
      add $event.file

    of nrekJoined:
      add "joined" |<> nimTestAlign
      add " \n"
      for file in event.joined:
        add "    - "
        add $file
        add "\n"




  endResult()

proc formatReport*(rep: TestReport): ColoredText =
  coloredResult()

  add rep.msg
  return

  let align = (12, '[', ']')

  case rep.kind:
    of trkCheckOk:
      add ("TEST" |<> align) + fgGreen

    of trkCheckFail:
      add formatCheckFail(rep)

    of trkTestSkip:
      add ("SKIP" |<> align) + fgYellow
      add " "
      add rep.name

    of trkSuiteFail:
      add ("SUITE" |<> align) + fgRed
      add " "
      add rep.name

    else:
      add $rep.kind
      add " "
      add $rep.location

  if rep of trkFailKinds:
    add " "
    add hshow(rep.failKind)

    if rep.failKind == tfkException:
      let e = rep.exception
      add "\n\n"
      add "  Exception was raised during execution"
      add "  name = "
      add ($e.name + fgCyan)
      add "\n"
      add "  trace = \n"
      let maxp = maxIt(e.getStackTraceEntries(), len(it.procname))

      for tr in e.getStackTraceEntries():
        add "    "
        add ($tr.procname |<< maxp) + fgCyan
        add " "
        add $tr.filename
        add ":"
        add hshow(tr.line)
        add "\n"


  endResult()

type
  NimRunStat* = object
    events*: array[TestReportKind, int]
    store*: array[TestReportKind, seq[TestReport]]

func `+`*(s1: sink NimRunStat, s2: NimRunStat): NimRunStat =
  result = s1
  for idx, _ in mpairs(s1.events):
    result.events[idx] += s2.events[idx]
    result.store[idx].add s2.store[idx]

func `[]`*(stat: NimRunStat, event: TestReportKind): int =
  stat.events[event]

func statReports*(stat: var NimRunStat, reports: seq[TestReport]) =
  for rep in items(reports):
    inc stat.events[rep.kind]
    stat.store[rep.kind].add rep

proc formatReport*(stat: seq[TestReport]): ColoredText =
  coloredResult()
  if len(stat) == 0:
    add "none"

  else:
    for report in items(stat):
      add formatReport(report)
      add "\n"

  endResult()

proc formatInlineStat*(stat: NimRunStat, fg: ForegroundColor): ColoredText =
  coloredResult()

  add "test " + fg
  add $stat[trkTestOk] + fgGreen
  add "/"
  add $stat[trkTestSkip] + fgYellow
  add "/"
  add $stat[trkTestFail] + fgRed
  add "/"
  add $stat[trkTestStart] + fgCyan
  result = result.alignLeft(15)

  add "suite " + fg
  add $stat[trkSuiteFail] + fgRed
  add "/"
  add $stat[trkSuiteEnd] + fgGreen
  add "/"
  add $stat[trkSuiteStart] + fgCyan
  result = result.alignLeft(30)

  add "check " + fg
  add $stat[trkCheckFail] + fgRed
  add "/"
  add $stat[trkCheckOk] + fgGreen
  add "/"
  add $(stat[trkCheckOk] + stat[trkCheckFail]) + fgCyan

  endResult()

proc formatStat*(stat: NimRunStat): ColoredText =
  let p = "" |<> (nimTestAlign[0], '.', '[', ']')
  clfmt("""
{p} Tests
{p}   executed: {stat[trkTestStart]:,fg-cyan}
{p}   passed  : {stat[trkTestOk]:,fg-green}
{p}   skipped : {stat[trkTestSkip]:,fg-yellow}
{p}   - {formatReport(stat.store[trkTestskip]):,indent}
{p}   failed  : {stat[trkTestFail]:,fg-red}
{p}   - {formatReport(stat.store[trkTestFail]):,indent}
{p}
{p} Suits
{p}   executed: {stat[trkSuiteStart]:,fg-cyan}
{p}   passed  : {stat[trkSuiteEnd]:,fg-green}
{p}   failed  : {stat[trkSuiteFail]:,fg-red}
{p}   - {formatReport(stat.store[trkSuiteFail]):,indent}
{p}
{p} Checks
{p}   executed: {(stat[trkCheckOk] + stat[trkCheckFail]):,fg-cyan}
{p}   passed  : {stat[trkCheckOk]:,fg-green}
{p}   failed  : {stat[trkCheckFail]:,fg-red}
{p}   - {formatReport(stat.store[trkCheckFail]):,indent}

""")

proc formatRun*(runs: seq[NimRunResult], dump: NimDump): ColoredText =
  coloredResult()

  var total: NimRunStat
  let align = nimTestAlign

  let nameW = maxIt(runs, it.run.file.name().len()) + 2

  for run in runs:
    var tmp: NimRunStat
    tmp.statReports(run.runReports)
    case run.kind:
      of nrrkFailedCompilation:
        add ("COMP-FAIL" |<> align) + fgYellow
        add "\n"
        var state: NimReportState
        for report in run.compileReports:
          state.register(report)
          if not (report of nrHint):
            add formatReport(report, dump, state)

        add "\n"

      of nrrkFailedExecution:
        add ("EXEC-FAIL" |<> align) + fgRed
        add " "
        add (run.run.file.name() |<< nameW) + fgRed
        add " "
        add formatInlineStat(tmp, fgRed)
        add "\n"

      of nrrkFailedTests:
        add ("TEST-FAIL" |<> align) + fgRed
        add " "
        add (run.run.file.name() |<< nameW) + fgRed
        add " "
        add formatInlineStat(tmp, fgRed)
        add "\n"


      of nrrkSuccess:
        add ("OK-SUCCESS" |<> align) + fgGreen
        add " "
        add (run.run.file.name() |<< nameW)
        add " "
        add formatInlineStat(tmp, fgDefault)
        add "\n"

      of nrrkNone:
        raise newUnexpectedKindError(run.kind)

    total = total + tmp

  add formatStat(total)

  endResult()

proc hasErrors*(runs: seq[NimRunResult]): bool =
  for run in runs:
    if run of {nrrkFailedCompilation, nrrkFailedExecution}:
      return true

proc runTestDir*(
    dir: AbsDir,
    conf: NimRunConf,
    first: seq[AbsFile] = @[],
  ): seq[NimRunResult] =

  let runs = runsFromDir(dir, conf = conf, first = first).createJoined(conf)
  for (run, compile) in compileRuns(runs, conf):
    result.add invokeCompiled(run, compile, conf)
