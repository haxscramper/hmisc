import ../preludes/cli_app
import ../other/[hjson, hlogger, hunittest, jsony_converters]
import ../algo/[hlex_base, lexcast]
import std/[sequtils, streams, parsecfg, sets]
import pkg/jsony

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

    hints*: seq[NimHint] ## Set of hints to enable during compilation
    flags*: array[NimFlag, Option[bool]]
    warnings*: seq[NimWarning] ## Set of warnings to enable during compilation
    globalMatrix*: seq[NimMatrixCell] ## List of global CI matrix cells.
    extraFlags*: seq[string]

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


  NimReportKind* = enum
    ## Type of the single nim compile entry report
    nrHint ## Compilation hint
    nrWarning ## Compilation warning
    nrError ## Compilation error
    nrUnparsed ## Report could not be parsed

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

  NimReport* = object
    parts*: seq[NimReportPart]
    fixit*: seq[NimFixit]

    case kind*: NimReportKind
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

func initNimRunConf*(dump: NimDump): NimRunConf =
  NimRunConf(
    dump: dump,
    parallelCompile: 4,
    randomPattern: "????_????_????"
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


proc makeCmd(
    conf: NimRun,
    dump: NimDump,
  ): ShellCmd =

  var cmd = shellCmd(nim)

  cmd.arg "compile"
  for flag in conf.cell.flags:
    if flag.canGet(state):
      cmd.opt($flag, if state: "on" else: "off")

  for (key, value) in conf.cell.defines:
    if value.canGet(value):
      cmd.opt("define", key & "=" & value)

    else:
      cmd.opt("define", key)


  with cmd:
    opt("gc", $conf.cell.gc)
    opt("backend", $conf.cell.backend)
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

  # if hints:
  #   cmd.opt("hints", "on")
  #   cmd.opt("processing", "filenames")

  cmd.opt("nimcache", $getNewTempDir(conf.file.name(), getAppTempDir()))

  for p in dump.paths:
    cmd.opt("path", p)

  cmd.arg conf.file

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

  # TODO parse spec configuration to generate multiple matrix values

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

  for file in first & toSeq(walkDir(dir, AbsFile, exts = @["nim"])):
    if file.name().startsWith("t"):
      result.add runsFromFile(file, conf)

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

  while idx < report.len and report[idx] == '[':
    if report[idx .. idx + 1] == "[]":
      inc idx, 3

    else:
      var tmp: seq[GccReport]
      parseHook(report, idx, tmp)
      inc idx
      diags.add tmp

  if idx < report.len and report[idx .. ^1].startsWith(ldNames):
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
    l: HLogger,
    report: NimReport,
    dump: NimDump,
    state: NimReportState = NimReportState()
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

      of neOverloadFail:
        l.err "No matching function"
        let ctx = report.overloadContext
        l.info ctx.expression

        for alt in ctx.alts:
          l.debug alt.signature
          l.info alt.argumentFail
          if alt.isOfType.canGet(argType):
            l.debug "But expression is of type "
            l.debug argType

      of neLdFail:
        l.err "Linker exited with error"
        l.err report.ldReport.message

      of neGccFail:
        let (diags, compile) = report.gccReport
        l.err "Compiler command exited with error"
        for group in diags:
          for diag in group:
            l.err diag.message
            for loc in diag.locations:
              l.dump loc.caret.line
              l.dump loc.caret.file
              l.dump loc.caret.column

      else:
        l.debug report.error

proc skipKinds*(
    reports: sink seq[NimReport],
    kind: set[NimReportKind] = {nrHint, nrWarning}
  ): seq[NimReport] =

  filterIt(reports, it.kind notin kind)

type
  NimRunResultKind* = enum
    nrrkFailedCompilation
    nrrkFailedExecution
    nrrkSuccess

  NimRunResult* = object
    run*: NimRun
    compileRes*: ShellResult
    compileReports*: seq[NimReport]
    msg*: string
    kind*: NimRunResultKind
    runReports*: seq[TestReport]



proc joinedRunsFromDir*(
    dir: AbsDir,
    conf: NimRunConf,
    first: seq[AbsFile] = @[],
  ): seq[NimRun] =

  let
    runs = runsFromDir(dir, conf, first)
    (joined, standalone) = runs.getJoinable()
    res = conf.getRandomFile("tmp_joined", ".nim")
    code = makeJoinedCode(joined)

  res.writeFile(code)

  var merged = runsFromFile(res, conf)
  for it in mitems(merged):
    for run in joined:
      it.joinedFrom.add run.file

  result.add merged
  result.add standalone

iterator compileRuns*(
    runs: seq[NimRun],
    conf: NimRunConf
  ): tuple[run: NimRun, compile: ShellResult] =

  var cmds: seq[(ShellCmd, NimRun)]
  for it in runs:
    let cmd = makeCmd(it, conf.dump)
    cmds.add(cmd, it)

  for (res, run) in runShellResult(
    cmds,
    maxPool = conf.parallelCompile
  ):
    yield (run, res)

proc makeRunCmd*(run: NimRun): ShellCmd =
  result = makeFileShellCmd(run.outfile)
  for flag in run.runFlags:
    result.add flag

proc parseRunReports*(res: ShellResult): seq[TestReport] =
  for line in res.getStdout().splitLines():
    echov line
    result.add fromJson(line, TestReport)

proc invokeCompiled*(
    run: NimRun,
    compile: ShellResult,
    conf: NimRunConf
  ): NimRunResult =

  result = NimRunResult(run: run, compileRes: compile)
  if not compile.isOk():
    result.kind = nrrkFailedCompilation
    if conf.parseCompilation:
      result.compileReports = compile.getCompileReports(run)

  else:
    let run = makeRunCmd(run)
    if conf.parseExecution:
      let execResult = shellResult(run)
      if execResult.isOk():
        result.kind = nrrkSuccess

      else:
        result.kind = nrrkFailedExecution
        result.runReports = parseRunReports(execResult)

    else:
      try:
        execShell(run)
        result.kind = nrrkSuccess

      except ShellError:
        result.kind = nrrkFailedExecution


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


proc formatReport*(
    report: NimReport,
    dump: NimDump,
    state: NimReportState,
  ): ColoredText =

  coloredResult()

  case report.kind:
    of nrUnparsed:
      add "Unparsed compiled output:\n"
      add indent(report.reportText, 2)
      add "\n"
      if report.parseFail.canGet(err):
        add "Parse failed due to an error\n  "
        add err.msg
        add "\n"

    of nrHint:
      add "Hint ..."

    of nrWarning:
      add "Warning ..."

    of nrError:
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


        # of neAmbiguousCall:
        #   let (k1, k2) = (report.definedAlts[0], report.definedAlts[1])
        #   let m = max(k1.name.len, k2.name.len)
        #   l.err "ambiguous call"
        #   l.info k1.name
        #   l.info k2.name
        #   l.info " match for ", report.matchFor

        # of neOverloadFail:
        #   l.err "No matching function"
        #   let ctx = report.overloadContext
        #   l.info ctx.expression

        #   for alt in ctx.alts:
        #     l.debug alt.signature
        #     l.info alt.argumentFail
        #     if alt.isOfType.canGet(argType):
        #       l.debug "But expression is of type "
        #       l.debug argType

        # of neLdFail:
        #   l.err "Linker exited with error"
        #   l.err report.ldReport.message

        # of neGccFail:
        #   let (diags, compile) = report.gccReport
        #   l.err "Compiler command exited with error"
        #   for group in diags:
        #     for diag in group:
        #       l.err diag.message
        #       for loc in diag.locations:
        #         l.dump loc.caret.line
        #         l.dump loc.caret.file
        #         l.dump loc.caret.column

        else:
          add $report.error
          # l.debug report.error


  # var hadInstOf = false
  # for part in report.parts:
  #   hadInstOf = true
  #   if part of {nrpInstOf}:
  #     l.logLines(part)

  # if hadInstOf:
  #   l.info "---------------"

  # for part in report.parts:
  #   if not(part of {nrpInstOf}):
  #     l.logLines(part)

  # if report of nrError:

  endResult()

proc formatRun*(run: NimRunResult, dump: NimDump): ColoredText =
  coloredResult()

  case run.kind:
    of nrrkFailedCompilation:
      add "Compilation failed"
      var state: NimReportState
      pprint run.compileReports
      for report in run.compileReports:
        state.register(report)
        add formatReport(report, dump, state)

    of nrrkFailedExecution:
      add "Test execution failed"

    of nrrkSuccess:
      add "Test execution succeded"

  endResult()




# proc runTestDir*(
#     dir: AbsDir,
#     dump: NimDump,
#     first: seq[AbsFile] = @[],
#     l: HLogger = newTermLogger(),
#   ): bool =

#   block joined_items:
#     l.info "Running joined tests"
#     if processRunResult(
#         run,
#         shellResult(makeCmd(run, dump, hints = hints)),
#         dump,
#         l,
#         parseRun
#       ).canGet(fail):

#       fails.add fail

#   if standalone.len == 0:
#     l.info "No standalone files"

#   else:
#     l.info "Running separate files"
#     var failCount = maxfail
#     for (res, run) in runShellResult(
#       standalone.mapIt((makeCmd(it, dump, hints = hints), it)), maxPool = 2):
#       if processRunResult(run, res, dump, l, parseRun).canGet(fail):
#         fails.add fail

#   for fail in fails:
#     reportRunFail(fail, l)

#   return fails.len == 0
