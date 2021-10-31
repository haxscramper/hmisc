import ../preludes/cli_app
import ../other/hjson
import ../algo/hlex_base
import std/[sequtils]

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

  NimRun = object
    flags: set[NimFlag]
    gc: NimGc
    backend: NimBackend
    file: AbsFile
    outFile: AbsFile
    tmpDir: AbsDir

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
    nwCannotOpenFile = "CannotOpenFile"
    nwOctalEscape = "OctalEscape"
    nwXIsNeverRead = "XIsNeverRead"
    nwXmightNotBeenInit = "XmightNotBeenInit"
    nwDeprecated = "Deprecated"
    nwConfigDeprecated = "ConfigDeprecated"
    nwDotLikeOps = "DotLikeOps"
    nwSmallLshouldNotBeUsed = "SmallLshouldNotBeUsed"
    nwUnknownMagic = "UnknownMagic"
    nwRedefinitionOfLabel = "RedefinitionOfLabel"
    nwUnknownSubstitutionX = "UnknownSubstitutionX"
    nwBrokenLink = "BrokenLink"
    nwLanguageXNotSupported = "LanguageXNotSupported"
    nwFieldXNotSupported = "FieldXNotSupported"
    nwWarnRstStyle = "warnRstStyle"
    nwCommentXIgnored = "CommentXIgnored"
    nwTypelessParam = "TypelessParam"
    nwUseBase = "UseBase"
    nwWriteToForeignHeap = "WriteToForeignHeap"
    nwUnsafeCode = "UnsafeCode"
    nwUnusedImport = "UnusedImport"
    nwInheritFromException = "InheritFromException"
    nwEachIdentIsTuple = "EachIdentIsTuple"
    nwUnsafeSetLen = "UnsafeSetLen"
    nwUnsafeDefault = "UnsafeDefault"
    nwProveInit = "ProveInit"
    nwProveField = "ProveField"
    nwProveIndex = "ProveIndex"
    nwUnreachableElse = "UnreachableElse"
    nwUnreachableCode = "UnreachableCode"
    nwIndexCheck = "IndexCheck"
    nwGcUnsafe = "GcUnsafe"
    nwGcUnsafe2 = "GcUnsafe2"
    nwUninit = "Uninit"
    nwGcMem = "GcMem"
    nwDestructor = "Destructor"
    nwLockLevel = "LockLevel"
    nwResultShadowed = "ResultShadowed"
    nwSpacing = "Spacing"
    nwCaseTransition = "CaseTransition"
    nwCycleCreated = "CycleCreated"
    nwObservableStores = "ObservableStores"
    nwStrictNotNil = "StrictNotNil"
    nwResultUsed = "ResultUsed"
    nwCannotOpen = "CannotOpen"
    nwFileChanged = "FileChanged"
    nwEnumConv = "EnumConv"
    nwAnyEnumConv = "AnyEnumConv"
    nwHoleEnumConv = "HoleEnumConv"
    nwCStringConv = "CStringConv"
    nwEffect = "Effect"
    nwUser = "User"

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


proc getCwdNimDump(): NimState =
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

proc runsFromDir*(dir: AbsDir, first: seq[AbsFile] = @[]): seq[NimRun] =
  for file in first & toSeq(walkDir(dir, AbsFile, exts = @["nim"])):
    result.add NimRun(
      file: file,
      outfile: file.withExt("out")
    )

proc parseReport(report: string): NimReport =
  var str = initPosStr(report)
  while ?str:
    var part: NimReportPart

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

proc runTestDir*(
    dir: AbsDir,
    dump: NimState,
    maxfail: int = high(int),
    first: seq[AbsFile] = @[]
  ) =

  let runs = runsFromDir(dir, first).mapIt((makeCmd(it, dump), it))
  var failCount = maxfail
  for (res, run) in runShellResult(runs, maxPool = 2):
    let reports = getReports(res, run)
    if reports.anyIt(it of nrError):
      inc failCount
      if maxFail < failCount:
        break


    else:
      echov run.file, "ok"

when isMainModule:
  startHax()
  runTestDir(
    AbsDir(relToSource"../../../tests"),
    getCwdNimDump(),
    1,
    @[AbsFile("/mnt/workspace/github/hmisc/tests/tMacros.nim")])
