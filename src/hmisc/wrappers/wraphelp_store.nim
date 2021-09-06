import
  ../other/oswrap,
  ../core/all,
  std/[options, macros, json, strutils, strformat]

type
  CxxSpellingLocation* = object
    file*: AbsFile
    line*, column*: int


  CxxHeaderKind* = enum
    ## Kind of the nim input header
    chkGlobal ## Global header file, must be installed and accessible via `includepath`
    ## when wrappers are compiled
    chkAbsolute ## Absolute path to the base header file
    chkPNode ## Unconstrained PNode - can be anything

  LibImport* = object
    library*: string
    importPath*: seq[string]

  CxxHeader* = object
    case kind*: CxxHeaderKind
      of chkGlobal:
        global*: string

      of chkAbsolute:
        file*: AbsFile

      of chkPNode:
        other*: string

  CxxBase* = object of RootObj
    iinfo*: LineInfo
    spellingLocation*: Option[CxxSpellingLocation]
    nimName*: string
    cxxName*: seq[string]
    icpp*: string
    private*: bool
    header*: Option[CxxHeader]
    docComment*: seq[string]
    haxdocIdent* {.requiresinit.}: JsonNode

  CxxTypeKind* = enum
    ## Kind of the wrapped Cxx type
    ctkIdent ## Identifier with optional list of template parameters
    ctkProc ## Procedural (callback) type
    ctkPtr


  CxxType* = ref object
    case kind*: CxxTypeKind
      of ctkPtr:
        wrapped*: CxxType

      of ctkIdent:
        isConst*: bool
        isMutable*: bool
        isComplex*: bool

        typeImport*: LibImport
        nimName*: string
        cxxName*: seq[string]
        genParams*: seq[CxxType]
        default*: Option[CxxType]

      of ctkProc:
        arguments*: seq[CxxArg]
        returnType*: CxxType


  CxxProcKind* = enum
    ## Procedure kind
    cpkRegular ## Regular proc: `hello()`
    cpkOperator ## Operator: `*`
    cpkHook ## Destructor/sink (etc.) hook: `=destroy`
    cpkAssgn ## Assignment proc `field=`

  CxxProc* = object of CxxBase
    arguments*: seq[CxxArg]
    returnType*: CxxType
    genParams*: seq[CxxType]
    kind*: CxxProcKind

    constructorOf*: Option[string]
    isConst*: bool
    methodOf*: Option[CxxType]

  CxxArg* = object of CxxBase
    nimType*: CxxType
    default*: Option[string] # ???

  CxxField* = object of CxxBase
    nimType*: CxxType
    isStatic*: bool

  CxxEnumValue* = object
    baseName*: string
    cxxName*: seq[string]
    nimName*: string
    value*: BiggestInt
    comment*: string

  CxxAlias* = object of CxxBase
    isDistinct*: bool
    newAlias*: CxxType
    baseType*: CxxType

  CxxEnum* = object of CxxBase
    values*: seq[CxxEnumValue]


  CxxObjectKind* = enum
    gokUnion
    gokStruct
    gokClass

  CxxObject* = object of CxxBase
    kind*: CxxObjectKind
    parent*: seq[CxxType]
    genParams*: seq[CxxType]
    mfields*: seq[CxxField]
    methods*: seq[CxxProc]
    nested*: seq[CxxEntry]
    isByref*: bool

  CxxForward* = object of CxxBase

  CxxMacro* = object of CxxBase
    arguments*: seq[string]


  CxxEntryKind* = enum
    cekEnum ## Enum wrapper
    cekProc ## Method, operator, or function
    cekObject ## Struct, union, or class
    cekAlias ## `typedef` or `using`
    cekPass ## Raw passthrough

    cekForward ## Forward declaration for struct/union/class/enum
    cekImport ## Import statement
    cekEmpty

    cekMacro
    cekComment

  CxxEntry* = ref object
    case kind*: CxxEntryKind
      of cekEnum:
        cxxEnum*: CxxEnum

      of cekProc:
        cxxProc*: CxxProc

      of cekObject:
        cxxObject*: CxxObject

      of cekAlias:
        cxxAlias*: CxxAlias

      of cekForward:
        cxxForward*: CxxForward

      of cekComment:
        cxxComment*: string

      of cekMacro:
        cxxMacro*: CxxMacro

      else:
        discard

  CxxFile* = object
    entries*: seq[CxxEntry]
    savePath*: LibImport

func isConstructor*(pr: CxxProc): bool =
  pr.constructorOf.isSome()

func isMethod*(pr: CxxProc): bool =
  pr.methodOf.isSome()

func getConstructed*(pr: CxxProc): string =
  pr.constructorOf.get()

func getIcppName*(pr: CxxProc, asMethod: bool = false): string =
  if asMethod:
    pr.cxxName[^1]

  else:
    pr.cxxName.join("::")

func initCxxHeader*(global: string): CxxHeader =
  CxxHeader(global: global, kind: chkGlobal)

func initCxxHeader*(file: AbsFile): CxxHeader =
  CxxHeader(kind: chkAbsolute, file: file)

func initCxxArg*(name: string, argType: CxxType): CxxArg =
  CxxArg(nimType: argType, nimName: name, haxdocIdent: newJNull())


func wrap*(wrapped: CxxType, kind: CxxTypeKind): CxxType =
  if kind == ctkIdent:
    result = wrapped

  else:
    result = CxxType(kind: kind)
    result.wrapped = wrapped

func initCxxType*(head: string, genParams: seq[CxxType] = @[]): CxxType =
  CxxType(kind: ctkIdent, nimName: head, genParams: @genParams)

func getReturn*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): CxxType =

  if pr.isConstructor:
    result = initCxxType(pr.getConstructed()).wrap(onConstructor)

  else:
    assertRef pr.returnType
    result = pr.returnType

func getIcpp*(
    pr: CxxProc, onConstructor: CxxTypeKind = ctkIdent): string =

  if pr.isConstructor:
    case onConstructor:
      of ctkIdent: result = &"{pr.getConstructed()}(@)"
      of ctkPtr: result = &"new {pr.getConstructed()}(@)"
      else: raise newUnexpectedKindError(onConstructor)

  else:
    if pr.isMethod:
      result = &"#.{pr.getIcppName(true)}(@)"

    else:
      result = &"{pr.getIcppName()}()"



func initCxxType*(arguments: seq[CxxArg], returnType: CxxType): CxxType =
  CxxType(
    kind: ctkProc, arguments: arguments, returnType: returnType)

func initCxxObject*(nimName, cxxName: string): CxxObject =
  CxxObject(nimName: nimName, cxxName: @[cxxName], haxdocIdent: newJNull())

func initCxxProc*(
    nimName, cxxName: string,
    arguments: seq[CxxArg] = @[], returnType: CxxType = initCxxType("void")
  ): CxxProc =

  CxxProc(
    nimName: nimName,
    cxxName: @[cxxName],
    haxdocIdent: newJNull(),
    returnType: returnType,
    arguments: arguments
  )


func add*(pr: var CxxProc, arg: CxxArg) =
  pr.arguments.add arg

func setHeaderRec*(entry: var CxxEntry, header: CxxHeader) =
  case entry.kind:
    of cekEnum: entry.cxxEnum.header = some header
    of cekProc: entry.cxxProc.header = some header
    of cekAlias: entry.cxxAlias.header = some header
    of cekObject:
      entry.cxxObject.header = some header
      for meth in mitems(entry.cxxObject.methods):
        meth.header = some header

      for nest in mitems(entry.cxxObject.nested):
        setHeaderRec(nest, header)

    else:
      discard

func box*(en: CxxEnum): CxxEntry =
  CxxEntry(kind: cekEnum, cxxEnum: en)

func box*(en: CxxForward): CxxEntry =
  CxxEntry(kind: cekForward, cxxForward: en)

func box*(ob: CxxObject): CxxEntry =
  CxxEntry(kind: cekObject, cxxObject: ob)

func box*(en: CxxProc): CxxEntry =
  CxxEntry(kind: cekProc, cxxProc: en)

func box*(en: CxxAlias): CxxEntry =
  CxxEntry(kind: cekAlias, cxxAlias: en)

func box*(en: CxxMacro): CxxEntry =
  CxxEntry(kind: cekMacro, cxxMacro: en)

func add*(
    s: var seq[CxxEntry],
    other: CxxMacro | CxxAlias | CxxObject | CxxForward | CxxProc
  ) =

  s.add box(other)
