import
  ../other/oswrap,
  std/[options, macros, json]

type
  CxxSpellingLocation* = object
    file*: AbsFile
    line*, column*: int


  CxxHeaderSpecKind* = enum
    ## Kind of the nim input header
    nhskGlobal ## Global header file, must be installed and accessible via `includepath`
    ## when wrappers are compiled
    nhskAbsolute ## Absolute path to the base header file
    nhskPNode ## Unconstrained PNode - can be anything

  CxxTypeKind* = enum
    ## Kind of the wrapped Cxx type
    ctkIdent ## Identifier with optional list of template parameters
    ctkProc ## Procedural (callback) type

  LibImport* = object
    library*: string
    importPath*: seq[string]

  CxxHeader* = object
    case kind*: CxxHeaderSpecKind
      of nhskGlobal:
        global*: string

      of nhskAbsolute:
        file*: AbsFile

      of nhskPNode:
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

  CxxType* = ref object
    case kind*: CxxTypeKind
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
    genParams*: seq[CxxType]
    mfields*: seq[CxxField]
    methods*: seq[CxxProc]
    nested*: seq[CxxEntry]

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
        saveEnum*: CxxEnum

      of cekProc:
        saveProc*: CxxProc

      of cekObject:
        saveObject*: CxxObject

      of cekAlias:
        saveAlias*: CxxAlias

      of cekForward:
        saveForward*: CxxForward

      of cekComment:
        saveComment*: string

      of cekMacro:
        saveMacro*: CxxMacro

      else:
        discard

  CxxFile* = object
    entries*: seq[CxxEntry]
    savePath*: LibImport


proc box*(en: CxxEnum): CxxEntry =
  CxxEntry(kind: cekEnum, saveEnum: en)

proc box*(en: CxxForward): CxxEntry =
  CxxEntry(kind: cekForward, saveForward: en)

proc box*(ob: CxxObject): CxxEntry =
  CxxEntry(kind: cekObject, saveObject: ob)

proc box*(en: CxxProc): CxxEntry =
  CxxEntry(kind: cekProc, saveProc: en)

proc box*(en: CxxAlias): CxxEntry =
  CxxEntry(kind: cekAlias, saveAlias: en)

proc box*(en: CxxMacro): CxxEntry =
  CxxEntry(kind: cekMacro, saveMacro: en)

proc add*(
    s: var seq[CxxEntry],
    other: CxxMacro | CxxAlias | CxxObject | CxxForward | CxxProc
  ) =

  s.add box(other)
