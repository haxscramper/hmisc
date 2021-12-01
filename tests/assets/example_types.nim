## Example type definitions to be used in the procedure calls.

import std/[tables, options, strtabs, intsets, lists, sets]

type
  ImRange* = range[0 .. 10]
  ImArray* = array[3, int]

  ImEnum* = enum One, Two, Three
  ImRefOfRoot* = ref object of RootObj
    a*: int

  ImRefOfRef* = ref object of ImRefOfRoot
    b*: int

  ImObjOfRoot* = object of RootObj
    a*: int

  ImObjOfObj* = object of ImObjOfRoot
    b*: int

  ImDist* = distinct string

  ImObj* = object
    x*: int
    y*: float

  ImEnumKey* = enum
    Even
    Odd

  ImEnumArray* = array[ImEnumKey, string]

  ImRequiresField* = object
    id* {.requiresinit.}: int

  ImRequires* {.requiresinit.} = object
    value*: int

  ImRef* = ref object
    fInt*: int
    fFloat*: float
    fString*: string
    fEmumKey*: ImEnumKey
    fObj*: ImObj
    fSet*: set[ImEnum]
    fRange*: ImRange

    fTuple*: (string, int)
    fTuple2*: (ImVariant, ImVariant, ImVariant, ImVariant, ImVariant)
    fSeq*: seq[string]
    fEnumArray*: ImEnumArray
    fIntArray*: ImArray
    fDist*: ImDist

    fRefOrRoot*: ImRefOfRoot
    fRefOfRef*: ImRefOfRef

    fObjOfRoot*: ImObjOfRoot
    fObjOfObj*: ImObjOfObj

  ImVariant* = object
    ignore*: bool
    case kind*: ImEnumKey
      of Even:
        even*: int

      of Odd:
        odd*: bool
        case also*: uint8
          of 3:
            discard

          of 4:
            fld1, fld2, fld3*: float

          else:
            `field + stropping`*: string

const singleEnum* = One
const enumSet* = {Two, Three}

type
  ImTorture* = ref object
    used*: seq[ImTorture]
    opt*: Option[ImTorture]
    ftables*: tuple[
      tab: Table[string, string],
      otab: OrderedTable[string, string],
      tabr: TableRef[string, string],
      otabr: OrderedTableRef[string, string],
      stabr: StringTableRef,
    ]

    fsets*: tuple[
      iset: IntSet,
      # hset: HashSet[int]
    ]

    fRequires1*: ImRequiresField
    fRequires2*: ImRequires
    fIarrayOffset*: array[2 .. 12, int]
    fIarray*: array[4, int]

    fBitSet*: set[uint8]


    case kind1*: range[0 .. 3]:
      of 0:
        fVar: ImVariant
        # FIXME changing size of this array causes XML serde to fail with -
        # again, absolutely incomprehensible error. 9 elements is fine, 10
        # elements is too much. 10 elements with `makeTorture()` generate
        # `8264` characters of data, while `9` only create `8185`
        # characters.
        fSizeArray*: array[9, float]

        # FIXME Nine-element array + this field cause infinite recursion bug
        # fTest*: char

      of 1:
        fSeq: seq[tuple[a: int, b: float, `c//q`: char]]

      of 2:
        case kind2*: bool
          of true:
            case kind3*: ImEnum
              of singleEnum:
                fRef: ImRef

              of enumSet:
                fRefArr: array[2, ImRef]

          of false:
            discard

      of 3:
        fu8*: uint8
        fi8*: int8

        fu16*: uint16
        fi16*: int16

        fu32*: uint32
        fi32*: int32

        fu64*: uint64
        fi64*: int64

        ff32*: float32
        ff64*: float64

proc default*(t: typedesc[ImRequiresField]): ImRequiresField =
  ImRequiresField(id: 123)

proc default*(t: typedesc[ImRequires]): ImRequires =
  ImRequires(value: 123)

proc makeTorture*(): ImTorture =
  result = ImTorture(
    fBitSet: {0u8, 1, 3, 4, 255},
    fRequires1: default(ImRequiresField), fRequires2: default(ImRequires),
    used: @[
      ImTorture(
        kind1: 0,
        fRequires1: default(ImRequiresField),
        fRequires2: ImRequires(value: 12)),
      ImTorture(
        kind1: 1,
        fRequires1: default(ImRequiresField),
        fRequires2: ImRequires(value: 23)),
      # FIXME adding more values causes XML deserialization to fail on
      # torture test. I have absolutely no idea /why/ - dumping parser
      # trace for the xml events on the simple string shows no issues.
      #

      # ImTorture(
      #   kind1: 2,
      #   fRequires1: default(ImRequiresField),
      #   fRequires2: ImRequires(value: 45)),
      ImTorture(
        fRequires1: default(ImRequiresField),
        fRequires2: default(ImRequires),
        kind1: 3,
        fu8: 0,
        fi8: 1,

        fu16: 3,
        fi16: 12,

        fu32: low(uint32),
        fi32: low(int32),

        fu64: high(uint64),
        fi64: high(int64),

        ff32: 65.12,
        ff64: 64.31
      ),
    ],
    kind1: 2,
    kind2: true,
    kind3: One,
    fRef: ImRef()
  )

  result.used.add result

func `$`*(i: ImRef): string = $(i[])
