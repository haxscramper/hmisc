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

    fBitSet: set[uint8]

    case kind1*: range[0 .. 3]:
      of 0:
        fVar: ImVariant

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

proc makeTorture*(): ImTorture =
  result = ImTorture(
    fBitSet: {0u8, 1, 3, 4, 255},
    used: @[
      ImTorture(kind1: 0),
      ImTorture(kind1: 1),
      ImTorture(kind1: 2),
      ImTorture(
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
    fRef: ImRef(
    )
  )

  result.used.add result
