import hmisc/wrappers/wraphelp
import std/bitops

{.emit: """/*TYPESECTION*/
#include <stdio.h>

typedef enum CEnum { enc1 = 1 << 0, enc2 = 1 << 1, en16 = 1 << 16 } CEnum;

bool takesBitSet1(unsigned int flag) { return flag & enc1; }
bool takesBitSet2(unsigned int flag) { return flag & enc2; }
bool takesBitSet12(unsigned int flag) { return (flag & enc1) && (flag & enc2); }
""".}

type
  CEnum {.importc.} = enum
    enc1
    enc2
    enc16

proc takesBitSet1(s: cuint): bool {.importc.}
proc takesBitSet2(s: cuint): bool {.importc.}
proc takesBitSet12(s: cuint): bool {.importc.}

assert takesBitSet1(setcast[cuint, CEnum]({enc1}))
assert takesBitSet2(setcast[cuint, CEnum]({enc2}))
assert takesBitSet12(setcast[cuint, CEnum]({enc1, enc2}))


proc getBits[I](i: I): seq[int] =
  for bit in countdown(sizeof(i) * 8 - 1, 0):
    result.add int(testBit(i, bit))

type En1 = enum en1Item
assert getBits(setcast[uint8, En1]({en1Item})) == @[0, 0, 0, 0, 0, 0, 0, 1]

type En2 = enum en2Item1, en2Item2
assert getBits(setcast[uint8, En2]({en2Item1, en2Item2})) == @[0, 0, 0, 0, 0, 0, 1, 1]

type
  En9 = enum
    en9Item1
    en9Item2
    en9Item3
    en9Item4
    en9Item5
    en9Item6
    en9Item7
    en9Item8
    en9Item9

assert getBits(setcast[uint16, En9]({en9Item1})) == @[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
assert setcast[uint16, En9]({en9Item1}) == 1
assert setcast[uint16, En9]({en9Item2}) == 2
assert setcast[uint16, En9]({en9Item2, en9Item1}) == 3

assert getBits(setcast[uint16, En9]({en9Item1, en9Item5, en9Item9})) == @[
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]
