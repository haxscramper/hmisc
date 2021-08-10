# This file is part of GumTree.
#
# GumTree is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GumTree is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with GumTree.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright 2011-2015 Jean-Rémy Falleri <jr.falleri@gmail.com>
# Copyright 2011-2015 Floréal Morandat <florealm@gmail.com>

# Nim implementation have been automatically generated, and /mostly/ based on
# the following files from the original project:
#
# - MappingStore.java
# - MultiMappingStore.java

{.this: this.}

import ./tree, ./jcommon

import
  hmisc/extra/hdrawing/term_buf

import
  std/strutils

type
  Mapping* = tuple[first, second: Tree]
  MappingStore* = ref object
    src*, dst*: Tree
    srcToDst, dstToSrc: Table[Tree, Tree]


proc newMapping*(a, b: Tree): Mapping = (a, b)

proc size*(this: MappingStore): int =
  return srcToDst.len()

iterator items*(this: MappingStore): Mapping =
  for src, dst in this.srcToDst:
    yield (src, dst)

import hmisc/other/blockfmt

proc pprint*(this: MappingStore) =
  let conf = pconf(
    stringPaths = matchField("first", "second"),
    forceLayouts = @{
      matchAll(): forceStack()
    }
  )

  pprint this.toSeq(), conf = conf

import hmisc/algo/clformat

proc `$`*(this: MappingStore): string =
  const box = AsciiBox.double
  for src, dst in this.srcToDst:
    result.add withBufStr(
      block:
        buf[0, 0] = &"""
{box.leftCross}{box.horizontal}[{src}]
{box.leftCross}{box.horizontal}[{dst}]
{box.vertical}
"""
        let srcStr = src.treeRepr(maxdepth = 3)
        let dstStr = dst.treeRepr(maxdepth = 3)

        buf[3, 1] = srcStr
        buf[3, srcStr.width() + 2] = dstStr

        let maxLine =  max(count(srcStr, '\n'), count(dstStr, '\n')) + 2
        for line in 0 ..< maxLine:
          buf[2 + line, 0] = box.vertical

        buf[2 + maxLine, 0] = box.horizontal.repeat(20).join("")
        buf[2 + maxLine, 0] = box.leftCross
    )

    result.add "\n"

#     result.add &"""
# {box.upLeft}[{src}]
# {box.leftCross}[{dst}]
# {box.downLeft}{box.horizontal.repeat(10)}
# """

proc addMapping*(this: MappingStore; src: Tree; dst: Tree): void =
  assertRef src
  assertRef dst
  srcToDst[src] = dst
  dstToSrc[dst] = src

proc addMappingRecursively*(this: MappingStore; src: Tree; dst: Tree): void =
  addMapping(src, dst)
  block:
    var i: int = 0
    while i < src.getChildren().len():
      addMappingRecursively(src.getChild(i), dst.getChild(i))
      postInc(i)

proc newMappingStore*(ms: MappingStore): MappingStore =
  new(result)
  assertRef ms.src
  assertRef ms.dst
  result.src = ms.src
  result.dst = ms.dst
  for m in ms:
    result.addMapping(m.first, m.second)

proc newMappingStore*(src: Tree; dst: Tree): MappingStore =
  new(result)
  assertRef src
  assertRef dst
  result.src = src
  result.dst = dst


proc getDstForSrc*(this: MappingStore; src: Tree): Tree =
  return srcToDst[src]

proc getSrcForDst*(this: MappingStore; dst: Tree): Tree =
  return dstToSrc[dst]

proc isSrcMapped*(this: MappingStore; src: Tree): bool =
  return srcToDst.contains(src)

proc isDstMapped*(this: MappingStore; dst: Tree): bool =
  return dstToSrc.contains(dst)

proc areBothUnmapped*(this: MappingStore; src: Tree; dst: Tree): bool =
  return not(isSrcMapped(src) or isDstMapped(dst))

proc areSrcsUnmapped*(this: MappingStore; srcs: seq[Tree]): bool =
  for src in srcs:
    if (isSrcMapped(src)):
      return false
  return true

proc areDstsUnmapped*(this: MappingStore; dsts: seq[Tree]): bool =
  for dst in dsts:
    if (isDstMapped(dst)):
      return false
  return true

proc hasUnmappedSrcChildren*(this: MappingStore; t: Tree): bool =
  for c in t.getDescendants():
    if (not(isSrcMapped(c))):
      return true
  return false

proc hasUnmappedDstChildren*(this: MappingStore; t: Tree): bool =
  for c in t.getDescendants():
    if (not(isDstMapped(c))):
      return true
  return false

proc has*(this: MappingStore; src: Tree; dst: Tree): bool =
  return srcToDst[src] == dst


proc isMappingAllowed*(this: MappingStore; src: Tree; dst: Tree): bool =
  return src.hasSameType(dst) and areBothUnmapped(src, dst)



type
  MultiMappingStore* = ref object
    srcToDsts*: Table[Tree, HashSet[Tree]]
    dstToSrcs*: Table[Tree, HashSet[Tree]]



proc addMapping*(this: MultiMappingStore; src: Tree; dst: Tree): void =
  if (not(srcToDsts.contains(src))):
    srcToDsts[src] = initHashSet[Tree]()

  srcToDsts[src].incl(dst)

  if (not(dstToSrcs.contains(dst))):
    dstToSrcs[dst] = initHashSet[Tree]()

  dstToSrcs[dst].incl(src)

proc newMultiMappingStore*(mappings: HashSet[Mapping]): MultiMappingStore =
  new(result)
  for m in mappings:
    result.addMapping(m.first, m.second)

proc newMultiMappingStore*(): MultiMappingStore =
  new(result)

proc getMappings*(this: MultiMappingStore): HashSet[Mapping] =
  var mappings: HashSet[Mapping]
  for src, _ in srcToDsts:
    for dst in srcToDsts[src]:
      mappings.incl(newMapping(src, dst))



proc removeMapping*(this: MultiMappingStore; src: Tree; dst: Tree): void =
  srcToDsts[src].excl(dst)
  dstToSrcs[dst].excl(src)

proc size*(this: MultiMappingStore): int =
  return getMappings().len()

proc getDsts*(this: MultiMappingStore; src: Tree): HashSet[Tree] =
  return srcToDsts[src]

proc getSrcs*(this: MultiMappingStore; dst: Tree): HashSet[Tree] =
  return dstToSrcs[dst]

proc allMappedSrcs*(this: MultiMappingStore): HashSet[Tree] =
  for key, _ in this.srcToDsts:
    result.incl key

proc allMappedDsts*(this: MultiMappingStore): HashSet[Tree] =
  for key, _ in this.dstToSrcs:
    result.incl key

proc hasSrc*(this: MultiMappingStore; src: Tree): bool =
  return srcToDsts.contains(src)

proc hasDst*(this: MultiMappingStore; dst: Tree): bool =
  return dstToSrcs.contains(dst)

proc has*(this: MultiMappingStore; src: Tree; dst: Tree): bool =
  return srcToDsts[src].contains(dst)

proc isSrcUnique*(this: MultiMappingStore; src: Tree): bool =
  return getDsts(src).len() == 1

proc isDstUnique*(this: MultiMappingStore; dst: Tree): bool =
  return getSrcs(dst).len() == 1

proc toString*(this: MultiMappingStore): string =
  for t, _ in srcToDsts:
    var l: string = srcToDsts[t].mapIt($it).join("\", \"")

    result.add &"\"{t} -> {l}\"\n"

iterator items*(this: MultiMappingStore): Mapping =
  for mapping in getMappings():
    yield mapping
