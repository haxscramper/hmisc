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
# - ZsMatcher.java

import ./jcommon, ./mapping_store, ./tree, ./matcher
import std/deques

{.this: this.}

type
  ZsMatcher* = ref object of Matcher
    mappings*: MappingStore
    zsSrc*: ZsTree
    zsDst*: ZsTree
    treeDist*: seq[seq[float]]
    forestDist*: seq[seq[float]]

  ZsTree* = ref object
    nodeCount*: int
    leafCount*: int
    llds*: seq[int]
    labels*: seq[Tree]
    kr*: seq[int]

proc newZsMatcher*(): ZsMatcher = ZsMatcher(name: "ZsMatcher")

proc setITree*(this: ZsTree; i: int; tree: Tree): void =
  labels[i - 1] = tree
  if (nodeCount < i):
    nodeCount = i

proc setLld*(this: ZsTree; i: int; lld: int): void =
  llds[i - 1] = lld - 1
  if (nodeCount < i):
    nodeCount = i


proc lld*(this: ZsTree; i: int): int =
  return llds[i - 1] + 1

proc isLeaf*(this: ZsTree; i: int): bool =
  return this.lld(i) == i

proc tree*(this: ZsTree; i: int): Tree =
  return labels[i - 1]

proc getFirstLeaf*(this: ZsMatcher; t: Tree): Tree =
  var current: Tree = t
  while (not(current.isLeaf())):
    current = current.getChild(0)
  return current


proc setKeyRoots*(this: ZsTree): void =
  kr = nseq(leafCount + 1, 0)
  var visited: seq[bool] = nseq(nodeCount + 1, false)
  fill(visited, false)
  var k: int = kr.len - 1
  block:
    var i: int = nodeCount
    while i >= 1:
      if (not(visited[lld(i)])):
        kr[k] = i
        visited[lld(i)] = true
        postDec(k)
      postDec(i)

proc newZsTree*(matcher: ZsMatcher, t: Tree): ZsTree =
  new(result)
  result.nodeCount = t.getMetrics().size
  result.leafCount = 0
  result.llds = nseq(result.nodeCount, 0)
  result.labels = nseq(result.nodeCount, Tree())
  var idx: int = 1
  var tmpData: Table[Tree, int]
  for n in t.postOrder():
    tmpData[n] = idx
    result.setITree(idx, n)
    result.setLld(idx, tmpData[matcher.getFirstLeaf(n)])
    if (n.isLeaf()):
      postInc(result.leafCount)
    postInc(idx)
  result.setKeyRoots()


  
proc getDeletionCost*(this: ZsMatcher; n: Tree): float =
  return 1.0

proc getInsertionCost*(this: ZsMatcher; n: Tree): float =
  return 1.0

proc getUpdateCost*(this: ZsMatcher; n1: Tree; n2: Tree): float =
  if (n1.getType() == n2.getType()):
    if ("\"\"" == n1.getLabel() or "\"\"" == n2.getLabel()):
      return 1.0
    else:
      return 1.0 
          # StringMetrics.qGramsDistance().compare(n1.getLabel(), n2.getLabel())
  else:
    return high(float)


proc forestDist*(this: ZsMatcher; i: int; j: int): void =
  forestDist[zsSrc.lld(i) - 1][zsDst.lld(j) - 1] = 0
  for di in zsSrc.lld(i) .. i:
    var costDel: float = getDeletionCost(zsSrc.tree(di))
    forestDist[di][zsDst.lld(j) - 1] = 
      forestDist[di - 1][zsDst.lld(j) - 1] + costDel

    for dj in zsDst.lld(j) .. j:
      var costIns: float = getInsertionCost(zsDst.tree(dj))
      forestDist[zsSrc.lld(i) - 1][dj] = 
        forestDist[zsSrc.lld(i) - 1][dj - 1] + costIns
        
      if (zsSrc.lld(di) == zsSrc.lld(i) and zsDst.lld(dj) == zsDst.lld(j)):
        var costUpd: float = getUpdateCost(zsSrc.tree(di), zsDst.tree(dj))
        forestDist[di][dj] = min(
          min(forestDist[di - 1][dj] + costDel, forestDist[di][dj - 1] + costIns),
          forestDist[di - 1][dj - 1] + costUpd)
          
        treeDist[di][dj] = forestDist[di][dj]
      else:
        forestDist[di][dj] = min(min(forestDist[di - 1][dj] + costDel,
            forestDist[di][dj - 1] + costIns), forestDist[zsSrc.lld(di) - 1][
            zsDst.lld(dj) - 1] +
            treeDist[di][dj])

proc computeTreeDist*(this: ZsMatcher): seq[seq[float]] =
  treeDist = nseq(zsSrc.nodeCount + 1, zsDst.nodeCount + 1, 0.0)
  forestDist = nseq(zsSrc.nodeCount + 1, zsDst.nodeCount + 1, 0.0)
  for i in 1 ..< zsSrc.kr.len:
    for j in 1 ..< zsDst.kr.len:
      forestDist(zsSrc.kr[i], zsDst.kr[j])

  return treeDist

proc match*(this: ZsMatcher): void =
  discard computeTreeDist()
  var rootNodePair: bool = true
  var treePairs: Deque[seq[int]]
  treePairs.addFirst(@[zsSrc.nodeCount, zsDst.nodeCount])
  while (not(treePairs.len() == 0)):
    var treePair: seq[int] = treePairs.popFirst()
    var lastRow: int = treePair[0]
    var lastCol: int = treePair[1]
    if (not(rootNodePair)):
      forestDist(lastRow, lastCol)
    rootNodePair = false
    var firstRow: int = zsSrc.lld(lastRow) - 1
    var firstCol: int = zsDst.lld(lastCol) - 1
    var row: int = lastRow
    var col: int = lastCol
    while (row > firstRow or col > firstCol):
      if (row > firstRow and
          forestDist[row - 1][col] + 1.0 == forestDist[row][col]):
        postDec(row)
      else:
        if (col > firstCol and
            forestDist[row][col - 1] + 1.0 == forestDist[row][col]):
          postDec(col)
        else:
          if (zsSrc.lld(row) - 1 == zsSrc.lld(lastRow) - 1 and
              zsDst.lld(col) - 1 == zsDst.lld(lastCol) - 1):
            var tSrc: Tree = zsSrc.tree(row)
            var tDst: Tree = zsDst.tree(col)
            if (tSrc.getType() == tDst.getType()):
              mappings.addMapping(tSrc, tDst)

            else:
              raise newLogicError("\"Should not map incompatible nodes.\"")

            postDec(row)
            postDec(col)
          else:
            treePairs.addFirst(@[row, col])
            row = zsSrc.lld(row) - 1
            col = zsDst.lld(col) - 1

method match*(
    this: ZsMatcher; 
    src: Tree; 
    dst: Tree; 
    mappings: MappingStore
  ): MappingStore =
  this.zsSrc = this.newZsTree(src)
  this.zsDst = this.newZsTree(dst)
  this.mappings = mappings
  this.match()
  return mappings






