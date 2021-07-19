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
# - MappingComparators.java


import 
  ./jcommon, 
  ./mapping_store, 
  ./tree,
  ./similarity_metrics

import
  std/[math]


import
  hmisc/algo/hseq_distance

{.this: this.}

type
  MappingComparators* = ref object
  
  FullMappingComparator* = ref object
    siblingsComparator*: SiblingsSimilarityMappingComparator
    parentsComparator*: ParentsSimilarityMappingComparator
    parentsPositionComparator*: PositionInParentsSimilarityMappingComparator
    positionComparator*: AbsolutePositionDistanceMappingComparator

  SiblingsSimilarityMappingComparator* = ref object
    ms*: MappingStore
    srcDescendants*: Table[Tree, HashSet[Tree]]
    dstDescendants*: Table[Tree, HashSet[Tree]]
    cachedSimilarities*: Table[Mapping, float]

  ParentsSimilarityMappingComparator* = ref object
    cachedSimilarities*: Table[Mapping, float]

  PositionInParentsSimilarityMappingComparator* = ref object
  
  AbsolutePositionDistanceMappingComparator* = ref object
  

proc newSiblingsSimilarityMappingComparator*(ms: MappingStore): 
    SiblingsSimilarityMappingComparator =
  SiblingsSimilarityMappingComparator(ms: ms)

proc newParentsSimilarityMappingComparator*(): ParentsSimilarityMappingComparator = 
  ParentsSimilarityMappingComparator()

proc newPositionInParentsSimilarityMappingComparator*():
    PositionInParentsSimilarityMappingComparator = 
  PositionInParentsSimilarityMappingComparator()

proc newAbsolutePositionDistanceMappingComparator*():
    AbsolutePositionDistanceMappingComparator = 
  AbsolutePositionDistanceMappingComparator()

proc newFullMappingComparator*(ms: MappingStore): FullMappingComparator =
  new(result)
  result.siblingsComparator = newSiblingsSimilarityMappingComparator(ms)
  result.parentsComparator = newParentsSimilarityMappingComparator()
  result.parentsPositionComparator = newPositionInParentsSimilarityMappingComparator()
  result.positionComparator = newAbsolutePositionDistanceMappingComparator()

proc commonDescendantsNb*(this: SiblingsSimilarityMappingComparator; src: Tree;
                          dst: Tree): int =
  if (not(srcDescendants.contains(src))):
    srcDescendants[src] = toHashSet(src.getDescendants())
  if (not(dstDescendants.contains(dst))):
    dstDescendants[dst] = toHashSet(dst.getDescendants())
  var common: int = 0
  for t in srcDescendants[src]:
    var m: Tree = ms.getDstForSrc(t)
    if (m != nil and dstDescendants[dst].contains(m)):
      postInc(common)
  return common

proc cmp*(this: SiblingsSimilarityMappingComparator; m1: Mapping;
              m2: Mapping): int =
  if (m1.first.getParent() == m2.first.getParent() and
      m1.second.getParent() == m2.second.getParent()):
    return 0
  if (not(cachedSimilarities.contains(m1))):
    cachedSimilarities[m1] = diceCoefficient(
        commonDescendantsNb(m1.first.getParent(), m1.second.getParent()),
        srcDescendants[m1.first.getParent()].len(),
        dstDescendants[m1.second.getParent()].len())
  if (not(cachedSimilarities.contains(m2))):
    cachedSimilarities[m2] = diceCoefficient(
        commonDescendantsNb(m2.first.getParent(), m2.second.getParent()),
        srcDescendants[m2.first.getParent()].len(),
        dstDescendants[m2.second.getParent()].len())
  return cmp(cachedSimilarities[m2], cachedSimilarities[m1])



proc cmp*(this: ParentsSimilarityMappingComparator; m1: Mapping; m2: Mapping): int =
  if (m1.first.getParent() == m2.first.getParent() and
      m1.second.getParent() == m2.second.getParent()):
    return 0

  proc typeLabelEq(t1, t2: Tree): bool = 
    hasSameTypeAndLabel(t1, t2)

  if (not(cachedSimilarities.contains(m1))):
    var commonParentsNbInM1: int = longestCommonSubsequence(
        m1.first.getParents(), m1.second.getParents(), typeLabelEq).len()

    var m1Sim: float = diceCoefficient(commonParentsNbInM1,
        m1.first.getParents().len(), m1.second.getParents().len())

    cachedSimilarities[m1] = m1Sim
  if (not(cachedSimilarities.contains(m2))):
    var commonParentsNbInM2: int = longestCommonSubsequence(
        m2.first.getParents(), m2.second.getParents(), typeLabelEq).len()

    var m2Sim: float = diceCoefficient(commonParentsNbInM2,
        m2.first.getParents().len(), m2.second.getParents().len())

    cachedSimilarities[m2] = m2Sim
  return cmp(cachedSimilarities[m2], cachedSimilarities[m1])


proc posVector*(this: PositionInParentsSimilarityMappingComparator; src: Tree): seq[float] =
  var current: Tree = src
  while (current.notNil() and current.getParent().notNil()):
    var parent: Tree = current.getParent()
    var pos: float = cast[float](parent.getChildPosition(current)) /
        cast[float](parent.getChildren().len())
    result.add(pos)
    current = parent
    
proc distance*(this: PositionInParentsSimilarityMappingComparator; m: Mapping): float =
  var posVector1 = posVector(m.first)
  var posVector2 = posVector(m.second)
  var sum: float = 0
  block:
    var i: int = 0
    while i < min(posVector1.len(), posVector2.len()):
      sum +=
          (posVector1[i] - posVector2[i]) *
          (posVector1[i] - posVector2[i])

      postInc(i)

  return sqrt(sum)


proc cmp*(this: PositionInParentsSimilarityMappingComparator; m1: Mapping;
              m2: Mapping): int =
  var m1Distance = distance(m1)
  var m2Distance = distance(m2)
  return cmp(m1Distance, m2Distance)

proc absolutePositionDistance*(this: AbsolutePositionDistanceMappingComparator;
                               src: Tree; dst: Tree): int =
  return abs(src.getMetrics().position - dst.getMetrics().position)

proc cmp*(this: AbsolutePositionDistanceMappingComparator; m1: Mapping;
              m2: Mapping): int =
  var m1PosDist = absolutePositionDistance(m1.first, m1.second)
  var m2PosDist = absolutePositionDistance(m2.first, m2.second)
  return cmp(m1PosDist, m2PosDist)


proc cmp*(this: FullMappingComparator; m1: Mapping; m2: Mapping): int =
  var res = siblingsComparator.cmp(m1, m2)
  if (res != 0):
    return res

  res = parentsComparator.cmp(m1, m2)
  if (res != 0):
    return res

  res = parentsPositionComparator.cmp(m1, m2)
  if (res != 0):
    return res

  return positionComparator.cmp(m1, m2)

proc sort*[T; Cmp: object or ref object](
    sequence: var seq[T], cmpObject: Cmp) =
  sort(sequence, proc(i1, t2: T): int = cmp(cmpObject, i1, t2))