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
# - SimilarityMetrics.java

import ./jcommon, ./tree, ./mapping_store

proc numberOfMappedDescendants*(src: Tree; dst: Tree;
                                mappings: MappingStore): int =
  var dstDescendants: HashSet[Tree] = toHashSet(dst.getDescendants())
  var mappedDescendants: int = 0
  for srcDescendant in src.getDescendants():
    if (mappings.isSrcMapped(srcDescendant)):
      var dstForSrcDescendant = mappings.getDstForSrc(srcDescendant)
      if (dstDescendants.contains(dstForSrcDescendant)):
        postInc(mappedDescendants)
  return mappedDescendants

proc chawatheSimilarity*(src: Tree; dst: Tree;
                         mappings: MappingStore): float =
  var max: int = max(src.getDescendants().len(), dst.getDescendants().len())
  return cast[float](numberOfMappedDescendants(src, dst, mappings)) /
      cast[float](max)

proc overlapSimilarity*(src: Tree; dst: Tree;
                        mappings: MappingStore): float =
  var min: int = min(src.getDescendants().len(), dst.getDescendants().len())
  return cast[float](numberOfMappedDescendants(src, dst, mappings)) /
      cast[float](min)

proc diceCoefficient*(
    commonElementsNb: int;
    leftElementsNb: int; 
    rightElementsNb: int
  ): float =
  return 2.0 * commonElementsNb.float() / float(leftElementsNb + rightElementsNb)



proc jaccardIndex*(commonElementsNb: int;
                   leftElementsNb: int; rightElementsNb: int): float =
  var denominator: float = float(leftElementsNb + rightElementsNb - commonElementsNb)
  var res: float = commonElementsNb.float() / denominator
  return res



proc diceSimilarity*(src: Tree; dst: Tree;
                     mappings: MappingStore): float =
  return diceCoefficient(numberOfMappedDescendants(src, dst, mappings),
                         src.getDescendants().len(), dst.getDescendants().len())

proc jaccardSimilarity*(src: Tree; dst: Tree;
                        mappings: MappingStore): float =
  return jaccardIndex(numberOfMappedDescendants(src, dst, mappings),
                      src.getDescendants().len(), dst.getDescendants().len())

