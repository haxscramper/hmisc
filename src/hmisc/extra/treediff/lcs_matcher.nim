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
# - LcsMatcher.java

import ./matcher, ./tree, ./mapping_store, ./jcommon

import hmisc/algo/hseq_distance


type
  LcsMatcher* = ref object of Matcher

proc newLcsMatcher*(): LcsMatcher = LcsMatcher(name: "LcsMatcher")

method match*(this: LcsMatcher; src: Tree; dst: Tree; mappings: MappingStore): MappingStore =
  let mappings = if isNil(mappings): newMappingStore(src, dst) else: mappings

  proc eqCmpTree(tree1, tree2: Tree): bool = 
    hasSameTypeAndLabel(tree1, tree2)


  var srcSeq: seq[Tree] = preOrder(src).toSeq()
  var dstSeq: seq[Tree] = preOrder(dst).toSeq()
  let lcs = longestCommonSubsequence(srcSeq, dstSeq, eqCmpTree)
  let (matches, xIndex, yIndex) = lcs[0]

  for (src, dst) in zip(xIndex, yIndex):
    var t1: Tree = srcSeq[src]
    var t2: Tree = dstSeq[dst]
    mappings.addMapping(t1, t2)
 
  return mappings