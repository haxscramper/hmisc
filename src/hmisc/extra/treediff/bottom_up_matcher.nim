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
# - GreedyBottomUpMatcher.java
# - AbstractBottomUpMatcher.java
# - CompleteBottomUpMatcher.java

import 
  ./jcommon, 
  ./matcher, 
  ./mapping_store, 
  ./tree, 
  ./similarity_metrics,
  ./zs_matcher


{.this: this.}

type
  BottomUpMatcher* = ref object of Matcher
    sizeThreshold*: int
    simThreshold*: float

  CompleteBottomUpMatcher* = ref object of BottomUpMatcher
  GreedyBottomUpMatcher* = ref object of BottomUpMatcher

const
  defaultSizeThreshold = 1000
  defaultSimThreshold = 0.5

proc newBottomUpMatcher*(): BottomUpMatcher =
  BottomUpMatcher(
    name: "BottomUpMatcher", 
    sizeThreshold: defaultSizeThreshold,
    simThreshold: defaultSimThreshold
  )

proc getDstCandidates*(this: BottomUpMatcher; mappings: MappingStore;
                       src: Tree): seq[Tree] =
  var seeds: seq[Tree] 
  for c in src.getDescendants():
    if (mappings.isSrcMapped(c)):
      seeds.add(mappings.getDstForSrc(c))
  var candidates: seq[Tree] 
  var visited: HashSet[Tree]
  for seed in mitems(seeds):
    while (seed.getParent().notNil()):
      var parent: Tree = seed.getParent()
      if (visited.contains(parent)):
        break
      visited.incl parent
      if (parent.getType() == src.getType() and
          not(mappings.isDstMapped(parent) or parent.isRoot())):
        candidates.add(parent)
      seed = parent
  return candidates

proc lastChanceMatch*(this: BottomUpMatcher; mappings: MappingStore;
                      src: Tree; dst: Tree): void =
  if (src.getMetrics().size < sizeThreshold or
      dst.getMetrics().size < sizeThreshold):
    var m: Matcher = newZsMatcher()
    var zsMappings: MappingStore = m.match(src, dst, newMappingStore(src, dst))
    for candidate in zsMappings:
      var srcCand: Tree = candidate.first
      var dstCand: Tree = candidate.second
      if (mappings.isMappingAllowed(srcCand, dstCand)):
        mappings.addMapping(srcCand, dstCand)

proc getSizeThreshold*(this: BottomUpMatcher): int =
  return sizeThreshold

proc setSizeThreshold*(this: BottomUpMatcher; sizeThreshold: int): void =
  this.sizeThreshold = sizeThreshold

proc getSimThreshold*(this: BottomUpMatcher): float =
  return simThreshold

proc setSimThreshold*(this: BottomUpMatcher; simThreshold: float): void =
  this.simThreshold = simThreshold

proc newCompleteBottomUpMatcher*(): CompleteBottomUpMatcher = 
  CompleteBottomUpMatcher(
    name: "CompleteBottomUpMatcher", 
    sizeThreshold: defaultSizeThreshold,
    simThreshold: defaultSimThreshold
  )

proc newGreedyBottomUpMatcher*(): GreedyBottomUpMatcher =
  GreedyBottomUpMatcher(
    name: "GreedyBottomUpMatcher", 
    sizeThreshold: defaultSizeThreshold,
    simThreshold: defaultSimThreshold
  )
 
method match*(this: CompleteBottomUpMatcher; src: Tree; dst: Tree;
            mappings: MappingStore): MappingStore =
  for t in src.postOrder():
    if (t.isRoot()):
      mappings.addMapping(t, dst)
      lastChanceMatch(mappings, t, dst)
      break
    else:
      if (not(mappings.isSrcMapped(t) or t.isLeaf())):
        var srcCandidates: seq[Tree] = t.getParents().filter(
          proc (p: Tree): bool = p.getType() == t.getType())

        var dstCandidates: seq[Tree] = getDstCandidates(mappings, t)
        var srcBest: Tree = nil
        var dstBest: Tree = nil
        var max: float = -(1.0)
        for srcCand in srcCandidates:
          for dstCand in dstCandidates:
            var sim: float = jaccardSimilarity(srcCand, dstCand, mappings)
            if (sim > max and sim >= simThreshold):
              max = sim
              srcBest = srcCand
              dstBest = dstCand

        if (srcBest.notNil()):
          lastChanceMatch(mappings, srcBest, dstBest)
          mappings.addMapping(srcBest, dstBest)
  return mappings

method match*(this: GreedyBottomUpMatcher; src: Tree; dst: Tree;
            mappings: MappingStore): MappingStore =
  for t in src.postOrder():
    if (t.isRoot()):
      mappings.addMapping(t, dst)
      lastChanceMatch(mappings, t, dst)
      break
    else:
      if (not(mappings.isSrcMapped(t) or t.isLeaf())):
        var candidates: seq[Tree] = getDstCandidates(mappings, t)
        var best: Tree = nil
        var max: float = -(1.0)
        for cand in candidates:
          var sim: float = diceSimilarity(t, cand, mappings)
          if (sim > max and sim >= simThreshold):
            max = sim
            best = cand
        if (best.notNil()):
          lastChanceMatch(mappings, t, best)
          mappings.addMapping(t, best)
  return mappings
