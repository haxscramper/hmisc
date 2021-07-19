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
# - CliqueSubtreeMatcher.java
# - AbstractSubtreeMatcher.java
# - GreedySubtreeMatcher.java
# - HungarianSubtreeMatcher.java


import 
  ./jcommon,
  ./matcher, 
  ./mapping_store, 
  ./tree,
  ./priority_queue,
  ./comparators,
  ./similarity_metrics,
  ./hungarian_algorithm

{.this: this.}

const
  DEFAULT_MIN_PRIORITY = 1

type
  PriorityCalculator* = proc(tree: Tree): int

  AbstractSubtreeMatcher* = ref object of Matcher
    minPriority*: int
    priorityCalculator*: PriorityCalculator
    src*: Tree
    dst*: Tree
    mappings*: MappingStore

  CliqueSubtreeMatcher* = ref object of AbstractSubtreeMatcher
  HungarianSubtreeMatcher* = ref object of AbstractSubtreeMatcher
  GreedySubtreeMatcher* = ref object of AbstractSubtreeMatcher

  MappingComparator* = ref object
    mappings*: MappingStore
    simMap*: Table[Mapping, seq[float]]
    srcDescendants*: Table[Tree, seq[Tree]]
    dstDescendants*: Table[Tree, HashSet[Tree]]

  CliqueComparator* = ref object
  MultiMappingComparator* = ref object

  
proc newMultiMappingComparator(): MultiMappingComparator =
  MultiMappingComparator()

proc newCliqueComparator(): CliqueComparator =
  CliqueComparator()

proc newAbstractSubtreeMatcher*(calc: PriorityCalculator): AbstractSubtreeMatcher =
  AbstractSubtreeMatcher(
    priorityCalculator: calc,
    name: "AbstractSubtreeMatcher",
    minPriority: DEFAULT_MIN_PRIORITY)

proc newHungarianSubtreeMatcher*(calc: PriorityCalculator): HungarianSubtreeMatcher = 
  HungarianSubtreeMatcher(
    priorityCalculator: calc,
    name: "HungarianSubtreeMatcher",
    minPriority: DEFAULT_MIN_PRIORITY)

proc newGreedySubtreeMatcher*(calc: PriorityCalculator): GreedySubtreeMatcher =
  GreedySubtreeMatcher(
    priorityCalculator: calc,
    name: "GreedySubtreeMatcher",
    minPriority: DEFAULT_MIN_PRIORITY)

proc newCliqueSubtreeMatcher*(calc: PriorityCalculator): CliqueSubtreeMatcher =
  CliqueSubtreeMatcher(
    priorityCalculator: calc,
    name: "CliqueSubtreeMatcher",
    minPriority: DEFAULT_MIN_PRIORITY)

method filterMappings(this: AbstractSubtreeMatcher, multiMappings: MultiMappingStore) {.base.} =
  raise newImplementBaseError(this, "filterMappings")


proc getMaxTreeSize*(this: AbstractSubtreeMatcher): int =
  return max(src.getMetrics().size, dst.getMetrics().size)


proc sim*(this: HungarianSubtreeMatcher; src: Tree; dst: Tree): float =
  var jaccard = jaccardSimilarity(src.getParent(),
      dst.getParent(), mappings)
  var posSrc: int = tern(src.isRoot(), 0, src.getParent().getChildPosition(src))
  var posDst: int = tern(dst.isRoot(), 0, dst.getParent().getChildPosition(dst))
  var maxSrcPos: int = tern(src.isRoot(), 1, src.getParent().getChildren().len())
  var maxDstPos: int = tern(dst.isRoot(), 1, dst.getParent().getChildren().len())
  var maxPosDiff: int = max(maxSrcPos, maxDstPos)
  var pos: float = 1.0 -
      cast[float](abs(posSrc - posDst)) / cast[float](maxPosDiff)
  var po: float = 1.0 -
      cast[float](abs(src.getMetrics().position - dst.getMetrics().position)) /
      cast[float](this.getMaxTreeSize())
  return 100 * jaccard + 10 * pos + po

proc cost*(this: HungarianSubtreeMatcher; src: Tree; dst: Tree): float =
  return 111.0 - sim(src, dst)

proc impact*(this: MultiMappingComparator; m: MultiMappingStore): int =
  var impact: int = 0
  for src in m.allMappedSrcs():
    var pSize: int = src.getParents().len()
    if (pSize > impact):
      impact = pSize
  for src in m.allMappedDsts():
    var pSize: int = src.getParents().len()
    if (pSize > impact):
      impact = pSize
  return impact

proc cmp*(this: MultiMappingComparator; m1: MultiMappingStore;
              m2: MultiMappingStore): int =
  return cmp(impact(m1), impact(m2))


proc minDepth*(this: CliqueComparator; trees: tuple[first, second: seq[Tree]]): int =
  var depth: int = high(int)
  for t in trees.first:
    if (depth > t.getMetrics().depth):
      depth = t.getMetrics().depth
  for t in trees.second:
    if (depth > t.getMetrics().depth):
      depth = t.getMetrics().depth
  return depth

proc size*(this: CliqueComparator; trees: tuple[first, second: seq[Tree]]): int =
  return trees.first.len() + trees.second.len()

  
proc cmp*(this: CliqueComparator; l1: tuple[first, second: seq[Tree]];
              l2: tuple[first, second: seq[Tree]]): int =
  var minDepth1: int = minDepth(l1)
  var minDepth2: int = minDepth(l2)
  if (minDepth1 != minDepth2):
    return -(1) * cmp(minDepth1, minDepth2)
  else:
    var size1: int = size(l1)
    var size2: int = size(l2)
    return -(1) * cmp(size1, size2)

proc numberOfCommonDescendants*(this: MappingComparator; src: Tree; dst: Tree): int =
  if (not(srcDescendants.contains(src))):
    srcDescendants[src] = src.getDescendants()
  if (not(dstDescendants.contains(dst))):
    dstDescendants[dst] = toHashSet(dst.getDescendants())
  var common: int = 0
  for t in srcDescendants[src]:
    var m: Tree = mappings.getDstForSrc(t)
    if (m != nil and dstDescendants[dst].contains(m)):
      postInc(common)
  return common


proc jaccardSimilarity*(this: MappingComparator; src: Tree; dst: Tree): float =
  var num: float = cast[float](numberOfCommonDescendants(src, dst))
  var den: float = cast[float](srcDescendants[src].len()) +
      cast[float](dstDescendants[dst].len()) -
      num
  return num / den


proc sims*(this: MappingComparator; src: Tree; dst: Tree): seq[float] =
  @[
    jaccardSimilarity(src.getParent(), dst.getParent()),
    float(src.positionInParent() - dst.positionInParent()),
    float(src.getMetrics().position - dst.getMetrics().position),
    float(src.getMetrics().position)
  ]




proc newMappingComparator*(matcherMappings: MappingStore, mappings: seq[Mapping]): MappingComparator =
  new(result)
  result.mappings = matcherMappings
  for mapping in mappings:
    result.simMap[mapping] = result.sims(mapping.first, mapping.second)
  
proc cmp*(this: MappingComparator; m1: Mapping; m2: Mapping): int =
  var sims1: seq[float] = simMap[m1]
  var sims2: seq[float] = simMap[m2]
  block:
    var i: int = 0
    while i < sims1.len:
      if (sims1[i] != sims2[i]):
        return -(1) * cmp(sims1[i], sims2[i])
      postInc(i)
  return 0




method match*(this: AbstractSubtreeMatcher; src: Tree; dst: Tree;
            mappings: MappingStore): MappingStore =


  this.src = src
  this.dst = dst
  this.mappings = mappings

  var multiMappings = newMultiMappingStore()

  var srcTrees: PriorityTreeQueue = newPriorityTreeQueue(
    src, this.minPriority, this.priorityCalculator)

  var dstTrees: PriorityTreeQueue = newPriorityTreeQueue(
    dst, this.minPriority, this.priorityCalculator)

  while (not(srcTrees.isEmpty() or dstTrees.isEmpty())):
    synchronize(srcTrees, dstTrees)
    if (srcTrees.isEmpty() or dstTrees.isEmpty()):
      break

    var currentPrioritySrcTrees = srcTrees.pop()
    var currentPriorityDstTrees = dstTrees.pop()

    for currentSrc in currentPrioritySrcTrees:
      for currentDst in currentPriorityDstTrees:  
        if (currentSrc.getMetrics().hash == currentDst.getMetrics().hash):
          if (currentSrc.isIsomorphicTo(currentDst)):
            multiMappings.addMapping(currentSrc, currentDst)

    for t in currentPrioritySrcTrees:
      if (not(multiMappings.hasSrc(t))):
        srcTrees.open(t)

    for t in currentPriorityDstTrees:
      if (not(multiMappings.hasDst(t))):
        dstTrees.open(t)

  filterMappings(this, multiMappings)
  return this.mappings

proc retainBestMapping*(
    this: AbstractSubtreeMatcher; 
    mappingList: var seq[Mapping];
    srcIgnored: var HashSet[Tree]; 
    dstIgnored: var HashSet[Tree]
  ): void =

  echov mappingList.len 
  while (mappingList.len() > 0):
    var mapping = mappingList[0] 
    mappingList.delete(0)
    if (not(srcIgnored.contains(mapping.first) or
        dstIgnored.contains(mapping.second))):
      mappings.addMappingRecursively(mapping.first, mapping.second)
      
      srcIgnored.incl(mapping.first)
      for it in mapping.first.getDescendants():
        srcIgnored.incl it

      dstIgnored.incl(mapping.second)
      for it in mapping.second.getDescendants():
        dstIgnored.incl it

proc getMinPriority*(this: AbstractSubtreeMatcher): int =
  return minPriority

proc setMinPriority*(this: AbstractSubtreeMatcher; minPriority: int): void =
  this.minPriority = minPriority


method filterMappings*(this: GreedySubtreeMatcher;
                     multiMappings: MultiMappingStore): void =
  var ambiguousList: seq[Mapping] 
  var ignored: HashSet[Tree]
  
  for src in multiMappings.allMappedSrcs():
    var isMappingUnique = false
    if (multiMappings.isSrcUnique(src)):
      var dst = multiMappings.getDsts(src).findAny()
      if (multiMappings.isDstUnique(dst)):
        mappings.addMappingRecursively(src, dst)
        isMappingUnique = true
    if (not(ignored.contains(src) or isMappingUnique)):
      var adsts = multiMappings.getDsts(src)
      # WARNING  var asrcs = multiMappings.getSrcs(multiMappings.getDsts(src).iterator().next());
      var asrcs = multiMappings.getSrcs(multiMappings.getDsts(src).findAny())
      for asrc in asrcs:
        for adst in adsts:
          ambiguousList.add(newMapping(asrc, adst))
      ignored.incl(asrcs)

  var srcIgnored: HashSet[Tree]
  var dstIgnored: HashSet[Tree]
  sort(ambiguousList, newFullMappingComparator(mappings))
  retainBestMapping(ambiguousList, srcIgnored, dstIgnored)


method filterMappings*(this: HungarianSubtreeMatcher;
                     multiMappings: MultiMappingStore): void =
  var ambiguousList: seq[MultiMappingStore] 
  var ignored: HashSet[Tree]
  for src in multiMappings.allMappedSrcs():
    if (multiMappings.isSrcUnique(src)):
      mappings.addMappingRecursively(src, multiMappings.getDsts(src).findAny())

    else:
      if (not(ignored.contains(src))):
        var ambiguous: MultiMappingStore = newMultiMappingStore()
        var adsts: HashSet[Tree] = multiMappings.getDsts(src)
        var asrcs: HashSet[Tree] = multiMappings.getSrcs(
            multiMappings.getDsts(src).findAny())
        for asrc in asrcs:
          for adst in adsts:
            ambiguous.addMapping(asrc, adst)
        ambiguousList.add(ambiguous)
        ignored.incl asrcs

  sort(ambiguousList, newMultiMappingComparator())
  for ambiguous in ambiguousList:
    var lstSrcs: seq[Tree] = toSeq(ambiguous.allMappedSrcs())
    var lstDsts: seq[Tree] = toSeq(ambiguous.allMappedDsts())
    var matrix: seq[seq[float]] = nseq(lstSrcs.len(), lstDsts.len(), 0.0)
    block:
      var i: int = 0
      while i < lstSrcs.len():
        block:
          var j: int = 0
          while j < lstDsts.len():
            matrix[i][j] = cost(lstSrcs[i], lstDsts[j])
            postInc(j)
        postInc(i)
    var hgAlg: HungarianAlgorithm = newHungarianAlgorithm(matrix)
    var solutions: seq[int] = hgAlg.execute()
    block:
      var i: int = 0
      while i < solutions.len:
        var dstIdx: int = solutions[i]
        if (dstIdx != -(1)):
          mappings.addMappingRecursively(lstSrcs[i], lstDsts[dstIdx])
        postInc(i)




proc fromClique*(this: CliqueSubtreeMatcher; clique: tuple[first, second: seq[Tree]]): seq[
    Mapping] =
  var cliqueAsMappings: seq[Mapping] = newSeq[Mapping]()
  for src in clique.first:
    for dst in clique.first:
      cliqueAsMappings.add(newMapping(src, dst))
  return cliqueAsMappings

  
method filterMappings*(this: CliqueSubtreeMatcher;
                     multiMappings: MultiMappingStore): void =
  var cliques: Table[Hash, tuple[first, second: seq[Tree]]]

  for m in multiMappings:
    var hash: int = m.first.getMetrics().hash
    if (not(cliques.contains(hash))):
      cliques[hash] = (nseq(0, Tree()), nseq(0, Tree()))

    cliques[hash].first.add(m.first)
    cliques[hash].second.add(m.second)

  var ccliques: seq[tuple[first, second: seq[Tree]]] 

  for hash, _ in cliques:
    var clique = cliques[hash]
    if (clique.first.len() == 1 and clique.second.len() == 1):
      mappings.addMappingRecursively(clique.first[0], clique.second[0])
    else:
      ccliques.add(clique)

  sort(ccliques, newCliqueComparator())

  for clique in ccliques:
    var cliqueAsMappings: seq[Mapping] = fromClique(clique)
    sort(cliqueAsMappings, newMappingComparator(this.mappings, cliqueAsMappings))
    var srcIgnored: HashSet[Tree]
    var dstIgnored: HashSet[Tree]
    retainBestMapping(cliqueAsMappings, srcIgnored, dstIgnored)



