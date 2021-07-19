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
# - RtedMatcher.java
# - RtedAlgorithm.java


import ./jcommon, ./tree, ./matcher, ./mapping_store

type
  RtedMatcher* = ref object of Matcher

{.this: this.}

type
  LabelDictionary* = ref object
    KEY_DUMMY_LABEL*: int
    count*: int
    strInt*: Table[string, int]
    intStr*: Table[int, string]
    newLabelsAllowed*: bool

proc newLabelDictionary*(): LabelDictionary =
  new(result)

proc store*(this: LabelDictionary; label: string): int =
  if (strInt.contains(label)):
    return strInt[label]
  else:
    if (not(newLabelsAllowed)):
      return KEY_DUMMY_LABEL
    else:
      var intKey: int = postInc(count)
      strInt[label] = intKey
      intStr[intKey] = label
      return intKey

proc read*(this: LabelDictionary; labelID: int): string =
  return intStr[labelID]

proc isNewLabelsAllowed*(this: LabelDictionary): bool =
  return newLabelsAllowed

proc setNewLabelsAllowed*(this: LabelDictionary; newLabelsAllowed: bool): void =
  this.newLabelsAllowed = newLabelsAllowed


type
  RtedFlags = enum


    POST2_SIZE = 0
    POST2_KR_SUM = 1
    POST2_REV_KR_SUM = 2
    POST2_DESC_SUM = 3
    POST2_PRE = 4
    POST2_PARENT = 5
    POST2_LABEL = 6
    KR = 7
    POST2_LLD = 8
    POST2_MIN_KR = 9
    RKR = 10
    RPOST2_RLD = 11
    RPOST2_MIN_RKR = 12
    RPOST2_POST = 13 
    POST2_STRATEGY = 14
    PRE2_POST = 15

  RtedPath = enum
    NONE = -1

    LEFT = 0
    RIGHT = 1
    HEAVY = 2

    BOTH = 3

    REVLEFT = 4
    REVRIGHT = 5
    REVHEAVY = 6



  RtedAlgorithm* = ref object
    it1*: InfoTree
    it2*: InfoTree
    size1*: int
    size2*: int
    ld*: LabelDictionary
    str*: seq[seq[RtedPath]]
    delta*: seq[seq[float]]
    deltaBit*: seq[seq[int]]
    ij*: seq[seq[int]]
    costV*: array[RtedPath, seq[seq[int]]]
    costW*: array[RtedPath, seq[int]]
    t*: seq[seq[float]]
    tCOPY*: seq[seq[float]]
    tTMP*: seq[seq[float]]
    s*: seq[seq[float]]
    q*: seq[float]
    da, db, dc*: float
    previousStrategy*: RtedPath
    strStat*: array[RtedPath, int]
    costDel, costIns, costMatch: float

  InfoTree* = ref object
    inputTree*: Tree
    info*: array[RtedFlags, seq[int]] ## an array with all the indeces
    ld*: LabelDictionary
    nodeType*: array[RtedPath, seq[bool]] ## store the type of a node: for every 
    ## node stores three boolean values (L, R, H)
    paths*: array[RtedPath, seq[int]]
    relSubtrees*: array[RtedPath, seq[seq[int]]]
    sizeTmp*: int
    descSizesTmp*: int
    krSizesSumTmp*: int
    revkrSizesSumTmp*: int
    preorderTmp*: int
    currentNode*: int
    switched*: bool
    leafCount*: int
    treeSize*: int


proc `+`(path: RtedPath, offset: int): RtedPath = RtedPath(path.int + offset)

type
  Enumeration[T] = object
    pos: int
    s: ptr seq[T]

proc enumeration*[T](s: var seq[T]): Enumeration[T] = 
  Enumeration[T](s: addr s)


proc hasMoreElements[T](it: Enumeration[T]): bool = 
  result = it.pos < it.s[].len

proc nextElement[T](it: var Enumeration[T]): T = 
  assert hasMoreElements(it)
  result = it.s[][it.pos]
  inc it.pos

proc gatherInfo*(this: InfoTree; inTree: Tree; postorder: int): int =
  var
    postorder = postorder
    currentSize: int      = 0
    childrenCount: int    = 0
    descSizes: int        = 0
    krSizesSum: int       = 0
    revkrSizesSum: int    = 0
    preorder: int         = preorderTmp
    heavyChild: int       = -(1)
    leftChild: int        = -(1)
    rightChild: int       = -(1)
    weight: int           = -(1)
    maxWeight: int        = -(1)
    currentPostorder: int = -(1)
    oldHeavyChild: int    = -(1)
    heavyRelSubtreesTmp: seq[int]
    leftRelSubtreesTmp: seq[int]
    rightRelSubtreesTmp: seq[int]

  var childrenPostorders: seq[int]
  postInc(preorderTmp)
  for idx, sub in inTree:
    let hasNext = idx < inTree.high
    postInc(childrenCount)
    postorder = gatherInfo(sub, postorder)
    childrenPostorders.add(postorder)
    currentPostorder = postorder
    weight = sizeTmp + 1
    if (weight >= maxWeight):
      maxWeight = weight
      oldHeavyChild = heavyChild
      heavyChild = currentPostorder

    else:
      heavyRelSubtreesTmp.add(currentPostorder)


    if (oldHeavyChild != -(1)):
      heavyRelSubtreesTmp.add(oldHeavyChild)
      oldHeavyChild = -(1)

    if (childrenCount == 1):
      leftChild = currentPostorder

    else:
      leftRelSubtreesTmp.add(currentPostorder)

    rightChild = currentPostorder
    if (hasNext):
      rightRelSubtreesTmp.add(currentPostorder)

    currentSize += 1 + sizeTmp
    descSizes += descSizesTmp
    if (childrenCount > 1):
      krSizesSum += krSizesSumTmp + sizeTmp + 1
    else:
      krSizesSum += krSizesSumTmp
      nodeType[LEFT][currentPostorder] = true
    if (hasNext):
      revkrSizesSum += revkrSizesSumTmp + sizeTmp + 1
    else:
      revkrSizesSum += revkrSizesSumTmp
      nodeType[RIGHT][currentPostorder] = true

  postInc(postorder)

  var currentDescSizes: int = descSizes + currentSize + 1
  info[POST2_DESC_SUM][postorder] = int(
    (currentSize + 1) * (currentSize + 1 + 3) / 2) - currentDescSizes

  info[POST2_KR_SUM][postorder] = krSizesSum + currentSize + 1
  info[POST2_REV_KR_SUM][postorder] = revkrSizesSum + currentSize + 1
  info[POST2_LABEL][postorder] = ld.store(inTree.getLabel())

  for i in childrenPostorders:
    info[POST2_PARENT][i] = postorder

  info[POST2_SIZE][postorder] = currentSize + 1

  if (currentSize == 0):
    postInc(leafCount)


  info[POST2_PRE][postorder] = preorder
  info[PRE2_POST][preorder] = postorder
  info[RPOST2_POST][treeSize - 1 - preorder] = postorder

  if (heavyChild != -(1)):
    paths[HEAVY][postorder] = heavyChild
    nodeType[HEAVY][heavyChild] = true

    if (leftChild < heavyChild and heavyChild < rightChild):
      info[POST2_STRATEGY][postorder] = BOTH.int

    elif (heavyChild == leftChild):
      info[POST2_STRATEGY][postorder] = RIGHT.int

    elif (heavyChild == rightChild):
      info[POST2_STRATEGY][postorder] = LEFT.int

  else:
    info[POST2_STRATEGY][postorder] = RIGHT.int

  if (leftChild != -(1)):
    paths[LEFT][postorder] = leftChild

  if (rightChild != -(1)):
    paths[RIGHT][postorder] = rightChild
    


  relSubtrees[HEAVY][postorder] = heavyRelSubtreesTmp
  relSubtrees[RIGHT][postorder] = rightRelSubtreesTmp
  relSubtrees[LEFT][postorder] = leftRelSubtreesTmp
  descSizesTmp = currentDescSizes
  sizeTmp = currentSize
  krSizesSumTmp = krSizesSum
  revkrSizesSumTmp = revkrSizesSum
  return postorder


proc postTraversalProcessing*(this: InfoTree): void =
  var nc1: int = treeSize
  info[KR] = newSeqWith(leafCount, default(int))
  info[RKR] = newSeqWith(leafCount, default(int))
  var nc: int = nc1
  var lc: int = leafCount
  var i: int = 0
  block:
    i = 0
    while i < treeSize:
      if (paths[LEFT][i] == -(1)):
        info[POST2_LLD][i] = i
      else:
        info[POST2_LLD][i] = info[POST2_LLD][paths[LEFT][i]]
      if (paths[RIGHT][i] == -(1)):
        info[RPOST2_RLD][treeSize - 1 - info[POST2_PRE][i]] = treeSize - 1 -
            info[POST2_PRE][i]
      else:
        info[RPOST2_RLD][treeSize - 1 - info[POST2_PRE][i]] = info[RPOST2_RLD][
            treeSize - 1 - info[POST2_PRE][paths[RIGHT][i]]]
      postInc(i)

  var visited: seq[bool] = newSeqWith(nc, false)
  var visitedR: seq[bool] = newSeqWith(nc, default(bool))

  var k: int = lc - 1
  var kR: int = lc - 1
  block:
    i = nc - 1
    while i >= 0:
      if (not(visited[info[POST2_LLD][i]])):
        info[KR][k] = i
        visited[info[POST2_LLD][i]] = true
        postDec(k)
      if (not(visitedR[info[RPOST2_RLD][i]])):
        info[RKR][kR] = i
        visitedR[info[RPOST2_RLD][i]] = true
        postDec(kR)
      postDec(i)
  var parent: int = -(1)
  var parentR: int = -(1)
  block:
    i = 0
    while i < leafCount:
      parent = info[KR][i]
      while (parent > -(1) and info[POST2_MIN_KR][parent] == -(1)):
        info[POST2_MIN_KR][parent] = i
        parent = info[POST2_PARENT][parent]
      parentR = info[RKR][i]
      while (parentR > -(1) and info[RPOST2_MIN_RKR][parentR] == -(1)):
        info[RPOST2_MIN_RKR][parentR] = i
        parentR = info[POST2_PARENT][info[RPOST2_POST][parentR]]
        if (parentR > -(1)):
          parentR = treeSize - 1 - info[POST2_PRE][parentR]
      postInc(i)


proc newInfoTree*(inputTree: Tree; ld: LabelDictionary): InfoTree =
  new(result)

  with result:
    inputTree = inputTree
    treeSize = inputTree.getMetrics().size


  let init = nseq(result.treeSize, -1)
  fill(result.info, nseq(result.treeSize, 0))

  result.info[POST2_PARENT] = init
  result.info[POST2_MIN_KR] = init
  result.info[RPOST2_MIN_RKR] = init
  result.info[POST2_STRATEGY] = init

  result.relSubtrees[LEFT] = nseq(result.treeSize, nseq(0, 0))
  result.relSubtrees[RIGHT] = nseq(result.treeSize, nseq(0, 0))
  result.relSubtrees[HEAVY] = nseq(result.treeSize, nseq(0, 0))

  result.nodeType[LEFT] = newSeqWith(result.treeSize, false)
  result.nodeType[RIGHT] = newSeqWith(result.treeSize, false)
  result.nodeType[HEAVY] = newSeqWith(result.treeSize, false)

  result.paths[LEFT] = init
  result.paths[RIGHT] = init
  result.paths[HEAVY] = init



  result.ld = ld
  result.currentNode = result.treeSize - 1
  discard gatherInfo(result, inputTree, -(1))
  postTraversalProcessing(result)

proc getSize*(this: InfoTree): int =
  return treeSize

proc ifNodeOfType*(this: InfoTree; postorder: int; jtype: RtedPath): bool =
  return nodeType[jtype][postorder]

proc getNodeTypeArray*(this: InfoTree; jtype: RtedPath): seq[bool] =
  return nodeType[jtype]

proc getInfo*(this: InfoTree; infoCode: RtedFlags; nodesPostorder: int): int =
  return info[infoCode][nodesPostorder]

proc getInfoArray*(this: InfoTree; infoCode: RtedFlags): seq[int] =
  return info[infoCode]

proc getNodeRelSubtrees*(
    this: InfoTree; pathType: RtedPath; nodePostorder: int): seq[int] =

  return relSubtrees[pathType][nodePostorder]

proc getPath*(this: InfoTree; pathType: RtedPath): seq[int] =
  return paths[pathType]

proc getCurrentNode*(this: InfoTree): int =
  return currentNode

proc setCurrentNode*(this: InfoTree; postorder: int): void =
  currentNode = postorder

proc setSwitched*(this: InfoTree; value: bool): void =
  switched = value

proc isSwitched*(this: InfoTree): bool =
  return switched


proc newRtedAlgorithm*(
    delCost: float; insCost: float; matchCost: float): RtedAlgorithm =
  new(result)
  result.costDel = delCost
  result.costIns = insCost
  result.costMatch = matchCost



proc setDeltaValue*(this: RtedAlgorithm; a: int; b: int; value: float;
                    switched: bool): void =
  if (switched):
    delta[b][a] = value
  else:
    delta[a][b] = value


proc setDeltaBitValue*(this: RtedAlgorithm; a: int; b: int; value: int;
                       switched: bool): void =
  if (switched):
    deltaBit[b][a] = value
  else:
    deltaBit[a][b] = value

proc treeEditDist*(this: RtedAlgorithm; it1: InfoTree; it2: InfoTree; i: int;
                   j: int): void =
  var m: int = i - it1.info[POST2_LLD][i] + 2
  var n: int = j - it2.info[POST2_LLD][j] + 2
  var forestdist: seq[seq[float]] = newSeqWith(m, newSeqWith(n, default(float)))
  var ioff: int = it1.info[POST2_LLD][i] - 1
  var joff: int = it2.info[POST2_LLD][j] - 1
  var switched: bool = it1.isSwitched()
  forestdist[0][0] = 0
  block:
    var i1: int = 1
    while i1 <= i - ioff:
      forestdist[i1][0] = forestdist[i1 - 1][0] + 1
      postInc(i1)
  block:
    var j1: int = 1
    while j1 <= j - joff:
      forestdist[0][j1] = forestdist[0][j1 - 1] + 1
      postInc(j1)
  block:
    var i1: int = 1
    while i1 <= i - ioff:
      block:
        var j1: int = 1
        while j1 <= j - joff:
          if (it1.info[POST2_LLD][i1 + ioff] == it1.info[POST2_LLD][i] and
              it2.info[POST2_LLD][j1 + joff] == it2.info[POST2_LLD][j]):
            var u: float = 0
            if (it1.info[POST2_LABEL][i1 + ioff] !=
                it2.info[POST2_LABEL][j1 + joff]):
              u = costMatch
            da = forestdist[i1 - 1][j1] + costDel
            db = forestdist[i1][j1 - 1] + costIns
            dc = forestdist[i1 - 1][j1 - 1] + u
            forestdist[i1][j1] = ((if da < db:
              ((if da < dc: da else: dc))
            else:
              ((if db < dc: db else: dc))
            ))
            setDeltaValue(i1 + ioff, j1 + joff, forestdist[i1 - 1][j1 - 1],
                          switched)
            setDeltaBitValue(i1 + ioff, j1 + joff, cast[int](((if forestdist[i1][
                j1] -
                forestdist[i1 - 1][j1 - 1] >
                0:
              1
            else:
              0
            ))), switched)
          else:
            var u: float = 
              if switched:
                deltaBit[j1 + joff][i1 + ioff].float() * costMatch
              else:
                deltaBit[i1 + ioff][j1 + joff].float() * costMatch

            da = forestdist[i1 - 1][j1] + costDel
            db = forestdist[i1][j1 - 1] + costIns
            dc = forestdist[
              it1.info[POST2_LLD][i1 + ioff] - 1 - ioff
            ][
              it2.info[POST2_LLD][j1 + joff] - 1 - joff
            ] + ((
                if switched: delta[j1 + joff][i1 + ioff]
                else:        delta[i1 + ioff][j1 + joff]
            )) + u



            forestdist[i1][j1] = ((if da < db:
              ((if da < dc: da else: dc))
            else:
              ((if db < dc: db else: dc))
            ))
          postInc(j1)
      postInc(i1)




proc treeEditDistRev*(this: RtedAlgorithm; it1: InfoTree; it2: InfoTree; i: int;
                      j: int): void =
  var 
    m: int = i - it1.info[RPOST2_RLD][i] + 2
    n: int = j - it2.info[RPOST2_RLD][j] + 2
    forestdist: seq[seq[float]] = newSeqWith(
      m, newSeqWith(n, default(float)))

    ioff: int = it1.info[RPOST2_RLD][i] - 1
    joff: int = it2.info[RPOST2_RLD][j] - 1
    switched: bool = it1.isSwitched()

  forestdist[0][0] = 0
  block:
    var i1: int = 1
    while i1 <= i - ioff:
      forestdist[i1][0] = forestdist[i1 - 1][0] + 1
      postInc(i1)
  block:
    var j1: int = 1
    while j1 <= j - joff:
      forestdist[0][j1] = forestdist[0][j1 - 1] + 1
      postInc(j1)
  block:
    var i1: int = 1
    while i1 <= i - ioff:
      block:
        var j1: int = 1
        while j1 <= j - joff:
          if (it1.info[RPOST2_RLD][i1 + ioff] == it1.info[RPOST2_RLD][i] and
              it2.info[RPOST2_RLD][j1 + joff] == it2.info[RPOST2_RLD][j]):
            var u: float = 0
            if (it1.info[POST2_LABEL][it1.info[RPOST2_POST][i1 + ioff]] !=
                it2.info[POST2_LABEL][it2.info[RPOST2_POST][j1 + joff]]):
              u = costMatch
            da = forestdist[i1 - 1][j1] + costDel
            db = forestdist[i1][j1 - 1] + costIns
            dc = forestdist[i1 - 1][j1 - 1] + u
            forestdist[i1][j1] = ((if da < db:
              ((if da < dc:
                da
              else:
                dc
              ))
            else:
              ((if db < dc:
                db
              else:
                dc
              ))
            ))
            setDeltaValue(it1.info[RPOST2_POST][i1 + ioff],
                          it2.info[RPOST2_POST][j1 + joff],
                          forestdist[i1 - 1][j1 - 1], switched)
            setDeltaBitValue(it1.info[RPOST2_POST][i1 + ioff],
                             it2.info[RPOST2_POST][j1 + joff], cast[int](((if forestdist[
                i1][j1] -
                forestdist[i1 - 1][j1 - 1] >
                0:
              1
            else:
              0
            ))), switched)
          else:
            var u: float = 0
            u = ((if switched:
              deltaBit[it2.info[RPOST2_POST][j1 + joff]][
                  it1.info[RPOST2_POST][i1 + ioff]].float() *
                  costMatch
            else:
              deltaBit[it1.info[RPOST2_POST][i1 + ioff]][
                  it2.info[RPOST2_POST][j1 + joff]].float() *
                  costMatch
            ))
            da = forestdist[i1 - 1][j1] + costDel
            db = forestdist[i1][j1 - 1] + costIns
            dc = forestdist[it1.info[RPOST2_RLD][i1 + ioff] - 1 - ioff][
                it2.info[RPOST2_RLD][j1 + joff] - 1 - joff] +
                ((if switched:
              delta[it2.info[RPOST2_POST][j1 + joff]][
                  it1.info[RPOST2_POST][i1 + ioff]]
            else:
              delta[it1.info[RPOST2_POST][i1 + ioff]][
                  it2.info[RPOST2_POST][j1 + joff]]
            )) +
                u
            forestdist[i1][j1] = ((if da < db:
              ((if da < dc:
                da
              else:
                dc
              ))
            else:
              ((if db < dc:
                db
              else:
                dc
              ))
            ))
          postInc(j1)
      postInc(i1)


proc spfL*(this: RtedAlgorithm; it1: InfoTree; it2: InfoTree): float =
  var fPostorder: int = it1.getCurrentNode()
  var gPostorder: int = it2.getCurrentNode()
  var minKR: int = it2.info[POST2_MIN_KR][gPostorder]
  var kr: seq[int] = it2.info[KR]
  if (minKR > -(1)):
    block:
      var j: int = minKR
      while kr[j] < gPostorder:
        treeEditDist(it1, it2, fPostorder, kr[j])
        postInc(j)
  treeEditDist(it1, it2, fPostorder, gPostorder)

  if it1.isSwitched():
    return delta[gPostorder][fPostorder] + 
      deltaBit[gPostorder][fPostorder].float() * costMatch

  else:
    return delta[fPostorder][gPostorder] + 
      deltaBit[fPostorder][gPostorder].float() * costMatch

proc spfR*(this: RtedAlgorithm; it1: InfoTree; it2: InfoTree): float =
  var fReversedPostorder: int = it1.getSize() - 1 -
      it1.info[POST2_PRE][it1.getCurrentNode()]
  var gReversedPostorder: int = it2.getSize() - 1 -
      it2.info[POST2_PRE][it2.getCurrentNode()]
  var minRKR: int = it2.info[RPOST2_MIN_RKR][gReversedPostorder]
  var rkr: seq[int] = it2.info[RKR]
  if (minRKR > -(1)):
    block:
      var j: int = minRKR
      while rkr[j] < gReversedPostorder:
        treeEditDistRev(it1, it2, fReversedPostorder, rkr[j])
        postInc(j)
  treeEditDistRev(it1, it2, fReversedPostorder, gReversedPostorder)
  return ((if it1.isSwitched():
    delta[it2.getCurrentNode()][it1.getCurrentNode()] +
        deltaBit[it2.getCurrentNode()][it1.getCurrentNode()].float() * costMatch
  else:
    delta[it1.getCurrentNode()][it2.getCurrentNode()] +
        deltaBit[it1.getCurrentNode()][it2.getCurrentNode()].float() * costMatch
  ))


proc jOfI*(this: RtedAlgorithm; it: InfoTree; aI: int; aSubtreeWeight: int;
           aSubtreeRevPre: int; aSubtreePre: int; aStrategy: RtedPath; treeSize: int): int =
  return ((if aStrategy == LEFT:
    aSubtreeWeight - aI -
        it.info[POST2_SIZE][treeSize - 1 - (aSubtreeRevPre + aI)]
  else:
    aSubtreeWeight - aI -
        it.info[POST2_SIZE][it.info[RPOST2_POST][treeSize - 1 - (aSubtreePre +
        aI)]]
  ))


proc computeIJTable*(this: RtedAlgorithm; it: InfoTree; subtreePreorder: int;
                     subtreeRevPreorder: int; subtreeSize: int; aStrategy: RtedPath;
                     treeSize: int): void =
  var change: int
  var post2pre: seq[int] = it.info[POST2_PRE]
  var rpost2post: seq[int] = it.info[RPOST2_POST]
  if (aStrategy == LEFT):
    block:
      var x: int = 0
      while x < subtreeSize:
        ij[0][x] = x + subtreePreorder
        postInc(x)
    block:
      var x: int = 1
      while x < subtreeSize:
        change = post2pre[treeSize - 1 - (x - 1 + subtreeRevPreorder)]
        block:
          var z: int = 0
          while z < subtreeSize:
            if (ij[x - 1][z] >= change):
              ij[x][z] = ij[x - 1][z] + 1
            else:
              ij[x][z] = ij[x - 1][z]
            postInc(z)
        postInc(x)
  else:
    block:
      var x: int = 0
      while x < subtreeSize:
        ij[0][x] = x + subtreeRevPreorder
        postInc(x)
    block:
      var x: int = 1
      while x < subtreeSize:
        change = treeSize - 1 -
            rpost2post[treeSize - 1 - (x - 1 + subtreePreorder)]
        block:
          var z: int = 0
          while z < subtreeSize:
            if (ij[x - 1][z] >= change):
              ij[x][z] = ij[x - 1][z] + 1
            else:
              ij[x][z] = ij[x - 1][z]
            postInc(z)
        postInc(x)


proc computePeriod*(this: RtedAlgorithm; it1: InfoTree; aVp: int; aNextVp: int;
                    it2: InfoTree; aStrategy: RtedPath): void =
  var 
    fTreeSize: int = it1.getSize()
    gTreeSize: int = it2.getSize()
    vpPreorder: int = it1.info[POST2_PRE][aVp]
    vpRevPreorder: int = fTreeSize - 1 - aVp
    vpSize: int = it1.info[POST2_SIZE][aVp]
    gSize: int = it2.info[POST2_SIZE][it2.getCurrentNode()]
    gPreorder: int = it2.info[POST2_PRE][it2.getCurrentNode()]
    gRevPreorder: int = gTreeSize - 1 - it2.getCurrentNode()
    nextVpPreorder: int = -(1)
    nextVpRevPreorder: int = -(1)
    nextVpSize: int = -(1)
    k: int

  if (aNextVp != -(1)):
    nextVpPreorder = it1.info[POST2_PRE][aNextVp]
    nextVpRevPreorder = fTreeSize - 1 - aNextVp
    nextVpSize = it1.info[POST2_SIZE][aNextVp]
    k = tern(aStrategy == LEFT, nextVpPreorder - vpPreorder,
             nextVpRevPreorder - vpRevPreorder)

    if (aStrategy != previousStrategy):
      computeIJTable(it2, gPreorder, gRevPreorder, gSize, aStrategy, gTreeSize)
  else:
    k = 1
    computeIJTable(it2, gPreorder, gRevPreorder, gSize, aStrategy, gTreeSize)
  var realStrategy: int = it1.info[POST2_STRATEGY][aVp]
  var switched: bool = it1.isSwitched()
  tTMP = tCOPY
  tCOPY = t
  t = tTMP
  if (vpSize - nextVpSize == 1):
    if (gSize == 1):
      setDeltaValue(it1.info[PRE2_POST][vpPreorder],
                    it2.info[PRE2_POST][gPreorder], 
                    float(vpSize - 1), switched)
    else:
      setDeltaValue(it1.info[PRE2_POST][vpPreorder],
                    it2.info[PRE2_POST][gPreorder], t[1][0], switched)

  var 
    gijForestPreorder: int
    previousI: int
    fForestPreorderKPrime: int
    jPrime: int
    kBis: int
    jOfIminus1: int
    gijOfIMinus1Preorder: int
    jOfI: int
    deleteFromLeft: float
    deleteFromRight: float
    match: float
    fLabel: int
    gLabel: int

  block:
    var i: int = gSize - 1
    while i >= 0:
      jOfI = jOfI(it2, i, gSize, gRevPreorder, gPreorder, aStrategy, gTreeSize)
      block:
        var kPrime: int = 1
        while kPrime <= k:
          fForestPreorderKPrime = tern(aStrategy == LEFT,
                                       vpPreorder + (k - kPrime), it1.info[
              POST2_PRE][fTreeSize - 1 - (vpRevPreorder + (k - kPrime))])

          kBis = kPrime -
              it1.info[POST2_SIZE][it1.info[PRE2_POST][fForestPreorderKPrime]]
          deleteFromRight = costIns
          deleteFromLeft = costDel
          match = 0
          match += tern(aStrategy == LEFT, kBis + nextVpSize, vpSize - k + kBis).float()
          if (i + jOfI == gSize - 1):
            deleteFromRight += float(vpSize - (k - kPrime))
          else:
            deleteFromRight += q[kPrime - 1]
          fLabel = it1.info[POST2_LABEL][
              it1.info[PRE2_POST][fForestPreorderKPrime]]
          block:
            var j: int = jOfI
            while j >= 0:
              gijForestPreorder = tern(aStrategy == LEFT, ij[i][j], it2.info[
                  POST2_PRE][gTreeSize - 1 - ij[i][j]])
              if (kPrime == 1):
                if (aStrategy != previousStrategy):
                  if (aStrategy == LEFT):
                    previousI = gijForestPreorder - gPreorder
                  else:
                    previousI = gTreeSize - 1 -
                        it2.info[RPOST2_POST][gTreeSize - 1 - gijForestPreorder] -
                        gRevPreorder
                  deleteFromLeft += tCOPY[previousI][i + j - previousI]
                else:
                  deleteFromLeft += tCOPY[i][j]
              else:
                deleteFromLeft += s[kPrime - 1 - 1][j]

              match += tern(switched, delta[
                  it2.info[PRE2_POST][gijForestPreorder]][
                  it1.info[PRE2_POST][fForestPreorderKPrime]], delta[
                  it1.info[PRE2_POST][fForestPreorderKPrime]][
                  it2.info[PRE2_POST][gijForestPreorder]])

              jPrime = j +
                  it2.info[POST2_SIZE][it2.info[PRE2_POST][gijForestPreorder]]
              gLabel = it2.info[POST2_LABEL][
                  it2.info[PRE2_POST][gijForestPreorder]]
              if (fLabel != gLabel):
                match += costMatch
              if (j != jOfI):
                deleteFromRight += s[kPrime - 1][j + 1]
                if (kBis == 0):
                  if (aStrategy != previousStrategy):
                    previousI = tern(aStrategy == LEFT,
                                     ij[i][jPrime] - gPreorder,
                                     ij[i][jPrime] - gRevPreorder)
                    match += tCOPY[previousI][i + jPrime - previousI]
                  else:
                    match += tCOPY[i][jPrime]
                else:
                  if (kBis > 0):
                    match += s[kBis - 1][jPrime]
                  else:
                    match += float(gSize - (i + jPrime))

              s[kPrime - 1][j] = tern(deleteFromLeft < deleteFromRight, tern(
                  deleteFromLeft < match, deleteFromLeft, match), tern(
                  deleteFromRight < match, deleteFromRight, match))

              deleteFromRight = costIns
              deleteFromLeft = costDel
              match = 0
              postDec(j)
          postInc(kPrime)
      t[i] = tern(realStrategy.RtedPath == BOTH and aStrategy == LEFT,
                  s[k - 1 - 1], s[k - 1])

      if (i > 0):
        jOfIminus1 = jOfI(it2, i - 1, gSize, gRevPreorder, gPreorder, aStrategy,
                          gTreeSize)
        if (jOfIminus1 <= jOfI):
          block:
            var x: int = 0
            while x < k:
              q[x] = s[x][jOfIminus1]
              postInc(x)
        if (i + jOfIminus1 < gSize):
          gijOfIMinus1Preorder = tern(aStrategy == LEFT, it2.info[POST2_PRE][
              gTreeSize - 1 - (gRevPreorder + (i - 1))], gPreorder + (i - 1))
          if (k - 1 - 1 < 0):
            if (aStrategy != previousStrategy):
              previousI = tern(aStrategy == LEFT, ij[i][jOfIminus1] - gPreorder,
                               ij[i][jOfIminus1] - gRevPreorder)
              setDeltaValue(it1.info[PRE2_POST][vpPreorder],
                            it2.info[PRE2_POST][gijOfIMinus1Preorder],
                            tCOPY[previousI][i + jOfIminus1 - previousI],
                            switched)
            else:
              setDeltaValue(it1.info[PRE2_POST][vpPreorder],
                            it2.info[PRE2_POST][gijOfIMinus1Preorder],
                            tCOPY[i][jOfIminus1], switched)
          else:
            setDeltaValue(it1.info[PRE2_POST][vpPreorder],
                          it2.info[PRE2_POST][gijOfIMinus1Preorder],
                          s[k - 1 - 1][jOfIminus1], switched)
      postDec(i)
  previousStrategy = aStrategy


proc spfH*(this: RtedAlgorithm; it1: InfoTree; it2: InfoTree;
           heavyPath: seq[int]): float =
  var 
    fSize: int = it1.info[POST2_SIZE][it1.getCurrentNode()]
    gSize: int = it2.info[POST2_SIZE][it2.getCurrentNode()]
    gRevPre: int = it2.getSize() - 1 - it2.getCurrentNode()
    gPre: int = it2.info[POST2_PRE][it2.getCurrentNode()]
    gTreeSize: int = it2.getSize()
    strategy: int
    jOfi: int

  t = newSeqWith(gSize, newSeqWith(gSize, default(float)))
  tCOPY = newSeqWith(gSize, newSeqWith(gSize, default(float)))
  s = newSeqWith(fSize, newSeqWith(gSize, default(float)))
  q = newSeqWith(fSize, default(float))

  var vp: int = -(1)
  var nextVp: int = -(1)
  block:
    var it: int = heavyPath.len - 1
    while it >= 0:
      vp = heavyPath[it]
      strategy = it1.info[POST2_STRATEGY][vp]
      if (strategy.RtedPath != BOTH):
        if (it1.info[POST2_SIZE][vp] == 1):
          block:
            var i: int = gSize - 1
            while i >= 0:
              jOfi = jOfI(it2, i, gSize, gRevPre, gPre, strategy.RtedPath, gTreeSize)
              block:
                var j: int = jOfi
                while j >= 0:
                  t[i][j] = float(gSize - (i + j)) * costIns
                  postDec(j)
              postDec(i)
          previousStrategy = strategy.RtedPath
        computePeriod(it1, vp, nextVp, it2, strategy.RtedPath)
      else:
        if (it1.info[POST2_SIZE][vp] == 1):
          block:
            var i: int = gSize - 1
            while i >= 0:
              jOfi = jOfI(it2, i, gSize, gRevPre, gPre, LEFT, gTreeSize)
              block:
                var j: int = jOfi
                while j >= 0:
                  t[i][j] = float(gSize - (i + j)) * costIns
                  postDec(j)
              postDec(i)
          previousStrategy = LEFT
        computePeriod(it1, vp, nextVp, it2, LEFT)
        if (it1.info[POST2_SIZE][vp] == 1):
          block:
            var i: int = gSize - 1
            while i >= 0:
              jOfi = jOfI(it2, i, gSize, gRevPre, gPre, RIGHT, gTreeSize)
              block:
                var j: int = jOfi
                while j >= 0:
                  t[i][j] = float(gSize - (i + j)) * costIns
                  postDec(j)
              postDec(i)
          previousStrategy = RIGHT
        computePeriod(it1, vp, nextVp, it2, RIGHT)
      nextVp = vp
      postDec(it)
  return t[0][0]



proc computeDistUsingStrArray*(this: RtedAlgorithm; it1: InfoTree; it2: InfoTree): float =
  var 
    postorder1: int = it1.getCurrentNode()
    postorder2: int = it2.getCurrentNode()
    stepStrategy: RtedPath = str[postorder1][postorder2]
    tmpPostorder: int
    stepPath: seq[int]
    stepRelSubtrees: seq[int]
    heavyPath: seq[int]

  case stepStrategy:
    of LEFT:
      tmpPostorder = postorder1
      stepPath = it1.getPath(LEFT)
      while (stepPath[postorder1] > -(1)):
        stepRelSubtrees = it1.getNodeRelSubtrees(LEFT, postorder1)
        if (stepRelSubtrees.isEmpty().not()):
          for rs in stepRelSubtrees:
            it1.setCurrentNode(rs)
            discard computeDistUsingStrArray(it1, it2)

        postorder1 = stepPath[postorder1]
      it1.setCurrentNode(tmpPostorder)
      it1.setSwitched(false)
      it2.setSwitched(false)
      postInc(strStat[3.RtedPath]) # WARN `strStat[3]++;`
      postInc(strStat[LEFT])
      return spfL(it1, it2)

    of RIGHT:
      tmpPostorder = postorder1
      stepPath = it1.getPath(RIGHT)
      while (stepPath[postorder1] > -(1)):
        stepRelSubtrees = it1.getNodeRelSubtrees(RIGHT, postorder1)
        if (stepRelSubtrees.isEmpty().not()):
          for rs in stepRelSubtrees:
            it1.setCurrentNode(rs)
            discard computeDistUsingStrArray(it1, it2)

        postorder1 = stepPath[postorder1]
      it1.setCurrentNode(tmpPostorder)
      it1.setSwitched(false)
      it2.setSwitched(false)
      postInc(strStat[3.RtedPath]) # WARN `strStat[3]++;`
      postInc(strStat[RIGHT])
      return spfR(it1, it2)

    of HEAVY:
      tmpPostorder = postorder1
      stepPath = it1.getPath(HEAVY)
      heavyPath = newSeq[int]()
      heavyPath.add(postorder1)
      while (stepPath[postorder1] > -(1)):
        stepRelSubtrees = it1.getNodeRelSubtrees(HEAVY, postorder1)
        if (stepRelSubtrees.isEmpty().not()):
          for rs in stepRelSubtrees:
            it1.setCurrentNode(rs)
            discard computeDistUsingStrArray(it1, it2)

        postorder1 = stepPath[postorder1]
        heavyPath.add(postorder1)
      it1.setCurrentNode(tmpPostorder)
      it1.setSwitched(false)
      it2.setSwitched(false)
      postInc(strStat[3.RtedPath]) # WARN `strStat[3]++;`
      postInc(strStat[HEAVY])
      return spfH(it1, it2, heavyPath)

    of REVLEFT:
      tmpPostorder = postorder2
      stepPath = it2.getPath(LEFT)
      while (stepPath[postorder2] > -(1)):
        stepRelSubtrees = it2.getNodeRelSubtrees(LEFT, postorder2)
        if (stepRelSubtrees.isEmpty().not()):
          for rs in stepRelSubtrees:
            it2.setCurrentNode(rs)
            discard computeDistUsingStrArray(it1, it2)

        postorder2 = stepPath[postorder2]
      it2.setCurrentNode(tmpPostorder)
      it1.setSwitched(true)
      it2.setSwitched(true)
      postInc(strStat[3.RtedPath]) # WARN `strStat[3]++;`
      postInc(strStat[LEFT])
      return spfL(it2, it1)

    of REVRIGHT:
      tmpPostorder = postorder2
      stepPath = it2.getPath(RIGHT)
      while (stepPath[postorder2] > -(1)):
        stepRelSubtrees = it2.getNodeRelSubtrees(RIGHT, postorder2)
        if (stepRelSubtrees.isEmpty().not()):
          for rs in stepRelSubtrees:
            it2.setCurrentNode(rs)
            discard computeDistUsingStrArray(it1, it2)

        postorder2 = stepPath[postorder2]
      it2.setCurrentNode(tmpPostorder)
      it1.setSwitched(true)
      it2.setSwitched(true)
      postInc(strStat[3.RtedPath]) # WARN `strStat[3]++;`
      postInc(strStat[RIGHT])
      return spfR(it2, it1)

    of REVHEAVY:
      tmpPostorder = postorder2
      stepPath = it2.getPath(HEAVY)
      heavyPath = newSeq[int]()
      heavyPath.add(postorder2)
      while (stepPath[postorder2] > -(1)):
        stepRelSubtrees = it2.getNodeRelSubtrees(HEAVY, postorder2)
        if (stepRelSubtrees.isEmpty().not()):
          for rs in stepRelSubtrees:
            it2.setCurrentNode(rs)
            discard computeDistUsingStrArray(it1, it2)

        postorder2 = stepPath[postorder2]
        heavyPath.add(postorder2)
      it2.setCurrentNode(tmpPostorder)
      it1.setSwitched(true)
      it2.setSwitched(true)
      postInc(strStat[3.RtedPath]) # WARN `strStat[3]++;`
      postInc(strStat[HEAVY])
      return spfH(it2, it1, heavyPath)

    else:
      return -(1)


proc nonNormalizedTreeDist*(this: RtedAlgorithm): float =
  if (it1.isNil() or it2.isNil()):
    stderr.write("\"No stored trees to compare.\"")

  if (str.len == 0):
    stderr.write("\"No strategy to use.\"")

  return computeDistUsingStrArray(it1, it2)

proc init*(this: RtedAlgorithm; src: Tree; dst: Tree): void =
  ld = newLabelDictionary()
  it1 = newInfoTree(src, ld)
  it2 = newInfoTree(dst, ld)
  size1 = it1.getSize()
  size2 = it2.getSize()
  ij = nseq(max(size1, size2), max(size1, size2), 0) 
  delta = nseq(size1, size2, 0.0)
  deltaBit = nseq(size1, size2, 0)
  costV = narr(RtedPath, nseq(size1, size2, 0))
  costW = narr(RtedPath, nseq(size2, 0))

  var 
    labels1 = it1.getInfoArray(POST2_LABEL)
    labels2 = it2.getInfoArray(POST2_LABEL)
    sizes1 = it1.getInfoArray(POST2_SIZE)
    sizes2 = it2.getInfoArray(POST2_SIZE)

  block:
    var x: int = 0
    while x < sizes1.len:
      block:
        var y: int = 0
        while y < sizes2.len:
          if (labels1[x] == labels2[y]):
            deltaBit[x][y] = 0
          else:
            deltaBit[x][y] = 1
          if (sizes1[x] == 1 and sizes2[y] == 1):
            delta[x][y] = 0
          else:
            if (sizes1[x] == 1):
              delta[x][y] = float(sizes2[y] - 1)
            if (sizes2[y] == 1):
              delta[x][y] = float(sizes1[x] - 1)
          postInc(y)
      postInc(x)


proc computeOptimalStrategy*(this: RtedAlgorithm): void =
  var 
    heavyMin: int
    revHeavyMin: int
    leftMin: int
    revLeftMin: int
    rightMin: int
    revRightMin: int
    min: int = -(1)
    strategy: int = -(1)
    parent1: int = -(1)
    parent2: int = -(1)
    nodeTypeLeft1: seq[bool] = it1.nodeType[LEFT]
    nodeTypeLeft2: seq[bool] = it2.nodeType[LEFT]
    nodeTypeRigt1: seq[bool] = it1.nodeType[RIGHT]
    nodeTypeRight2: seq[bool] = it2.nodeType[RIGHT]
    nodeTypeHeavy1: seq[bool] = it1.nodeType[HEAVY]
    nodeTypeHeavy2: seq[bool] = it2.nodeType[HEAVY]
    post2size1: seq[int] = it1.info[POST2_SIZE]
    post2size2: seq[int] = it2.info[POST2_SIZE]
    post2descSum1: seq[int] = it1.info[POST2_DESC_SUM]
    post2descSum2: seq[int] = it2.info[POST2_DESC_SUM]
    post2krSum1: seq[int] = it1.info[POST2_KR_SUM]
    post2krSum2: seq[int] = it2.info[POST2_KR_SUM]
    post2revkrSum1: seq[int] = it1.info[POST2_REV_KR_SUM]
    post2revkrSum2: seq[int] = it2.info[POST2_REV_KR_SUM]
    post2parent1: seq[int] = it1.info[POST2_PARENT]
    post2parent2: seq[int] = it2.info[POST2_PARENT]

  str = newSeqWith(size1, newSeqWith(size2, NONE))

  block:
    var v: int = 0
    while v < size1:
      fill(costW[0.RtedPath], 0)
      fill(costW[1.RtedPath], 0)
      fill(costW[2.RtedPath], 0)

      block:
        var w: int = 0
        while w < size2:
          if (post2size2[w] == 1):
            costW[LEFT][w] = 0
            costW[RIGHT][w] = 0
            costW[HEAVY][w] = 0

          if (post2size1[v] == 1):
            costV[LEFT][v][w] = 0
            costV[RIGHT][v][w] = 0
            costV[HEAVY][v][w] = 0

          heavyMin = cast[int](post2size1[v]) * cast[int](post2descSum2[w]) +
              costV[HEAVY][v][w]
          revHeavyMin = cast[int](post2size2[w]) * cast[int](post2descSum1[v]) +
              costW[HEAVY][w]
          leftMin = cast[int](post2size1[v]) * cast[int](post2krSum2[w]) +
              costV[LEFT][v][w]
          revLeftMin = cast[int](post2size2[w]) * cast[int](post2krSum1[v]) +
              costW[LEFT][w]
          rightMin = cast[int](post2size1[v]) * cast[int](post2revkrSum2[w]) +
              costV[RIGHT][v][w]
          revRightMin = cast[int](post2size2[w]) * cast[int](post2revkrSum1[v]) +
              costW[RIGHT][w]

          var mins: seq[int] = @[
            leftMin, rightMin, heavyMin, high(int), 
            revLeftMin, revRightMin, revHeavyMin]

          min = leftMin
          strategy = 0
          block:
            var i: int = 1
            while i <= 6:
              if (mins[i] < min):
                min = mins[i]
                strategy = i
              postInc(i)
          str[v][w] = strategy.RtedPath

          parent1 = post2parent1[v]
          if (parent1 != -(1)):
            costV[HEAVY][parent1][w] += tern(nodeTypeHeavy1[v], costV[HEAVY][v][w], min)
            costV[RIGHT][parent1][w] += tern(nodeTypeRigt1[v], costV[RIGHT][v][w], min)
            costV[LEFT][parent1][w] += tern(nodeTypeLeft1[v], costV[LEFT][v][w], min)
            
          parent2 = post2parent2[w]
          if (parent2 != -(1)):
            costW[HEAVY][parent2] += tern(nodeTypeHeavy2[w], costW[HEAVY][w], min)
            costW[LEFT][parent2] += tern(nodeTypeLeft2[w], costW[LEFT][w], min)
            costW[RIGHT][parent2] += tern(nodeTypeRight2[w], costW[RIGHT][w], min)

          postInc(w)
      postInc(v)



proc setCustomCosts*(this: RtedAlgorithm; costDel: float; costIns: float;
                     costMatch: float): void =
  this.costDel = costDel
  this.costIns = costIns
  this.costMatch = costMatch

proc setCustomStrategy*(this: RtedAlgorithm; strategyArray: seq[seq[RtedPath]]): void =
  str = strategyArray

proc setCustomStrategy*(this: RtedAlgorithm; strategy: RtedPath; ifSwitch: bool): void =
  str = nseq(size1, size2, NONE)
  if (ifSwitch):
    block:
      var i: int = 0
      while i < size1:
        block:
          var j: int = 0
          while j < size2:
            str[i][j] = tern(it1.info[POST2_SIZE][i] >= it2.info[POST2_SIZE][j],
                             strategy, strategy + 4)

            postInc(j)
        postInc(i)
  else:
    block:
      var i: int = 0
      while i < size1:
        fill(str[i], strategy)
        postInc(i)


proc forestDist*(
    this: RtedAlgorithm; 
    ted1: InfoTree; 
    ted2: InfoTree; 
    i: int;
    j: int; 
    treedist: var seq[seq[float]]; 
    forestdist: var seq[seq[float]]
  ): void =

  forestdist[ted1.getInfo(POST2_LLD, i - 1) + 1 - 1][
      ted2.getInfo(POST2_LLD, j - 1) + 1 - 1] = 0
  block:
    var di: int = ted1.getInfo(POST2_LLD, i - 1) + 1
    while di <= i:
      forestdist[di][ted2.getInfo(POST2_LLD, j - 1) + 1 - 1] = forestdist[di - 1][
          ted2.getInfo(POST2_LLD, j - 1) + 1 - 1] +
          costDel
      block:
        var dj: int = ted2.getInfo(POST2_LLD, j - 1) + 1
        while dj <= j:
          forestdist[ted1.getInfo(POST2_LLD, i - 1) + 1 - 1][dj] = forestdist[
              ted1.getInfo(POST2_LLD, i - 1) + 1 - 1][dj - 1] +
              costIns
          if (ted1.getInfo(POST2_LLD, di - 1) == ted1.getInfo(POST2_LLD, i - 1) and
              ted2.getInfo(POST2_LLD, dj - 1) == ted2.getInfo(POST2_LLD, j - 1)):
            var costRen: float = 0
            if (not(ted1.getInfo(POST2_LABEL, di - 1) ==
                ted2.getInfo(POST2_LABEL, dj - 1))):
              costRen = costMatch
            forestdist[di][dj] = min(min(forestdist[di - 1][dj] + costDel,
                forestdist[di][dj - 1] + costIns),
                                     forestdist[di - 1][dj - 1] + costRen)
            treedist[di][dj] = forestdist[di][dj]
          else:
            forestdist[di][dj] = min(min(forestdist[di - 1][dj] + costDel,
                forestdist[di][dj - 1] + costIns), forestdist[
                ted1.getInfo(POST2_LLD, di - 1) + 1 - 1][
                ted2.getInfo(POST2_LLD, dj - 1) + 1 - 1] +
                treedist[di][dj])
          postInc(dj)
      postInc(di)

import std/deques

proc computeEditMapping*(this: RtedAlgorithm): Deque[seq[int]] =
  var 
    treedist: seq[seq[float]] = nseq(size1 + 1, size2 + 1, 0.0)
    forestdistArr: seq[seq[float]] = nseq(size1 + 1, size2 + 1, 0.0)
    rootNodePair: bool = true

  block:
    var i: int = 0
    while i < size1:
      treedist[i][0] = i.float()
      postInc(i)
  block:
    var j: int = 0
    while j < size2:
      treedist[0][j] = j.float
      postInc(j)
  block:
    var i: int = 1
    while i <= size1:
      block:
        var j: int = 1
        while j <= size2:
          treedist[i][j] = delta[i - 1][j - 1] + deltaBit[i - 1][j - 1].float()
          postInc(j)
      postInc(i)
  forestDist(it1, it2, size1, size2, treedist, forestdistArr)

  var editMapping: Deque[seq[int]] 
  var treePairs: Deque[seq[int]]
  treePairs.addFirst @[size1, size2]

  while (not(treePairs.len() == 0)):
    var 
      treePair: seq[int] = treePairs.popFirst()
      lastRow: int = treePair[0]
      lastCol: int = treePair[1]

    if (not(rootNodePair)):
      forestDist(it1, it2, lastRow, lastCol, treedist, forestdistArr)
    rootNodePair = false
    var 
      firstRow: int = it1.getInfo(POST2_LLD, lastRow - 1) + 1 - 1
      firstCol: int = it2.getInfo(POST2_LLD, lastCol - 1) + 1 - 1
      row: int = lastRow
      col: int = lastCol

    while (row > firstRow or col > firstCol):
      if (row > firstRow and
          forestdistArr[row - 1][col] + costDel == forestdistArr[row][col]):
        editMapping.addFirst(@[row, 0])
        postDec(row)
      else:
        if (col > firstCol and
            forestdistArr[row][col - 1] + costIns == forestdistArr[row][col]):
          editMapping.addFirst(@[0, col])
          postDec(col)
        else:
          if (it1.getInfo(POST2_LLD, row - 1) ==
              it1.getInfo(POST2_LLD, lastRow - 1) and
              it2.getInfo(POST2_LLD, col - 1) ==
              it2.getInfo(POST2_LLD, lastCol - 1)):
            editMapping.addFirst(@[row, col])
            postDec(row)
            postDec(col)
          else:
            treePairs.addFirst(@[row, col])
            row = it1.getInfo(POST2_LLD, row - 1) + 1 - 1
            col = it2.getInfo(POST2_LLD, col - 1) + 1 - 1

  return editMapping


method match*(this: RtedMatcher; src: Tree; dst: Tree; mappings: MappingStore): MappingStore =
  let mappings = if isNil(mappings): newMappingStore(src, dst) else: mappings
  var a: RtedAlgorithm = newRtedAlgorithm(1.0, 1.0, 1.0)
  a.init(src, dst)
  a.computeOptimalStrategy()
  discard a.nonNormalizedTreeDist()
  var arrayMappings = a.computeEditMapping()
  var srcs: seq[Tree] = postOrder(src).toSeq()
  var dsts: seq[Tree] = postOrder(dst).toSeq()
  for m in arrayMappings:
    if (m[0] != 0 and m[1] != 0):
      var srcg: Tree = srcs[m[0] - 1]
      var dstg: Tree = dsts[m[1] - 1]
      if (mappings.isMappingAllowed(srcg, dstg)):
        mappings.addMapping(srcg, dstg)
      
  assertRef(mappings)
  return mappings


proc newRtedMatcher*(): RtedMatcher = RtedMatcher(name: "RtedMatcher")