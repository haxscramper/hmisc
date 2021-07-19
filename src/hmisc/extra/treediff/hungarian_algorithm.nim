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
# - HungarianAlgorithm.java

import ./jcommon

{.this: this.}


type
  HungarianAlgorithm* = ref object
    costMatrix*: seq[seq[float]]
    rows*: int
    cols*: int
    dim*: int
    labelByWorker*: seq[float]
    labelByJob*: seq[float]
    minSlackWorkerByJob*: seq[int]
    minSlackValueByJob*: seq[float]
    matchJobByWorker*: seq[int]
    matchWorkerByJob*: seq[int]
    parentWorkerByCommittedJob*: seq[int]
    committedWorkers*: seq[bool]

proc newHungarianAlgorithm*(costMatrix: seq[seq[float]]): HungarianAlgorithm =
  var this = HungarianAlgorithm()
  this.dim = max(costMatrix.len, costMatrix[0].len)
  this.rows = costMatrix.len
  this.cols = costMatrix[0].len
  this.costMatrix = nseq(this.dim, this.dim, 0.0)
  block:
    var w: int = 0
    while w < this.dim:
      if (w < costMatrix.len):
        if (costMatrix[w].len != this.cols):
          raise newArgumentError("\"Irregular cost matrix\"")

        this.costMatrix[w] = costMatrix[w][0 ..< this.dim]

      else:
        this.costMatrix[w] = nseq(this.dim, 0.0)

      postInc(w)

  this.labelByWorker = nseq(this.dim, 0.0)
  this.labelByJob = nseq(this.dim, 0.0)
  this.minSlackWorkerByJob = nseq(this.dim, default(int))
  this.minSlackValueByJob = nseq(this.dim, 0.0)
  this.committedWorkers = nseq(this.dim, default(bool))
  this.parentWorkerByCommittedJob = nseq(this.dim, default(int))
  this.matchJobByWorker = nseq(this.dim, default(int))
  fill(this.matchJobByWorker, -(1))
  this.matchWorkerByJob = nseq(this.dim, default(int))
  fill(this.matchWorkerByJob, -(1))

  return this

proc computeInitialFeasibleSolution*(this: HungarianAlgorithm): void =
  block:
    var j: int = 0
    while j < dim:
      labelByJob[j] = high(float)
      postInc(j)
  block:
    var w: int = 0
    while w < dim:
      block:
        var j: int = 0
        while j < dim:
          if (costMatrix[w][j] < labelByJob[j]):
            labelByJob[j] = costMatrix[w][j]
          postInc(j)
      postInc(w)


proc reduce*(this: HungarianAlgorithm): void =
  block:
    var w: int = 0
    while w < dim:
      var min: float = high(float)
      block:
        var j: int = 0
        while j < dim:
          if (costMatrix[w][j] < min):
            min = costMatrix[w][j]
          postInc(j)
      block:
        var j: int = 0
        while j < dim:
          costMatrix[w][j] -= min
          postInc(j)
      postInc(w)
  var min: seq[float] = nseq(dim, 0.0)
  block:
    var j: int = 0
    while j < dim:
      min[j] = high(float)
      postInc(j)
  block:
    var w: int = 0
    while w < dim:
      block:
        var j: int = 0
        while j < dim:
          if (costMatrix[w][j] < min[j]):
            min[j] = costMatrix[w][j]
          postInc(j)
      postInc(w)
  block:
    var w: int = 0
    while w < dim:
      block:
        var j: int = 0
        while j < dim:
          costMatrix[w][j] -= min[j]
          postInc(j)
      postInc(w)

proc match*(this: HungarianAlgorithm; w: int; j: int): void =
  matchJobByWorker[w] = j
  matchWorkerByJob[j] = w

proc greedyMatch*(this: HungarianAlgorithm): void =
  block:
    var w: int = 0
    while w < dim:
      block:
        var j: int = 0
        while j < dim:
          if (matchJobByWorker[w] == -(1) and matchWorkerByJob[j] == -(1) and
              costMatrix[w][j] - labelByWorker[w] - labelByJob[j] == 0):
            match(w, j)
          postInc(j)
      postInc(w)

proc updateLabeling*(this: HungarianAlgorithm; slack: float): void =
  block:
    var w: int = 0
    while w < dim:
      if (committedWorkers[w]):
        labelByWorker[w] += slack
      postInc(w)
  block:
    var j: int = 0
    while j < dim:
      if (parentWorkerByCommittedJob[j] != -(1)):
        labelByJob[j] -= slack
      else:
        minSlackValueByJob[j] -= slack
      postInc(j)






proc executePhase*(this: HungarianAlgorithm): void =
  while (true):
    var minSlackWorker: int = -(1)
    var minSlackJob: int = -(1)
    var minSlackValue: float = high(float)
    block:
      var j: int = 0
      while j < dim:
        if (parentWorkerByCommittedJob[j] == -(1)):
          if (minSlackValueByJob[j] < minSlackValue):
            minSlackValue = minSlackValueByJob[j]
            minSlackWorker = minSlackWorkerByJob[j]
            minSlackJob = j
        postInc(j)
    if (minSlackValue > 0):
      updateLabeling(minSlackValue)
    parentWorkerByCommittedJob[minSlackJob] = minSlackWorker
    if (matchWorkerByJob[minSlackJob] == -(1)):
      var committedJob: int = minSlackJob
      var parentWorker: int = parentWorkerByCommittedJob[committedJob]
      while (true):
        var temp: int = matchJobByWorker[parentWorker]
        match(parentWorker, committedJob)
        committedJob = temp
        if (committedJob == -(1)):
          break
        parentWorker = parentWorkerByCommittedJob[committedJob]
      return
    else:
      var worker: int = matchWorkerByJob[minSlackJob]
      committedWorkers[worker] = true
      block:
        var j: int = 0
        while j < dim:
          if (parentWorkerByCommittedJob[j] == -(1)):
            var slack: float = costMatrix[worker][j] - labelByWorker[worker] -
                labelByJob[j]
            if (minSlackValueByJob[j] > slack):
              minSlackValueByJob[j] = slack
              minSlackWorkerByJob[j] = worker
          postInc(j)

proc fetchUnmatchedWorker*(this: HungarianAlgorithm): int =
  var w: int
  block:
    w = 0
    while w < dim:
      if (matchJobByWorker[w] == -(1)):
        break
      postInc(w)
  return w


proc initializePhase*(this: HungarianAlgorithm; w: int): void =
  fill(committedWorkers, false)
  fill(parentWorkerByCommittedJob, -(1))
  committedWorkers[w] = true
  block:
    var j: int = 0
    while j < dim:
      minSlackValueByJob[j] =
          costMatrix[w][j] - labelByWorker[w] - labelByJob[j]
      minSlackWorkerByJob[j] = w
      postInc(j)



proc execute*(this: HungarianAlgorithm): seq[int] =
  reduce()
  computeInitialFeasibleSolution()
  greedyMatch()
  var w: int = fetchUnmatchedWorker()
  while (w < dim):
    initializePhase(w)
    executePhase()
    w = fetchUnmatchedWorker()

  result = matchJobByWorker[0 ..< rows]

  block:
    w = 0
    while w < result.len:
      if (result[w] >= cols):
        result[w] = -(1)
      postInc(w)
