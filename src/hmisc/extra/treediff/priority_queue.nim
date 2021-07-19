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
# - DefaultPriorityTreeQueue.java
# - PriorityTreeQueue.java

import hmisc/types/hmap

import ./tree, ./jcommon

{.this: this.}

type
  PriorityTreeQueue* = ref object
    priorityCalculator*: proc(tree: Tree): int
    trees*: Map[int, seq[Tree]]
    minimumPriority*: int

proc currentPriority*(this: PriorityTreeQueue): int =
  return trees.lastPair().key

proc setMinimumPriority*(this: PriorityTreeQueue; minimumPriority: int): void =
  this.minimumPriority = minimumPriority

proc setPriorityCalculator*(
    this: PriorityTreeQueue;
    priorityCalculator: proc(tree: Tree): int
  ): void =

  this.priorityCalculator = priorityCalculator

proc getMinimumPriority*(this: PriorityTreeQueue): int =
  return this.minimumPriority

proc len*(this: PriorityTreeQueue): int = trees.len

proc add*(this: PriorityTreeQueue; t: Tree): void =
  var priority: int = priorityCalculator(t)

  if (priority < this.getMinimumPriority()):
    return

  if priority notin trees:
    trees[priority] = @[]

  trees[priority].add(t)



proc newPriorityTreeQueue*(
    root: Tree; 
    minimumPriority: int;
    priorityCalculator: proc(tree: Tree): int
  ): PriorityTreeQueue =

  assertRef root
  assertRef priorityCalculator

  new(result)
  result.setMinimumPriority(minimumPriority)
  result.setPriorityCalculator(priorityCalculator)
  result.add(root)

proc pop*(this: PriorityTreeQueue): seq[Tree] =
  discard trees.pop(currentPriority(), result)
  # return trees.remove(currentPriority())

proc open*(this: PriorityTreeQueue; tree: Tree): void =
  for c in tree.getChildren():
    add(c)

proc popOpen*(this: PriorityTreeQueue): seq[Tree] =
  var pop: seq[Tree] = pop()
  for t in pop:
    open(t)
  return pop




proc isEmpty*(this: PriorityTreeQueue): bool =
  return trees.len() == 0

proc clear*(this: PriorityTreeQueue): void =
  trees.clear()

proc synchronize*(
    q1: PriorityTreeQueue;
    q2: PriorityTreeQueue
  ): void =

  if (q1.isEmpty() or q2.isEmpty()):
    q1.clear()
    q2.clear()
    return
  
  while (q1.currentPriority() != q2.currentPriority()):
    var pop: seq[Tree] = tern(q1.currentPriority() > q2.currentPriority(),
                              q1.popOpen(), q2.popOpen())
    if (q1.isEmpty() or q2.isEmpty()):
      q1.clear()
      q2.clear()
      return
