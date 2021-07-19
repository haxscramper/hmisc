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
# - Matcher.java
# - CompositeMatcher.java



import ./jcommon, ./tree, ./mapping_store

import std/[sequtils]

{.this: this.}

type
  Matcher* = ref object of RootObj
    name*: string

  CompositeMatcher* = ref object of Matcher
    matchers*: seq[Matcher]


method match*(
    matcher: Matcher, src, dst: Tree, mappings: MappingStore
  ): MappingStore {.base.} =
  raise newImplementBaseError(matcher, "match")

method match*(
    this: CompositeMatcher; src: Tree; 
    dst: Tree; mappings: MappingStore): MappingStore =

  var mappings = mappings

  for matcher in matchers:
    mappings = matcher.match(src, dst, mappings)
  return mappings

proc newCompositeMatcher*(matcher: varargs[Matcher]): CompositeMatcher = 
  CompositeMatcher(matchers: toSeq(matcher))
