import unittest

# import hmisc/macros/cl_loop

# test "Simple value collect":
#   check loop1((lfor i in 0..2; lcoll i)) == @[0, 1, 2]
#   check loop1((lfor i in 0..2; lmax i)) == 2

# test "Return type inference":
#   check loopt(
#     (lfor i in 0..2; lcoll i; lmax i)
#   ) is tuple[lcoll: seq[int], lmax: int]

#   check block:
#           let (vals, lmax) = loopt((lfor i in 0..2; lcoll i; lmax i))
#           vals == @[0, 1, 2] and lmax == 2
