import hmisc/cl_loop

template notCompiles(body: untyped): untyped =
  not compiles(body)


# echo loop1((lfor i in 0..2; lcoll b, i))
