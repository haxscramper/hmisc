import types/[seq2d, hprimitives]

# REFACTOR remove
proc toSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  makeSeq2D(s)

proc toStrBlock*(s: seq[string]): StrBlock =
  StrBlock(s)
