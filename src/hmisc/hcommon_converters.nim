import types/[seq2d, hprimitives]

# REFACTOR remove
converter toSeq2D*[T](s: seq[seq[T]]): Seq2d[T] =
  makeSeq2D(s)

converter toStrBlock*(s: seq[string]): StrBlock =
  StrBlock(s)
