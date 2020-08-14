type
  Private* = object
    field1: int
    field2: float
    case kind*: bool
      of true:
        field3: string
      of false:
        field4: seq[int]
