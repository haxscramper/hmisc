import hmisc/types/colortext
import unittest

suite "Colortext c++":
  echo colorizeToStr(
    "int main () { /* Long comment */ }",
    cppStyleMap,
    "c++"
  )
