import hmisc/types/colorstring
import unittest

suite "Nimscript colorstring":
  test "1":
    echo "eee".toRed()
    echo "eeee".toGreen()
