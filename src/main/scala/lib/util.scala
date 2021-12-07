package lib

def isTrue(p: Boolean): Boolean = p

def range(start: Int, end: Int, step: Int = 1): List[Int] =
  (start to end by step).toList