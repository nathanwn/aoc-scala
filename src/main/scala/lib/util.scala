package lib

def isTrue(p: Boolean): Boolean = p

def range(start: Int, end: Int, step: Int = 1): List[Int] =
  (start to end by step).toList

def product[A, B](la: List[A], lb: List[B]): List[(A, B)] =
  la.flatMap(x => lb.map(y => (x, y)))

def clearBits(x: Int, bits: List[Int]): Int =
  (List(x) ::: bits).reduce((mask, bit) => mask & ~(1 << bit))

def getSoleBitIndex(x: Int): Int =
  var p = Integer.lowestOneBit(x)
  var idx = -1
  while (p > 0) {
    p >>= 1
    idx += 1
  }
  idx