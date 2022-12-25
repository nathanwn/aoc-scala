package lib.geometry

class Segment[T: Numeric](val start: Point[T], val end: Point[T]):
  def this(pointPair: (Point[T], Point[T])) =
    this(pointPair(0), pointPair(1))
