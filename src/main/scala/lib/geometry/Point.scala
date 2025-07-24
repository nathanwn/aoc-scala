package lib.geometry

class Point[T](val x: T, val y: T):
    def this(coor: (T, T)) =
        this(coor(0), coor(1))
