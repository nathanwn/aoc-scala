package years.y2021

import Day19.Vec
import lib.day.AocDay

import scala.collection.mutable

object Day19 extends AocDay[Array[List[Vec]], Int]:
  def cos(alpha: Int): Int = Math.round(Math.cos(Math.toRadians(alpha))).toInt
  def sin(alpha: Int): Int = Math.round(Math.sin(Math.toRadians(alpha))).toInt

  class Vec(val n: Int, val keys: Array[Int]) extends Ordered[Vec]:
    def this(n: Int) = this(n, Array.fill(n)(0))
    def this(keys: Array[Int]) = this(keys.length, keys)

    def apply(i: Int): Int = this.keys(i)
    def update(i: Int, key: Int): Unit = this.keys(i) = key

    def +(that: Vec): Vec =
      val res = Vec(this.n)
      for (i <- 0 until this.n) res(i) = this(i) + that(i)
      res

    def -(that: Vec): Vec =
      val res = Vec(this.n)
      for (i <- 0 until this.n) res(i) = this(i) - that(i)
      res

    def compare(that: Vec): Int = {
      (0 until this.n).find(i => this(i) != that(i)) match
        case Some(i) =>
          if this(i) < that(i) then -1
          else 1
        case None => 0
    }

    override def equals(that: Any): Boolean =
      that match
        case that: Vec =>
          (0 until this.n).find(i => this(i) != that(i)) match
            case Some(_) => false
            case None    => true
        case _ => false

    def distManhattan(that: Vec): Int =
      this.keys.zip(that.keys).map((x, y) => Math.abs(x - y)).sum

    override def toString: String =
      val builder: StringBuilder = StringBuilder()
      builder.append("Vec[")
      builder.append(keys.head)
      keys.tail.foreach(key => builder.append(s",$key"))
      builder.append("]")
      builder.toString

  class SqMat(val n: Int, keys: Array[Array[Int]]):
    def this(n: Int) = this(n, Array.fill(n, n)(0))
    def this(keys: Array[Array[Int]]) = this(keys.length, keys)

    def apply(i: Int, j: Int): Int = this.keys(i)(j)
    def update(i: Int, j: Int, key: Int): Unit = this.keys(i)(j) = key

    def *(that: SqMat): SqMat =
      val res = SqMat(this.n)
      for (i <- 0 until this.n)
        for (j <- 0 until this.n)
          for (k <- 0 until this.n)
            res(i, j) = res(i, j) + this(i, k) * that(k, j)
      res

    def *(that: Vec): Vec =
      val res = Vec(this.n)
      for (i <- 0 until this.n)
        for (j <- 0 until this.n)
          res(i) = res(i) + this(i, j) * that(j)
      res

    def rotateX(a: Int): SqMat =
      this * SqMat(
        Array(
          Array(1, 0, 0),
          Array(0, cos(a), -sin(a)),
          Array(0, sin(a), cos(a))
        )
      )

    def rotateY(a: Int): SqMat =
      this * SqMat(
        Array(
          Array(cos(a), 0, sin(a)),
          Array(0, 1, 0),
          Array(-sin(a), 0, cos(a))
        )
      )

    def rotateZ(a: Int): SqMat =
      this * SqMat(
        Array(
          Array(cos(a), -sin(a), 0),
          Array(sin(a), cos(a), 0),
          Array(0, 0, 1)
        )
      )

    override def toString: String =
      val builder: StringBuilder = StringBuilder()
      builder.append("SqMat[\n")
      keys.foreach(row =>
        builder.append("  [");
        builder.append(row.head);
        row.tail.foreach(value => builder.append(s",$value"));
        builder.append("]\n")
      )
      builder.append("]")
      builder.toString

  def parse(input: String): Array[List[Vec]] =
    input
      .substring(input.indexOf("\n") + 1)
      .split("\n\n--- scanner \\d* ---\n")
      .map(scannerInput =>
        scannerInput
          .split('\n')
          .toList
          .map(line => new Vec(line.split(',').map(Integer.parseInt)))
      )

  def intersect(sortedListA: List[Vec], sortedListB: List[Vec]): Int =
    if sortedListA.isEmpty || sortedListB.isEmpty then 0
    else if sortedListA.head == sortedListB.head then
      1 + intersect(sortedListA.tail, sortedListB.tail)
    else if sortedListA.head < sortedListB.head then
      intersect(sortedListA.tail, sortedListB)
    else intersect(sortedListA, sortedListB.tail)

  def connect(us: List[Vec], vs0: List[Vec], B: SqMat): Option[Vec] =
    val vs = vs0.map(v => B * v)
    (for
      u0 <- us;
      v0 <- vs
    yield (u0, v0))
      .find((u0: Vec, v0: Vec) =>
        val du = us.map(u => u - u0).sorted
        val dv = vs.map(v => v - v0).sorted
        intersect(du, dv) >= 12
      ) match
      case Some(u0, v0) =>
        // u0 and v0 are coordinates of the same points
        // with the same basis but different origins Ou and Ov
        // d = (Ou, Ov) = u0 - v0
        Some(u0 - v0)
      case _ => None

  class Position(val from: Int, val o: Vec, val B: SqMat)

  def solve(scanners: Array[List[Vec]]): Array[Position] =
    val B0 = SqMat(
      Array(
        Array(1, 0, 0),
        Array(0, 1, 0),
        Array(0, 0, 1)
      )
    )

    val Bs = (0 until 359 by 90)
      .flatMap(ax =>
        (0 until 359 by 90).flatMap(ay =>
          (0 until 359 by 90).map(az => B0.rotateX(ax).rotateY(ay).rotateZ(az))
        )
      )
      .distinctBy(_.toString)

    val ps = Array.fill[Option[Position]](scanners.length)(None)
    val q: mutable.Queue[Int] = mutable.Queue()

    ps(0) = Some(Position(-1, Vec(3), B0))
    q.enqueue(0)

    while q.nonEmpty do
      val i = q.dequeue()
      ps(i) match
        case Some(p) => println(s"(${p.from}, $i)")
        case _       =>
      for (j <- scanners.indices)
        if i != j && ps(j).isEmpty then
          Bs.find(B =>
            (connect(scanners(i), scanners(j), B), ps(i)) match
              case (Some(d), Some(p)) =>
                ps(j) = Some(Position(i, p.o + p.B * d, p.B * B));
                q.enqueue(j)
                true
              case _ => false
          )

    ps.flatten

  def solve1(scanners: Array[List[Vec]]): Int =
    val set = mutable.TreeSet[Vec]()
    val ps = solve(scanners)
    (0 until ps.length).foreach(i =>
      val p = ps(i)
      scanners(i).map(relative => p.o + p.B * relative).map(set.add)
    )
    set.size

  def solve2(scanners: Array[List[Vec]]): Int =
    val ps = solve(scanners)
    val coors = ps.toList.map(_.o)
    (for
      u <- coors;
      v <- coors
    yield u.distManhattan(v)).max
