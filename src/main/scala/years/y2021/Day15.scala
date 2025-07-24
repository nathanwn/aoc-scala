package years.y2021

import lib.day.AocDay

import scala.collection.mutable

object Day15 extends AocDay[Array[Array[Int]], Int]:
    def parse(input: String): Array[Array[Int]] =
        input.split('\n').map(line => line.map(c => c - '0').toArray)

    // Just regular O(|E|log(|E|)) Dijkstra implementation, nothing special :)
    def solve(grid: Array[Array[Int]]): Int =
        val R = grid.length
        val C = grid(0).length

        type PQEntry = ((Int, Int), Int) // ((r, c), d)
        // Min PQ by d
        val pq: mutable.PriorityQueue[PQEntry] =
            mutable.PriorityQueue()(Ordering.by[PQEntry, Int](_._2).reverse)
        val distance = mutable.Map[(Int, Int), Int]()

        for
            r <- 0 until R;
            c <- 0 until C
        do distance((r, c)) = Integer.MAX_VALUE

        val s = (0, 0)
        pq.addOne(s, 0)
        distance(s) = 0

        while pq.nonEmpty do
            val (u, d) = pq.dequeue()
            val (ur, uc) = u

            if u == (R - 1, C - 1) then return distance(u)

            if d == distance(u) then
                List((-1, 0), (1, 0), (0, -1), (0, 1)).foreach((dr, dc) =>
                    val v: (Int, Int) = (ur + dr, uc + dc)
                    val (vr, vc) = v
                    if 0 <= vr && vr < grid.length && 0 <= vc && vc < grid(
                          0
                        ).length
                        && distance(u) + grid(vr)(vc) < distance(v)
                    then
                        distance(v) = distance(u) + grid(vr)(vc);
                        pq.addOne(v, distance(v))
                )
        0

    def solve1(input: Array[Array[Int]]): Int =
        solve(input)

    def solve2(input: Array[Array[Int]]): Int =
        val R = input.length
        val C = input(0).length
        val grid = Array.ofDim[Int](R * 5, C * 5)
        for
            kr <- 0 to 4;
            kc <- 0 to 4;
            r <- 0 until R;
            c <- 0 until C
        do grid(kr * R + r)(kc * C + c) = (input(r)(c) - 1 + kr + kc) % 9 + 1
        solve(grid)
