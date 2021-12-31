import lib.*

import scala.collection.mutable

object Day23 extends AocDay[Array[Array[Char]], Long]("data/day23"):
  type Cell = (Int, Int)
  val weights = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  val cRoom = Map('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)
  val agents: List[Char] = weights.keys.toList

  object Direction:
    val Up: (Int, Int) = (-1, 0)
    val Down: (Int, Int) = (1, 0)
    val Left: (Int, Int) = (0, -1)
    val Right: (Int, Int) = (0, 1)

  class Node(val grid: Array[Array[Char]]):
    def apply(i: Int, j: Int): Char = this.grid(i)(j)
    def apply(cell: Cell): Char = this.grid(cell._1)(cell._2)
    def update(i: Int, j: Int, key: Char): Unit = this.grid(i)(j) = key
    def update(cell: Cell, key: Char): Unit = this.grid(cell._1)(cell._2) = key

    override def clone(): Node =
      val newGrid: Array[Array[Char]] = Array.ofDim[Char](grid.length, grid(0).length)
      for (r <- grid.indices)
        for (c <- grid(0).indices)
          newGrid(r)(c) = grid(r)(c)
      new Node(newGrid)

    /**
     * Constraints:
     * 1. Agents will never stop on the space immediately outside any room.
     * 2. Agents will never move from the hallway into a room unless that room is their destination room
     * 3. Once an agent stops moving in the hallway, it will stay in that spot until it can move into a room.
     *
     * From these constraints, we can reduce to 3 possible type of movements
     * 1. From room to hallway
     * 2. From room to room
     * 3. From hallway to room
     */
    def adjacencyList: List[Edge] =
      def emptyHallway(cx: Int, cy: Int): Boolean =
        range(cx, cy).forall(this(1, _) == '.')

      def roomRow(cTo: Int): Int =
        cRoom.find(_._2 == cTo).map(_._1) match
          case None => -1
          case Some(agent) =>
            range(2, grid.length - 2).findLast(this(_, cTo) == '.') match
              case None => -1
              case Some(rTo) =>
                range(rTo + 1, grid.length - 2).find(this(_, cTo) != agent) match
                  case Some(_) => -1
                  case None => rTo

      def fromRoomToHallway(fromCell: Cell): List[Edge] =
        val (rFrom, cFrom) = fromCell
        val agent = this(fromCell)
        assert(range(2, grid.length - 2).contains(rFrom))
        assert(agents.contains(agent))

        if rFrom > 2 && range(2, rFrom - 1).exists(this(_, cFrom) != '.') then
          return List()

        val hallwayCells = range(1, 11)
          .filter(!cRoom.values.toList.contains(_))
          .map((1, _))

        hallwayCells
          .filter(toCell =>
            val (rTo, cTo) = toCell
            val cLeft = math.min(cFrom, cTo)
            val cRight = math.max(cFrom, cTo)
            emptyHallway(cLeft, cRight)
          ).map(toCell =>
            val (rTo, cTo) = toCell
            val w = (rFrom - 1 + math.abs(cFrom - cTo)) * weights(agent)
            val neighbor: Node = clone()
            neighbor(fromCell) = '.'
            neighbor(toCell) = agent
            new Edge(neighbor, w)
          )

      def fromRoomToRoom(fromCell: Cell): Option[Edge] =
        val (rFrom, cFrom) = fromCell
        val agent = this(fromCell)
        assert(range(2, grid.length - 2).contains(rFrom))
        assert(agents.contains(agent))

        if rFrom > 2 && range(2, rFrom - 1).exists(this(_, cFrom) != '.') then
          return None

        val cTo = cRoom(agent)
        val cLeft = math.min(cFrom, cTo)
        val cRight = math.max(cFrom, cTo)
        if !emptyHallway(cLeft, cRight) then return None

        val rTo = roomRow(cTo)
        if rTo == -1 then return None

        val toCell = (rTo, cTo)
        val w = (cRight - cLeft + rFrom - 1 + rTo - 1) * weights(agent)
        val neighbor = clone()
        neighbor(fromCell) = '.'
        neighbor(toCell) = agent
        Some(new Edge(neighbor, w))

      def fromHallwayToRoom(fromCell: Cell): Option[Edge] =
        val (rFrom, cFrom) = fromCell
        val agent = this(fromCell)
        assert(rFrom == 1)
        assert(agents.contains(agent))

        val cTo = cRoom(agent)
        val cLeft = if cFrom < cTo then cFrom + 1 else cTo
        val cRight = if cFrom > cTo then cFrom - 1 else cTo
        if !emptyHallway(cLeft, cRight) then return None

        val rTo = roomRow(cTo)
        if rTo == -1 then return None

        val toCell = (rTo, cTo)
        val w = (cRight - cLeft + 1 + rTo - 1) * weights(agent)
        val neighbor = clone()
        neighbor(fromCell) = '.'
        neighbor(toCell) = agent
        Some(new Edge(neighbor, w))

      val hallwayCells = range(1, 11)
        .map((1, _))
        .filter(cell => agents.contains(this(cell)))
      val roomCells = product(range(2, grid.length - 2), cRoom.values.toList)
        .filter(cell => agents.contains(this(cell)))
        .filter(cell =>
          val (r, c) = cell
          c != cRoom(this(cell)) ||
            (r < grid.length - 2 && range(r + 1, grid.length - 2).exists(rr =>
              !cRoom.contains(this(rr, c)) || cRoom(this(rr, c)) != c))
            )
      val hallwayToRoomEdges: List[Edge] = hallwayCells.flatMap(fromHallwayToRoom)
      val roomToHallwayEdges: List[Edge] = roomCells.flatMap(fromRoomToHallway)
      val roomToRoomEdges: List[Edge] = roomCells.flatMap(fromRoomToRoom)
      val edges = hallwayToRoomEdges ++ roomToHallwayEdges ++ roomToRoomEdges
      edges

    def isFinal: Boolean =
      agents.forall(agent =>
        range(2, grid.length - 2).forall(r => this(r, cRoom(agent)) == agent)
      )

    override def toString: String =
      grid.map(row => row.mkString + '\n').mkString

    override def hashCode(): Int =
      toString.hashCode

    override def equals(obj: Any): Boolean =
      obj match
        case obj: Node => toString == obj.toString
        case _ => false

  class Edge(val v: Node, val w: Int)

  def parse(input: String): Array[Array[Char]] =
    input.split('\n').map(line => line.toCharArray)

  def Dijkstra(source: Node): Long =
    type PQEntry = (Node, Int, Option[Node]) // (v, d, u)
    val pq: mutable.PriorityQueue[PQEntry] = mutable.PriorityQueue()(Ordering.by[PQEntry, Int](_._2).reverse)
    val distance = mutable.Map[Node, Int]()
    distance(source) = 0
    pq.addOne(source, 0, None)
    val prev = mutable.Map[Node, Option[Node]]()

    while pq.nonEmpty do
      val (u, d, p) = pq.dequeue()
      prev(u) = p
      if u.isFinal then return d
      if d == distance(u) then
        u.adjacencyList.foreach(e =>
          if !distance.contains(e.v) || distance(u) + e.w < distance(e.v) then
            distance(e.v) = distance(u) + e.w;
            pq.addOne(e.v, distance(e.v), Some(u))
        )
    0L

  def solve1(grid: Array[Array[Char]]): Long =
    Dijkstra(new Node(grid))

  def solve2(grid: Array[Array[Char]]): Long =
    val newGrid = Array(
      grid(0), grid(1), grid(2),
      "###D#C#B#A###".toCharArray,
      "###D#B#A#C###".toCharArray,
      grid(3), grid(4)
    )
    Dijkstra(new Node(newGrid))