import Day18.Node
import lib.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day18 extends AocDay[List[Node], Int]("data/day18") :
  abstract class Node:
    var parent: Option[InnerNode]

  case class InnerNode(var parent: Option[InnerNode],
                       var left: Option[Node] = None,
                       var right: Option[Node] = None
                      ) extends Node {
    override def toString: String =
      (left, right) match
        case (Some(leftChild), Some(rightChild)) => s"[$leftChild,$rightChild]"
        case _ => ""

    override def equals(obj: Any): Boolean = super.equals(obj)
  }

  case class LeafNode(var parent: Option[InnerNode],
                      var value: Int
                     ) extends Node {
    override def toString: String = s"$value"
  }

  def parse(input: String): List[Node] =
    def parseNode(parent: Option[InnerNode])(input: String): Option[Node] =
      def findDelim(): Int =
        var counter = 0
        var res = -1
        range(1, input.length - 2).foreach(i => input(i) match
          case '[' => counter += 1
          case ']' => counter -= 1
          case ',' => if counter == 0 then res = i
          case _ =>
        )
        res

      val delimPos = findDelim()

      if delimPos != -1  then
        val node = InnerNode(parent)
        node.left = parseNode(Some(node))(input.substring(1, delimPos))
        node.right = parseNode(Some(node))(input.substring(delimPos + 1, input.length - 1))
        Some(node)
      else
        Some(LeafNode(parent, Integer.parseInt(input)))

    input.split('\n').toList.flatMap(parseNode(None))

  def reduce(tree: Node): Unit =
    var done = false
    while explode(tree) || split(tree) do None

  def explode(node: Node, level: Int = 0): Boolean =
    node match
      case _: LeafNode => false
      case innerNode: InnerNode =>
        (innerNode.left, innerNode.right) match
          case (Some(leftLeaf: LeafNode), Some(rightLeaf: LeafNode)) =>
            if level >= 4 then
              firstLeafOnTheLeft(leftLeaf) match
                case Some(leaf: LeafNode) => leaf.value += leftLeaf.value
                case None =>
              firstLeafOnTheRight(rightLeaf) match
                case Some(leaf: LeafNode) => leaf.value += rightLeaf.value
                case None =>
              innerNode.parent match
                case Some(parent) =>
                  if parent.left.contains(innerNode) then
                    parent.left = Some(LeafNode(Some(parent), 0))
                  else
                    parent.right = Some(LeafNode(Some(parent), 0))
                case None =>
              true
            else
              false
          case (Some(left), Some(right)) =>
            explode(left, level + 1) || explode(right, level + 1)

  def split(node: Node): Boolean =
    node match
      case innerNode: InnerNode =>
        (innerNode.left, innerNode.right) match
          case (Some(left), Some(right)) => split(left) || split(right)
          case (Some(left), _) => split(left)
          case (_, Some(right)) => split(right)
      case leafNode: LeafNode =>
        if leafNode.value < 10 then false
        else
          val splitNode = InnerNode(leafNode.parent)
          splitNode.left = Some(LeafNode(Some(splitNode), leafNode.value / 2))
          splitNode.right = Some(LeafNode(Some(splitNode), (leafNode.value + 1) / 2))
          leafNode.parent match
            case Some(parent) =>
              if parent.left.contains(leafNode) then
                parent.left = Some(splitNode)
              else
                parent.right = Some(splitNode)
            case None =>
          true

  def magnitude(node: Option[Node]): Int =
    node match
      case Some(node: InnerNode) => 3 * magnitude(node.left) + 2 * magnitude(node.right)
      case Some(node: LeafNode) => node.value
      case _ => 0

  def firstLeafOnTheLeft(node: Node): Option[LeafNode] =
    @tailrec
    def walkUp(node: Node): Option[InnerNode] =
      node.parent match
        case Some(parent: InnerNode) =>
          if parent.right.contains(node) then Some(parent)
          else walkUp(parent)
        case _ => None

    @tailrec
    def walkDown(node: Node): Option[LeafNode] =
      node match
        case leaf: LeafNode => Some(leaf)
        case innerNode: InnerNode =>
          innerNode.right match
            case Some(right) => walkDown(right)
            case _ => None

    node.parent match
      case Some(parent) =>
        walkUp(parent) match
          case Some(lca: InnerNode) =>
            lca.left match
              case Some(left) => walkDown(left)
              case _ => None
          case _ => None
      case _ => None

  def firstLeafOnTheRight(node: Node): Option[LeafNode] =
    @tailrec
    def walkUp(node: Node): Option[InnerNode] =
      node.parent match
        case Some(parent: InnerNode) =>
          if parent.left.contains(node) then Some(parent)
          else walkUp(parent)
        case _ => None

    @tailrec
    def walkDown(node: Node): Option[LeafNode] =
      node match
        case leaf: LeafNode => Some(leaf)
        case innerNode: InnerNode =>
          innerNode.left match
            case Some(left) => walkDown(left)
            case _ => None

    node.parent match
      case Some(parent) =>
        walkUp(parent) match
          case Some(lca: InnerNode) =>
            lca.right match
              case Some(right) => walkDown(right)
              case _ => None
          case _ => None
      case _ => None

  def combine(left: Node, right: Node): InnerNode =
    val tree = InnerNode(None, Some(left), Some(right))
    left.parent = Some(tree)
    right.parent = Some(tree)
    tree

  def copy(node: Option[Node], parent: Option[InnerNode] = None): Node =
    node match
      case Some(innerNode: InnerNode) =>
        val clonedNode = InnerNode(parent)
        clonedNode.left = Some(copy(innerNode.left, Some(clonedNode)))
        clonedNode.right = Some(copy(innerNode.right, Some(clonedNode)))
        clonedNode
      case Some(leafNode: LeafNode) =>
        LeafNode(parent, leafNode.value)

  def solve1(trees: List[Node]): Int =
    val finalTree = trees.tail.foldLeft(trees.head)((left, right) =>
      val tree = combine(left, right)
      reduce(tree);
      tree
    )
    magnitude(Some(finalTree))

  def solve2(trees: List[Node]): Int =
    val a = trees.toArray
    product(range(0, trees.length - 1), range(0, trees.length - 1)).map((i, j) =>
      if i == j then 0
      else
        val left = copy(Some(a(i)))
        val right = copy(Some(a(j)))
        val tree = combine(left, right)
        reduce(tree)
        magnitude(Some(tree))
    ).max
