package years.y2021

import Day16.Node
import lib.day.AocDay

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day16 extends AocDay[Node, Long]:
    abstract class Node(version: Int)
    private case class Literal(version: Int, value: Long) extends Node(version)
    private case class Operator(version: Int, typeId: Int, children: Seq[Node])
        extends Node(version)

    def parse(input: String): Node =
        // My first attempt writing parser combinators
        // Each parse function returns the thing it tries to parse + the remaining string
        def parseLiteral(bits: String): (Long, String) =
            val builder = new StringBuilder
            var p = 0
            while bits.charAt(p) == '1' do
                builder.append(bits.substring(p + 1, p + 5))
                p += 5
            builder.append(bits.substring(p + 1, p + 5))
            p += 5
            (BigInt(builder.toString(), 2).longValue, bits.substring(p))

        def parseOperator(bits: String): (Seq[Node], String) =
            val lengthTypeId = bits.charAt(0)
            if lengthTypeId == '0' then
                val length = Integer.parseInt(bits.substring(1, 16), 2)
                var remain = bits.substring(16, length + 16)
                val children = new ListBuffer[Node]
                while remain.nonEmpty do
                    val res: (Node, String) = parseNode(remain)
                    remain = res._2
                    children.append(res._1)
                (children.toList, bits.substring(length + 16))
            else
                val numSubNodes = Integer.parseInt(bits.substring(1, 12), 2)
                var remain = bits.substring(12)
                val children = (1 to numSubNodes).map(_ =>
                    val res: (Node, String) = parseNode(remain)
                    remain = res._2
                    res._1
                )
                (children, remain)

        def parseNode(bits: String): (Node, String) =
            val version = Integer.parseInt(bits.substring(0, 3), 2)
            val typeId = Integer.parseInt(bits.substring(3, 6), 2)
            val packets = new ListBuffer
            var remain = bits.substring(6)
            if typeId == 4 then
                val res = parseLiteral(remain)
                val value = res._1
                remain = res._2
                (Literal(version, value), remain)
            else
                val res = parseOperator(remain)
                val children = res._1
                remain = res._2
                (Operator(version, typeId, children), remain)

        def hexToBin(c: Char): String =
            (if '0' <= c && c <= '9' then c - '0'
             else c - 'A' + 10).toBinaryString.reverse.padTo(4, '0').reverse

        val bits = input.map(hexToBin).mkString
        parseNode(bits)._1

    override def solve1(root: Node): Long =
        def visit(node: Node): Long =
            node match
                case Literal(version, _) => version
                case Operator(version, _, children) =>
                    version + children.map(visit).sum

        visit(root)

    override def solve2(root: Node): Long =
        def visit(node: Node): Long =
            node match
                case Literal(_, value) => value
                case Operator(_, typeId, children) =>
                    typeId match
                        case 0 => children.map(visit).sum
                        case 1 => children.map(visit).product
                        case 2 => children.map(visit).min
                        case 3 => children.map(visit).max
                        case 5 =>
                            children
                                .map(visit)
                                .reduce((x, y) => if x > y then 1 else 0)
                        case 6 =>
                            children
                                .map(visit)
                                .reduce((x, y) => if x < y then 1 else 0)
                        case 7 =>
                            children
                                .map(visit)
                                .reduce((x, y) => if x == y then 1 else 0)

        visit(root)
