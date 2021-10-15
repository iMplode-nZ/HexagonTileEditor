package util

import scala.language.implicitConversions

class AsciiFormatter {
    def format(options: AsciiFormatter.Options): AsciiFormatter.BoundedCharGrid = {
        options.nodes.foldLeft(options.background)((grid, x) => grid + (x.draw --> options.origin))
    }
}

object AsciiFormatter {
    val charRatio = 0.6
    case class Options(nodes: Iterable[Node], background: BoundedCharGrid, origin: Point)
    trait Node {
        def draw: CharGrid
    }
    class Shape(val grid: CharGrid, val mask: CharMask = CharMask.empty) extends Node {
        def draw: CharGrid = grid
        def shift(distance: Point): Shape = new Shape(grid --> distance, mask)
        def -->(distance: Point): Shape = shift(distance)
        def <(m: CharMask): Shape = new Shape(grid < m, mask & m)
    }
    abstract class CharMask { self =>
        def contains(x: Int, y: Int): Boolean

        def contains(a: Point): Boolean = contains(a.x, a.y)

        def union(mask: CharMask): CharMask = new CharMask {
            def contains(x: Int, y: Int): Boolean = self.contains(x, y) || mask.contains(x, y)
        }
        def |(mask: CharMask): CharMask = union(mask)

        def opposite: CharMask = new CharMask {
            def contains(x: Int, y: Int) = !self.contains(x, y)
        }
        def unary_! : CharMask = opposite

        def intersection(mask: CharMask): CharMask = new CharMask {
            def contains(x: Int, y: Int): Boolean = self.contains(x, y) && mask.contains(x, y)
        }
        def &(mask: CharMask): CharMask = intersection(mask)

        def subtraction(mask: CharMask): CharMask = self & !mask
        def -(mask: CharMask): CharMask = subtraction(mask)

        def xor(mask: CharMask): CharMask = new CharMask {
            def contains(x: Int, y: Int): Boolean = self.contains(x, y) ^ mask.contains(x, y)
        }
        def ^(mask: CharMask): CharMask = xor(mask)

        def shift(origin: Point): CharMask = new CharMask {
            def contains(x: Int, y: Int): Boolean = self.contains(Point(x, y) - origin)
        }
    }
    object CharMask {
        val empty = new CharMask {
            def contains(x: Int, y: Int): Boolean = false
        }
        def fromCharGrid(grid: CharGrid): CharMask = new CharMask {
            def contains(x: Int, y: Int): Boolean = grid.contains(x, y)
        }
        def fromString(x: String): CharMask = fromCharGrid(CharGrid.fromString(x))
    }
    object BoundedCharGrid {
        def empty(x: Array[Array[CharLike]]) = CharGrid.empty.flatten(x, Point(0, 0))
        def fromString(x: String): BoundedCharGrid = new BoundedCharGrid(x.split("\n"))
        def fromSeq(x: Seq[String]): BoundedCharGrid = new BoundedCharGrid(x)
        def fromCAA(x: Array[Array[CharLike]]): BoundedCharGrid = new BoundedCharGrid(x.map(a => a.mkString))
    }
    class BoundedCharGrid private(x: Seq[String]) extends CharGrid {
        override def toString: String = x.mkString("\n")
        def toSeq: Seq[String] = x
        def toCAA: Array[Array[CharLike]] = {
            val arr = new Array[Array[CharLike]](height)
            toSeq.zipWithIndex.foreach(a => arr(a._2) = a._1.toArray)
            arr
        }
        def at(a: Int, b: Int) = toSeq(b).charAt(a)
        def contains(a: Int, b: Int): Boolean = b < height && b >= 0 && a < width(b) && a >= 0
        def height: Int = toSeq.size
        def width(a: Int): Int = toSeq(a).size

        override def overlay(a: CharGrid): BoundedCharGrid = super.overlay(a).flatten(toCAA, Point(0,0))
        override def +(a: CharGrid): BoundedCharGrid = overlay(a)
    }
    object CharGrid {
        val nothing = ' '
        val empty = new CharGrid {
            def at(x: Int, y: Int): CharLike = ' '
            def contains(x: Int, y: Int): Boolean = false
        }
        def fromString(a: String): CharGrid = {
            val split = a.split("\n")
            new CharGrid {
                def at(x: Int, y: Int): CharLike = split(y).charAt(x)
                def contains(x: Int, y: Int): Boolean = y < split.size && y >= 0 && x < split(y).size && x >= 0 && split(y).charAt(x) != nothing
            }
        }
        def fromChar(a: CharLike): CharGrid = new CharGrid {
            def at(x: Int, y: Int): CharLike = a
            def contains(x: Int, y: Int): Boolean = true
        }
    }
    abstract class CharGrid { self =>
        def at(x: Int, y: Int): CharLike
        def contains(x: Int, y: Int): Boolean

        def at(a: Point): CharLike = at(a.x, a.y)
        def contains(a: Point): Boolean = contains(a.x, a.y)

        def overlay(a: CharGrid): CharGrid = new CharGrid {
            def at(x: Int, y: Int): CharLike = if(a.contains(x, y)) a.at(x, y) else self.at(x, y)
            def contains(x: Int, y: Int): Boolean = a.contains(x, y) || self.contains(x, y)
        }
        def +(a: CharGrid): CharGrid = overlay(a)

        def flatten(grid: Array[Array[CharLike]], start: Point = Point.origin): BoundedCharGrid = {
            for {
                i <- 0 until grid.size
                current = grid(i)
                j <- 0 until current.size
            } current(j) = {
                val x = start.x + j
                val y = start.y + i
                if(contains(x, y)) at(x, y) else CharGrid.nothing
            }
            BoundedCharGrid.fromCAA(grid)
        }
        def ->|(grid: Array[Array[CharLike]], start: Point = Point.origin): BoundedCharGrid = flatten(grid, start)

        def shift(origin: Point): CharGrid = new CharGrid {
            def at(x: Int, y: Int) = self.at(Point(x, y) - origin)
            def contains(x: Int, y: Int) = self.contains(Point(x, y) - origin)
        }
        def -->(origin: Point): CharGrid = shift(origin)

        def mask(a: CharMask): CharGrid = new CharGrid {
            def at(x: Int, y: Int) = self.at(x, y)
            def contains(x: Int, y: Int) = self.contains(x, y) && a.contains(x, y)
        }
        def <(a: CharMask): CharGrid = mask(a)
    }
    type CharLike = Char
    case class Point(x: Int, y: Int) {
        def unary_- = Point(-x, -y)
        def +(other: Point) = Point(x + other.x, y + other.y)
        def -(other: Point) = Point(x - other.x, y - other.y)
        def *(other: Int) = Point(x * other, y * other)
        def *(other: Double) = Point(Math.round(x * other).toInt, Math.round(y * other).toInt)
        def ?[T](other: Point)(compass: ((T, T, T), (T, T, T), (T, T, T), (T, T, T)), equals: (T, T, T, T), exact: T) = {
            val dist = other - this
            dist match {
                case Point(0, 0) => exact
                case Point(0, x) if x > 0 => equals._1
                case Point(x, 0) if x > 0 => equals._2
                case Point(0, x) if -x > 0 => equals._3
                case Point(x, 0) if -x > 0 => equals._4
                case Point(x, y) if x == y && x > 0 => compass._1._2
                case Point(x, y) if -x == y && -x > 0 => compass._2._2
                case Point(x, y) if -x == -y && -x > 0 => compass._3._2
                case Point(x, y) if x == -y && x > 0 => compass._4._2
                case Point(x, y) if x > y && y > 0 => compass._1._1
                case Point(x, y) if y > x && x > 0 => compass._1._3
                case Point(x, y) if -x > y && y > 0 => compass._2._3
                case Point(x, y) if y > -x && -x > 0 => compass._2._1
                case Point(x, y) if -x > -y && -y > 0 => compass._3._1
                case Point(x, y) if -y > -x && -x > 0 => compass._3._3
                case Point(x, y) if x > -y && -y > 0 => compass._4._3
                case Point(x, y) if -y > x && x > 0 => compass._4._1
            }
        }
/*
       (4)
(3) \(3)|(1)/ (4)
     \  |  /
      \ | /
    (1)\|/(3)
(3) ----+---- (1)
    (3)/|\(1)
      / | \
     /  |  \
(2) /(1)|(3)\ (1)
       (2)
(Ray from this to other)
*/
        override def toString = "(" + x + ", " + y + ")"
    }
    object Point {
        val origin = Point(0, 0)
        val x = Point(1, 0)
        val y = Point(0, 1)

        def max(points: Point*): Point = Point(points.map(a => a.x).max, points.map(a => a.y).max)
        def min(points: Point*): Point = Point(points.map(a => a.x).min, points.map(a => a.y).min)
        def abs(point: Point): Point = Point(math.abs(point.x), math.abs(point.y))
    } //Starts from (0, 0), goes down and to the right.
}