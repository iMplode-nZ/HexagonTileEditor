import org.jline.terminal.TerminalBuilder

import event._
import eventloop._
import Loop._

import scala.io.StdIn

import util.AsciiFormatter
import util.AsciiFormatter._
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}

object Main {
    import Hexagon._
    import Tiles.Packet

    inline val size = 3
    inline val insert = 2
    inline val spacing = 4

    inline val height = 50
    inline val width = 230
    inline val divider = 50
    inline val speed = 1000 / 30
    inline val flash = 1000

    inline def home: Point = Point(100, 25)

    inline val sideInsert = 4
    inline val sideSize = 6
    inline val hiddenDivider = -2
    inline def startSide: Point = Point(13, 5)
    inline def textStart: Point = Point(2, 20)
    val rotations = Array(Direction.Up, Direction.Rup, Direction.Rdown, Direction.Down, Direction.Ldown, Direction.Lup)
    val clear = "\033[H\033[2J"

    def hex: Hexagon = Hexagon(Point.origin, size, insert = insert)


    def main(args: Array[String]): Unit = {
        //Init:

        println("Test")

        var on = true

        def flashHex(pos: Point = Point(0, 0), size: Int = size, insert: Int = insert): Hexagon = {
            val sflash: Char => Char = c => if(on) c else ' '
            val uflash: Char => Char = c => if(on) '_' else ' '
            Hexagon(pos, size, insert, sflash, uflash, sflash, sflash, uflash, sflash)
        }

        val display = Box(Point(divider, 0), Point(width - 1, height - 3))
        val info = Box(Point(0, 0), Point(divider, height - 3))
        val footer = Box(Point(0, height - 3), Point(width - 1, height - 1))
        val layout = ShapeCluster(Seq(footer, info, display))
        val mesh = new DrawingMesh(Tile(hex, 0, Direction.Up))

        val terminal = TerminalBuilder.terminal
        terminal.enterRawMode
        val reader = terminal.reader
        val writer = terminal.writer

        var current = mesh(Position.origin).get
        var position = home
        var currentOutline = flashHex(Point.origin, size + 1)
        var footText = ""

        var ignore = false

        //Loop:

        def packet: Packet = Packet(mesh, current, current.value.rotation)

        def draw(): Unit = mesh.synchronized(current.synchronized {
            val data = Tiles.data(current.value.id)
            def side: Node = new Cluster(Seq(
                Hexagon(startSide, sideSize, insert = sideInsert),
                new Shape(data.big(packet).shift(startSide)),
                new Shape(CharGrid.fromString(data.name(packet) + " (" + current.value.id + ")" + ":") --> textStart),
                new Shape(CharGrid.fromString(data.info(packet)) --> (textStart + Point(1, 2)))
            ))
            writer.print(clear)
            val format = new AsciiFormatter().format(Options(Seq(
                mesh.toShape(spacing, insert).shift(position) < display.mask,
                new Shape(currentOutline.draw --> (position - Point(2, 1)) < display.mask),
                new Shape(CharGrid.fromString(footText) --> Point(1, height - 2)),
                side,
                layout
            ), BoundedCharGrid.empty(Array.ofDim[CharLike](height, width)), Point.origin))
            writer.println(format)
            writer.flush
        })
        def calculate(direction: Direction): Unit = mesh.synchronized(current.synchronized {
            val newpos = current.position + direction
            if(newpos == Position.origin || Direction.values.exists(a => mesh.at(newpos + a).exists(x => x.value.id != -1))) {
                on = true
                ignore = true
                val old = current
                val at = mesh.at(current, direction)
                current = at.getOrElse((mesh += (newpos, Tile(hex, -1, Direction.Up))).get)
                currentOutline = flashHex(newpos.toPoint(spacing, insert), size + 1)
                if(old.value.id == -1) mesh.remove(old)
            }
        })

        var toExit = false


        // This makes sure that both threads are exited.
        def texit(): Unit = if(toExit) System.exit(0)

        def exit(): Unit = {
            toExit = true
            System.exit(0)
        }


        var lastKeyPress = 0;
        var numLastKey = 1

        def displayChar(x: Int): String = {
            if(lastKeyPress == x) {
                numLastKey += 1
            } else numLastKey = 1
            lastKeyPress = x

            val charFixed = x.toChar match {
                case 13 => "[Enter]"
                case 127 => "[Delete]"
                case a => "'" + a.toString + "'"
            }
            val numberOfTimes = if(numLastKey > 1) {
                " (x" + numLastKey + ")"
            } else { "" }
            "Last Keypress: " + charFixed + numberOfTimes + ". "
        }

        new Thread(() => while(true) {
            var read = reader.read
            if(read == 27) exit()
            else if(read == 'w') calculate(Direction.Up)
            else if(read == 's') calculate(Direction.Down)
            else if(read == 'a') {
                footText = displayChar(read)
                footText += "Enter 'w' to go left up, 's' to go left down."
                read = reader.read
                if(read == 'w') calculate(Direction.Lup)
                else if(read == 's') calculate(Direction.Ldown)
                footText = ""
            }
            else if(read == 'd') {
                footText = displayChar(read)
                footText += "Enter 'w' to go right up, 's' to go right down."
                read = reader.read
                if(read == 'w') calculate(Direction.Rup)
                else if(read == 's') calculate(Direction.Rdown)
                footText = ""
            } else if(read == 'q') current.synchronized {
                val index = (((rotations.zipWithIndex.find(a => a._1 == current.value.rotation).get._2 - 1) % rotations.length) + rotations.length) % rotations.length
                current = Tiles.data(current.value.id).rotate(Packet(mesh, current, rotations(index)))
            } else if(read == 'e') current.synchronized {
                val index = (((rotations.zipWithIndex.find(a => a._1 == current.value.rotation).get._2 + 1) % rotations.length) + rotations.length) % rotations.length
                current = Tiles.data(current.value.id).rotate(Packet(mesh, current, rotations(index)))
            } else if(read == 13 && current.value.id == -1) current.synchronized {
                current = (mesh += (current.position, Tile(hex, 0, Direction.Up))).get
            }
            else if(read == 'i') position += Point(0, 1)
            else if(read == 'k') position += Point(0, -1)
            else if(read == 'j') position += Point(1, 0)
            else if(read == 'l') position += Point(-1, 0)
            else if(read == 'u') current.synchronized {
                val id = current.value.id
                val before = id - 1
                if(Tiles.data.contains(before) && id > hiddenDivider) {
                    Tiles.data(id).remove(packet)
                    current = (mesh += (current.position, Tile(hex, before, current.value.rotation))).get
                    val data = Tiles.data(before)
                    data.add(packet)
                    data.rotate(packet)
                }
            } else if(read == 'o') current.synchronized {
                val id = current.value.id
                val after = id + 1
                if(Tiles.data.contains(after)) {
                    Tiles.data(id).remove(packet)
                    current = (mesh += (current.position, Tile(hex, after, current.value.rotation))).get
                    val data = Tiles.data(after)
                    data.add(packet)
                    data.rotate(packet)
                }
            }
            else if(read == 'h') position = home
            else if(read == 127) if(current.position != Position.origin) current = (mesh += (current.position, Tile(hex, -1, Direction.Up))).get

            footText = displayChar(read)
        }).start

        loop.setInterval(() => if(ignore) ignore = false else on = !on, flash)
        loop.setInterval(() => {
            texit()
            draw()
        }, speed)

        loop.start()
    }
}

case class Box(first: Point, second: Point) extends Shape({
    val start = Point.min(first, second)
    val end = Point.max(first, second)
    val width = end.x - 1 - start.x
    val height = end.y - 1 - start.y
    CharGrid.fromString("+" + "-" * width + "+" + "\n" + ("|" + " " * width + "|" + "\n") * height + "+" + "-" * width + "+").shift(start)
}, {
    val start = Point.min(first, second) + Point(1, -1)
    val end = Point.max(first, second) + Point(-1, 1)
    val width = end.x - 1 - start.x
    val height = end.y - 1 - start.y
    CharMask.fromString(("+" * width + "\n") * height).shift(start + Point(1, 1))
})
case class Dot(location: Point, shape: CharLike = '+') extends Shape(CharGrid.fromString(shape.toString).shift(location))
case class Line(first: Point, second: Point, char: Char => Char = x => x) extends Shape({
    val min = Point.min(first, second)
    val max = Point.max(first, second)
    val difference = max - min
    val array = Array.fill[CharLike](difference.y + 1, difference.x + 1)(' ')
    def lerp(start: Point, end: Point, t: Double): Point = start + (end - start) * t
    def diagonal(start: Point, end: Point) = {
        val dist = end - start
        math.max(math.abs(dist.x), math.abs(dist.y))
    }
    val n = diagonal(first, second)
    val filler = (first ? second)((('-', '\\', '|'), ('|', '/', '-'), ('-', '\\', '|'), ('|', '/', '-')), ('|', '-', '|', '-'), '+')
    for(x <- 0 to n) {
        val t = if(n == 0) 0.0 else x * 1.0 / n
        val point = lerp(first, second, t) - min
        array(point.y)(point.x) = char(filler)
    }
    val start = first - min
    val end = second - min
    CharGrid.fromString(array.map(x => x.mkString).mkString("\n")).shift(min)
})
case class PointLine(first: Point, second: Point) extends Cluster(Seq(Line(first, second), Dot(first, '+'), Dot(second, '+')))
case class Hexagon(location: Point, size: Int, insert: Int = 0,
        lup: Char => Char = x => x,
        upp: Char => Char = x => '_',
        rup: Char => Char = x => x,
        rdo: Char => Char = x => x,
        dow: Char => Char = x => '_',
        ldo: Char => Char = x => x
) extends Cluster({
    val dist = math.sqrt(charRatio * charRatio + 1) * size
    val sideways = (0.5 * dist / charRatio).toInt
    val up = (math.sqrt(3) * dist * 0.5).toInt - 1
    val horizontalSize = math.round(dist / charRatio).toInt + insert
    val reloc = location + Point(0, up + 1)
    val loc = reloc + Point(size * 2 - 1 + horizontalSize, 0)
    Seq(
        Line(reloc, reloc + Point(sideways, -up), lup),
        Line(reloc + Point(0, 1), reloc + Point(sideways, up + 1), ldo),
        Line(reloc + Point(sideways + 1, -up - 1), reloc + Point(sideways + horizontalSize, -up - 1), upp),
        Line(reloc + Point(sideways + 1, up + 1), reloc + Point(sideways + horizontalSize, up + 1), dow),
        Line(reloc + Point(sideways + horizontalSize + 1, -up), reloc + Point(sideways * 2 + horizontalSize + 1, 0), rup),
        Line(reloc + Point(sideways + horizontalSize + 1, up + 1), reloc + Point(sideways * 2 + horizontalSize + 1, 1), rdo)
    )
})
object Hexagon {
    enum Direction(val x: Int, val y: Int, val z: Int) extends PositionLike {
        case Up extends Direction(0, 1, -1)
        case Down extends Direction(0, -1, 1)
        case Lup extends Direction(-1, 1, 0)
        case Ldown extends Direction(-1, 0, 1)
        case Rup extends Direction(1, 0, -1)
        case Rdown extends Direction(1, -1, 0)
        def unary_! : Direction = Direction.values.filter(a => Position.origin == this + a)(0)
    }
    object Direction {
        implicit val ord: Ordering[Direction] = Ordering.by[Direction, Int](a => a.ordinal)
    }
    trait PositionLike {
        def x: Int
        def y: Int
        def z: Int
        def equals(other: PositionLike): Boolean = x == other.x && y == other.y && z == other.z
        def unary_- = Position(-x, -y, -z)
        def +(other: PositionLike) = Position(x + other.x, y + other.y, z + other.z)
        def toPoint(size: Int, insert: Int = 0): Point = {
            val dist = math.sqrt(charRatio * charRatio + 1) * size

            val sideways = math.round(0.5 * dist / charRatio).toInt
            val up = math.round(math.sqrt(3) * 0.5 * dist).toInt
            val horizontalSize = math.round(dist / charRatio).toInt + insert
            Point(-sideways, -up) * y + Point(-sideways, up) * z + Point(horizontalSize, 0) * x
        }
    }
    case class Position(x: Int, y: Int, z: Int) extends PositionLike

    object Position {
        def origin: Position = Position(0, 0, 0)
    }
}

class Cluster(nodes: => Seq[Node]) extends Node {
    def draw: CharGrid = nodes.map(x => x.draw).fold(CharGrid.empty)((a, b) => a + b)
}
class ShapeCluster(shapes: Seq[Shape], override val mask: CharMask = CharMask.empty) extends Shape(shapes.map(x => x.grid).fold(CharGrid.empty)((a, b) => a + b), shapes.map(x => x.mask).fold(mask)((a, b) => a | b))

abstract class Mesh[S <: MeshHex, T] {
    def at(x: Int, y: Int, z: Int): Option[S[T]]
    def add(x: Int, y: Int, z: Int, a: T): Option[S[T]]

    def has(x: Int, y: Int, z: Int): Boolean = at(x, y, z).nonEmpty

    def at(position: Hexagon.Position): Option[S[T]] = at(position.x, position.y, position.z)
    def add(position: Hexagon.Position, a: T): Option[S[T]] = add(position.x, position.y, position.z, a)
    def has(position: Hexagon.Position): Boolean = has(position.x, position.y, position.z)
    def add(hex: MeshHex[T]): Option[S[T]] = add(hex.position, hex.value)

    def apply(x: Int, y: Int, z: Int): Option[S[T]] = at(x, y, z)
    def +=(x: Int, y: Int, z: Int, a: T): Option[S[T]] = add(x, y, z, a)

    def apply(position: Hexagon.Position): Option[S[T]] = at(position)
    def +=(position: Hexagon.Position, a: T): Option[S[T]] = add(position, a)
    def +=(hex: MeshHex[T]): Option[S[T]] = add(hex)

    def at(hex: S[T], direction: Hexagon.Direction): Option[S[T]] = at(hex.position + direction)
    def add(hex: S[T], direction: Hexagon.Direction, a: T): Option[S[T]] = add(hex.position + direction, a)

    def apply(hex: S[T], direction: Hexagon.Direction): Option[S[T]] = at(hex, direction)
    def +=(hex: S[T], direction: Hexagon.Direction, a: T) = add(hex, direction, a)

    def traverse(f: S[T] => Unit): Unit = {
        def helper(option: Option[S[T]], set: Set[S[T]]): Set[S[T]] = {
            option.fold(set)(current => if(set.contains(current)) set else {
                f(current)
                Hexagon.Direction.values.foldLeft(set)((set, direction) => helper(at(current, direction), set + current))
            })
        }
        helper(at(0, 0, 0), Set())
    }
    def map[A](mesh: Mesh[S, A], f: S[T] => S[A]): Mesh[S, A] = {
        traverse(a => mesh += f(a))
        mesh
    }
}

class LinkedMesh[T](center: T) extends Mesh[LinkedMesh.LinkedNode, T] {
    import LinkedMesh.LinkedNode
    val origin = LinkedNode(center, Hexagon.Position(0, 0, 0), MutableMap())
    def at(x: Int, y: Int, z: Int): Option[LinkedNode[T]] = {
        if(x == 0 && y == 0 && z == 0) Some(origin) else {
            var value: Option[LinkedNode[T]] = None
            traverse(a => if(a.position == Hexagon.Position(x, y, z)) value = Some(a))
            value
        }
    }
    override def at(hex: LinkedNode[T], direction: Hexagon.Direction): Option[LinkedNode[T]] = hex.map.get(direction)

    def add(x: Int, y: Int, z: Int, a: T): Option[LinkedNode[T]] = Hexagon.Direction.values.find(d => has(Hexagon.Position(x, y, z) + d)).flatMap(d => add(at(Hexagon.Position(x, y, z) + d).get, !d, a))
    override def add(hex: LinkedNode[T], direction: Hexagon.Direction, a: T): Option[LinkedNode[T]] = {
        val position = hex.position + direction
        val end = LinkedNode(a, position, MutableMap(!direction -> hex))
        hex.map += (direction -> end)
        val directions = Hexagon.Direction.values.filterNot(x => x == !direction)
        directions.foreach(d =>
            at(position + d).foreach(h => {
                end.map += d -> h
                h.map += !d -> end
            }))
        Some(end)
    }

    def remove(s: LinkedNode[T]): Unit = Hexagon.Direction.values.foreach(x => at(s, x).foreach(a => a.map -= !x))
}

object LinkedMesh {
    case class LinkedNode[T](value: T, position: Hexagon.Position, map: MutableMap[Hexagon.Direction, LinkedNode[T]]) extends MeshHex[T] {
        override def hashCode: Int = (value, position).hashCode
    }
}

abstract class MeshHex[T] {
    def position: Hexagon.Position
    def value: T
}

case class Hex[T](position: Hexagon.Position, value: T) extends MeshHex[T]

class DrawingMesh[T <: Node](center: T) extends LinkedMesh[T](center) {
    def toShape(size: Int, insert: Int = 0): Shape = {
        var grid = CharGrid.empty
        traverse(x => grid += x.value.draw.shift(x.position.toPoint(size, insert)))
        grid
        new Shape(grid)
    }
}

case class Tile(var hex: Hexagon, val id: Tiles.Id, var rotation: Hexagon.Direction) extends Node {
    def draw: CharGrid = hex.draw + Tiles.data(id).small(rotation)
}

object Tiles {
    import Hexagon.Direction._
    type Id = Int
    case class Packet(mesh: DrawingMesh[Tile], self: LinkedMesh.LinkedNode[Tile], dir: Hexagon.Direction)
    trait Data {
        def small: Map[Hexagon.Direction, CharGrid]
        def big(packet: Packet): CharGrid
        def name(packet: Packet): String
        def info(packet: Packet): String
        def add(packet: Packet): Unit = ()
        def remove(packet: Packet): Unit = ()
        def rotate(packet: Packet): LinkedMesh.LinkedNode[Tile] = {
            val value = packet.self.value
            (packet.mesh += (packet.self.position, Tile(value.hex, value.id, packet.dir))).get
        }
    }
    case class StaticTileData(small: Map[Hexagon.Direction, CharGrid], name: String, big: CharGrid, info: String) extends Data{
        def small(packet: Packet): Map[Hexagon.Direction, CharGrid] = small
        def big(packet: Packet): CharGrid = big
        def name(packet: Packet): String = name
        def info(packet: Packet): String = info
    }
        val smallhex = """   ________
  /        \
 /          \
/            \
\            /
 \          /
  \________/"""
    val smallint = """
   xxxxxxxx
  xxxxxxxxxx
 xxxxxxxxxxxx
 xxxxxxxxxxxx
  xxxxxxxxxx
"""
    val smallintbottomfill = """
   xxxxxxxx
  xxxxxxxxxx
 xxxxxxxxxxxx
 xxxxxxxxxxxx
  xxxxxxxxxx
   xxxxxxxx"""
    val bighex = """      ________________
     /                \
    /                  \
   /                    \
  /                      \
 /                        \
/                          \
\                          /
 \                        /
  \                      /
   \                    /
    \                  /
     \________________/"""
    val bigint = """
      xxxxxxxxxxxxxxxx
     xxxxxxxxxxxxxxxxxx
    xxxxxxxxxxxxxxxxxxxx
   xxxxxxxxxxxxxxxxxxxxxx
  xxxxxxxxxxxxxxxxxxxxxxxx
 xxxxxxxxxxxxxxxxxxxxxxxxxx
 xxxxxxxxxxxxxxxxxxxxxxxxxx
  xxxxxxxxxxxxxxxxxxxxxxxx
   xxxxxxxxxxxxxxxxxxxxxx
    xxxxxxxxxxxxxxxxxxxx
     xxxxxxxxxxxxxxxxxx
"""
    val bigintbottomfill = """
      xxxxxxxxxxxxxxxx
     xxxxxxxxxxxxxxxxxx
    xxxxxxxxxxxxxxxxxxxx
   xxxxxxxxxxxxxxxxxxxxxx
  xxxxxxxxxxxxxxxxxxxxxxxx
 xxxxxxxxxxxxxxxxxxxxxxxxxx
 xxxxxxxxxxxxxxxxxxxxxxxxxx
  xxxxxxxxxxxxxxxxxxxxxxxx
   xxxxxxxxxxxxxxxxxxxxxx
    xxxxxxxxxxxxxxxxxxxx
     xxxxxxxxxxxxxxxxxx
      xxxxxxxxxxxxxxxx"""
    val data = Map[Id, Data](
        -1 -> StaticTileData(defaultMap(CharGrid.fromChar(' ') < CharMask.fromString(smallhex)), "", CharGrid.fromChar(' ') < CharMask.fromString(bighex), ""),
        0 -> StaticTileData(defaultMap(CharGrid.empty), "Empty", CharGrid.empty, ""),
        1 -> StaticTileData(defaultMap(CharGrid.fromChar('X') < CharMask.fromString(smallintbottomfill)), "Wall", CharGrid.fromChar('X') < CharMask.fromString(bigintbottomfill), ""),
        2 -> StaticTileData(defaultMap(CharGrid.fromString("""

    +----+
    |XXXX|
    |XXXX|
    +----+""")), "Building", CharGrid.fromString("""



        +----------+
        |XXXXXXXXXX|
        |XXXXXXXXXX|
        |XXXXXXXXXX|
        |XXXXXXXXXX|
        +----------+"""), ""),
        3 -> StaticTileData(defaultMap(CharGrid.fromString("""

    .  -_
     v@X\
    | $*|
    +-/@""")), "Rubble", CharGrid.fromString("""



        ..    -___
        .xXXX/XX\
         vXvX#X/X&
        X^&*/-7&8 |
        |xv^%*vX@&@
        +--"""), "")
    )

    def defaultMap(value: CharGrid): Map[Hexagon.Direction, CharGrid] = Map(
        Up -> value,
        Lup -> value,
        Ldown -> value,
        Down -> value,
        Rdown -> value,
        Rup -> value
    )

}