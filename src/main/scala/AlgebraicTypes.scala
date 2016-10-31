package fpscala.algebra

case class Cartasian(x: Double, y: Double)
case class Polar(x: Double, y: Double)

sealed trait Tree
case object Empty extends Tree
case class Node(x: Int, l: Tree, r: Tree) extends Tree
