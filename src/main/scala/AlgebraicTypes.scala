package fpscala.algebra

// product types
object ProductExample {
  val prod_ex1 = (1, 2)
  val prod_ex2 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
    18, 19, 20, 21, 22)
// tuples are only allowed to have at most 22 elements
//  val prod_ex3 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
//    18, 19, 20, 21, 22, 23)

  type Cartasian = (Double, Double)
  type Polar = (Double, Double)
  val point_ex1: Cartasian = (1, 2)
  val point_ex2: Polar = (3, 60)
  // Oops
  val point_ex3: Cartasian = point_ex2
}

case class Cartasian(x: Double, y: Double)
case class Polar(x: Double, y: Double)

object ProductExample2 {
  val point_ex1 = Cartasian(1, 2)
  val point_ex2 = Polar(3, 60)
  // type error!
  // val point_ex3: Cartasian = point_ex2
}

sealed trait Nat
case object Z extends Nat
case class S(pred: Nat) extends Nat

object NatExample {
  val nat_ex1 = Z
  val nat_ex2 = S(S(Z))
  val nat_ex3 = nat_ex2.pred
  def plus(m: Nat, n: Nat): Nat = m match {
    case Z => n
    case S(x) => S(plus(x, n))
  }
}

sealed trait Tree
case object Empty extends Tree
case class Node(l: Tree, x: Int, r: Tree) extends Tree

object TreeExample {
  val tree_ex1: Tree = Empty
  val tree_ex2: Tree = Node(Empty, 3, Empty)

  // let's try pattern matching!
  def max(t: Tree, d: Int): Int = t match {
    case Empty => d
    case Node(_, v, r) => max(r, v)
  }

  val m = max(tree_ex1, 0)
}
