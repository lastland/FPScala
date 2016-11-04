package fpscala.typeclass

object Order extends Enumeration {
  val LT, EQ, GT = Value
}

trait Ordered[A, B] {
  def compare(lhs: A, rhs: B): Order.Value
  def <(lhs: A, rhs: B) = compare(lhs, rhs) == Order.LT
  def >(lhs: A, rhs: B) = compare(lhs, rhs) == Order.GT
  def eq(lhs: A, rhs: B) = compare(lhs, rhs) == Order.EQ
}

object Ordered {
  import Order._
  implicit val orderedInt = new Ordered[Int, Int] {
    def compare(lhs: Int, rhs: Int) =
      if (lhs < rhs) LT
      else if (lhs == rhs) EQ
      else GT
  }

  implicit val orderedDouble = new Ordered[Double, Double] {
    def compare(lhs: Double, rhs: Double) =
      if (lhs < rhs) LT
      else if (lhs == rhs) EQ
      else GT
  }

  implicit val orderedString = new Ordered[String, String] {
    def compare(lhs: String, rhs: String) =
      if (lhs < rhs) LT
      else if (lhs == rhs) EQ
      else GT
  }
}

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[+A](l: Tree[A], x: A, r: Tree[A]) extends Tree[A]

object Tree {
  def insert[A](x: A, t: Tree[A])
    (implicit order: Ordered[A, A]): Tree[A] = t match {
    case Empty => Node(Empty, x, Empty)
    case Node(l, v, r) =>
      if (order.<(v, x)) Node(l, v, insert(x, r))
      else if (order.eq(v, x)) t
      else Node(insert(x, l), v, r)
  }
}

object Example {
  val t = Tree.insert("a", Tree.insert("A", Empty))
  // would be Node(Empty, "A", Node(Empty, "a", Empty))
}

object StringOrderExample {
  implicit val orderedCapString = new Ordered[String, String] {
    def compare(lhs: String, rhs: String) = {
      val l = lhs.capitalize
      val r = rhs.capitalize
      if (l < r) Order.LT
      else if (l == r) Order.EQ
      else Order.GT
    }
  }

  val t = Tree.insert("a", Tree.insert("A", Empty))
  // would be Node(Empty, "A", Empty)
}

sealed trait Nat
case object Z extends Nat
case class S(n: Nat) extends Nat

object NatExample {
  def compareNat(l: Nat, r: Nat): Order.Value =
    (l, r) match {
      case (Z, Z) => Order.EQ
      case (Z, _) => Order.LT
      case (_, Z) => Order.GT
      case (S(x), S(y)) => compareNat(x, y)
    }

  implicit def orderedNat = new Ordered[Nat, Nat] {
    def compare(lhs: Nat, rhs: Nat) = compareNat(lhs, rhs)
  }
}
