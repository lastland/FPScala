package fpscala.typeclass

trait Ordered[A] {
  def <(lhs: A, rhs: A): Boolean
  def eq(lhs: A, rhs: A): Boolean
}

object Ordered {
  implicit def orderedInt = new Ordered[Int] {
    def <(lhs: Int, rhs: Int) = lhs < rhs
    def eq(lhs: Int, rhs: Int) = lhs == rhs
  }

  implicit def orderedDouble = new Ordered[Double] {
    def <(lhs: Double, rhs: Double) = lhs < rhs
    def eq(lhs: Double, rhs: Double) = lhs == rhs
  }

  implicit def orderedString = new Ordered[String] {
    def <(lhs: String, rhs: String) = lhs < rhs
    def eq(lhs: String, rhs: String) = lhs == rhs
  }
}

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[+A](l: Tree[A], x: A, r: Tree[A]) extends Tree[A]

object Tree {
  def max[A](t: Tree[A], default: A): A = t match {
    case Empty => default
    case Node(l, v, r) => max(r, v)
  }

  def maxOption[A](t: Tree[A]): Option[A] = t match {
    case Empty => None
    case Node(l, v, r) => Some(max(r, v))
  }

  def insert[A](x: A, t: Tree[A])
    (implicit order: Ordered[A]): Tree[A] = t match {
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
  implicit def orderedCapString = new Ordered[String] {
    def <(lhs: String, rhs: String) = lhs.capitalize < rhs.capitalize
    def eq(lhs: String, rhs: String) = lhs.capitalize == rhs.capitalize
  }

  val t = Tree.insert("a", Tree.insert("A", Empty))
  // would be Node(Empty, "A", Empty)
}
