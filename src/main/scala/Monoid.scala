package fpscala.monoid
import scala.language.implicitConversions

trait Monoid[M] {
  def mempty: M
  def mappend(x: M, y: M): M
  // mconcat is omitted
}

object Monoid {
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def mempty = List()
    def mappend(x: List[A], y: List[A]) = x ++ y
  }

  implicit def stringMonoid[A] = new Monoid[String] {
    def mempty = ""
    def mappend(x: String, y: String) = x ++ y
  }
}

trait MonoidOps[M] {
  def self: M
  def monoid: Monoid[M]
  final def mappend(y: M) = monoid.mappend(self, y)
}

object MonoidOps {
  implicit def toMonoidOps[M](x: M)(implicit m: Monoid[M]) = new MonoidOps[M] {
    def self = x
    def monoid = m
  }
}

object Example {
  import MonoidOps._

  val list_ex1 = List(1, 2) mappend List(3, 4)
  val list_ex2 = List(1, 2) mappend implicitly[Monoid[List[Int]]].mempty
  val list_ex3 = "PL" mappend "Club"
}
