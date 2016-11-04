package fpscala.monad
import scala.language.higherKinds
import scala.language.implicitConversions

import fpscala.applicative.{Applicative, ApplicativeOps, Apply}

trait Monad[M[_]] { self: Applicative[M] =>
  def ret[A](a: => A): M[A] = pure(a)
  def bind[A, B](a: => M[A], f: A => M[B]): M[B]
}

object Monad {
  implicit def listMonad = new Monad[List]
      with Applicative.ListApplicative {
    def bind[A, B](a: => List[A], f: A => List[B]) =
      a.flatMap(f)
  }

  implicit def optionMonad = new Monad[Option]
      with Applicative.OptionApplicative {
    def bind[A, B](a: => Option[A], f: A => Option[B]) =
      a.flatMap(f)
  }
}

trait MonadOps[A, M[_]] {
  def self: M[A]
  final def bind[B](f: A => M[B])
    (implicit m: Monad[M]) = m.bind(self, f)
}

trait MonadRetOps[A] {
  def self: A
  final def ret[M[_]](implicit m: Monad[M]): M[A] = m.ret(self)
}

object MonadOps {
  implicit def toMonadOps[A, M[_]](m: M[A]) =
    new MonadOps[A, M] with Apply[A, M] {
    def self = m
  }

  implicit def toMonadRetOps[A](a: A) =
    new MonadRetOps[A] with ApplicativeOps[A] {
    def self = a
  }
}

object Example {
  import MonadOps._

  val list_ex1 = 1.ret[List] bind ((x: Int) => List(x, x+1, x+2))
  val list_ex2 = List(1, 2, 3) bind ((x: Int) => List(x, x * 2, x * 3))

  val option_ex1 = 1.ret[Option] bind ((x: Int) => Some(x))
  val option_ex2 = "x".ret[Option] bind ((x: String) => None)
}
