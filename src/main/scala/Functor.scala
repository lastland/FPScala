package fpscala.functor
import scala.language.higherKinds
import scala.language.implicitConversions

/*
 class Functor f where
   fmap :: (a -> b) -> f a -> f b
 */

trait Functor[F[_]] {
  def fmap[A, B](f: A => B)(a: F[A]): F[B]
}

object Functor {
  implicit val listFunctor = new Functor[List] {
    def fmap[A, B](f: A => B)(a: List[A]) = a.map(f)
  }

  implicit val optionFunctor = new Functor[Option] {
    def fmap[A, B](f: A => B)(a: Option[A]) = a.map(f)
  }

  type FI[A] = Function1[Int, A]

  implicit val functionFunctor = new Functor[FI] {
    def fmap[A, B](f: A => B)(a: FI[A]) = f.compose(a)
  }
}

trait FunctorOps[F[_], A] {
  def self: F[A]
  implicit def functor: Functor[F]
  final def fmap[B](f: A => B): F[B] = functor.fmap(f)(self)
}

object FunctorOps {
  implicit def toFunctorOps[F[_], A](f: F[A])
    (implicit F: Functor[F]) = new FunctorOps[F, A] {
    def self = f
    implicit def functor = F
  }
}

object Example {
  import FunctorOps._

  val list_ex1 = List(1, 2, 3) fmap (_ + 1)
  val list_ex2 = List(1, 2, 3) fmap (_.toString)

  val option_ex1 = Option(1) fmap (_ + 41)
  val option_ex2 = (None: Option[Int]) fmap (_ + 41)

  val function_ex1 = (((_: Int) + 1): Functor.FI[Int]) fmap (_ * 10)
  val function_ex2 = (((x: Int) => Some(x)): Functor.FI[Option[Int]]) fmap (
    _ fmap (_ + 41))

  def compose[A, B](f: A => B)(x: Int => A): Int => B =
    (x: Functor.FI[A]) fmap f

  val function_ex3 = compose ((x: Int) => x * 10) (_ + 1)
  val function_ex4 = compose ((x: Option[Int]) => x fmap (_ + 41)) (Some(_))

  val list_curry_ex1 = List(1, 2, 3) fmap ((_: Int) * (_: Int)).curried
  val list_curry_ex2 = list_curry_ex1 fmap (_(2))
}
