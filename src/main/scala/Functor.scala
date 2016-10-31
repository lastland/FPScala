package fpscala.functor

trait Functor[F[_]] {
  def fmap[A, B](f: A => B)(a: F[A]): F[B]
}

object Functor {
  implicit def listFunctor = new Functor[List] {
    def fmap[A, B](f: A => B)(a: List[A]) = a.map(f)
  }

  implicit def optionFunctor = new Functor[Option] {
    def fmap[A, B](f: A => B)(a: Option[A]) = a.map(f)
  }

  type FI[A] = Function1[Int, A]

  implicit def functionFunctor = new Functor[FI] {
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

  def compose[A, B](f: A => B)(x: Int => A): Int => B =
    (x: Functor.FI[A]) fmap f

  val function_ex1 = compose ((x: Int) => x * 10) (_ + 1)
  val function_ex2 = compose ((x: Option[Int]) => x fmap (_ + 41)) (Some(_))

  val list_curry_ex1 = List(1, 2, 3) fmap ((_: Int) * (_: Int)).curried
  val list_curry_ex2 = list_curry_ex1 fmap (_(2))
}
