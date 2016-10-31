package fpscala.applicative

trait Applicative[F[_]] {
  def pure[A](a: => A): F[A]
  def <*>[A, B](f: F[A => B])(a: => F[A]): F[B]
}

object Applicative {
  trait ListApplicative extends Applicative[List] {
    final def pure[A](a: => A) = List(a)
    final def <*>[A, B](f: List[A => B])(a: => List[A]) =
      for {
        f1 <- f
        a1 <- a
      } yield f1(a1)
  }

  trait OptionApplicative extends Applicative[Option] {
    def pure[A](a: => A) = Some(a)
    def <*>[A, B](f: Option[A => B])(a: => Option[A]) =
      for {
        f1 <- f
        a1 <- a
      } yield f1(a1)
  }

  implicit def listApplicative[A] = new ListApplicative {}

  implicit def optionApplicative[A] = new OptionApplicative {}
}

trait ApplicativeOps[A] {
  def self: A
  final def pure[F[_]](implicit functor: Applicative[F]) =
    functor.pure(self)
}

trait Apply[A, F[_]] {
  def self: F[A]
  final def <*>[B](f: F[A => B])(implicit functor: Applicative[F]) =
    functor.<*>(f)(self)
}

object ApplicativeOps {
  implicit def toApplicativeOps[A](a: A) = new ApplicativeOps[A] {
    def self = a
  }
}

object ApplyOps {
  implicit def toApply[A, F[_]](a: F[A]) = new Apply[A, F] {
    def self = a
  }
}

object Example {
  import ApplicativeOps._

  val list_ex1 = 1.pure[List]
  val list_ex2 = ((x: Int) => x + 1).pure[List]

  val option_ex1 = 1.pure[Option]
  val option_ex2 = ((x: Int) => x + 1).pure[Option]

  import ApplyOps._

  val option_ex3 = Option(1) <*> Option((x: Int) => x + 1)
  val option_ex4 = 1.pure[Option] <*> None

  val list_ex3 = List(1, 2) <*> List((x: Int) => x + 1, (x: Int) => x * 2)
  val list_ex4 = 1.pure[List] <*> ((x: Int) => x + 41).pure[List]

  import fpscala.functor.FunctorOps._
  val option_ex5 = Option(8) <*> (Option(2) fmap ((_: Int) * (_: Int)).curried)
}
