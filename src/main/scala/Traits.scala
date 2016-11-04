package fpscala.traits

trait A {
  def a = 1
}

trait B {
  def b = 2
}

trait C {
  def a = 3
}

trait D extends A {
  override def a = 4
}

trait E extends A {
  override def a = 5
}
