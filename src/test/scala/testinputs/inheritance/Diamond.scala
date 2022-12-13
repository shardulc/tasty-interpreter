package testinputs.inheritance

object Diamond:

  class Parent:
    def value = 2

  trait Foo1 extends Parent:
    override def value = 3

  trait Foo2 extends Parent:
    override def value = 5

  class Child1 extends Foo1, Foo2
  class Child2 extends Foo2, Foo1

  val obj1 = Child1()
  val test1 = obj1.value
  val obj2 = Child2()
  val test2 = obj2.value

  val obj3 = Parent()
  val test3 = obj3.value
