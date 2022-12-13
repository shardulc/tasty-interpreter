package testinputs.inheritance

object Super:

  class Foo:
    def foo() = 2

  class SimpleSuper extends Foo:
    def bar() = foo() * 3
    def superBar() = super.foo() * 3

  val obj0 = SimpleSuper()
  val test00 = obj0.bar()         // 6 = 2*3
  val test01 = obj0.superBar()    // 6 = 2*3


  class OverrideAndSuper extends Foo:
    override def foo() = 5
    def bar() = foo() * 3
    def superBar() = super.foo() * 3

  val obj1 = OverrideAndSuper()
  val test10 = obj1.bar()         // 15 = 3*5
  val test11 = obj1.superBar()    // 6 = 2*3


  class SuperInsideOverride extends Foo():
    override def foo() = super.foo() * 7

  class SuperChain extends SuperInsideOverride:
    override def foo() = super.foo() * 11

  val obj2 = SuperChain()
  val test20 = obj2.foo()         // 154 = 2*7*11


  class Outer extends Foo():
    override def foo() = 3
    class Inner extends SuperChain:
      override def foo() = Outer.super.foo() * 3
      def bar() = super.foo() * 3

  val obj3Outer = Outer()
  val obj3 = obj3Outer.Inner()
  val test30 = obj3.foo()         // 6 = 2*3
  val test31 = obj3.bar()         // 462 = 2*3*7*11
