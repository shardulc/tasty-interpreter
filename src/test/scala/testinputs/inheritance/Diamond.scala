package testinputs.inheritance

object Diamond:

  class Parent:
    def value = 2

  trait Foo extends Parent:
    override def value = 3

  trait Bar extends Parent:
    override def value = 5

  class Child1 extends Foo, Bar:
    def valueFoo = super[Foo].value
    def valueBar = super[Bar].value
    def valueParent = super[Parent].value
    def valueSuper = super.value

  class Child2 extends Bar, Foo:
    class InnerChild:
      def valueFoo = Child2.super[Foo].value
      def valueBar = Child2.super[Bar].value
      def valueParent = Child2.super[Parent].value
      def valueSuper = Child2.super.value

  val obj1 = Child1()
  val test1 = obj1.value
  val test1Foo = obj1.valueFoo
  val test1Bar = obj1.valueBar
  val test1Parent = obj1.valueParent
  val test1Super = obj1.valueSuper

  val obj2 = Child2()
  val test2 = obj2.value
  val obj2Inner = obj2.InnerChild()
  val test2Foo = obj2Inner.valueFoo
  val test2Bar = obj2Inner.valueBar
  val test2Parent = obj2Inner.valueParent
  val test2Super = obj2Inner.valueSuper

  val obj3 = Parent()
  val test3 = obj3.value


  trait DoubleDiamondA:
    def foo = 2

  trait DoubleDiamondB:
    def foo = 3

  trait DoubleDiamondAA extends DoubleDiamondA:
    override def foo = 5

  trait DoubleDiamondBB extends DoubleDiamondB:
    override def foo = 7
    def baz = super.foo

  trait DoubleDiamondAB extends DoubleDiamondA, DoubleDiamondB:
    override def foo = 11

  class DoubleDiamondAAABBB extends DoubleDiamondAA, DoubleDiamondAB, DoubleDiamondBB:
    override def foo = 13
    def barSuper = super.foo
    def barAA = super[DoubleDiamondAA].foo
    def barAB = super[DoubleDiamondAB].foo
    def barBB = super[DoubleDiamondBB].foo
    def bazSuper = super.baz

  class DoubleDiamondAABBAB extends DoubleDiamondAA, DoubleDiamondBB, DoubleDiamondAB:
    override def foo = 13
    def barSuper = super.foo
    def barAA = super[DoubleDiamondAA].foo
    def barAB = super[DoubleDiamondAB].foo
    def barBB = super[DoubleDiamondBB].foo

  val objdd0 = DoubleDiamondAAABBB()
  val testdd0Super = objdd0.barSuper
  val testdd0AA = objdd0.barAA
  val testdd0AB = objdd0.barAB
  val testdd0BB = objdd0.barBB
  val testdd0baz = objdd0.bazSuper

  val objdd1 = DoubleDiamondAABBAB()
  val testdd1Super = objdd1.barSuper
  val testdd1AA = objdd1.barAA
  val testdd1AB = objdd1.barAB
  val testdd1BB = objdd1.barBB
