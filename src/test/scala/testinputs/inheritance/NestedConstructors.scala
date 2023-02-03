package testinputs.inheritance

object NestedConstructors:

  class Common(val bar: Int)

  class Inner extends Common(5):
    val foo = 2

  class Outer extends Common(7):
    val i = Inner()
    val foo = 3

  val obj0 = Outer()
  val test0_1 = obj0.i.foo
  val test0_2 = obj0.foo
  val test0_3 = obj0.i.bar
  val test0_4 = obj0.bar


  class TakesInner(val i: Inner):
    val foo = i.foo

  class MakesInner extends TakesInner(Inner()):
    val bar = 11

  val obj1 = MakesInner()
  val test1_0 = obj1.bar        // 11
  val test1_1 = obj1.foo        // 2
  val test1_2 = obj1.i.foo      // 2
  val test1_3 = obj1.i.bar      // 5


  class ParentFoo:
    val foo = computeFoo
    def computeFoo = 2

  class ChildFoo extends ParentFoo:
    override def computeFoo = 3

  val obj2 = ChildFoo()
  val test2_0 = obj2.foo                // 3
  val test2_1 = obj2.computeFoo         // 3
