package testinputs.inheritance

object Override:

  class OverrideFoo:
    val x = 2
    def xx = x * 11

  val obj1 = OverrideFoo()
  val test1 = obj1.x            // 2
  val test1_1 = obj1.xx         // 22 = 2*11


  class OverrideFooSub extends OverrideFoo:
    override val x = 3

  val obj2: OverrideFoo = OverrideFooSub()
  val test2 = obj2.x            // 3
  val test2_1 = obj2.xx         // 33 = 3*11


  class OverrideFooWithSuperFields extends OverrideFooSub:
    def y = x * 5
    val w = y * 7

  val obj3 = OverrideFooWithSuperFields()
  val test3 = obj3.w      // 105 = 3*5*7
  val test4 = obj3.x      // 3
  val test5 = obj3.y      // 15 = 3*5


  class OverrideFooUniformAccess extends OverrideFooWithSuperFields:
    override val y = 13

  val obj4 = OverrideFooUniformAccess()
  val test6 = obj4.w      // 0
  val test7 = obj4.y      // 13
