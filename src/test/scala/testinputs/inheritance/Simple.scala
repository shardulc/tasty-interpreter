package testinputs.inheritance

object Simple:

  class SimpleFoo:
    val x = 2

  val obj1 = SimpleFoo()
  val test1 = obj1.x      // 2


  class SimpleFooSub extends SimpleFoo

  val obj2 = SimpleFooSub()
  val test2 = obj2.x      // 2


  class SimpleFooWithSuperFields extends SimpleFooSub:
    val w = x * 7
    def y = x * 5

  val obj3 = SimpleFooWithSuperFields()
  val test3 = obj3.w      // 14 = 2*7
  val test4 = obj3.x      // 2
  val test5 = obj3.y      // 10 = 2*5


  class SimpleFooWithMultilevelSuperFields extends SimpleFooWithSuperFields:
    val v = x * 3
    val z = y * 11

  val obj4 = SimpleFooWithMultilevelSuperFields()
  val test6 = obj4.v      // 6 = 2*3
  val test7 = obj4.z      // 110 = 2*5*11
