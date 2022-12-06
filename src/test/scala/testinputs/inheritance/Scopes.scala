package testinputs.inheritance

object Scopes:

  class ScopesFoo(x: Int):
    val y = x * 2


  class ScopesFooSubOne(x: Int, z: Int) extends ScopesFoo(z):
    val w = x * 3
    val v = y * 5

  val obj1 = ScopesFooSubOne(17, 19)
  val test11 = obj1.v     // 190 = 2*5*19
  val test12 = obj1.w     // 51 = 3*17
  val test13 = obj1.y     // 38 = 2*19


  class ScopesFooSubTwo(y: Int) extends ScopesFoo(y):
    val z = y * 7
    val wshadowed = this.y * 11
    val wshadowed2 = {
      val foo: this.type = this
      foo.y
    }

  val obj2 = ScopesFooSubTwo(17)
  val test20 = obj2.wshadowed2    // 34 = 2*17
  val test21 = obj2.wshadowed     // 187 = 11*17
  val test22 = obj2.y             // 34 = 2*17
  val test23 = obj2.z             // 119 = 7*17


  class ScopesFooOuter(x: Int):
    val y = 13

    class ScopesFooInnerOne(y: Int) extends ScopesFoo(y):
      val yparam = y
      val yshadowed = this.y
      val youter = ScopesFooOuter.this.y

    class ScopesFooInnerTwo extends ScopesFoo(y):
      val yinherited = this.y
      val youter = ScopesFooOuter.this.y

  val obj3 = ScopesFooOuter(17)
  val obj4 = obj3.ScopesFooInnerOne(19)
  val test41 = obj4.yparam        // 19
  val test42 = obj4.yshadowed     // 19
  val test43 = obj4.youter        // 13
  val test44 = obj4.y             // 38 = 2*19
  val obj5 = obj3.ScopesFooInnerTwo()
  val test52 = obj5.yinherited    // 26 = 2*13
  val test53 = obj5.youter        // 13
  val test54 = obj5.y             // 26 = 2*13


  class ScopesFooConstructorFields(val x: Int, override val y: Int)
      extends ScopesFoo(x*23)

  val obj6 = ScopesFooConstructorFields(17, 19)
  val test61 = obj6.x     // 17
  val test62 = obj6.y     // 19
