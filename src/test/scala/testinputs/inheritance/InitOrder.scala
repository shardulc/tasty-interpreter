package testinputs.inheritance

object InitOrder:

  class InitOrderFooLazy:
    val x = y
    val y = 2

    val xdef = ydef
    def ydef = 3

    lazy val xlazy = ylazy
    val ylazy = 5

    val xlazyref = ylazyref
    lazy val ylazyref = 7

  val obj1 = InitOrderFooLazy()
  val test1 = obj1.x              // 0
  val test2 = obj1.xdef           // 3
  val test3 = obj1.xlazy          // 5
  val test4 = obj1.xlazyref       // 7


  class InitOrderFooSuper:
    val xmid = 2
    val ysup = xmid * 5
    val zsub = 7
    def ysupdef = zsub * 17

  class InitOrderFooMid extends InitOrderFooSuper:
    override val xmid = 3
    val wmid = zsub * 13

  class InitOrderFooSub extends InitOrderFooMid:
    override val zsub = 11

  val obj2 = InitOrderFooMid()
  val test5 = obj2.xmid           // 3
  val test6 = obj2.ysup           // 0
  val test7 = obj2.zsub           // 7
  val test8 = obj2.wmid           // 91 = 7*13
  val test9 = obj2.ysupdef        // 119 = 7*17

  val obj3 = InitOrderFooSub()
  val test10 = obj3.xmid          // 3
  val test11 = obj3.ysup          // 0
  val test12 = obj3.zsub          // 11
  val test13 = obj3.wmid          // 0
  val test14 = obj3.ysupdef       // 187 = 11*17
