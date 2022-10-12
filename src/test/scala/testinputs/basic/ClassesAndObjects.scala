package testinputs.basic

class Foo(x: Int):
  val y = x + 1
  def bar(z: Int) = x + z

object Foo:
  def doit =
    val app = Foo(4)
    val one = app.y
    val two = app.bar(2)
    one

  def doitagain =
    val app = Foo(4)
    val one = app.y
    val two = app.bar(2)
    two
