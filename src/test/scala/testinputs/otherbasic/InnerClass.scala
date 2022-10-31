package testinputs.otherbasic

class Bar(x: Int):
  class Baz:
    def add(y: Int) = x + y

object Foobar:
  def doit =
    val four = Bar(4)
    val baz = four.Baz()
    val seven = baz.add(3)
    seven
