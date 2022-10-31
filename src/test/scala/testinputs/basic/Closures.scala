package testinputs.basic

def counter =
  var x = 0
  def next() =
    x += 1
    x
  () => next()

val x = counter
val y = x()
val xx = counter
val yy = xx()
val z = x()
