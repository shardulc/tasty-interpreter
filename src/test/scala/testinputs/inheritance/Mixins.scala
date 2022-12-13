package testinputs.inheritance

object Mixins:

  class Bike:
    protected var currentSpeed = 0
    def getCurrentSpeed = currentSpeed
    def pedal(power: Int) =
      if power > 0 then currentSpeed += power
    def brake(power: Int) =
      if currentSpeed > 0 then currentSpeed -= power
      if currentSpeed < 0 then currentSpeed = 0


  trait Bell:
    type Sound = String
    val bellModel = "Bell"
    def ring(): Sound
    def ringTwice() =
      ring() + " " + ring()

  class ClownBike extends Bike, Bell:
    def ring() = "ploooeeey"


  val clown = ClownBike()
  val test00 = clown.ring()
  val test01 = clown.ringTwice()
  val test02 = clown.bellModel


  trait Gears(speeds: Int) extends Bike:
    private var currentGear = 1
    def getCurrentGear = currentGear
    def shiftUp() =
      if currentGear < speeds then currentGear += 1
    def shiftDown() =
      if currentGear > 1 then currentGear -= 1

    override def pedal(power: Int) =
      if power > 0 then currentSpeed += power * currentGear


  class RoadBike extends Gears(21)

  val road = RoadBike()
  road.pedal(10)
  val test10 = road.getCurrentSpeed
  road.shiftUp()
  val test11 = road.getCurrentGear
  road.pedal(10)
  val test12 = road.getCurrentSpeed


  class BrokenBike extends Bike, Gears(21):
    override def shiftDown(): Unit = ()

  val broken1 = BrokenBike()
  broken1.shiftUp()
  broken1.pedal(10)
  val test20 = road.getCurrentSpeed
  broken1.shiftDown()
  broken1.pedal(10)
  val test21 = road.getCurrentSpeed


  trait BrokenGears extends Gears:
    override def shiftUp() =
      super.shiftUp()
      super.shiftUp()

  class AnotherBrokenBike extends Gears(7), BrokenGears
  class SameAnotherBrokenBike extends BrokenGears, Gears(7)

  val broken2 = AnotherBrokenBike()
  broken2.shiftUp()
  broken2.pedal(10)
  val test30 = broken2.getCurrentSpeed

  val broken3 = SameAnotherBrokenBike()
  broken3.shiftUp()
  broken3.pedal(10)
  val test31 = broken3.getCurrentSpeed


  trait ChildrensBike extends Bike:
    override def pedal(power: Int) =
      super.pedal(power)
      if currentSpeed > 5 then currentSpeed = 5

  class BrokenChildrensBike extends BrokenBike, ChildrensBike

  val broken4 = BrokenChildrensBike()
  broken4.pedal(10)
  val test40 = broken4.getCurrentSpeed
  broken4.shiftUp()
  broken4.brake(10)
  val test41 = broken4.getCurrentSpeed
  broken4.pedal(1)
  val test42 = broken4.getCurrentSpeed
  broken4.shiftDown()
  broken4.pedal(1)
  val test43 = broken4.getCurrentSpeed
  val test44 = broken4.getCurrentGear
