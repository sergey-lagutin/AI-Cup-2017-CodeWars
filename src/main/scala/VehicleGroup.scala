import model.Vehicle

trait VehicleGroup {
  private val SHRINK_DELAY = 1000
  private var groupVehicles: List[Vehicle] = Nil
  private var toShrink: Boolean = false
  private var nextShrinkTick: Int = SHRINK_DELAY

  def groupNumber: Int

  def isAlive: Boolean = groupVehicles.nonEmpty

  def updateVehicles(vehicles: List[Vehicle], tickIndex: Int): Unit = {
    this.groupVehicles = vehicles
    if (vehicles.nonEmpty && nextShrinkTick < tickIndex) {
      val xs = vehicles.map(_.getX)
      val ys = vehicles.map(_.getY)
      val minX = xs.min
      val minY = ys.min
      val maxX = xs.max
      val maxY = ys.max
      val maxSize = GameMap.SQUARE_SIZE * 2
      if (maxX - minX > maxSize || maxY - minY > maxSize) {
        toShrink = true
        nextShrinkTick = tickIndex + SHRINK_DELAY
      }
      else {
        toShrink = false
        nextShrinkTick = 0
      }
    }
  }

  def needToShrink: Boolean = toShrink

  def center: Point =
    Point(
      groupVehicles.map(_.getX).sum / groupVehicles.size,
      groupVehicles.map(_.getY).sum / groupVehicles.size
    )

  def vehicles: List[Vehicle] = groupVehicles

  def resetShrink(): Unit = toShrink = false

  protected def shrinkTimeout: Int = SHRINK_DELAY
}

class CaptureGroup(val groupNumber: Int) extends VehicleGroup {
  var building: Building = _

  override def resetShrink(): Unit = {
    super.resetShrink()
    building = null
  }
}

class AttackGroup(val groupNumber: Int) extends VehicleGroup {
  override protected def shrinkTimeout: Int = 2000
}