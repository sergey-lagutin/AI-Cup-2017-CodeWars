import model.Vehicle

trait VehicleGroup {
  private var groupVehicles: List[Vehicle] = Nil

  def groupNumber: Int

  def isAlive: Boolean = groupVehicles.nonEmpty

  def updateVehicles(vehicles: List[Vehicle]): Unit =
    this.groupVehicles = vehicles

  def center: Point =
    Point(
      groupVehicles.map(_.getX).sum / groupVehicles.size,
      groupVehicles.map(_.getY).sum / groupVehicles.size
    )

  def vehicles: List[Vehicle] = groupVehicles
}

class CaptureGroup(val groupNumber: Int) extends VehicleGroup {
  var building: Building = _
  var needToShrink: Boolean = false
}
