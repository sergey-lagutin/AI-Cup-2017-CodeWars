import model.Vehicle

class CaptureGroup(val groupNumber: Int) {
  var building: Building = _
  var vehicles: List[Vehicle] = _
  var needToShrink: Boolean = false

  def isAlive: Boolean = vehicles.nonEmpty

  def center: Point =
    Point(
      vehicles.map(_.getX).sum / vehicles.size,
      vehicles.map(_.getY).sum / vehicles.size
    )
}
