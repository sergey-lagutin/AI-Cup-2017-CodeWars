import model.Vehicle

class CaptureGroup(val groupNumber: Int) {
  var building: Building = _
  var vehicles: List[Vehicle] = _

  def isAlive: Boolean = vehicles.nonEmpty
}
