import model.{ActionType, Move, VehicleType}

sealed trait Action {
  def action(move: Move): Unit
}

case class Select(right: Double, bottom: Double, vehicleType: VehicleType = null) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(ActionType.CLEAR_AND_SELECT)
    move.setRight(right)
    move.setBottom(bottom)
    move.setVehicleType(vehicleType)
  }
}

case class GoTo(x: Double, y: Double) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(ActionType.MOVE)
    move.setX(x)
    move.setY(y)
  }
}

case class Rotate(x: Double, y: Double, angle: Double) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(ActionType.ROTATE)
    move.setX(x)
    move.setY(y)
    move.setAngle(angle)
  }
}