import model.ActionType.{SCALE, TACTICAL_NUCLEAR_STRIKE}
import model.{ActionType, Move, VehicleType}

sealed trait Action {
  def action(move: Move): Unit
}

case class Select(left: Double, top: Double, right: Double, bottom: Double, vehicleType: VehicleType = null) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(ActionType.CLEAR_AND_SELECT)
    move.setLeft(left)
    move.setTop(top)
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

case class NuclearStrike(x: Double, y: Double, vehicleId: Long) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(TACTICAL_NUCLEAR_STRIKE)
    move.setX(x)
    move.setY(y)
    move.setVehicleId(vehicleId)
  }
}

case class Scale(x: Double, y: Double, factor: Double) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(SCALE)
    move.setX(x)
    move.setY(y)
    move.setFactor(factor)
  }
}
