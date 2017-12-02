import model.ActionType._
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

case class SelectGroup(number: Int) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(ActionType.CLEAR_AND_SELECT)
    move.setGroup(number)
  }
}

object GoTo {
  def apply(v: Dir): GoTo = new GoTo(v.dx, v.dy)
}

case class GoTo(dx: Double, dy: Double) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(ActionType.MOVE)
    move.setX(dx)
    move.setY(dy)
  }
}

object Rotate {
  def apply(p: Point, angle: Double): Rotate = new Rotate(p.x, p.y, angle)
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

object Scale {
  def apply(p: Point, factor: Double): Scale = new Scale(p.x, p.y, factor)
}

case class Scale(x: Double, y: Double, factor: Double) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(SCALE)
    move.setX(x)
    move.setY(y)
    move.setFactor(factor)
  }
}

case class Assign(number: Int) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(ASSIGN)
    move.setGroup(number)
  }
}

case class Production(building: Building, vehicleType: VehicleType) extends Action {
  override def action(move: Move): Unit = {
    move.setAction(SETUP_VEHICLE_PRODUCTION)
    move.setFacilityId(building.id)
    move.setVehicleType(vehicleType)
  }
}