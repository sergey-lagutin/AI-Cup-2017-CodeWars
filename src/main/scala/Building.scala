import model.{Facility, FacilityType, VehicleType}

case class Building(
                     id: Long,
                     `type`: FacilityType,
                     ownerPlayerId: Long,
                     leftTop: Point,
                     capturePoints: Double,
                     vehicleType: VehicleType,
                     productionProgress: Int
                   ) {
  def center: Point = Point(leftTop.x + GameMap.SQUARE_SIZE, leftTop.y + GameMap.SQUARE_SIZE)
}

object Building {
  def apply(facility: Facility): Building = new Building(
    facility.getId,
    facility.getType,
    facility.getOwnerPlayerId,
    Point(facility.getLeft, facility.getTop),
    facility.getCapturePoints,
    facility.getVehicleType,
    facility.getProductionProgress)
}
