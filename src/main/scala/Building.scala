import model.{Facility, FacilityType, Vehicle, VehicleType}

case class Building(
                     id: Long,
                     `type`: FacilityType,
                     ownerPlayerId: Long,
                     leftTop: Point,
                     capturePoints: Double,
                     vehicleType: VehicleType,
                     productionProgress: Int
                   ) {

  lazy val center: Point = Point(leftTop.x + GameMap.SQUARE_SIZE, leftTop.y + GameMap.SQUARE_SIZE)
  lazy val rightBottom: Point = Point(leftTop.x + 2 * GameMap.SQUARE_SIZE, leftTop.y + 2 * GameMap.SQUARE_SIZE)
  val firstCapturePosition: Point = Point(leftTop.x + GameMap.SQUARE_SIZE * 1.5, leftTop.y + GameMap.SQUARE_SIZE * 1.5)
  val secondCapturePosition: Point = Point(leftTop.x + GameMap.SQUARE_SIZE * 0.5, leftTop.y + GameMap.SQUARE_SIZE * 0.5)


  val aroundBox: (Point, Point) = {
    def nonNegative(d: Double): Double =
      if (d >= 0) d else 0

    def nonOut(d: Double): Double =
      if (d <= GameMap.MAP_SIZE) d else GameMap.MAP_SIZE

    (Point(nonNegative(leftTop.x - GameMap.SQUARE_SIZE / 2),
      nonNegative(leftTop.y - GameMap.SQUARE_SIZE / 2)),
      Point(nonOut(rightBottom.x - GameMap.SQUARE_SIZE / 2),
        nonOut(rightBottom.y - GameMap.SQUARE_SIZE / 2)))
  }

  def isFactory: Boolean = `type` == FacilityType.VEHICLE_FACTORY

  def vehicleOnFactory(myUnits: Iterable[Vehicle]): List[Vehicle] =
    myUnits.toList
      .filter(u =>
        inRange(u.getX, leftTop.x, rightBottom.x) &&
          inRange(u.getY, leftTop.y, rightBottom.y)
      )

  private def inRange(x: Double, x0: Double, x1: Double): Boolean =
    x0 <= x && x <= x1
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
