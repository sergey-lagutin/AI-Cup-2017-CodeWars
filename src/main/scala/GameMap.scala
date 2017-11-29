import model.Vehicle

object GameMap {

  val SQUARE_SIZE = 32.0
  val SQUARE_COUNT = 32

  def pointToSquare(x: Double, y: Double): (Int, Int) =
    ((x / SQUARE_SIZE).toInt, (y / SQUARE_SIZE).toInt)

  def vehicleToSquare(v: Vehicle): (Int, Int) =
    pointToSquare(v.getX, v.getY)

  def squareCenter(square: (Int, Int)): (Double, Double) =
    (square._1 * SQUARE_SIZE + SQUARE_SIZE / 2,
      square._2 * SQUARE_SIZE + SQUARE_SIZE / 2)
}
