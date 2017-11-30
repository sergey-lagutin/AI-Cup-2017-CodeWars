case class Point(x: Double, y: Double) {
  def squaredDistanceTo(that: Point): Double =
    math.pow(x - that.x, 2) + math.pow(y - that.y, 2)

  def -(that: Point): Vect = Vect(x - that.x, y - that.y)
}

case class Vect(dx: Double, dy: Double)