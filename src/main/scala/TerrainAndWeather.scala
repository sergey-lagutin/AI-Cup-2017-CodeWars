import model.VehicleType._
import model.{TerrainType, Vehicle, WeatherType}

trait TerrainAndWeather extends WorldAware {

  private lazy val visionRange = Map(
    FIGHTER -> game.getFighterVisionRange,
    HELICOPTER -> game.getHelicopterVisionRange,
    TANK -> game.getTankVisionRange,
    IFV -> game.getIfvVisionRange,
    ARRV -> game.getArrvVisionRange
  )

  private lazy val weatherVisionRange = Map(
    WeatherType.CLEAR -> game.getClearWeatherVisionFactor,
    WeatherType.CLOUD -> game.getCloudWeatherVisionFactor,
    WeatherType.RAIN -> game.getRainWeatherVisionFactor
  )

  private lazy val terrainVisionRange = Map(
    TerrainType.FOREST -> game.getForestTerrainVisionFactor,
    TerrainType.PLAIN -> game.getPlainTerrainVisionFactor,
    TerrainType.SWAMP -> game.getSwampTerrainVisionFactor
  )

  def getActualVisionRange(spotter: Vehicle): Double =
    visionRange(spotter.getType) *
      (if (spotter.isAerial) weatherFactor(spotter.getX, spotter.getY)
      else terrainFactor(spotter.getX, spotter.getY))

  private def weatherFactor(unitX: Double, unitY: Double): Double = {
    val (x, y) = GameMap.pointToSquare(unitX, unitY)
    val weatherType = world.getWeatherByCellXY()(x)(y)
    weatherVisionRange(weatherType)
  }

  private def terrainFactor(unitX: Double, unitY: Double): Double = {
    val (x, y) = GameMap.pointToSquare(unitX, unitY)
    val terrainType = world.getTerrainByCellXY()(x)(y)
    terrainVisionRange(terrainType)
  }
}
