import java.util
import java.util.Random

import model.VehicleType._
import model.{FacilityType, Game, Move, Player, TerrainType, Vehicle, VehicleType, WeatherType, World}

import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.mutable
import scala.language.implicitConversions

final class MyStrategy extends Strategy with WorldAware with TerrainAndWeather {

  final private val vehicleById = new util.HashMap[Long, Vehicle]
  final private val updateTickByVehicleId = new util.HashMap[Long, Integer]
  final private val delayedMoves = new util.ArrayDeque[Action]
  private var random: Random = _
  private var terrainTypeByCellXY: Array[Array[TerrainType]] = _
  private var weatherTypeByCellXY: Array[Array[WeatherType]] = _
  private var me: Player = _
  var world: World = _
  var game: Game = _
  private var move: Move = _
  private val buildings = new mutable.HashMap[Long, Building]()

  /**
    * Основной метод стратегии, осуществляющий управление армией. Вызывается каждый тик.
    *
    * @param me    Информация о вашем игроке.
    * @param world Текущее состояние мира.
    * @param game  Различные игровые константы.
    * @param move  Результатом работы метода является изменение полей данного объекта.
    */
  override def move(me: Player, world: World, game: Game, move: Move) {
    initializeStrategy(world, game)
    initializeTick(me, world, game, move)
    if (me.getRemainingActionCooldownTicks > 0)
      return
    if (executeDelayedMove())
      return
    makeMove()
    executeDelayedMove()
  }

  /**
    * Инциализируем стратегию.
    * <p>
    * Для этих целей обычно можно использовать конструктор, однако в данном случае мы хотим инициализировать генератор
    * случайных чисел значением, полученным от симулятора игры.
    */
  private def initializeStrategy(world: World, game: Game): Unit = if (random == null) {
    random = new Random(game.getRandomSeed)
    terrainTypeByCellXY = world.getTerrainByCellXY
    weatherTypeByCellXY = world.getWeatherByCellXY
  }

  /**
    * Сохраняем все входные данные в полях класса для упрощения доступа к ним, а также актуализируем сведения о каждой
    * технике и времени последнего изменения её состояния.
    */
  private def initializeTick(me: Player, world: World, game: Game, move: Move): Unit = {
    this.me = me
    this.world = world
    this.game = game
    this.move = move
    for (vehicle <- world.getNewVehicles) {
      vehicleById.put(vehicle.getId, vehicle)
      updateTickByVehicleId.put(vehicle.getId, world.getTickIndex)
    }
    for (vehicleUpdate <- world.getVehicleUpdates) {
      val vehicleId = vehicleUpdate.getId
      if (vehicleUpdate.getDurability == 0) {
        vehicleById.remove(vehicleId)
        updateTickByVehicleId.remove(vehicleId)
      }
      else {
        vehicleById.put(vehicleId, new Vehicle(vehicleById.get(vehicleId), vehicleUpdate))
        updateTickByVehicleId.put(vehicleId, world.getTickIndex)
      }
    }

    world.getFacilities.foreach { f =>
      buildings.put(f.getId, Building(f))
    }

    captureGroups.foreach { group =>
      Option(group.building).foreach { b =>
        group.building = buildings(b.id)
      }
      group.vehicles =
        vehicleById.values()
          .filter(_.getGroups.contains(group.groupNumber))
          .toList
    }
    captureGroups
      .filter(_.building != null)
      .filter(g => isMy(g.building))
      .foreach { g =>
        g.building = null
        g.needToShrink = true
      }
  }

  private def isMy(b: Building) = b.ownerPlayerId == world.getMyPlayer.getId

  private def isOpponent(b: Building) = b.ownerPlayerId == world.getOpponentPlayer.getId

  /**
    * Достаём отложенное действие из очереди и выполняем его.
    *
    * @return Возвращает { @code true}, если и только если отложенное действие было найдено и выполнено.
    */
  private def executeDelayedMove(): Boolean = {
    val delayedMove = delayedMoves.poll
    if (delayedMove == null) {
      false
    }
    else {
      println(s"[${world.getTickIndex}]: $delayedMove")
      delayedMove.action(move)
      true
    }
  }

  private implicit def richList(list: Seq[Double]) = new {
    def average: Option[Double] =
      if (list.nonEmpty) Some(list.sum / list.length)
      else None
  }

  /**
    * Основная логика нашей стратегии.
    */
  private def makeMove(): Unit = {
    if (world.getTickIndex == 0) {
      initMinefield()
    } else {
      addNuke()
      if (buildings.nonEmpty) {
        setUpProduction()
        captureBuildings()
      }
    }
  }

  private def setUpProduction(): Unit = {
    buildings.values
      .filter(isMy)
      .filter(_.`type` == FacilityType.VEHICLE_FACTORY)
      .filter(_.vehicleType == null)
      .foreach { b =>
        delayedMoves.add(Production(b, HELICOPTER))
      }
  }

  private def captureBuildings(): Unit = {
    val emptyBuildings = buildings.values
      .filter(_.ownerPlayerId == -1)
      .filterNot(b => captureGroups.exists(_.building == b))
    val freeCaptureGroups = captureGroups.filter(_.building == null).filter(_.isAlive)
    val possibleTasks: List[(CaptureGroup, Building)] =
      (for {
        b <- emptyBuildings
        g <- freeCaptureGroups
        v = g.vehicles.minBy(_.getSquaredDistanceTo(b.leftTop.x, b.leftTop.y))
      } yield (g, b, v.getDistanceTo(b.leftTop.x, b.leftTop.y)))
        .toList
        .sortBy(_._3)
        .map {
          case (g, b, d) => (g, b)
        }

    val tasks = possibleTasks.foldLeft((List.empty[CaptureTask], Set.empty[CaptureGroup], Set.empty[Building])) {
      case ((tasks, groups, buildings), (g, b)) =>
        if (groups(g) || buildings(b)) (tasks, groups, buildings)
        else (CaptureTask(g, b) :: tasks, groups + g, buildings + b)
    }._1
      .sortBy(_.group.groupNumber).reverse

    tasks.foreach(addCaptureTask)

    captureGroups
      .filter(_.building == null)
      .flatMap { group =>
        buildings.values
          .toList
          .filter(b => captureGroups.count(_.building == b) < 2)
          .sortBy(_.center.squaredDistanceTo(group.center))
          .headOption
          .flatMap { b =>
            group.building = b
            Some(CaptureTask(group, b))
          }
      }.foreach(addCaptureTask)
  }

  private def addCaptureTask(task: CaptureTask): Unit = {
    delayedMoves.add(SelectGroup(task.group.groupNumber))
    delayedMoves.add(GoTo(task.building.center - task.group.center))
    task.group.building = task.building
  }

  case class CaptureTask(group: CaptureGroup, building: Building)

  private def addNuke(): Boolean =
    if (world.getMyPlayer.getRemainingNuclearStrikeCooldownTicks == 0) {
      val targets = opponentUnits
        .map(GameMap.vehicleToSquare)
        .groupBy(identity)
        .toList
        .sortBy(_._2.size)
        .reverse
        .map(_._1)
        .map(GameMap.squareCenter)

      val spotters = myUnits
      val targetOption = (for {
        target <- targets
        spotter <- spotters
        if spotter.getDistanceTo(target._1, target._2) <= getActualVisionRange(spotter)
      } yield (target, spotter)).headOption

      targetOption.foreach {
        case ((x, y), spotter) =>
          delayedMoves.addFirst(NuclearStrike(x, y, spotter.getId))
      }
      targetOption.isDefined
    } else false

  private def selectAll(vehicleType: VehicleType) =
    Select(0, 0, world.getWidth, world.getHeight, vehicleType)

  private def myUnits = vehicleById.values.filter { v => v.getPlayerId == me.getId }

  private def my(vType: VehicleType) =
    myUnits.filter(_.getType == vType)

  private def opponentUnits = vehicleById.values.filter { v => v.getPlayerId != me.getId }

  private def initMinefield(): Unit =
    if (buildings.isEmpty) {
      Seq(FIGHTER, HELICOPTER, TANK, IFV, ARRV).zipWithIndex
        .foreach {
          case (t, i) =>
            val vehicles = my(t)
            val xs = vehicles.map(_.getX)
            val ys = vehicles.map(_.getY)
            val minX = xs.min
            val minY = ys.min
            val startX = minX + i * 5
            val startY = minY + i * 5
            Seq(selectAll(t),
              GoTo(startX, startY),
              Scale(minX, minY, 10.0))
              .foreach(delayedMoves.add)
        }
    } else {
      Seq(FIGHTER, HELICOPTER).zipWithIndex
        .foreach {
          case (t, i) =>
            val vehicles = my(t)
            val xs = vehicles.map(_.getX)
            val ys = vehicles.map(_.getY)
            val minX = xs.min
            val minY = ys.min
            val startX = minX + i * 5
            val startY = minY + i * 5
            Seq(selectAll(t),
              GoTo(startX, startY),
              Scale(minX, minY, 10.0))
              .foreach(delayedMoves.add)
        }

      case class AssignTask(leftTop: Point, rightBottom: Point, vehicleType: VehicleType)

      val tasks = Seq(IFV, TANK, ARRV).flatMap { t =>
        val vehicles = my(t)
        val xs = vehicles.map(_.getX)
        val ys = vehicles.map(_.getY)
        val minX = xs.min
        val minY = ys.min
        val maxX = xs.max
        val maxY = ys.max
        val centerX = (maxX + minX) / 2
        val centerY = (maxY + minY) / 2
        List(
          AssignTask(Point(minX, minY), Point(centerX, centerY), t),
          AssignTask(Point(centerX, minY), Point(maxX, centerY), t),
          AssignTask(Point(minX, centerY), Point(centerX, maxY), t),
          AssignTask(Point(centerX, centerY), Point(maxX, maxY), t)
        )
      }

      tasks.sortBy(_.leftTop.squaredDistanceTo(Point(0, 0)))
        .foreach { t =>
          assignGroup(t.leftTop, t.rightBottom, t.vehicleType)
        }
    }

  private var groupNumber = 0

  private var groups: List[Int] = Nil

  private var captureGroups: List[CaptureGroup] = Nil

  private def nextGroupNumber: Int = {
    groupNumber += 1
    groupNumber
  }

  private def assignGroup(leftTop: Point, rightBottom: Point, vehicleType: VehicleType): Unit = {
    delayedMoves.add(Select(leftTop.x, leftTop.y, rightBottom.x, rightBottom.y, vehicleType))
    val number = nextGroupNumber
    delayedMoves.add(Assign(number))
    groups = number :: groups
    captureGroups = new CaptureGroup(number) :: captureGroups
  }
}
