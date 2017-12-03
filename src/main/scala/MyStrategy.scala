import java.util
import java.util.Random

import model.VehicleType._
import model.{Game, Move, Player, TerrainType, Vehicle, VehicleType, WeatherType, World}

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

    groups.foreach { group =>
      group.updateVehicles(
        vehicleById.values()
          .filter(_.getGroups.contains(group.groupNumber))
          .toList, world.getTickIndex
      )
    }
    captureGroups.foreach { group =>
      Option(group.building).foreach { b =>
        group.building = buildings(b.id)
      }
    }
  }

  private def resetBuildings(): Unit =
    captureGroups
      .filter(_.building != null)
      .filter(g => isMy(g.building))
      .foreach { g =>
        g.building = null
      }

  private def isMy(b: Building) = b.ownerPlayerId == world.getMyPlayer.getId

  private var attackGroups: List[AttackGroup] = Nil

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

  private def isMyFactory(b: Building) = isMy(b) && b.isFactory

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
        setUpAttackGroups()
        shrinkGroups()
        captureBuildings()
        attackOpponent()
      } else {
        moveTroops()
      }
    }
  }

  private var nextMoveTroopsTick = 2000

  private def moveTroops(): Unit =
    if (world.getTickIndex > nextMoveTroopsTick) {
      Seq(selectAll(null), GoTo(Dir(10, 10)))
        .foreach(delayedMoves.add)
      nextMoveTroopsTick += 1000
    }

  private def setUpProduction(): Unit = {
    buildings.values
      .filter(isMyFactory)
      .filter(_.vehicleType == null)
      .foreach { b =>
        delayedMoves.add(Production(b, HELICOPTER))
      }
  }

  private def captureBuildings(): Unit = {
    val aliveGroups = captureGroups.filter(_.isAlive)
    val availableCaptureGroups =
      aliveGroups.filter(g => g.building == null || isMy(g.building))

    val emptyBuildings = {
      val empties = buildings.values
        .filter(_.ownerPlayerId == -1)
        .filterNot(b => aliveGroups.exists(_.building == b))
      val availableGroupCount = availableCaptureGroups.size
      if (empties.size <= availableGroupCount) empties
      else empties
        .toList
        .sortBy(b => aliveGroups.map(_.center.squaredDistanceTo(b.firstCapturePosition)).min)
        .take(availableGroupCount)
    }

    val fromFarestToNearest = emptyBuildings
      .toList
      .sortBy(b => aliveGroups.map(_.center.squaredDistanceTo(b.firstCapturePosition)).min)
      .reverse

    val tasks = fromFarestToNearest.foldLeft((List.empty[CaptureTask], Set.empty[CaptureGroup])) {
      case ((tasks, groups), building) =>
        val group = availableCaptureGroups.filterNot(groups)
          .minBy(_.center.squaredDistanceTo(building.firstCapturePosition))
        (CaptureTask(group, building, building.firstCapturePosition) :: tasks, groups + group)
    }._1.reverse

    tasks.foreach(addCaptureTask)

    aliveGroups
      .filter(_.building == null)
      .flatMap { group =>
        buildings.values
          .toList
          .filter(b => aliveGroups.count(_.building == b) < 2)
          .filterNot(b => aliveGroups.exists(g => g.building == b && group.vehicleType == g.vehicleType))
          .sortBy(_.center.squaredDistanceTo(group.center))
          .headOption
          .flatMap { b =>
            group.building = b
            Some(CaptureTask(group, b, b.secondCapturePosition))
          }
      }.foreach(addCaptureTask)
  }

  private def addCaptureTask(task: CaptureTask): Unit = {
    delayedMoves.add(SelectGroup(task.group.groupNumber))
    delayedMoves.add(GoTo(task.target - task.group.center))
    task.group.building = task.building
  }

  private def attackOpponent(): Unit = {
    attackGroups
      .filter(_.isAlive)
      .foreach { g =>
        val ourCenter = g.center
        val nearestVehicle = opponentUnits.minBy(_.getDistanceTo(ourCenter.x, ourCenter.y))
        delayedMoves.add(SelectGroup(g.groupNumber))
        delayedMoves.add(GoTo(Point(nearestVehicle.getX, nearestVehicle.getY) - ourCenter))
      }
  }

  private def shrinkGroups(): Unit =
    groups
      .filter(_.isAlive)
      .filter(_.needToShrink)
      .foreach { g =>
        delayedMoves.add(SelectGroup(g.groupNumber))
        delayedMoves.add(Scale(g.center, 10))
        delayedMoves.add(Rotate(g.center, if (random.nextBoolean) math.Pi else -math.Pi))
        delayedMoves.add(Scale(g.center, 0.1))
        g.resetShrink()
      }

  private def assignCaptureGroup(leftTop: Point, rightBottom: Point, vehicleType: VehicleType): Unit = {
    delayedMoves.add(Select(leftTop.x, leftTop.y, rightBottom.x, rightBottom.y, vehicleType))
    val number = nextGroupNumber
    delayedMoves.add(Assign(number))
    val group = new CaptureGroup(number, vehicleType)
    groups = group :: groups
    captureGroups = group :: captureGroups
  }

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

  private def selectAll(vehicleType: VehicleType, add: Boolean = false) =
    Select(0, 0, world.getWidth, world.getHeight, vehicleType, add)

  private def myUnits = vehicleById.values.filter { v => v.getPlayerId == me.getId }

  private def my(vType: VehicleType) =
    myUnits.filter(_.getType == vType)

  private def opponentUnits = vehicleById.values.filter { v => v.getPlayerId != me.getId }

  private def setUpAttackGroups(): Unit = {
    val withoutGroups = myUnits.filter(_.getGroups.isEmpty)
    buildings.values
      .filter(isMyFactory)
      .filter(_.vehicleOnFactory(withoutGroups).size >= 20)
      .foreach(assignAttackGroup)
  }

  private var groupNumber = 0

  private var groups: List[VehicleGroup] = Nil

  private var captureGroups: List[CaptureGroup] = Nil

  private def fill(types: VehicleType*): Unit =
    types.sortBy(my(_).map(_.getDistanceTo(0, 0)).min).reverse
      .zipWithIndex
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

  private def initMinefield(): Unit =
    if (buildings.isEmpty) {
      fill(FIGHTER, HELICOPTER)
      fill(TANK, IFV, ARRV)
    } else {
      fill(FIGHTER, HELICOPTER)

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
          assignCaptureGroup(t.leftTop, t.rightBottom, t.vehicleType)
        }
    }

  private def nextGroupNumber: Int = {
    groupNumber += 1
    groupNumber
  }

  case class CaptureTask(group: CaptureGroup, building: Building, target: Point)

  private def assignAttackGroup(building: Building): Unit = {
    val (leftTop, rightBottom) = building.aroundBox
    delayedMoves.add(Select(leftTop.x, leftTop.y, rightBottom.x, rightBottom.y, HELICOPTER))
    delayedMoves.add(Select(leftTop.x, leftTop.y, rightBottom.x, rightBottom.y, FIGHTER, add = true))
    val number = nextGroupNumber
    delayedMoves.add(Assign(number))
    val group = new AttackGroup(number)
    groups = group :: groups
    attackGroups = group :: attackGroups
  }
}
