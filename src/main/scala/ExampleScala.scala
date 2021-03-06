import java.util
import java.util.Random
import java.util.function.Consumer

import model.{ActionType, Game, Move, Player, TerrainType, Vehicle, VehicleType, WeatherType, World}

import scala.collection.convert.ImplicitConversionsToScala._
import scala.language.implicitConversions

final class ExampleScala extends Strategy {

  final private val vehicleById = new util.HashMap[Long, Vehicle]
  final private val updateTickByVehicleId = new util.HashMap[Long, Integer]
  final private val delayedMoves = new util.ArrayDeque[Consumer[Move]]
  private var random: Random = _
  private var terrainTypeByCellXY: Array[Array[TerrainType]] = _
  private var weatherTypeByCellXY: Array[Array[WeatherType]] = _
  private var me: Player = _
  private var world: World = _
  private var game: Game = _
  private var move: Move = _

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
  }

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
      delayedMove.accept(move)
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
  private def makeMove(): Unit = { // Каждые 180 тиков ...
    if (world.getTickIndex % 180 == 0) { // ... для каждого типа техники ...
      for {
        vehicleType <- VehicleType.values
        targetTypes = ExampleScala.preferredTargetTypesByVehicleType.getOrElse(vehicleType, Nil)
        // ... если этот тип может атаковать ...
        if targetTypes.nonEmpty
      } {
        // ... получаем центр формации ...
        val xOpt = streamVehicles(ExampleScala.Ownership.ALLY, vehicleType).map(_.getX).average
        val yOpt = streamVehicles(ExampleScala.Ownership.ALLY, vehicleType).map(_.getY).average
        // ... получаем центр формации противника или центр мира ...
        val targetX = targetTypes
          .flatMap(targetType => streamVehicles(ExampleScala.Ownership.ENEMY, targetType).map(_.getX).average)
          .headOption
          .getOrElse(streamVehicles(ExampleScala.Ownership.ENEMY).map(_.getX).average.getOrElse(world.getWidth / 2.0D))
        val targetY = targetTypes
          .flatMap(targetType => streamVehicles(ExampleScala.Ownership.ENEMY, targetType).map(_.getY).average)
          .headOption
          .getOrElse(streamVehicles(ExampleScala.Ownership.ENEMY).map(_.getY).average.getOrElse(world.getHeight / 2.0D))
        // .. и добавляем в очередь отложенные действия для выделения и перемещения техники.
        (xOpt, yOpt) match {
          case (Some(x), Some(y)) =>
            delayedMoves.add((move: Move) => {
              def foo(move: Move) {
                move.setAction(ActionType.CLEAR_AND_SELECT)
                move.setRight(world.getWidth)
                move.setBottom(world.getHeight)
                move.setVehicleType(vehicleType)
              }

              foo(move)
            })
            delayedMoves.add((move: Move) => {
              def foo(move: Move) {
                move.setAction(ActionType.MOVE)
                move.setX(targetX - x)
                move.setY(targetY - y)
              }

              foo(move)
            })
          case _ =>
        }
      }
      // Также находим центр формации наших БРЭМ ...
      val xOpt = streamVehicles(ExampleScala.Ownership.ALLY, VehicleType.ARRV).map(_.getX).average
      val yOpt = streamVehicles(ExampleScala.Ownership.ALLY, VehicleType.ARRV).map(_.getY).average
      // .. и отправляем их в центр мира.
      (xOpt, yOpt) match {
        case (Some(x), Some(y)) =>
          delayedMoves.add((move: Move) => {
            def foo(move: Move): Unit = {
              move.setAction(ActionType.CLEAR_AND_SELECT)
              move.setRight(world.getWidth)
              move.setBottom(world.getHeight)
              move.setVehicleType(VehicleType.ARRV)
            }

            foo(move)
          })
          delayedMoves.add((move: Move) => {
            def foo(move: Move): Unit = {
              move.setAction(ActionType.MOVE)
              move.setX(world.getWidth / 2.0D - x)
              move.setY(world.getHeight / 2.0D - y)
            }

            foo(move)
          })
        case _ =>
      }
      return
    }

    // Если ни один наш юнит не мог двигаться в течение 60 тиков ...
    if (streamVehicles(ExampleScala.Ownership.ALLY).forall(vehicle => world.getTickIndex - updateTickByVehicleId.get(vehicle.getId) > 60)) { // ... находим центр нашей формации ...
      val xOpt = streamVehicles(ExampleScala.Ownership.ALLY).map(_.getX).average
      val yOpt = streamVehicles(ExampleScala.Ownership.ALLY).map(_.getY).average
      // ... и поворачиваем её на случайный угол.
      (xOpt, yOpt) match {
        case (Some(x), Some(y)) =>
          delayedMoves.add((move: Move) => {
            def foo(move: Move): Unit = {
              move.setAction(ActionType.CLEAR_AND_SELECT)
              move.setRight(world.getWidth)
              move.setBottom(world.getHeight)
            }

            foo(move)
          })
          delayedMoves.add((move: Move) => {
            def foo(move: Move): Unit = {
              move.setAction(ActionType.ROTATE)
              move.setX(x)
              move.setY(y)
              move.setAngle(if (random.nextBoolean) StrictMath.PI
              else -StrictMath.PI)
            }

            foo(move)
          })
        case _ =>
      }
    }
  }

  import ExampleScala.Ownership._

  private def streamVehicles(ownership: Ownership, vehicleType: VehicleType = null): Seq[Vehicle] = {
    val vehicles = vehicleById.values.filter { v =>
      ownership match {
        case ALLY =>
          v.getPlayerId == me.getId
        case ENEMY =>
          v.getPlayerId != me.getId
        case _ => true
      }
    }.toList

    if (vehicleType != null) vehicles.filter(_.getType == vehicleType)
    else vehicles
  }

}

object ExampleScala {

  /**
    * Список целей для каждого типа техники, упорядоченных по убыванию урона по ним.
    */
  private val preferredTargetTypesByVehicleType = Map[VehicleType, List[VehicleType]](
    VehicleType.FIGHTER -> List(VehicleType.HELICOPTER, VehicleType.FIGHTER),
    VehicleType.HELICOPTER -> List(VehicleType.TANK, VehicleType.ARRV, VehicleType.HELICOPTER, VehicleType.IFV, VehicleType.FIGHTER),
    VehicleType.IFV -> List(VehicleType.HELICOPTER, VehicleType.ARRV, VehicleType.IFV, VehicleType.FIGHTER, VehicleType.TANK),
    VehicleType.TANK -> List(VehicleType.IFV, VehicleType.ARRV, VehicleType.TANK, VehicleType.FIGHTER, VehicleType.HELICOPTER)
  )

  object Ownership extends Enumeration {
    type Ownership = Value
    val ANY, ALLY, ENEMY = Value
  }

}