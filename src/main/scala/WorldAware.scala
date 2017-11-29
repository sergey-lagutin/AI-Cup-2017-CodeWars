import model.{Game, World}

trait WorldAware {
  def game: Game

  def world: World

}
