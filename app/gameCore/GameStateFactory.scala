package gameCore

/** factory to create GameStates */
object GameStateFactory {

  /** create a game state with default settings (to begin a new game) */
  def create(fieldSize:Int,player1:Player,player2:Player,boatCount: IndexedSeq[Int],totalBoatsCount:Int): GameState = {
    val boatsToPlace: Map[Player,Int] = Map(player1 -> totalBoatsCount, player2 -> totalBoatsCount)
    val playerTurn: Player = player1
    var fieldState1: FieldState = FieldStateFactory.create(fieldSize,boatCount,totalBoatsCount)
    var fieldState2: FieldState = FieldStateFactory.create(fieldSize,boatCount,totalBoatsCount)
    new GameState(fieldSize,player1,player2,boatCount,totalBoatsCount:Int,boatsToPlace,playerTurn,fieldState1,fieldState2)
  }

}
