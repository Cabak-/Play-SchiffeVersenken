package gameCore

import PlayerMessages._

/** immutable model for the game state, including all information about the current
  * phase of the game and the state of the fields
  * @param fieldSize size of the field
  * @param player1 first player
  * @param player2 second player
  * @param boatCount amount of boats of length i
  * @param totalBoatsCount total amount of boats
  * @param boatsToPlace the boats player 1 and 2 still have to set
  * @param fieldState1 field state for player 1
  * @param fieldState2 field state for player 2
  */
class GameState(fieldSize: Int, val player1: Player, val player2: Player, boatCount: IndexedSeq[Int], totalBoatsCount: Int,
                val boatsToPlace: Map[Player,Int], val playerTurn: Player, val fieldState1: FieldState, val fieldState2: FieldState) {

  /** map from players to their field states */
  val fieldStateMap: Map[Player,FieldState] = Map(player1 -> fieldState1, player2 -> fieldState2)

  /** returns the opponent of a given player */
  def getOpponent(player: Player): Player = {
    if (playerTurn == player1) return player2
    else return player1
  }

  /** switches the turn to the other player */
  def switchTurn: GameState = {
    val newPlayerTurn = getOpponent(playerTurn)
    new GameState(fieldSize, player1, player2, boatCount, totalBoatsCount, boatsToPlace, newPlayerTurn, fieldState1, fieldState2)
  }

  /** checks if the boat placement phase was ended */
  def isPlacementFinished: Boolean = {
    return (boatsToPlace(player1) == 0 && boatsToPlace(player2) == 0)
  }

  /** returns the id of the next boat a player can place */
  def getNextBoatID(placementPlayer: Player): Int = {
    if (boatsToPlace(placementPlayer) <= 0) return -1
    else return totalBoatsCount - boatsToPlace(placementPlayer) + 1
  }

  /** returns the length of the next boat to be placed (by the player who has the turn) */
  def getNextBoatLength: Int = {
    val nextID: Int = getNextBoatID(playerTurn)
    def calculateLength(iterator:Int,next:Int,l:Int): Int = {
      if (iterator + boatCount(l) >= next) {
        return l
      } else {
        return calculateLength(iterator+boatCount(l),next,l-1)
      }
    }
    return calculateLength(0,nextID,5)
  }

  /** checks if it is a player's turn */
  def isPlayersTurn(player: Player): Boolean = (playerTurn == player)

  /** registers a boat placement by a player */
  def placeBoat(player: Player, x: Int, y: Int, l: Int, horizontal: Boolean): GameState = {
    // is it the player's turn?
    if (!isPlayersTurn(player)) throw new NotPlayersTurnException
    // is the player still in the boat placement phase?
    if (boatsToPlace(player) <= 0) throw new PlacementFinishedException

    // try to place the boat, can produce InvalidPlacementExceptions
    val newBoat: Boat = new Boat(x,y,l,horizontal,getNextBoatID(player))
    //val newGameArea: StaticGameArea = fieldStateMap(player).gameArea.placeBoat(newBoat)
    if (player == player1) {
      // change field state 1
      val newFieldState1: FieldState = fieldState1.placeBoat(newBoat)

      // boat was successfully placed (no exceptions caused)
      val newBoatsToPlace = boatsToPlace + (player -> (boatsToPlace(player) - 1))

      // checks if it is the next player's turn
      val newPlayerTurn: Player = if (newBoatsToPlace(player) == 0) getOpponent(playerTurn) else playerTurn


      return new GameState(fieldSize, player1, player2, boatCount, totalBoatsCount, newBoatsToPlace, newPlayerTurn, newFieldState1, fieldState2)
    } else {
      // change field state 2
      val newFieldState2: FieldState = fieldState2.placeBoat(newBoat)

      // boat was successfully placed (no exceptions caused)
      val newBoatsToPlace = boatsToPlace + (player -> (boatsToPlace(player) - 1))

      // checks if it is the next player's turn
      val newPlayerTurn: Player = if (newBoatsToPlace(player) == 0) getOpponent(playerTurn) else playerTurn

      return new GameState(fieldSize, player1, player2, boatCount, totalBoatsCount, newBoatsToPlace, newPlayerTurn, fieldState1, newFieldState2)
    }
  }

  /** registers a shot by a player at a cell */
  def proceedShot(player: Player, x: Int, y: Int): GameState = {
    // is it the player's turn?
    if (!isPlayersTurn(player)) throw new NotPlayersTurnException
    // is the placement phase finished?
    if (!isPlacementFinished) throw new PlacementNotFinishedException

    val opponent: Player = getOpponent(player)
    // check if the shot is outside the field
    if (x < 0 || x >= fieldSize || y < 0 || y >= fieldSize) {
      throw new ShotNotInFieldException
    }

    // shoot and thereby change the field state
    if (player == player1) {
      // change field state 2
      val newFieldState2: FieldState = fieldState2.registerShot(x,y)

      // switch the turn
      val newPlayerTurn: Player = getOpponent(playerTurn)

      return new GameState(fieldSize, player1, player2, boatCount, totalBoatsCount, boatsToPlace, newPlayerTurn, fieldState1, newFieldState2)
    } else {
      // change field state 1
      val newFieldState1: FieldState = fieldState1.registerShot(x,y)

      // switch the turn
      val newPlayerTurn: Player = getOpponent(playerTurn)

      return new GameState(fieldSize, player1, player2, boatCount, totalBoatsCount, boatsToPlace, newPlayerTurn, newFieldState1, fieldState2)
    }
  }

}
