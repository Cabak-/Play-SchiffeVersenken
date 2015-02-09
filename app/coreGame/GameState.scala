package coreGame

import PlayerMessages._

/** model for the game state, including information about the current
  * phase of the game and non-static information about the fields */
class GameState(fieldSize: Int, player1: Player, player2: Player, boatCount: Array[Int], totalBoatsCount: Int) {

  /** the boats player 1 and 2 still have to set */
  var boatsToPlace: Map[Player,Int] = Map(player1 -> totalBoatsCount, player2 -> totalBoatsCount)

  /** which player's turn */
  var playerTurn: Player = player1

  /** field state for player 1 */
  val fieldState1: FieldState = new FieldState(fieldSize,boatCount,totalBoatsCount,StaticGameAreaFactory.create(fieldSize,totalBoatsCount))

  /** field state for player 2 */
  val fieldState2: FieldState = new FieldState(fieldSize,boatCount,totalBoatsCount,StaticGameAreaFactory.create(fieldSize,totalBoatsCount))

  /** map from players to their field states */
  val fieldStateMap: Map[Player,FieldState] = Map(player1 -> fieldState1, player2 -> fieldState2)


  /** resets the field states (needs to be done at the beginning of the game) */
  def resetFieldStates: Unit = {
    fieldState1.reset
    fieldState2.reset
  }

  /** resets the boat life (needs to be done at the beginning of the game) */
  def resetBoatLife: Unit = {
    fieldState1.resetLife
    fieldState2.resetLife
  }

  /** returns the opponent of a given player */
  def getOpponent(player: Player): Player = {
    if (playerTurn == player1) return player2
    else return player1
  }

  /** switches the turn to the other player */
  def switchTurn: Unit = {
    playerTurn = getOpponent(playerTurn)
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
        return calculateLength(iterator+boatCount(l),next,l+1)
      }
    }
    return calculateLength(0,nextID,1)
  }

  /** checks if it is a player's turn */
  def isPlayersTurn(player: Player): Boolean = (playerTurn == player)

  /** registers a boat placement by a player */
  def placeBoat(player: Player, x: Int, y: Int, l: Int, horizontal: Boolean): Unit = {
    // is it the player's turn?
    if (!isPlayersTurn(player)) throw new NotPlayersTurnException
    // is the player still in the boat placement phase?
    if (boatsToPlace(player) <= 0) throw new PlacementFinishedException

    // try to place the boat, can produce InvalidPlacementExceptions
    val newBoat: Boat = new Boat(x,y,l,horizontal,getNextBoatID(player))
    fieldStateMap(player).gameArea = fieldStateMap(player).gameArea.placeBoat(newBoat)

    // boat was successfully placed (no exceptions caused)
    boatsToPlace = boatsToPlace + (player -> (boatsToPlace(player) - 1))

    // checks if it is the next player's turn
    if (boatsToPlace(player) == 0) {
      // reset the boat life for the player who has finished placing his boats
      fieldStateMap(player).resetLife
      // switch the turn
      switchTurn
    }
  }

  /** registers a shot by a player at a cell */
  def proceedShot(player: Player, x: Int, y: Int): Int = {
    // is it the player's turn?
    if (!isPlayersTurn(player)) throw new NotPlayersTurnException
    // is the placement phase finished?
    if (!isPlacementFinished) throw new PlacementNotFinishedException

    val opponent: Player = getOpponent(player)
    // check if the shot is outside the field
    if (x < 0 || x >= fieldSize || y < 0 || y >= fieldSize) {
      throw new ShotNotInFieldException
    }

    // shoot and return the result (0: water, 1: hit, 2: hit + boat sunk)
    val shotResult: Int = fieldStateMap(opponent).registerShot(x,y)

    // switch the turn
    switchTurn

    // has the player won the game? -> 3
    if (fieldStateMap(opponent).isFinished) {
      return 3
    }

    return shotResult;
  }

}
