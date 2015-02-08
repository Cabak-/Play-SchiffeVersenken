package models
import PlayerMessages._

/** trait to control the gameplay */
trait GameController {

  /** id of the Game */
  def id: String

	/** size of the fields */
	def fieldSize: Int = 10

	/** amount of boats of the length 0 to 5 */
	def boatCount: Array[Int] = Array(0,0,2,1,0,0)

	/** the total amount of boats for each player */
	def totalBoatCount: Int

	/** the player objects */
	def players: Array[Player]

  /** total amount of players */
  def totalPlayerCount: Int = 2

	/** the game state */
	def gameState: GameState

	/** calculates the total number of boats */
	def getTotalBoatCount(): Int = {
		def sumRest(i: Int): Int = {
			if (boatCount.length - 1 > i) {
				return sumRest(i + 1) + boatCount(i)
			} else {
				return boatCount(i)
			}
		};
		return sumRest(0)
	}

	/** registers a boat placement by a player */
	def placeBoat(player: Player, x: Int, y: Int, l: Int, horizontal: Boolean): Boolean = {
		try {
			gameState.placeBoat(player,x,y,l,horizontal)
			player.passMessage(BOAT_PLACED)
			return true
		} catch {
			case e: NotPlayersTurnException => player.passException(e)
			case e: BoatOverhangException => player.passException(e)
			case e: BoatOverlapException => player.passException(e)
			case e: InvalidActionException => player.passException(e)
			case e: InvalidPlacementException => player.passException(e)
		}
		return false
	}

	/** registers a shot by a player */
	def proceedShot(player: Player, x: Int, y: Int): Boolean = {
		try {
			val shotResult: Int = gameState.proceedShot(player,x,y)
			shotResult match {
				case 0 => player.passMessage(WATER)
				case 1 => player.passMessage(HIT)
				case 2 => player.passMessage(SUNK)
			}
			return true
		} catch {
			case e: NotPlayersTurnException => player.passException(e)
			case e: PlacementFinishedException => player.passException(e)
			case e: PlacementNotFinishedException => player.passException(e)
			case e: ShotNotInFieldException => player.passException(e)
			case e: ShotOnUncoveredCellException => player.passException(e)
			case e: InvalidActionException => player.passException(e)
		}
		return false
	}

}

/** concrete game controller class for asynchronous play via a browser interface */
class ConcreteGameController(val id: String, val player1: RemotePlayer, val player2: RemotePlayer) extends GameController {

	/** array of player */
	val players: Array[Player] = Array(player1,player2)

	/** calculate the total boat count */
	val totalBoatCount: Int = getTotalBoatCount()

	/** the game state */
	val gameState: GameState = new GameState(fieldSize,player1,player2,boatCount,totalBoatCount)

  gameState.resetFieldStates

}


/** concrete game controller class with both players playing locally */
class ConsoleGameController(val id: String, val player1: ConsolePlayer, val player2: ConsolePlayer) extends GameController {

	/** array of player */
	val players: Array[Player] = Array(player1,player2)

	/** calculate the total boat count */
	val totalBoatCount: Int = getTotalBoatCount()

	/** create the game state */
	val gameState: GameState = new GameState(fieldSize,player1,player2,boatCount,totalBoatCount)

	/** initializes the game */
	def initGame(): Unit = {
		// demand players to place all of their boats
		def iterateBoatPlacements(player: Player, l: Int, left: Int): Unit = {
			if (left > 0) {
				// output the current field
				//gameState.fieldStateMap(player).gameArea.show
				// request the placement from the player
				val newBoat: Boat = player.requestPlacement(gameState.getNextBoatID(player),l)
				// was the placement successful?
				if (placeBoat(player,newBoat.startX,newBoat.startY,newBoat.length,newBoat.isHorizontal)) {
					// output the altered field
					gameState.fieldStateMap(player).gameArea.show
					// proceed
					iterateBoatPlacements(player,l,left-1)
				} else { // repeat the placement
					iterateBoatPlacements(player,l,left)
				}
			}
		};
		// demand player 1 to place all of his boats
		for (l <- 1 until boatCount.length) {
			iterateBoatPlacements(player1,l,boatCount(l))
		}

		// switch to player 2
		player2.requestEnter
		// demand player 2 to place all of his boats
		for (l <- 1 until boatCount.length) {
			iterateBoatPlacements(player2,l,boatCount(l))
		}

		// initialize the field states
		gameState.resetFieldStates
    gameState.resetBoatLife

		// let the players shoot until the game ends
		def proceedGame: Unit = {
			// determine which player's turn it is
			val playerTurn: ConsolePlayer = gameState.playerTurn.asInstanceOf[ConsolePlayer]
			// determine the opponent and his field state
			val opponent: Player = gameState.getOpponent(playerTurn)
			val opponentFieldState: FieldState = gameState.fieldStateMap(opponent)

			// prompt the player to press enter
			playerTurn.requestEnter

			// output the current field state
			opponentFieldState.show

			// prompt the player for a shot
			val shotCoordinates: Array[Int] = playerTurn.requestShot()

			// proceed the shot
			if (proceedShot(playerTurn,shotCoordinates(0),shotCoordinates(1))) {
				// shot was successful;
				// output the current field state
				opponentFieldState.show

				// check if the game is finished
				if (opponentFieldState.isFinished) {
					playerTurn.passMessage(YOU_WIN)
				} else {
					// switch the turn and proceed
					gameState.switchTurn
					proceedGame
				}
			}	else { // shot was not successful
				proceedGame
			}
		};
		proceedGame

	}

}
