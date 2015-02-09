package gameCore

/** main object of the game,
  * to play a local game in the console
  */
object Main {

  /** main method of the game */
  def main(args: Array[String]) {
      // determine the size of the field (both horizontally and vertically)
      val fieldSize: Int = 10

      // determine the amount of boats of the length 0 to 5
      val boatCount = Vector(0,0,4,3,2,1)
      //val boatCount = Vector(0,0,2,0,0,0)

      /** create local player 1 */
      val player1: ConsolePlayer = new ConsolePlayer("1","Player 1")

      /** create local player 2 */
      val player2: ConsolePlayer = new ConsolePlayer("2","Player 2")

      /** create game ID */
      val gameID: String = "game1"

      // create the game object
      val game = new ConsoleGameController(gameID,fieldSize,boatCount,player1,player2)

      // output the total number of boats
      println("Total boat count: " + game.getTotalBoatCount())

      // initialize the game
      game.initGame()
  }

}
