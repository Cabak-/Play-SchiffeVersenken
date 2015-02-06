/** main object of the game
object Main {

  /** main method of the game */
  def main(args: Array[String]) {
      // determine the size of the field (both horizontally and vertically)
      val areaSize: Int = 10

      // determine the amount of boats of the length 0 to 5
      //val boatCount = Array(0,0,4,3,2,1)
      val boatCount = Array(0,0,2,0,0,0)

      /** create local player 1 */
      val player1: ConsolePlayer = new ConsolePlayer("Player 1")

      /** create local player 2 */
      val player2: ConsolePlayer = new ConsolePlayer("Player 2")

      // create the game object
      val game = new ConsoleGameController(player1,player2,areaSize,boatCount)

      // output the total number of boats
      println("Total boat count: " + game.getTotalBoatCount())

      // initialize the game
      game.initGame()
  }

}
*/