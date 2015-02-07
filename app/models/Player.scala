package models
import PlayerMessages._

/** trait for players that participate in the game */
trait Player {

  /** Id of the Player */
  def playerID: String

  /** name of the player */
  def playerName: String

  /** the view for the player */
  def view: View

  /** passes a message to the player */
  def passMessage(msg: PlayerMessages): Unit = {
    view.outputMessage(msg)
  }

  /** passes an exception to the player */
  def passException(e: Exception): Unit = {
    view.outputException(e)
  }

  /** demands the player to place a boat and returns the new created boat */
  def requestPlacement(id: Int, l: Int): Boat

  /** demands the player to shoot on a cell */
  def requestShot(): Array[Int]

}


/** a remote player who plays in the browser */
class RemotePlayer(val playerID: String, val playerName: String) extends Player {

  /** uses a browser view */
  val view: View = new BrowserView(this)

  /** demands the player to place a boat and returns the new created boat */
  def requestPlacement(id: Int, l: Int): Boat = {
    return null
  }

  /** demands the player to shoot on a cell */
  def requestShot(): Array[Int] = {
    return null
  }

}


/** a local player that answers request by console prompts */
class ConsolePlayer(val playerID: String, val playerName: String) extends Player {

  /** uses a console view */
  val view: ConsoleView = new ConsoleView(this)

  /** demands the player to place a boat and returns a new boat object */
  @throws[NumberFormatException]("The given value is not a valid number.")
  override def requestPlacement(id: Int, l: Int): Boat  = {
    println("-------------------------------------")
    println(playerName + ", bitte platziere Boot #" + id + " mit der Laenge " + l + ":")
    val coordinates: Array[Int] = view.readCoordinates

    println("Bitte waehle die Ausrichtung fuer das Boot (h/v):")
    val horizontal: Boolean = view.readBoolean("h","v",false)

    // create the boat object and return it
    new Boat(coordinates(0), coordinates(1), l, horizontal, id)
  }

  /** demands the player to shoot on a cell, returns an array of integers with the 2 coordinates */
  @throws[NumberFormatException]("The given value is not a valid number.")
  override def requestShot(): Array[Int] = {
    println("-------------------------------------")
    println(playerName + ", bitte gib die Koordinaten an, auf die geschossen werden soll:")

    return view.readCoordinates
  }

  /** demands the player to press enter */
  def requestEnter: Unit = {
    view.requestEnter
  }

}