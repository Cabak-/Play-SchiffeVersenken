package models
import PlayerMessages._

/** trait for views to output information for a player */
trait View {

  def player: Player

  /** outputs a message for the player - may be overridden! */
  def outputMessage(msg: PlayerMessages): Unit = {
    msg match {
      case BOAT_PLACED => println("Das Boot wurde platziert.")
      case WATER => println("Wasser.")
      case HIT => println("Treffer!")
      case SUNK => println("Versenkt!")
      case YOU_WIN => println("Alle Boote wurden versenkt! " + player.playerName + " gewinnt!")
      case YOU_LOSE => println("Alle deine Boote wurden versenkt! Du hast das Spiel leider verloren.")
    }
  }

  /** informs the player about an exception - may be overridden! */
  def outputException(e: Exception): Unit = {
    e match {
      // InvalidPlacementException
      case e: BoatOverhangException =>
        println("Das Boot ist nicht vollstaendig im Spielfeld enthalten.")
      case e: BoatOverlapException =>
        println("Ein anderes Boot liegt zu nahe an der ausgewaehlten Position.");
      case e: InvalidPlacementException =>
        println("Das Boot wurde nicht korrekt platziert!")
      // InvalidActionException
      case e: NotPlayersTurnException =>
        println("Du bist nicht an der Reihe!")
      case e: PlacementFinishedException =>
        println("Es wurden bereits alle Boot platziert!")
      case e: PlacementNotFinishedException =>
        println("Es wurden noch nicht alle Boote platziert!")
      case e: ShotNotInFieldException =>
        println("Die eingegebenen Koordinaten sind ungueltig!")
      case e: ShotOnUncoveredCellException =>
        println("Du hast auf diese Position bereits geschossen!");
      case e: InvalidActionException =>
        println("Eine ungueltige Aktion wurde ausgefuehrt!")
    }
  }

}


/** a console view that works with simple input and output */
class ConsoleView(val player: ConsolePlayer) extends View {

  /** reads coordinates from STDIN */
  def readCoordinates: Array[Int] = {
    val patternCoordinates = "\\d,\\d"
    for (ln <- io.Source.stdin.getLines) {
      if (ln.matches(patternCoordinates)) {
        val parts = ln.split(',')
        return Array(parts(0).toInt,parts(1).toInt)
      } else {
        println("Ungueltige Eingabe. Bitte geben Sie die Koordinaten in der Form \"x,y\" ein (ohne Anfuehrungszeichen).")
      }
    }
    return null
  }

  /** reads a boolean value from STDIN */
  def readBoolean(trueString: String, falseString: String, falseIsDefault: Boolean): Boolean = {
    for (ln <- io.Source.stdin.getLines) {
      if (ln.matches(trueString)) {
        return true
      } else if (ln.matches(falseString)) {
        return false
      } else if (falseIsDefault) {
        return false
      } else {
        println("Ungueltige Eingabe. Bitte geben Sie entweder \"" + trueString + "\" oder \"" + falseString + "\" ein (ohne Anfuehrungszeichen).")
      }
    }
    return false
  }

  /** demands the player to press enter */
  def requestEnter: Unit = {
    println(player.playerName + " [BITTE ENTER DRUECKEN]")
    for (ln <- io.Source.stdin.getLines()) return
  }

}

/** a browser-based view */
class BrowserView(val player: RemotePlayer) extends View {

}