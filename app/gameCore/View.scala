package gameCore

import PlayerMessages._

/** trait for views to output information for a player */
trait View {

  def player: Player

  /** returns a string message for a message type */
  def getStringMessage(msg: PlayerMessages): String = {
    msg match {
      case BOAT_PLACED => "Das Boot wurde platziert."
      case WATER => "Wasser."
      case HIT => "Treffer!"
      case SUNK => "Versenkt!"
      case YOU_WIN => "Alle Boote wurden versenkt! " + player.playerName + " gewinnt!"
      case YOU_LOSE => "Alle deine Boote wurden versenkt! Du hast das Spiel leider verloren."
    }
  }

  /** outputs a message for the player - may be overridden! */
  def outputMessage(msg: PlayerMessages): Unit = {
    println(getStringMessage(msg))
  }

  /** returns a string message for an exception */
  def getStringMessageFromException(e: Exception): String = {
    e match {
      // InvalidPlacementException
      case e: BoatOverhangException =>
        "Das Boot ist nicht vollstaendig im Spielfeld enthalten."
      case e: BoatOverlapException =>
        "Ein anderes Boot liegt zu nahe an der ausgewaehlten Position."
      case e: InvalidPlacementException =>
        "Das Boot wurde nicht korrekt platziert!"
      // InvalidActionException
      case e: NotPlayersTurnException =>
        "Du bist nicht an der Reihe!"
      case e: PlacementFinishedException =>
        "Es wurden bereits alle Boot platziert!"
      case e: PlacementNotFinishedException =>
        "Es wurden noch nicht alle Boote platziert!"
      case e: ShotNotInFieldException =>
        "Die eingegebenen Koordinaten sind ungueltig!"
      case e: ShotOnUncoveredCellException =>
        "Du hast auf diese Position bereits geschossen!"
      case e: InvalidActionException =>
        "Eine ungueltige Aktion wurde ausgefuehrt!"
    }
  }

  /** informs the player about an exception - may be overridden! */
  def outputException(e: Exception): Unit = {
    println(getStringMessageFromException(e))
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

  /** saves the last message that the player received */
  var lastMessage: String = null

  /** outputs a message for the player */
  override def outputMessage(msg: PlayerMessages): Unit = {
    lastMessage = getStringMessage(msg)
  }

  /** outputs an exception for the player */
  override def outputException(e: Exception): Unit = {
    lastMessage = getStringMessageFromException(e)
  }

  /** resets the message for the player */
  def resetMessage: Unit = {
    lastMessage = null
  }

}