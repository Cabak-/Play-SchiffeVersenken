package controllers

import java.lang.ProcessBuilder.Redirect
import models._
import play.api._
import play.api.mvc._
import scala.util.control.Breaks._


object Application extends Controller {

  val arrayOfGames: Array[GameController] = new Array[GameController](1)
  var arrayOfPlayers: Array[RemotePlayer] = new Array[RemotePlayer](2)

  def index = Action { implicit request =>
    request.session.get("name").map { user =>
      Ok(views.html.index("Welcome!"))
    }.getOrElse {
      Ok(views.html.index("Welcome!")).withSession(
        "name" -> "default"
      )
    }

  }

  def authenticate = Action { implicit request =>
    val name = request.body.asFormUrlEncoded.get("name") // Insert a player
      val player : RemotePlayer = new RemotePlayer(createID(), name(0))
      breakable{
        for (i <- 0 to arrayOfPlayers.length - 1) {
          if (arrayOfPlayers(i) == null) {
            arrayOfPlayers(i) = player
            break
          }
        }
      }
      if (waitFor2ndPlayer) {
        Redirect(routes.Application.game).withSession(
          request.session + ("name" -> player.playerName
            + "id" -> player.playerID)
        )
      } else {
        Ok(views.html.index("Leider hat sich kein Spieler Gefunden! BItte versuchen sie es später noch einmal!")).withSession(
          request.session + ("name" -> player.playerName
            + "id" -> player.playerID)
        )
      }
  }

  def game = {
      if (arrayOfGames(0) == null) {
        val newGameController: ConcreteGameController = new ConcreteGameController(createID(), arrayOfPlayers(0), arrayOfPlayers(1))
        arrayOfGames(0) = newGameController
        Ok(views.html.game("Das Spiel kann beginnen!! \n "
          + "Spieler 1 : " + arrayOfGames(0).players(0).playerName
          + " Spieler 2 : " + arrayOfGames(0).players(1).playerName
          + " Game ID: " + arrayOfGames(0).id, arrayOfGames(0)))
      }else {
        Ok(views.html.index("Leider sind alle game Instanzen bereits vergeben! Versuchen sie es später bitte noch einmal!"))
      }

  }

  def waitFor2ndPlayer: Boolean =  {
    for( i <- 0 until 10){
      Thread.sleep(1000)
      if (arrayOfPlayers(1) != null) {
        return true
      }
    }
    return false
  }

  def createID() : String = {
    return java.util.UUID.randomUUID().toString()
  }

  def endGame = Action {
    Redirect(routes.Application.index)
  }

}