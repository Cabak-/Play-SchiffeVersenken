package controllers

import java.lang.ProcessBuilder.Redirect
import models._
import play.api._
import play.api.mvc._


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
    request.session.get("name").map { user =>
      val player : RemotePlayer = new RemotePlayer(createID(), name(0))
      if(arrayOfPlayers(0) == null){
        arrayOfPlayers(0) = player
      }else{
        arrayOfPlayers(1) = player
      }
      if (waitFor2ndPlayer) {
        Redirect(routes.Application.game).withSession(
          request.session + ("name" -> name(0))
        )
      } else {
        Redirect(routes.Application.index).withSession(
          request.session + ("name" -> name(0))
        )
      }
    }.getOrElse {
      Unauthorized("Oops, you are not connected")
    }
  }

  def game = Action {
    if (arrayOfGames(0) == null) {
    val gameController: ConcreteGameController = new ConcreteGameController(createID(), arrayOfPlayers(0), arrayOfPlayers(1))
    arrayOfGames(0) = gameController
  }
    Ok(views.html.game("Das Spiel kann beginnen!! \n "
      + "Spieler 1 : " + arrayOfPlayers(0).playerName
      + " Spieler 2 : " + arrayOfPlayers(1).playerName
      + " Game ID: " + arrayOfGames(0).id))
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