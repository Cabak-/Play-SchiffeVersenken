package controllers

import java.lang.ProcessBuilder.Redirect
import models._
import play.api._
import play.api.mvc._

object Application extends Controller {
  val arrayOfGames : Array[GameController] = new Array[GameController](1)
  var MapOfPlayers : Map[String , RemotePlayer] = Map()

  def index = Action {
    Ok(views.html.index("Welcome!")).withSession(
      "uuid" -> "default"
    )
    //Ok(views.html.index("Wollen Sie Spielen?!"))
  }

  def authenticate = Action { request =>
    val session = request.session
    val name = request.body.asFormUrlEncoded.get("name")
    // Generate a unique id
    val uuid : String = java.util.UUID.randomUUID().toString
    val player : RemotePlayer = new RemotePlayer(name(0))
    MapOfPlayers = MapOfPlayers + (uuid -> player)
    if(waitFor2ndPlayer){
      Redirect(routes.Application.game)with
    }else{
      Redirect(routes.Application.index)
    }
  }

  def game = Action { request =>
    request.session.get("id").map { id =>
      Ok("Hello " + id)
      Ok(views.html.game("Das Spiel Beginnt !" +id))


      val game = new ConsoleGameController(,player2,areaSize,boatCount)
    }.getOrElse {
      Unauthorized("Oops, you are not connected")
    }



  }

  def waitFor2ndPlayer: Boolean = {
    for(i <- 0 until 10){
      Thread.sleep(1000)
      if(mapOfPlayers.length >= 2){
        return true;
      }
    }
    return false;
  }

  def endGame = Action {
    Redirect(routes.Application.index)
  }
}