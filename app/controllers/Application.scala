package controllers

import java.lang.ProcessBuilder.Redirect
import models._
import play.api._
import play.api.mvc._
import scala.util.control.Breaks._
import play.api.cache.Cache
import play.api.Play.current


object Application extends Controller {

  val arrayOfGames: Array[GameController] = new Array[GameController](1)
  val arrayOfPlayers: Array[RemotePlayer] = new Array[RemotePlayer](2)

  def index = Action {
      Ok(views.html.index(""))
  }

  def authenticate = Action { implicit request =>
    val name = request.body.asFormUrlEncoded.get("name") // Insert a player
      val player : RemotePlayer = new RemotePlayer(createID(), name(0))
      breakable{
        for (i <- 0 to arrayOfPlayers.length - 1) {
          if (arrayOfPlayers(i) == null) {
            arrayOfPlayers(i) = player
            Cache.set("player"+player.playerID, player)
            break
          }
        }
      }
    Ok(views.html.waiting()).withSession( "id" -> player.playerID)
  }

  def game = Action { implicit request =>
    val userID : String = request.session.get("id").getOrElse(null)
    if (arrayOfGames(0) == null) {
      val newGameController: ConcreteGameController = new ConcreteGameController(createID(), arrayOfPlayers(0), arrayOfPlayers(1))
      arrayOfGames(0) = newGameController
      Ok(views.html.game("Das Spiel kann beginnen!! \n "
        + "Spieler 1 : " + arrayOfGames(0).players(0).playerName
        + " Spieler 2 : " + arrayOfGames(0).players(1).playerName
        + " Game ID: " + arrayOfGames(0).id, arrayOfGames(0)))
    }else if(userID == arrayOfGames(0).players(0).playerID || userID == arrayOfGames(0).players(1).playerID){
      Ok(views.html.game("Das Spiel kann beginnen!! \n "
        + "Spieler 1 : " + arrayOfGames(0).players(0).playerName
        + " Spieler 2 : " + arrayOfGames(0).players(1).playerName
        + " Game ID: " + arrayOfGames(0).id, arrayOfGames(0)))
    }else{
      Ok(views.html.index("Leider sind alle game Instanzen bereits vergeben! Versuchen sie es später bitte noch einmal!"))
    }
  }

  def waiting = Action { implicit request =>
    var redirect : Boolean = false
    breakable{
      for( i <- 0 until 10){
        Thread.sleep(1000)
        if (arrayOfPlayers(1) != null) {
          redirect = true
          break
        }
      }
    }
    if(redirect){
      Redirect(routes.Application.game())
    }else{
    for(i <- 0 until arrayOfPlayers.length-1){
      arrayOfPlayers(i) = null
    }
    Ok(views.html.index("Leider hat sich kein Spieler Gefunden! BItte versuchen sie es später noch einmal!"))
    }
  }

  def createID() : String = {
    return java.util.UUID.randomUUID().toString()
  }

  def endGame = Action {
    Redirect(routes.Application.index)
  }

}