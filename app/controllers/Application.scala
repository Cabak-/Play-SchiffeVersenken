package controllers

import java.lang.ProcessBuilder.Redirect
import models._
import play.api._
import play.api.mvc._


object Application extends Controller {

  val arrayOfGames: Array[GameController] = new Array[GameController](1)

  def index = Action { implicit request =>
    Ok(views.html.index("Welcome!")).withSession(
      "uuid" -> "default"
    )
    //Ok(views.html.index("Wollen Sie Spielen?!"))
  }

  def authenticate = Action { request =>
    val name = request.body.asFormUrlEncoded.get("name") // Insert a player
    if (waitFor2ndPlayer) {
      Redirect(routes.Application.game)
    } else {
      Redirect(routes.Application.index)
    }
  }

  def game = Action {
    Ok(views.html.game("Das Spiel kann beginnen!!"))
  }

  def waitFor2ndPlayer: Boolean = {
  for (i <- 0 until 10) {
    Thread.sleep(1000)
    if (true) {
      return true;
    }
  }
  return false;
  }

  def endGame = Action {
    Redirect(routes.Application.index)
  }

}