package controllers

import java.lang.ProcessBuilder.Redirect
import models._
import play.api._
import play.api.data.Form
import play.api.mvc._
import scala.util.control.Breaks._
import play.api.cache.Cache
import play.api.Play.current
import play.api.data._
import play.api.data.Forms._


object Application extends Controller {

  val arrayOfGames: Array[GameController] = new Array[GameController](1)
  val arrayOfPlayers: Array[RemotePlayer] = new Array[RemotePlayer](2)

  /** show the index page */
  def index = Action {
      Ok(views.html.index(""))
  }

  /** process the authentification */
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

  /** defines the shoot action */
  case class ShootAction(player: String, x: Int, y: Int)

  /** form for the shoot action */
  val shootActionForm = Form(
    mapping(
      "player" -> text,
      "x" -> number,
      "y" -> number
    )(ShootAction.apply)(ShootAction.unapply)
  )

  /** defines the boat placement action */
  case class BoatPlacementAction(player: String, x: Int, y: Int, orientation: String)

  /** form for the boat placement action */
  val boatPlacementActionForm = Form(
    mapping(
      "player" -> text,
      "x" -> number,
      "y" -> number,
      "orientation" -> text
    )(BoatPlacementAction.apply)(BoatPlacementAction.unapply)
  )

  /** action to shoot at a cell */
  def shoot = Action { implicit request =>
    shootActionForm.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.game("Fehlerhafte Eingabe",arrayOfGames(0)))
      },
      actionData => {
        // binding success
        println("SHOT:\n" + actionData.player + "\n" + actionData.x + " / " + actionData.y + "\n--------------")
        /*val newUser = models.User(userData.name, userData.age)
        val id = models.User.create(newUser)
        Redirect(routes.Application.home(id))*/
      }
    )

    Ok(views.html.game("Schuss abgefeuert!",arrayOfGames(0)))
  }

  /** action to place a boat */
  def placeBoat = Action { implicit request =>
    boatPlacementActionForm.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.game("Fehlerhafte Eingabe",arrayOfGames(0)))
      },
      actionData => {
        // binding success
        println("BOAT PLACEMENT:\n" + actionData.player + "\n" + actionData.x + " / " + actionData.y + " - " + actionData.orientation + "\n--------------")

        /*val newUser = models.User(userData.name, userData.age)
        val id = models.User.create(newUser)
        Redirect(routes.Application.home(id))*/
      }
    )

    Ok(views.html.game("Boot platziert!",arrayOfGames(0)))
  }

  /** show the main game view */
  def game = Action { implicit request =>
    val userID: String = request.session.get("id").getOrElse(null)

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

  /** wait for another player to join */
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

  /** creates a session ID */
  def createID() : String = {
    return java.util.UUID.randomUUID().toString()
  }

  /** finish the game */
  def endGame = Action {
    Redirect(routes.Application.index)
  }

}