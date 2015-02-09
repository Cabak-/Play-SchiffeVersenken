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
            //Cache.set("player"+player.playerID, player)
            break
          }
        }
      }
    Ok(views.html.searching()).withSession( "id" -> player.playerID)
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
  case class BoatPlacementAction(player: String, x: Int, y: Int, length: Int, orientation: String)

  /** form for the boat placement action */
  val boatPlacementActionForm = Form(
    mapping(
      "player" -> text,
      "x" -> number,
      "y" -> number,
      "boatLength" -> number,
      "orientation" -> text
    )(BoatPlacementAction.apply)(BoatPlacementAction.unapply)
  )

  /** action to shoot at a cell */
  def shoot = Action { implicit request =>
    var result : String = ""
    shootActionForm.bindFromRequest.fold(
      formWithErrors => {
        result = "Bei der Weitergabe der Daten ist ein Fehler aufgetreten!"
        // binding failure, you retrieve the form containing errors:
      },
      actionData => {
        //println("SHOT:\n" + actionData.player + "\n" + actionData.x + " / " + actionData.y + "\n--------------")
        val player = { if (actionData.player == arrayOfGames(0).players(1).playerID) arrayOfGames(0).players(1) else arrayOfGames(0).players(0) }
        arrayOfGames(0).proceedShot(player,actionData.x,actionData.y)
        result = player.asInstanceOf[RemotePlayer].view.lastMessage

      }
    )
      Ok(views.html.game(result,arrayOfGames(0)))
  }

  /** action to place a boat */
  def placeBoat = Action { implicit request =>
    var result : String = ""
    boatPlacementActionForm.bindFromRequest.fold(
      formWithErrors => {
        result = "Bei der Weitergabe der Daten ist ein Fehler aufgetreten!"
       // binding failure, you retrieve the form containing errors:
      },
      actionData => {
        //println("BOAT PLACEMENT:\n" + actionData.player + "\n" + actionData.x + " / " + actionData.y + " - " + actionData.length + " - " + actionData.orientation + "\n--------------")
        val player = { if (actionData.player == arrayOfGames(0).players(1).playerID) arrayOfGames(0).players(1) else arrayOfGames(0).players(0) }
        val horizontal = if(actionData.orientation == "v") false else true
        arrayOfGames(0).placeBoat(player, actionData.x, actionData.y, actionData.length, horizontal)
        if(arrayOfGames(0).gameState.isPlayersTurn(player)) {
          result = player.asInstanceOf[RemotePlayer].view.lastMessage
        }else{
          result = player.asInstanceOf[RemotePlayer].view.lastMessage+" Warte bitte auf den anderen Spieler!"
        }
      }
    )
      Ok(views.html.game(result, arrayOfGames(0)))
  }

  /** show the main game view */
  def game = Action { implicit request =>
    val userID: String = request.session.get("id").getOrElse(null)

    if (arrayOfGames(0) == null) {
      val newGameController: ConcreteGameController = new ConcreteGameController(createID(), arrayOfPlayers(0), arrayOfPlayers(1))
      arrayOfGames(0) = newGameController
      Ok(views.html.game("Das Spiel kann beginnen!! Du Bist am Zug! Bitte Platziere deine Boote!", arrayOfGames(0)))
    }else if(userID == arrayOfGames(0).players(0).playerID || userID == arrayOfGames(0).players(1).playerID){
      Ok(views.html.game("Das Spiel kann beginnen!! Warte bis "+arrayOfGames(0).players(0).playerName+" seine Boote platziert hat!", arrayOfGames(0)))
    }else{
      Ok(views.html.index("Leider sind alle game Instanzen bereits vergeben! Versuchen sie es später bitte noch einmal!"))
    }
  }

  /** searching for another player to join */
  def searching = Action { implicit request =>
    var redirect : Boolean = false
    breakable{
      for( i <- 0 to 10){
        Thread.sleep(990)
        if (arrayOfPlayers(1) != null) {
          redirect = true
          break
        }
      }
    }
    if(redirect){
      Redirect(routes.Application.game())
    }else{
      for(i <- 0 to arrayOfPlayers.length-1){
        arrayOfPlayers(i) = null
      }
      Ok(views.html.index("Leider hat sich kein Spieler Gefunden! BItte versuchen sie es später noch einmal!"))
    }
  }

  /** Waiting for PlayerMove */
  def waiting = Action { implicit request =>
    val userID: String = request.session.get("id").getOrElse(null)
    var redirect : Boolean = false
    breakable{
    for(i <- 0 to arrayOfPlayers.length-1){
      if(userID == arrayOfPlayers(i).playerID) {
          for( j <- 0 to 1000){
            Thread.sleep(1000)
            if(arrayOfGames(0) == null){
              break
            }else{
              if (arrayOfGames(0).gameState.isPlayersTurn(arrayOfPlayers(i))) {
                redirect = true
                break
              }
            }
          }
        }
      }
    }
    if(redirect){
      Ok(views.html.game("Du bist an der Reihe!!",arrayOfGames(0)))
    }else{
      Ok(views.html.index("Leider hat der andere Spieler aufgegeben!"))
    }
  }

  /** creates a session ID */
  def createID() : String = {
    return java.util.UUID.randomUUID().toString()
  }

  /** finish the game */
  def endGame = Action {
    for(i <- 0 to arrayOfPlayers.length-1){
      arrayOfPlayers(i) = null
    }
    for(i <- 0 to arrayOfGames.length-1){
      arrayOfGames(i) = null
    }
    Redirect(routes.Application.index)
  }

}