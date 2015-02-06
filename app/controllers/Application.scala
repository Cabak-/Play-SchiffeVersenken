package controllers

import java.lang.ProcessBuilder.Redirect
import models._
import play.api._
import play.api.mvc._
import scala.slick.driver.H2Driver.simple._

object Application extends Controller {

  // The query interface for the Game table
  val games: TableQuery[SQLGame] = TableQuery[SQLGame]

  // the query interface for the Player table
  val players: TableQuery[SQLPlayer] = TableQuery[SQLPlayer]

  // Create a connection (called a "session") to an in-memory H2 database
  val db = Database.forURL("jdbc:h2:mem:hello", driver = "org.h2.Driver")
  db.withSession { implicit session =>
    // Create the schema by combining the DDLs for the Games and Players
    // tables using the query interfaces
    (games.ddl ++ players.ddl).create
  }

  val arrayOfGames : Array[GameController] = new Array[GameController](1)

  def index = Action {
    Ok(views.html.index("Welcome!")).withSession(
      "uuid" -> "default"
    )
    //Ok(views.html.index("Wollen Sie Spielen?!"))
  }

  def authenticate = Action { request =>
    val session = request.session
    val name = request.body.asFormUrlEncoded.get("name")
    /* Create / Insert */

    // Insert some suppliers
    players += (name(0))

    if(waitFor2ndPlayer){
      Redirect(routes.Application.game)
    }else{
      Redirect(routes.Application.index)
    }
  }

  def game = Action { request =>
    request.session.get("id").map { id =>
      Ok("Hello " + id)
      Ok(views.html.game("Das Spiel Beginnt !" +id))


      //val game = new ConsoleGameController(,player2,areaSize,boatCount)
    }.getOrElse {
      Unauthorized("Oops, you are not connected")
    }



  }

  def waitFor2ndPlayer: Boolean = {
    for(i <- 0 until 10){
      Thread.sleep(1000)
      // Query the Coffees table using a foreach and print each row
      val filterQuery: Query[SQLPlayer, (Int, String), Seq] =
        players.filter(_.count > 1)
      println("Generated SQL for filter query:\n" + filterQuery.selectStatement)
      if(filterQuery.list.isEmpty){
        return true;
      }
    }
    return false;
  }

  def endGame = Action {
    Redirect(routes.Application.index)
  }
}