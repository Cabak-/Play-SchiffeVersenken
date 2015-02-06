package models

import scala.slick.driver.H2Driver.simple._
import scala.slick.lifted.{ProvenShape, ForeignKeyQuery}

// A Game class with 6 columns: id, fieldsize, progress
class SQLGame(tag: Tag)
  extends Table[(Int, String, Int)](tag, "GAMES") {

  // This is the primary key column:
  def gID: Column[Int] = column[Int]("GAMEID", O.PrimaryKey, O.AutoInc)
  def fieldsize: Column[String] = column[String]("FIELDSIZE")
  def progress: Column[Int] = column[Int]("PROGRESS")


  // Every table needs a * projection with the same type as the table's type parameter
  def * : ProvenShape[(Int, String, Int)] =
    (gID, fieldsize, progress)
}

// A Coffees table with 5 columns: name, supplier id, price, sales, total
class SQLPlayer(tag: Tag)
  extends Table[(Int, String)](tag, "PLAYERS") {

  def pID: Column[Int] = column[Int]("PLAYERID", o.PrimaryKey, O.AutoInc)
  def name: Column[String] = column[String]("PLAYER")

  def * : ProvenShape[(Int, String)] =
    (pID, name)

  // A reified foreign key relation that can be navigated to create a join
  def supplier: ForeignKeyQuery[Game, (Int, String, String)] =
    foreignKey("gFK", gID, TableQuery[Game])(_.id)
}
