package gameCore

import collection.immutable.Vector._

/** factory to create GameAreas */
object GameAreaFactory {

  def create(fieldSize:Int,totalBoatsCount:Int): GameArea = {
    val boatVector: Vector[Boat] = Vector[Boat]()
    new GameArea(fieldSize,totalBoatsCount,boatVector,IndexedSeq.fill(fieldSize*fieldSize)(0))
  }

}
