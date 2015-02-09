package coreGame

import collection.immutable.Vector._

/** factory to create GameAreas */
object StaticGameAreaFactory {

  def create(fieldSize:Int,totalBoatsCount:Int): StaticGameArea = {
    val boatVector: Vector[Boat] = Vector[Boat]()
    new StaticGameArea(fieldSize,totalBoatsCount,boatVector,IndexedSeq.fill(fieldSize*fieldSize)(0))
  }

}
