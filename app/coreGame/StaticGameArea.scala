package coreGame

/** immutable class to manage the positioning of boats */
class StaticGameArea(val fieldSize: Int, val totalBoatCount: Int, val boatObjects: IndexedSeq[Boat], val cells: IndexedSeq[Int]) {

  /** checks if a cell contains a part of a boat, returns the respective boat id */
  def checkCell(x: Int, y: Int): Int = {
    return cells(y*fieldSize+x)
  }

  /** places a boat by creating a new sequence recursively */
  def createAlteredSequence(old: IndexedSeq[Int],start1:Int,end1:Int,coo2:Int,id:Int,horizontal:Boolean): IndexedSeq[Int] = {
    if (horizontal) {
      if (start1 >= end1) {
        return old.updated(coo2 * fieldSize + start1, id)
      } else {
        return createAlteredSequence(old.updated(coo2 * fieldSize + start1, id), start1 + 1, end1, coo2, id, horizontal)
      }
    } else {
      if (start1 >= end1) {
        return old.updated(start1 * fieldSize + coo2, id)
      } else {
        return createAlteredSequence(old.updated(start1 * fieldSize + coo2, id), start1 + 1, end1, coo2, id, horizontal)
      }
    }
  }

  /* places a boat in the GameArea by receiving a boat object and creating a new game area */
  @throws[BoatOverlapException]("The placement of your boat was unsuccessful!")
  @throws[BoatOverhangException]("The placement of your boat was unsuccessful!")
  def placeBoat(boat: Boat): StaticGameArea = {
    val endX: Int = boat.getEndX
    val endY: Int = boat.getEndY
    println ("StartX: " + boat.startX + " EndX: "+endX +" StartY: "+boat.startY +" EndY: " + endY)
    if (checkForOverhang(boat)) {
      if(checkForOverlap(boat)){
        if (boat.startX != endX) { // horizontal placement
          return new StaticGameArea(fieldSize,totalBoatCount,boatObjects:+(boat),createAlteredSequence(cells,boat.startX,endX,boat.startY,boat.id,true))
        } else {
          return new StaticGameArea(fieldSize,totalBoatCount,boatObjects:+(boat),createAlteredSequence(cells,boat.startY,endY,boat.startX,boat.id,false))
        }
      } else {
        throw new BoatOverlapException
      }
    } else {
      throw new BoatOverhangException
    }
    // error: return the unchanged GameArea
    return this
  }

  /* checks if a boat can be placed without extending beyond the GameArea */
  def checkForOverhang(boat: Boat): Boolean = {
    val endX: Int = boat.getEndX
    val endY: Int = boat.getEndY
    return (boat.startX >= 0 && boat.startY >= 0 && endX < fieldSize && endY < fieldSize)
  }

  /* checks if a boat can be placed in the GameArea without being too close to another boat */
  def checkForOverlap(boat: Boat): Boolean = {
    def minOrMax(c: Int): Int = {
      if (c < 0) return 0
      else if (c > fieldSize-1) return fieldSize - 1
      else return c
    };
    val startX: Int = minOrMax(boat.startX - 1)
    val startY: Int = minOrMax(boat.startY - 1)
    val endX: Int = minOrMax(boat.getEndX + 1)
    val endY: Int = minOrMax(boat.getEndY + 1)
    for(x <- startX to endX){
      for(y <- startY to endY){
        if(checkCell(x,y) != 0) return false
      }
    }
    return true
  }

  /* outputs the GameArea on the console */
  def show{
    println("------------------------")
    for (y <- 0 until cells.length) {
      for (x <- 0 until cells.length) {
        print(" " + checkCell(x,y))
      }
      println()
    }
    println("------------------------")
  }

}