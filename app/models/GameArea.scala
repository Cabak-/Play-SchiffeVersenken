package models
/** class to manage the positioning of boats */
class GameArea(val fieldSize: Int, val totalBoatCount: Int) {

	/** array of all boat objects that have been placed in the GameArea */
	val boatObjects = new Array[Boat](totalBoatCount)

	/** array with information about the cells (integer values are 0 for water or the id of a boat) */
	val cells = Array.ofDim[Int](fieldSize,fieldSize)

	/** places a boat in the GameArea by receiving a boat object */
	@throws[BoatOverlapException]("The placement of your boat was unsuccessful!")
	@throws[BoatOverhangException]("The placement of your boat was unsuccessful!")
	def placeBoat(boat: Boat) = {
	  val endX: Int = boat.getEndX
	  val endY: Int = boat.getEndY
	  println ("StartX: " + boat.startX + " EndX: "+endX +" StartY: "+boat.startY +" EndY: " + endY)
	  if (checkForOverhang(boat)) {
		  if(checkForOverlap(boat)){
			  for( x <- boat.startX to endX)
			    for( y <- boat.startY to endY)
				  cells(x)(y) = boat.id
				boatObjects(boat.id-1) = boat
		  } else {
		    throw new BoatOverlapException
		  }
	  } else {
	  	throw new BoatOverhangException
	  }
	}
	
	/** checks if a boat can be placed without extending beyond the GameArea */
	def checkForOverhang(boat: Boat): Boolean = {
		val endX: Int = boat.getEndX
		val endY: Int = boat.getEndY
		return (boat.startX >= 0 && boat.startY >= 0 && endX < fieldSize && endY < fieldSize)
	}
	
	/** checks if a boat can be placed in the GameArea without being too close to another boat */
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
		   if(cells(x)(y) != 0) return false
		 }
	  }
	  return true
	}

	/** checks if a cell contains a part of a boat, returns the respective boat id */
	def checkCell(x: Int, y: Int): Int = cells(x)(y)
	
	/** outputs the GameArea on the console */
	def show{
		println("------------------------")
		for (y <- 0 until cells.length) {
			for (x <- 0 until cells.length) {
				print(" " + cells(x)(y))
			}
			println()
		}
		println("------------------------")
	}
	
}