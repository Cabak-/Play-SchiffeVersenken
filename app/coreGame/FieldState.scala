package coreGame

/** class to keep track of the non-static information about a game field,
  * i.e. which cells have been uncovered by shots */
class BasicFieldState(val areaSize: Int, val boatCount: Array[Int], val totalBoatCount: Int) {

	/** keeps track of the cells that have already been uncovered by shots */
	val visualCells = Array.ofDim[Int](areaSize,areaSize)

	/* numbers of boats that have not been sunk yet */
	val boatsLeft: Array[Int] = new Array[Int](6)

	/** resets the field state */
	def reset: Unit = {
		// resets the cells
		for (x <- 0 until areaSize) {
			for (y <- 0 until areaSize)
				visualCells(x)(y) = 0
		}
		// resets the left boats
		for (i <- 0 until boatCount.size) boatsLeft(i) = boatCount(i)
	}

	/** uncovers a cell on the field in the game state */
	def uncoverCell(x: Int, y: Int, id: Int): Unit = {
		//visualCells(x)(y) = id
		if (id == -1) {
			visualCells(x)(y) = -1
		} else {
			visualCells(x)(y) = 1 // TODO - id statt 1 eintragen? gehen die Boot-IDs bei 0 oder 1 los?
		}
	}

	/** sinks a boat of a given length in the game state */
	def sinkBoat(l: Int): Unit = {
		if (boatsLeft(l) > 0) boatsLeft(l) = boatsLeft(l) - 1
	}

	/** checks if all boats have been sunk */
	def isFinished: Boolean = {
		for (i <- 1 until boatsLeft.size) {
      //println("BOATS OF LENGTH " + i + " LEFT: " + boatsLeft(i))
			if (boatsLeft(i) > 0) return false
		}
		return true
	}
	
	/** outputs the GameState on the console */
	def show{
	  /*for( y <- 0 until visualCells.length){
		println("---------------------------------------------------------------------------------")
	    for( x <- 0 until visualCells.length){
	      print("   " + visualCells(x)(y) +"\t|")
	    }
		println()
	  }
	  println("---------------------------------------------------------------------------------")*/
		println("------------------------")
		for (y <- 0 until visualCells.length) {
			for (x <- 0 until visualCells.length) {
				print(" " + visualCells(x)(y))
			}
			println()
		}
		println("------------------------")
	}

}


/** extension of the field state class for states where the life
	* of each boat is also known and the instance of the program is also
	* able to tell when a boat was sunk */
class FieldState(areaSize: Int, boatCount: Array[Int], totalBoatCount: Int, var gameArea: StaticGameArea) extends BasicFieldState(areaSize,boatCount,totalBoatCount) {

	/** create the game area */
	//val gameArea: GameArea = new GameArea(totalBoatCount)

	/** keeps track of the life of each boat (the key is the boat id - 1) */
	val boatLife = new Array[Int](totalBoatCount)

	/** reset the boat life */
	def resetLife(): Unit = {
    println(gameArea.boatObjects.size) // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		for (i <- 0 until boatLife.size) {
			boatLife(i) = gameArea.boatObjects(i).length
		}
	}

	/** makes a change in the game state resulted by a shot at a cell,
		* returns 0 for water */
	def registerShot(x: Int, y: Int): Int = {
		if (visualCells(x)(y) != 0) { // error: cell was already uncovered
			throw new ShotOnUncoveredCellException
		} else {
			// determine the id of the boat, if a boat was hit
			val id: Int = gameArea.checkCell(x,y)
			if (id > 0) { // boat was hit
				// mark the cell
				uncoverCell(x, y, id)
				// update the boat life
				boatLife(id - 1) = boatLife(id - 1) - 1

				// check if the boat has been sunk
				if (boatLife(id - 1) > 0) {
					return 1
				} else {
					// sink the boat
					sinkBoat(gameArea.boatObjects(id - 1).length)
					return 2
				}
			} else {
				// water was hit
				// mark the cell
				uncoverCell(x, y, -1)
				return 0
			}
		}
	}

}