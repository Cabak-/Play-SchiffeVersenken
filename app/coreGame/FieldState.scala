package coreGame

/** immutable class to keep track of the non-static information about a game field,
  * i.e. which cells have been uncovered by shots */
class BasicFieldState(val areaSize: Int, val boatCount: IndexedSeq[Int], val totalBoatCount: Int,
                      val boatsLeft: IndexedSeq[Int], val visualCells: IndexedSeq[Int]) {

  /** gets the cell value at given coordinates */
  def getVisualCell(x:Int,y:Int): Int = {
    return visualCells(y*areaSize+x)
  }

	/** resets the field state */
	/*def reset: Unit = {
		// resets the cells
		for (x <- 0 until areaSize) {
			for (y <- 0 until areaSize)
				visualCells(x)(y) = 0
		}
		// resets the left boats
		for (i <- 0 until boatCount.size) boatsLeft(i) = boatCount(i)
	}*/

	/** uncovers a cell on the field in the game state */
	def uncoverCell(x: Int, y: Int, id: Int): BasicFieldState = {
		//visualCells(x)(y) = id
		if (id == -1) {
      return new BasicFieldState(areaSize,boatCount,totalBoatCount,boatsLeft,visualCells.updated(y*areaSize+x, -1))
			//visualCells(x)(y) = -1
		} else {
      return new BasicFieldState(areaSize,boatCount,totalBoatCount,boatsLeft,visualCells.updated(y*areaSize+x, 1))
			//visualCells(x)(y) = 1
		}
	}

	/** sinks a boat of a given length in the game state */
	/*def sinkBoat(l: Int): BasicFieldState = {
		if (boatsLeft(l) > 0) boatsLeft(l) = boatsLeft(l) - 1
	}*/

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
		println("------------------------")
		for (y <- 0 until visualCells.length) {
			for (x <- 0 until visualCells.length) {
				print(" " + getVisualCell(x,y))
			}
			println()
		}
		println("------------------------")
	}

}


/** extension of the field state class for states where the life
	* of each boat is also known and the instance of the program is also
	* able to tell when a boat was sunk */
class FieldState(areaSize: Int, boatCount: IndexedSeq[Int], totalBoatCount: Int,boatsLeft: IndexedSeq[Int],
                 visualCells: IndexedSeq[Int], val boatLife: IndexedSeq[Int], val gameArea: StaticGameArea, val shotResult: Int)
  extends BasicFieldState(areaSize,boatCount,totalBoatCount,boatsLeft,visualCells) {

	/** reset the boat life */
	/*def resetLife(): Unit = {
		for (i <- 0 until boatLife.size) {
			boatLife(i) = gameArea.boatObjects(i).length
		}
	}*/

  /** changes the game area to place a new boat */
  def placeBoat(b: Boat): FieldState = {
    println("fieldState placeBoat")
    return new FieldState(areaSize,boatCount,totalBoatCount,boatsLeft,visualCells,boatLife:+(b.length),gameArea.placeBoat(b),-1)
  }

	/** makes a change in the game state resulted by a shot at a cell,
		* returns 0 for water */
	def registerShot(x: Int, y: Int): FieldState = {
		if (getVisualCell(x,y) != 0) { // error: cell was already uncovered
			throw new ShotOnUncoveredCellException
		} else {
			// determine the id of the boat, if a boat was hit
			val id: Int = gameArea.checkCell(x,y)
			if (id > 0) { // boat was hit
				// mark the cell
				val newCells: IndexedSeq[Int] = visualCells.updated(y*areaSize+x, 1)

				// update the boat life
        val newBoatLife: IndexedSeq[Int] = boatLife.updated(id-1, boatLife(id-1) - 1)

        /*println("-----------------------------")
        for (i <- 0 until newBoatLife.length) {
          println("LIFE:" + i + " - " + newBoatLife(i))
        }
        println("new boat life of id #" + id + ": " + newBoatLife(id-1))*/

				// check if the boat has been sunk
        val newBoatsLeft: IndexedSeq[Int] = {
          if (newBoatLife(id - 1) > 0) boatsLeft
          else boatsLeft.updated(gameArea.boatObjects(id-1).length,boatsLeft(gameArea.boatObjects(id-1).length - 1))
        };

        /*for (i <- 0 until newBoatsLeft.length) {
          println("LEFT:" + i + " - " + newBoatsLeft(i))
        }*/

        // get the shot result
        val newShotResult: Int = {
          if (boatLife(id - 1) > 0) 1
          else 2
        };

        return new FieldState(areaSize,boatCount,totalBoatCount,newBoatsLeft,newCells,newBoatLife,gameArea,newShotResult)
			} else {
				// water was hit
        // mark the cell
        val newCells: IndexedSeq[Int] = visualCells.updated(y*areaSize+x, -1)
        val newShotResult: Int = 0
        return new FieldState(areaSize,boatCount,totalBoatCount,boatsLeft,newCells,boatLife,gameArea,newShotResult)

			}
		}
	}

}