package coreGame

/** factory to create FieldStates */
object FieldStateFactory {

  /** clones a indexed sequence */
  def cloneSequence(old: IndexedSeq[Int]): IndexedSeq[Int] = {
    if (old.length == 0) IndexedSeq[Int]()
    else old.updated(0,old(0))
  }

  /** creates a BasicFieldState */
  def createBasic(fieldSize:Int,boatCount:IndexedSeq[Int],totalBoatsCount:Int): BasicFieldState = {
    val boatsLeft: IndexedSeq[Int] = cloneSequence(boatCount)
    new BasicFieldState(fieldSize,boatCount,totalBoatsCount,boatsLeft,IndexedSeq.fill(fieldSize*fieldSize)(0))
  }

  /** creates a FieldState */
  def create(fieldSize:Int,boatCount:IndexedSeq[Int],totalBoatsCount:Int): FieldState = {
    val boatsLeft: IndexedSeq[Int] = cloneSequence(boatCount)
    val boatLife: Vector[Int] = Vector[Int]()
    val gameArea: StaticGameArea = StaticGameAreaFactory.create(fieldSize,totalBoatsCount)
    val shotResult: Int = -1
    new FieldState(fieldSize,boatCount,totalBoatsCount,boatsLeft,IndexedSeq.fill(fieldSize*fieldSize)(0),boatLife,gameArea,shotResult)
  }

}
