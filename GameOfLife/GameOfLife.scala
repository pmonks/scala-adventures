/*
 * Copyright (c) 2010-2012 Peter Monks (pmonks@gmail.com)
 *
 * This work is licensed under the Creative Commons Attribution-ShareAlike
 * 3.0 Unported License. To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-sa/3.0/ or send a letter to
 * Creative Commons, 444 Castro Street, Suite 900, Mountain View, California,
 * 94041, USA.
 */

type Cell = (Int, Int)

abstract class GameOfLifeBoard(val aliveCells : Set[Cell])
{
  /** Executes a "time tick" - returns a new board containing the next generation */
  def tick : GameOfLifeBoard

  /** Is the board empty? */
  def empty : Boolean = aliveCells.size == 0

  /** Is the given cell alive? */
  protected def alive(cell : Cell) : Boolean = aliveCells contains cell

  /** Is the given cell dead? */
  protected def dead(cell : Cell) : Boolean = !alive(cell)

}


class InfiniteGameOfLifeBoard(aliveCells : Set[Cell])
  extends GameOfLifeBoard(aliveCells)
{
  override def tick : GameOfLifeBoard = new InfiniteGameOfLifeBoard(nextGeneration)

  /** The next generation of this board */
  protected def nextGeneration : Set[Cell] = aliveCells flatMap neighbours filter shouldCellLiveInNextGeneration

  /** Should the given cell should live in the next generation? */
  protected def shouldCellLiveInNextGeneration(cell : Cell) : Boolean = (alive(cell) && (numberOfAliveNeighbours(cell) == 2 || numberOfAliveNeighbours(cell) == 3)) ||
                                                                        (dead(cell)  && numberOfAliveNeighbours(cell) == 3)

  /** The number of alive neighbours for the given cell */
  protected def numberOfAliveNeighbours(cell : Cell) : Int = aliveNeighbours(cell) size

  /** The alive neighbours for the given cell */
  protected def aliveNeighbours(cell : Cell) : Set[Cell] = aliveCells intersect neighbours(cell)

  /** The coordinates of all of the neighbouring cells of the given cell */
  protected def neighbours(cell : Cell) : Set[Cell] = Set((cell._1-1, cell._2-1), (cell._1, cell._2-1), (cell._1+1, cell._2-1),
                                                          (cell._1-1, cell._2),                         (cell._1+1, cell._2),
                                                          (cell._1-1, cell._2+1), (cell._1, cell._2+1), (cell._1+1, cell._2+1))

  /** Information on where the currently live cells are */
  protected def xVals  = aliveCells map { cell => cell._1 }
  protected def xMin   = (xVals reduceLeft (_ min _)) - 1
  protected def xMax   = (xVals reduceLeft (_ max _)) + 1
  protected def xRange = xMin until xMax + 1
  protected def yVals  = aliveCells map { cell => cell._2 }
  protected def yMin   = (yVals reduceLeft (_ min _)) - 1
  protected def yMax   = (yVals reduceLeft (_ max _)) + 1
  protected def yRange = yMin until yMax + 1


  /** A simple graphical representation of this board */
  override def toString : String =
  {
    yRange.map(y => {
      xRange.map(x => if (alive(x,y)) "#" else ".").mkString(" ")
    }).mkString("\n")
  }

  // Equality stuff
  override def equals(other : Any) : Boolean =
  {
    other match
    {
      case that : InfiniteGameOfLifeBoard => (that canEqual this) &&
                                             that.aliveCells == this.aliveCells
      case _ => false
    }
  }
  
  def canEqual(other : Any) : Boolean = other.isInstanceOf[InfiniteGameOfLifeBoard]
  
  override def hashCode = aliveCells.hashCode

}


class FiniteGameOfLifeBoard(val boardWidth : Int, val boardHeight : Int, aliveCells : Set[Cell])
  extends InfiniteGameOfLifeBoard(aliveCells)
{
  override def tick : GameOfLifeBoard = new FiniteGameOfLifeBoard(boardWidth, boardHeight, nextGeneration)
    
  /** The coordinates of all of the neighbouring cells of the given cell */
  override protected def neighbours(cell : Cell) : Set[Cell] = super.neighbours(cell) filter { cell => cell._1 >= 0 && cell._1 < boardWidth &&
                                                                                               cell._2 >= 0 && cell._2 < boardHeight }

  /** Information on where the currently live cells are */
  override protected def xRange = 0 until boardWidth
  override protected def yRange = 0 until boardHeight

  // Equality stuff
  override def equals(other : Any) : Boolean =
  {
    other match
    {
      case that : FiniteGameOfLifeBoard => (that canEqual this) &&
                                           that.boardWidth  == this.boardWidth &&
                                           that.boardHeight == this.boardHeight &&
                                           that.aliveCells  == this.aliveCells
      case _ => false
    }
  }
  
  override def canEqual(other : Any) : Boolean = other.isInstanceOf[FiniteGameOfLifeBoard]
  
  override def hashCode : Int =
  {
    41 * (
      41 * (
        41 + super.hashCode 
      ) + boardHeight.hashCode
    ) + boardWidth.hashCode
  }

}


class GameOfLife(initialBoard: GameOfLifeBoard)
{
  /**
   * Run the game of life until the board is empty or the exact same board is seen twice
   * Important note: this method does NOT necessarily terminate!!
   */
  def go : Unit =
  {
    var currentBoard   = initialBoard
    var previousBoards = List[GameOfLifeBoard]()

    while (!currentBoard.empty && !(previousBoards contains currentBoard))
    {
      print(27.toChar + "[2J")  // ANSI: clear screen
      print(27.toChar + "[;H")  // ANSI: move cursor to top left corner of screen
      println(currentBoard.toString)
      Thread.sleep(75)

      // Warning: unbounded list concatenation can result in OutOfMemoryExceptions  ####TODO: replace with LRU bounded list
      previousBoards = List(currentBoard) ::: previousBoards
      currentBoard   = currentBoard tick
    }

    // Print the final board
    print(27.toChar + "[2J")  // ANSI: clear screen
    print(27.toChar + "[;H")  // ANSI: move cursor to top left corner of screen
    println(currentBoard.toString)
  }
}



/** Script starts here */
val simple = Set((1,1))
val square = Set((4,4), (4,5), (5,4), (5,5))
val glider = Set((2,1), (3,2), (1,3), (2,3), (3,3))

val initialBoard = glider

(new GameOfLife(new FiniteGameOfLifeBoard(20, 20, initialBoard))).go
//(new GameOfLife(new InfiniteGameOfLifeBoard(initialBoard))).go
