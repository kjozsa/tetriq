package org.fsdev.tetriq

// (x, y) pairs
case class Position(val x: Int, val y: Int) {
  override def toString = "|" + x + ", " + y + "|"
  def +(pos: Position) = new Position(x + pos.x, y + pos.y)
}

object Position {
  implicit def tuple2ToPosition(tuple: Tuple2[Int, Int]) = Position(tuple._1, tuple._2)
}

abstract sealed class Tile(val position: Position*)
case object I extends Tile((0, 0), (1, 0), (2, 0), (3, 0))
case object J extends Tile((0, 0), (1, 0), (2, 0), (2, 1))
case object L extends Tile((0, 0), (1, 0), (2, 0), (0, 1))
case object O extends Tile((0, 0), (1, 0), (0, 1), (1, 1))
case object S extends Tile((1, 0), (2, 0), (0, 1), (1, 1))
case object T extends Tile((0, 0), (1, 0), (2, 0), (1, 1))
case object Z extends Tile((0, 0), (1, 0), (1, 1), (2, 1))

// todo
case class Score()

// the moving block
class Piece(val form: Tile, start: Position) {
  var positions: Seq[Position] = form.position map (_ + start)

  override def toString = form.getClass().getSimpleName() + " at " + positions

  def tick { positions = positions map (_ + (0, 1)) }
  def moveLeft { positions = positions map (_ + (-1, 0)) }
  def moveRight { positions = positions map (_ + (1, 0)) }
}

// occupied positions + active block
class GameBoard(val blocks: Seq[Position], val current: Piece) {
  val (sizeX, sizeY) = (10, 8)
  override def toString() = "\n" + (
    for {
      y <- 0 to sizeY;
      x <- 0 to sizeX
      pos = Position(x, y)
    } yield if (view contains pos) "X" else "." + (if (x == sizeX) "\n" else "")).mkString

  def view = blocks ++ current.positions

  def add(newPiece: Piece): Either[Score, GameBoard] = {
    if (view intersect newPiece.positions isEmpty) {
      Right(new GameBoard(view ++ current.positions, newPiece))
    } else Left(new Score)
  }

  def tick: Either[Score, GameBoard] = {
    val newpos = current.positions map (_ + (0, 1))
    if (blocks intersect newpos isEmpty) {
      current.tick
      Right(this)
    } else {
      add(new Piece(O, (1, 0))) // TODO random
    }
  }
}

object Main extends App {
  var board = new GameBoard(Seq((2, 3), (3, 3), (4, 3), (5, 3), (6, 3)), new Piece(T, (5, 0)))
  println(board)

  var state = board.tick
  
//  for {
//	  i <- 1 to 5 
//    state <- state.right
//  } yield println(state.tick)
}
