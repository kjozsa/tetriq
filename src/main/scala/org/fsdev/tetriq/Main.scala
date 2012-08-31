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
class Block(val tileForm: Tile, start: Position) {
  var positions: Seq[Position] = tileForm.position map (_ + start)

  override def toString = tileForm.getClass().getSimpleName() + " at " + positions

  def tick { positions = positions map (_ + (0, 1)) }
  def moveLeft { positions = positions map (_ + (-1, 0)) }
  def moveRight { positions = positions map (_ + (1, 0)) }
}

// occupied positions + active block
class GameBoard(val blocks: Seq[Position], val current: Block) {
  val (sizeX, sizeY) = (10, 8)
  override def toString() = (
    for {
      y <- 0 to sizeY;
      x <- 0 to sizeX
      pos = Position(x, y)
    } yield if (view.contains(pos)) "X" else "." + (if (x == sizeX) "\n" else "")).mkString

  def view = blocks ++ current.positions

  def add(newBlock: Block): Either[Score, GameBoard] = {
    if (view intersect newBlock.positions isEmpty) {
      Right(new GameBoard(view, newBlock))
    } else Left(new Score)
  }
}

object Main extends App {
  val board = new GameBoard(Seq(), new Block(O, (5, 0)))
  println(board)

  board.current.tick
  println(board)

  board.current.moveLeft
  println(board)

  board.current.tick
  println(board)

  board.current.moveLeft
  println(board)
}
