package org.fsdev.tetriq

// (x, y) pairs
class Position(val x: Int, val y: Int) {
  override def toString = "(" + x + ", " + y + ")"
  def +(pos: Position) = new Position(x + pos.x, y + pos.y)
}

object Position {
  implicit def tuple2ToPosition(tuple: Tuple2[Int, Int]) = Position(tuple._1, tuple._2)
  def apply(x: Int, y: Int) = new Position(x, y)
}

// form of pieces
abstract sealed class Form(val position: Position*)
case object I extends Form((0, 0), (0, 1), (0, 2))
case object J extends Form((0, 0), (0, 1), (0, 2), (-1, 2))
case object L extends Form((0, 0), (0, 1), (0, 2), (1, 2))
case object Z extends Form((0, 0), (1, 0), (2, 0), (1, 1), (0, 2), (1, 2), (2, 2))
case object S extends Form((0, 0), (1, 0), (2, 0), (1, 1), (0, 2), (1, 2), (2, 2))
case object T extends Form((0, 0), (1, 0), (2, 0), (1, 1), (1, 2));
case object O extends Form((0, 0), (1, 0), (0, 1), (1, 1));

// the moving block
class Block(val form: Form, start: Position) {
  var positions: Seq[Position] = form.position map (_ + start)

  override def toString = form.getClass().getSimpleName() + " at " + positions

  def moveLeft { positions = positions map (_ + (-1, 0)) }
  def moveRight { positions = positions map (_ + (1, 0)) }
}

// occupied positions + active block
class GameBoard(val blocks: Seq[Position], val current: Block) {
  val (sizeX, sizeY) = (10, 20)
  def add(newBlock: Block) = new GameBoard(blocks ++ current.positions, newBlock)
}

object Main extends App {
  val board = new GameBoard(Seq((0, 1), (3, 3)), new Block(T, (1, 1)))
  board.blocks map println
  val board2 = board add new Block(T, (1, 1))
  board.blocks map println
}