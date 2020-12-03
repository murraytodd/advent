package advent.Y2019

import scala.annotation.tailrec
import scala.util.Using

object Day2 {

  type State = Vector[Int]

  @tailrec
  def compute(step: Int, state: State): State = {
    // println(s"Computing step $step")
    // println(s"State is $state")
    assert(step * 4 < state.size, "Program needed to terminate by now. Can't read next instructions.")
    val iPos = step * 4
    val xPos = iPos + 1
    val yPos = iPos + 2
    val resPos = iPos + 3

    val instruction = state(iPos)
    lazy val x = state(state(xPos))
    lazy val y = state(state(yPos))
    lazy val destination = state(resPos)
    // if (instruction != 99) println(s"$instruction -> $x and $y to $destination")

    state(iPos) match {
      case 1 => compute(step + 1, state.updated(destination, x + y))
      case 2 => compute(step + 1, state.updated(destination, x * y))
      case 99 => state
    }
  }

}
