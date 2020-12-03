package advent.Y2019

import scala.annotation.tailrec

object Day2:

  type State = Vector[Int]

  @tailrec
  def compute(state: State, step: Int = 0): State =
    assert(step * 4 < state.size, "Program needed to terminate by now. Can't read next instructions.")
    val iPos = step * 4
    val xPos = iPos + 1
    val yPos = iPos + 2
    val resPos = iPos + 3

    val instruction = state(iPos)
    lazy val x = state(state(xPos))
    lazy val y = state(state(yPos))
    lazy val destination = state(resPos)

    instruction match
      case 1 => compute(state.updated(destination, x + y), step + 1)
      case 2 => compute(state.updated(destination, x * y), step + 1)
      case 99 => state

