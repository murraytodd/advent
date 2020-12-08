package advent.Y2020

import scala.annotation.tailrec

object Day8 {
  
  sealed trait Instruction
  
  case class Nop(ignore: Int) extends Instruction
  case class Jmp(steps: Int) extends Instruction
  case class Acc(value: Int) extends Instruction
  
  object Instruction {
    val parser = "(nop|acc|jmp) ([+-][0-9]+)".r
    def apply(s: String): Instruction = s match {
      case parser("nop",x) => Nop(x.toInt)
      case parser("jmp",x) => Jmp(x.toInt)
      case parser("acc",x) => Acc(x.toInt)
    }
  }
  
  type Program = IndexedSeq[Instruction]
  
  case class ComputeState(inst: Int, accum: Int) {
    def execute(i: Instruction): ComputeState = i match {
      case Nop(ignore) => ComputeState(inst + 1, accum)
      case Jmp(steps) => ComputeState(inst + steps, accum)
      case Acc(value) => ComputeState(inst + 1, accum + value)
    }
  }
  
  implicit class ProgramOps(p: Program) {
    @tailrec
    final def findRepeat(mems: Set[Int] = Set.empty, 
                         state: ComputeState = ComputeState(0,0)): Either[ComputeState, Int] = {
      if (mems.contains(state.inst)) {
        Left(state)
      } else if (state.inst == p.size) { // indicates proper termination 
        Right(state.accum)
      } else {
        findRepeat(mems + state.inst, state.execute(p(state.inst)))
      }
    }
    
    def withFix(step: Int): Program = {
      p(step) match {
        case Nop(ignore) => p.updated(step, Jmp(ignore))
        case Jmp(steps) => p.updated(step, Nop(steps))
        case Acc(value) => throw new Exception("Fix strategy doesn't include Acc steps.")
      }
    }
    
    def fix: Option[(Int,Int)] = {
      val candidates = p.zipWithIndex.filterNot(_._1.isInstanceOf[Acc]).map(_._2)
      candidates.map(i => (i, p.withFix(i).findRepeat()))
        .collectFirst { case (i, Right(accum)) => (i,accum) }
    }
  }
}
