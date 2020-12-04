package advent.Y2019

import scala.util.Try

object Day4:

  case class Password(pw: String, range: Range = 0 to 999999):
    val pwn = pw.toIntOption
    val chars = pw.toString.toCharArray
    def isSixDigitNumber = pwn.isDefined && (pw.size == 6)
    def inRange = pwn.isDefined && (pwn.get >= range.start) && (pwn.get <= range.end)
    def doubleExists = ! (0 to 4).forall(i => chars(i) != chars(i+1))
    def alwaysIncreasing = (0 to 4).forall(i => chars(i) <= chars(i+1))
    def isValid = isSixDigitNumber && inRange && doubleExists && alwaysIncreasing

    def newDoubleCriteria = chars.groupBy(identity).values.map(_.size).toSet.contains(2)
    def isValid2 = isSixDigitNumber && inRange && alwaysIncreasing && newDoubleCriteria

  object Password:
    def apply(x: Int): Password = Password(x.toString)
