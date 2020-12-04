package advent.Y2020

import scala.util.Try

object Day4:
  
  def isNumWith(f: Int => Boolean)(s: String): Boolean =
    val n = Try(s.toInt).map(f)
    n.getOrElse(false)
  
  val hgtParser = "([0-9]+)(cm|in)".r
  val hclParser = "#[0-9a-f]{6}".r
  val eclParser = "(amb|blu|brn|gry|grn|hzl|oth)".r
  val pidParser = "[0-9]{9}".r
  
  val passportEntries = Map[String, String => Boolean] (
    "byr" -> isNumWith(f => (f >= 1920) && (f <= 2002)),
    "iyr" -> isNumWith(f => (f >= 2010) && (f <= 2020)),
    "eyr" -> isNumWith(f => (f >= 2020) && (f <= 2030)),
    "hgt" -> { (s: String) =>
      s match {
        case hgtParser(n, "cm") => isNumWith(g => ((g >= 150) && (g <= 193)))(n)
        case hgtParser(n, "in") => isNumWith(g => ((g >= 59) && (g <= 76)))(n)
        case _ => false
      }
    },
    "hcl" -> hclParser.matches,
    "ecl" -> eclParser.matches,
    "pid" -> pidParser.matches,
    "cid" -> { (_: String) => true }
  )
  
  case class Passport(entries: Map[String,String]):
    val hasAllFields = (passportEntries.keySet - "cid").forall(entries.contains(_))
    val isValid = (hasAllFields) && (entries.forall{(k,v) => passportEntries(k)(v)})
  
  object Passport:
    val fieldReader = "([a-z]{3}):(.+)".r
    def apply(data: String): Passport =
      Passport(data.split("[\n, ]").map{ _ match { case fieldReader(field,value) => field -> value } }.toMap)
