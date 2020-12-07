package advent.Y2020

object Day7 {
  
  case class Color(adjective: String, hue: String) { self =>
    def canContain(target: Color)(implicit rules: Rules): Boolean = {
      assert(rules.contains(self), s"No rules found for color $self")
      val r = rules(self)
      if (r.contains(target)) {
        true
      } else if (r.isEmpty) {
        false
      } else {
        r.keySet.map(_ canContain target).contains(true)
      }
    }
    def requires(implicit rules: Rules): Int = {
      assert(rules.contains(self), s"No rules found for color $self")
      val r = rules(self)
      if (r.isEmpty) {
        0
      } else {
        r.values.sum + r.map(child => child._2 * child._1.requires).sum // the bags plus their (recursive) contents
      }
    }
  }

  type Rules = Map[Color, Map[Color, Int]]

  object Rule {
    val parentParser = """([a-z]+) ([a-z]+) bags contain (.+)""".r
    val rulesParser = """([0-9]+) ([a-z]+) ([a-z]+) bags?[,.]""".r
    def apply(s: String): (Color, Map[Color, Int]) = {
      s match {
        case parentParser(adj, hue, contents) => {
          Color(adj,hue) -> rulesParser.findAllMatchIn(contents).map(m => (Color(m.group(2),m.group(3)), m.group(1).toInt)).toMap
        }  
      }
    }
  }
}
