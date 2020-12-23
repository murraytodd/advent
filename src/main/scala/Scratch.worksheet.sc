import advent.Y2020.Day11._

val part1State1 = Array("L.LL.LL.LL","LLLLLLL.LL","L.L.L..L..","LLLL.LL.LL","L.LL.LL.LL","L.LLLLL.LL",
                        "..L.L.....","LLLLLLLLLL","L.LLLLLL.L","L.LLLLL.LL")

val preSeating = Seating.fromString(part1State1)
preSeating.spaces(0)
preSeating.spaces(1)

val seating = preSeating.updateState

val part2Example = Array(".............",".L.L.#.#.#.#.",".............")

val p2seating = Seating.fromString(part2Example)
p2seating.spaces
p2seating.lineOfSight(1,1,0,1)
p2seating.lineOfSight(1,3,0,1)
p2seating.lineOfSight(1,3,1,0)
Seating.directions

val phase3 = preSeating.updateLineOfSightState
phase3.lineOfSightNeighbors(0,0)
val phase4 = phase3.updateLineOfSightState
phase4.lineOfSight(0,0,0,1)
phase4.lineOfSight(0,0,1,0)
phase4.lineOfSight(0,0,1,1)
phase4.lineOfSightNewState(0,0)
phase4.lineOfSightNewState(0,1)
phase4.lineOfSightNewState(0,2)
phase4.lineOfSightNewState(0,3)
