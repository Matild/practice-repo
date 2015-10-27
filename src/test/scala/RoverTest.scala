import org.scalatest.{ShouldMatchers, FlatSpec}

class RoverTest extends FlatSpec with ShouldMatchers {
  val rover = new Rover()

  "MarsRover" should "land in correct position" in {
    val movements = "LMLMLMLMM".toList
    val initialPos=Position(1,2,'N')
    val finalPos=rover.adjustPositions(movements, initialPos, GridSize(5,5))
    finalPos.x should be (1)
    finalPos.y should be (3)
    finalPos.facing should be ('N')

  }

  "it" should "land in correct position" in {
    val movements = "MMRMMRMRRM".toList
    val initialPos=Position(3,3,'E')
    val finalPos=rover.adjustPositions(movements, initialPos, GridSize(5,5))
    finalPos.x should be (5)
    finalPos.y should be (1)
    finalPos.facing should be ('N')

  }
}
