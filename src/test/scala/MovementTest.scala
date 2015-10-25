import org.scalatest.{ShouldMatchers, FlatSpec}

class MovementTest extends FlatSpec with ShouldMatchers {
  val movt = new Movement()

  "MarsRover" should "land in correct position" in {
    val movements = "LMLMLMLMM".toList
    val initialPos=Position(1,2,'N')
    val gridX=5
    val gridY=5
    val finalPos=movt.adjustPositions(movements, initialPos, (gridX, gridY))
    finalPos.x should be (1)
    finalPos.y should be (3)
    finalPos.facing should be ('N')

  }

  "it" should "land in correct position" in {
    val movements = "MMRMMRMRRM".toList
    val initialPos=Position(3,3,'E')
    val gridX=5
    val gridY=5
    val finalPos=movt.adjustPositions(movements, initialPos, (gridX, gridY))
    finalPos.x should be (5)
    finalPos.y should be (1)
    finalPos.facing should be ('N')

  }
}
