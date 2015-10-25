case class Position(
                     x: Int, y: Int, facing: Char
                     )

class Movement {
  def moveForward(robot: Position, gridLimits: (Int, Int)) = {
    val changedPos=robot.facing match {
      case 'W' => robot.copy(robot.x - 1, robot.y, robot.facing)
      case 'N' => robot.copy(robot.x, robot.y + 1, robot.facing)
      case 'S' => robot.copy(robot.x, robot.y - 1, robot.facing)
      case 'E' => robot.copy(robot.x + 1, robot.y, robot.facing)
    }
    if(isValidMovt(changedPos.x,changedPos.y,gridLimits)) changedPos
    else throw new Exception(s"Invalid movement $changedPos")
  }

  def changeOrientation(pos: Position, movt: Char) = {
    val leftOrientChange = Map('N' -> 'W', 'W' -> 'S', 'S' -> 'E','E'->'N')
    val rightOrientChange = Map('N' -> 'E', 'E' -> 'S', 'S' -> 'W','W'->'N')
    movt match {
      case 'L' => pos.copy(facing = leftOrientChange(pos.facing))
      case _ => pos.copy(facing = rightOrientChange(pos.facing))
    }
  }

  def adjustPositions(movements: List[Char], pos: Position, gridLimits: (Int, Int)) = {
    movements.foldLeft(pos) (
      (pos,movement) => movement match {
        case 'M' => moveForward(pos, gridLimits)
        case 'L' | 'R' => changeOrientation(pos, movement)
        case _ => throw new Exception("Invalid orientation change")
      }
    )
  }


  def isValidMovt(x: Int, y: Int, gridLimit: (Int, Int)) =  x >= 0 && x < gridLimit._1 && y >= 0 && y < gridLimit._2

}
