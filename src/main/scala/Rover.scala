
case class Position( x: Int, y: Int, facing: Char )

case class GridSize(x: Int, y: Int)

class Rover {
  def moveForward(robot: Position, gridSize: GridSize) = {
    val changedPos = robot.facing match {
      case 'W' => robot.copy(x = robot.x - 1)
      case 'N' => robot.copy(y = robot.y + 1)
      case 'S' => robot.copy(y = robot.y - 1)
      case 'E' => robot.copy(x = robot.x + 1)
    }
    if (isValidMovt(changedPos.x, changedPos.y, gridSize)) changedPos
    else throw new Exception(s"Invalid movement $changedPos")
  }

  def changeOrientation(pos: Position, movt: Char) = {
    val leftOrientChange = Map('N' -> 'W', 'W' -> 'S', 'S' -> 'E', 'E' -> 'N')
    val rightOrientChange = Map('N' -> 'E', 'E' -> 'S', 'S' -> 'W', 'W' -> 'N')
    movt match {
      case 'L' => pos.copy(facing = leftOrientChange(pos.facing))
      case _ => pos.copy(facing = rightOrientChange(pos.facing))
    }
  }

  def adjustPositions(movements: List[Char], pos: Position, gridSize: GridSize) = {
    movements.foldLeft(pos)(
      (pos, movement) => movement match {
        case 'M' => moveForward(pos, gridSize)
        case 'L' | 'R' => changeOrientation(pos, movement)
        case _ => throw new Exception("Invalid orientation change")
      }
    )
  }


  def isValidMovt(x: Int, y: Int, gridSize: GridSize) = x >= 0 && x < gridSize.x && y >= 0 && y < gridSize.y

}