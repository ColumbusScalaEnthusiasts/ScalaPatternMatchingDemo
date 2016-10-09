package pmatch

/**
  * Created by dnwiebe on 9/27/16.
  */
object A_Grades {
  def fromGradeWithSwitch (grade: String): Int = {
    val basePoints = grade.charAt (0) match {
      case 'A' => 95
      case 'B' => 85
      case 'C' => 75
      case 'D' => 65
      case 'F' => 55
      case g => throw new IllegalArgumentException (s"Not familiar with grade '$g'")
    }
    basePoints + modifierPoints (grade)
  }

  val BASE_GRADES = Map ('A' -> 95, 'B' -> 85, 'C' -> 75, 'D' -> 65, 'F' -> 55)
  def fromGradeDataDriven (grade: String): Int = {
    val basePoints = BASE_GRADES.get (grade.charAt (0)) match {
      case Some (x) => x
      case None => throw new IllegalArgumentException (s"Not familiar with grade '${grade.charAt (0)}'")
    }
    basePoints + modifierPoints (grade)
  }

  private def modifierPoints (grade: String): Int = {
    grade match {
      case g if g.length < 2 => 0
      case g if g.charAt (1) == '-' => -3
      case g if g.charAt (1) == '+' => 3
      case g => throw new IllegalArgumentException (s"Don't know what grade modifier '${g.charAt(1)}' means")
    }
  }

  ///////////////////////

  def toGrade (points: Int): String = {
    points match {
      case p if (p < 0) || (p > 100) => throw new IllegalArgumentException (s"Scores must be between 0 and 100, not $p")
      case 100 => "A+"
      case p if p < 60 => "F"
      case p => {
        val baseGrade = ('A' + (9 - (p / 10))).toChar
        val flag = modifierFlag (points)
        s"$baseGrade$flag"
      }
    }
  }

  private def modifierFlag (points: Int): String = {
    points % 10 match {
      case p if p >= 7 => "+"
      case p if p >= 3 => ""
      case p => "-"
    }
  }
}
