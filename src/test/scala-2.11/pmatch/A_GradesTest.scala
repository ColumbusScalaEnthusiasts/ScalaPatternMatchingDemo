package pmatch

import org.scalatest.path
import pmatch.utils.TestUtils._

/**
  * Created by dnwiebe on 9/27/16.
  */
class A_GradesTest extends path.FunSpec {
  import pmatch.A_Grades._

  checkGradesToPoints ("fromGradeWithSwitch", fromGradeWithSwitch)
  checkGradesToPoints ("fromGradeDataDriven", fromGradeDataDriven)

  private def checkGradesToPoints (name: String, f: String => Int) {
    describe (s"When converting letter grades to points via $name") {
      List (
        ("A+", 98),
        ("A",  95),
        ("A-", 92),
        ("B+", 88),
        ("B",  85),
        ("B-", 82),
        ("C+", 78),
        ("C",  75),
        ("C-", 72),
        ("D+", 68),
        ("D",  65),
        ("D-", 62),
        ("F",  55)
      ).foreach {pair =>
        val (grade, points) = pair

        describe (s"a grade of '$grade'") {
          val result = f (grade)

          it (s"evaluates to $points points") {
            assert (result === points)
          }
        }
      }

      describe ("A grade with an unexpected modifier") {
        val result = capture {
          f ("A*")
        }

        it ("causes the correct exception") {
          assert (result.get.isInstanceOf [IllegalArgumentException])
          assert (result.get.getMessage === "Don't know what grade modifier '*' means")
        }
      }

      describe ("A grade with an unexpected letter") {
        val result = capture {
          f ("P+")
        }

        it ("causes the correct exception") {
          assert (result.get.isInstanceOf [IllegalArgumentException])
          assert (result.get.getMessage === "Not familiar with grade 'P'")
        }
      }
    }
  }

  ////////////////////////

  describe ("The BASE_GRADES map") {
    it ("contains the correct data") {
      assert (BASE_GRADES === Map ('A' -> 95, 'B' -> 85, 'C' -> 75, 'D' -> 65, 'F' -> 55))
    }
  }

  ////////////////////////

  describe ("When converting points to letter grades") {
    List (
      (0, 59, "F"),
      (60, 62, "D-"),
      (63, 66, "D"),
      (67, 69, "D+"),
      (70, 72, "C-"),
      (73, 76, "C"),
      (77, 79, "C+"),
      (80, 82, "B-"),
      (83, 86, "B"),
      (87, 89, "B+"),
      (90, 92, "A-"),
      (93, 96, "A"),
      (97, 100, "A+")
    ).foreach { triple =>
      val (lo, hi, grade) = triple

      (lo to hi).foreach { points =>
        describe (s"A point score of $points") {
          val result = A_Grades.toGrade (points)

          it (s"evaluates to a grade of $grade") {
            assert (result === grade)
          }
        }
      }
    }

    List (-1, 101).foreach {points =>
      describe (s"An invalid score of $points") {
        val exception = capture {A_Grades.toGrade (points)}

        it ("triggers the expected exception") {
          assert (exception.get.getMessage === s"Scores must be between 0 and 100, not $points")
        }
      }
    }
  }
}
