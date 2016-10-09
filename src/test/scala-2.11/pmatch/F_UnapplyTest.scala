package pmatch

import java.util.{Calendar, Date, GregorianCalendar}

import org.scalatest.path
import pmatch.F_Unapply._

class F_UnapplyTest extends path.FunSpec {

  val inputAndExpectedOutput = List (
    (("Ford",      "Fiesta",    2011),         Some (false)),
    (("Ford",      "Mustang",   2011),         Some (true)),  // 2011 Mustang is recalled
    (("Ford",      "Fusion",    2014),         Some (false)),
    (("Chevrolet", "Sonic",     2002),         Some (true)),  // Sonics before 2009 are recalled
    (("Chevrolet", "Sonic",     2005),         Some (true)),  // Sonics before 2009 are recalled
    (("Chevrolet", "Sonic",     2008),         Some (true)),  // Sonics before 2009 are recalled
    (("Chevrolet", "Sonic",     2009),         Some (false)),
    (("Chevrolet", "Silverado", 2005),         Some (false)),
    (("Chrysler",  "Pacifica",  yearsAgo (0)), Some (false)),
    (("Chrysler",  "Pacifica",  yearsAgo (1)), Some (false)),
    (("Chrysler",  "Pacifica",  yearsAgo (2)), Some (true)),  // All Chryslers 2 or more years old are recalled
    (("Chrysler",  "Ypsilon",   yearsAgo (2)), Some (true)),  // All Chryslers 2 or more years old are recalled
    (("Bugatti",   "Veyron",    2005),         None)          // Unknown make
  )

  inputAndExpectedOutput.foreach {pair =>
    val ((make, model, year), expectedResult) = pair

    checkIsRecalled (new CarNonCase (make, model, year), isRecalledNoMatching, "nested ifs", expectedResult)
    checkIsRecalled (new CarNonCase (make, model, year), isRecalledNoMatchingNonCase, "unapply", expectedResult)
    checkIsRecalled (new CarNonCase (make, model, year), isRecalledWithMatchingNonCase, "matching", expectedResult)

    checkIsRecalled (CarCase (make, model, year), isRecalledNoMatching, "nested ifs", expectedResult)
    checkIsRecalled (CarCase (make, model, year), isRecalledWithMatchingCase, "matching", expectedResult)
  }

  private def checkIsRecalled (subject: Car, f: Car => Option[Boolean], matchingClause: String,
                               expectedResult: Option[Boolean]): Unit = {
    val className = subject.getClass.getSimpleName

    describe (s"$className with $matchingClause: Is a ${subject.year} ${subject.make} ${subject.model} recalled?") {
      val result = f (subject)

      it (s"${resultString (expectedResult)}") {
        assert (result === expectedResult)
      }
    }
  }

  private def resultString (result: Option[Boolean]): String = {
    result match {
      case Some (true) => "Yes. Yes it is."
      case Some (false) => "No."
      case None => "Can't say. Never heard of that car."
    }
  }

  /////////////////////////////

  describe ("When asked whether cars are Ford sports cars") {
    val result = List (
      new CarNonCase ("Ford", "Mustang", 1976),
      new CarNonCase ("Ford", "Galaxie", 1980),
      new CarNonCase ("Chevrolet", "Chevette", 1984)
    ).map {isFordSportsCar (_)}

    it ("responds properly") {
      assert (result === List (
        Some (true),
        Some (false),
        None
      ))
    }
  }

  /////////////////////////////

  describe ("When asked whether cars are historic") {
    val result = List (
      new CarNonCase ("Ford", "Mustang", 1976),
      new CarNonCase ("Ford", "Mustang", 2011)
    ).map {isHistoricCar (_)}

    it ("responds properly") {
      assert (result === List (
        true,
        false
      ))
    }
  }




  private def yearsAgo (years: Int): Int = {
    val calendar = new GregorianCalendar ()
    calendar.setTime (new Date ())
    calendar.get (Calendar.YEAR) - years
  }
}
