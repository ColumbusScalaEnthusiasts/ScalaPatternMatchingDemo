package pmatch

import java.util.{Calendar, Date, GregorianCalendar}

/**
  * Created by dnwiebe on 10/4/16.
  */
object J_Unapply {

  trait Car {val make: String; val model: String; val year: Int}

  class CarNonCase (val make: String, val model: String, val year: Int) extends Car

  def isRecalledNoMatching (subject: Car): Option[Boolean] = {
    isRecalledThreeParams (subject.make, subject.model, subject.year)
  }

  private def isRecalledThreeParams (make: String, model: String, year: Int): Option[Boolean] = {
    if (make == "Ford") {
      if ((model == "Mustang") && (year == 2011)) {
        Some (true)
      }
      else {
        Some (false)
      }
    }
    else if (make == "Chevrolet") {
      if ((model == "Sonic") && (year <= 2008)) {
        Some (true)
      }
      else {
        Some (false)
      }
    }
    else if (make == "Chrysler") {
      if (yearsAgo (year) >= 2) {
        Some (true)
      }
      else {
        Some (false)
      }
    }
    else {
      None
    }
  }

  ////////////////////////

  object CarNonCase {
    // Normally type below would be CarNonCase, not Car
    def unapply (car: Car): Option[(String, String, Int)] = {
      Some ((car.make, car.model, car.year))
    }
  }

  def isRecalledNoMatchingNonCase (subject: Car): Option[Boolean] = {
    val tripleOpt = CarNonCase.unapply (subject.asInstanceOf [CarNonCase])
    if (tripleOpt.isEmpty) throw new UnsupportedOperationException ("Never happen")
    val (make, model, year) = tripleOpt.get
    isRecalledThreeParams (make, model, year)
  }

  ////////////////////////

  def isRecalledWithMatchingNonCase (subject: Car): Option[Boolean] = {
    subject match {
      case CarNonCase ("Ford", "Mustang", 2011) => Some (true)
      case CarNonCase ("Chevrolet", "Sonic", year) if year <= 2008 => Some (true)
      case CarNonCase ("Chrysler", _, year) if yearsAgo (year) > 1 => Some (true)
      case CarNonCase (make, _, _) if List ("Ford", "Chevrolet", "Chrysler").contains (make) => Some (false)
      case _ => None
    }
  }

  ////////////////////////

  case class CarCase (make: String, model: String, year: Int) extends Car

  def isRecalledWithMatchingCase (subject: Car): Option[Boolean] = {
    subject match {
      case CarCase ("Ford", "Mustang", 2011) => Some (true)
      case CarCase ("Chevrolet", "Sonic", year) if year <= 2008 => Some (true)
      case CarCase ("Chrysler", _, year) if yearsAgo (year) > 1 => Some (true)
      case CarCase (make, _, _) if List ("Ford", "Chevrolet", "Chrysler").contains (make) => Some (false)
      case _ => None
    }
  }

  ////////////////////////

  object FordModel {
    def unapply (subject: Car): Option[String] = {
      subject.make match {
        case "Ford" => Some (subject.model)
        case _ => None
      }
    }
  }

  def isFordSportsCar (subject: Car): Option[Boolean] = {
    subject match {
      case FordModel ("Mustang") => Some (true)
      case FordModel (_) => Some (false)
      case _ => None
    }
  }

  ////////////////////////

  object HistoricCar {
    def unapply (subject: Car): Boolean = {
      yearsAgo (subject.year) >= 25
    }
  }

  def isHistoricCar (subject: Car): Boolean = {
    subject match {
      case HistoricCar () => true
      case _ => false
    }
  }




  private def yearsAgo (year: Int): Int = {
    new GregorianCalendar ().get (Calendar.YEAR) - year
  }
}
