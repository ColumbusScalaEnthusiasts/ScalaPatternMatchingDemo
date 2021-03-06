package pmatch

/**
  * Created by dnwiebe on 9/28/16.
  */
object C_Types {
  case class DogWrong (name: String)
  case class BlenderWrong (capacity: Double)
  case class RifleWrong (evilFeatureCount: Int)
  case class CargoShipWrong (isOilTanker: Boolean)

  def soundOf (item: Any): String = {
    item match {
      case i: DogWrong => if (i.name == "Pookie") "yap" else "woof"
      case i: BlenderWrong => "whir"
      case i: RifleWrong => "crack"
    }
  }

  ///////////////////

  trait Soundable {def sound: String}
  case class DogRight (name: String) extends Soundable {def sound = if (name == "Pookie") "yap" else "woof"}
  case class BlenderRight (capacity: Double) extends Soundable {def sound = "whir"}
  case class RifleRight (evilFeatureCount: Int) extends Soundable {def sound = "crack"}
  case class CargoShipRight (isOilTanker: Boolean)
}
