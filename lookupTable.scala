package scales
import scala.collection.mutable.Map
import scales.exprs.Jasmin

trait Scoped {
  def load(state: LookupTable)
}

abstract class Pos
case class Local(v : Int) extends Pos
case class Field(f : String) extends Pos

class LookupTable {

  var locs = Map[String, Pos]()
  var parent : Option[LookupTable] = None
  var types = Map[String, String]()

  def enterScope(defs: List[Scoped]) : LookupTable = {
    val child = new LookupTable()
    child.locs = Map[String, Pos]()
    child.types = Map[String, String]()
    defs map (_.load(child))
    child
  }

  def enterScope(pos: Map[String, Pos], tys: Map[String, String]) : LookupTable = {
    val child = new LookupTable()
    child.locs = pos
    child.types = tys
    child
  }

  def get(id: String) = {
    locs(id) match {
      case Field(f) => {
        println("  aload_0")
        println("  getfield " + f + " " + Jasmin.typecast(types(id)))
      }
      case Local(n) => {
        println("  ;TODO Get local")       
      }
    }

  }

  def put(id: String) = {
    //TODO 
    //Do something like `types(id) + "store_" + locs(id)`
  }
}
