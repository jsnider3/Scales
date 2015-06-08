package scales
import scala.collection.mutable.Map
import scales.exprs.Expr
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
    child.parent = Some(this)
    child
  }

  def enterScope(pos: Map[String, Pos], tys: Map[String, String]) : LookupTable = {
    val child = new LookupTable()
    child.locs = pos
    child.types = tys
    child.parent = Some(this)
    child
  }

  def get(id: String) = {
    getLoc(id) match {
      case Field(f) => {
        println("  aload_0")
        println("  getfield " + f + " " + Jasmin.typecast(getType(id)))
      }
      case Local(n) => {
        getType(id) match {
          //DEBUG Int[] may be a special case.
          case "Int" => println("  iload_" + n)
          case "Bool" => println("  iload_" + n)
          case _ => println("  aload_" + n)
        }
      }
    }
  }

  def getLoc(id: String) : Pos = {
    if (locs.contains(id)) {
      locs(id)
    } else {
      parent.get.getLoc(id)
    }
  }

  def getType(id: String) : String = {
    if (types.contains(id)) {
      types(id)
    } else {
      parent.get.getType(id)
    }
  }

  def put(id: String, ex: Expr, state: LookupTable) = {
     getLoc(id) match {
      case Field(f) => {
        println("  aload_0")
        ex.compile(state)
        println("  putfield " + f + " " + Jasmin.typecast(getType(id)))
      }
      case Local(n) => {
        getType(id) match {
          //DEBUG Int[] may be a special case.
          case "Int" => println("  istore_" + n)
          case "Bool" => println("  istore_" + n)
          case _ => println("  astore_" + n)
        }
      }
    }
  }
}
