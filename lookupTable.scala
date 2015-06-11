package scales
import scala.collection.mutable.Map
import scales.exprs.Expr
import scales.exprs.Jas

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
    child.parent = Some(this)
    defs map (_.load(child))
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
        println("  getfield " + f + " " + Jas.typecast(getType(id)))
      }
      case Local(n) => {
        getType(id) match {
          //DEBUG Int[] may be a special case.
          case "Int" => println("  iload " + n)
          case "Bool" => println("  iload " + n)
          case _ => println("  aload " + n)
        }
      }
    }
  }

  def getLoc(id: String) : Pos = {
    if (locs.contains(id)) {
      locs(id)
    } else if (parent != None) {
      parent.get.getLoc(id)
    } else {
      throw new RuntimeException("Can't find " + id)
    }
  }

  def getType(id: String) : String = {
    if (types.contains(id)) {
      types(id)
    } else {
      parent.get.getType(id)
    }
  }

  def nextLocal () : Local = {
    if (locs.isEmpty && parent == None) {
      Local(1)
    } else if (locs.isEmpty) {
      parent.get.nextLocal()
    } else {
      val allocs = locs.values map (a => a match {
        case Field(b) => 0
        case Local(n) => n 
      })
      Local(allocs.max + 1)
    }
  }

  def put(id: String, ex: Expr) = {
    getLoc(id) match {
      case Field(f) => {
        println("  aload_0")
        ex.compile(this)
        println("  putfield " + f + " " + Jas.typecast(getType(id)))
      }
      case Local(n) => {
        ex.compile(this)
        getType(id) match {
          case "Int" => println("  istore " + n)
          case "Bool" => println("  istore " + n)
          case _ => println("  astore " + n)
        }
      }
    }
  }

}
