package scales

abstract class Pos
case class Local(v : Int) extends Pos
case class Field() extends Pos

class LookupTable {

  var locs = Map[String, Pos]()
  var parent : Option[LookupTable] = None
  var types = Map[String, String]()

  def copy(orig: LookupTable) : LookupTable = {
    var cp = new LookupTable()
    cp.locs = this.locs
    cp.parent = this.parent
    cp.types = this.types
    cp
  }

  def enterScope(/*defs: List[Def]*/) = {
    //TODO 
    parent = Some(copy(this))
    locs = Map[String, Pos]()
    types = Map[String, String]()
  }

  def exitScope = {
    if (parent == None) {
      locs = Map[String, Pos]()
      types = Map[String, String]()
    } else {
     locs = parent.get.locs 
     types = parent.get.types 
     parent = parent.get.parent
    }
  }

  def get(id: String) = {
    //TODO 
    //Do something like `types(id) + "load_" + locs(id)`

  }

  def put(id: String) = {
    //TODO 
    //Do something like `types(id) + "store_" + locs(id)`
  }
}
