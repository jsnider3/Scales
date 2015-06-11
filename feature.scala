package scales.exprs
import scala.collection.mutable.Map
import scales.Local
import scales.Log
import scales.LookupTable
import scales.Main
import scales.Pos
import scales.Scoped

case class Typed(name: String, ty: String) 

abstract class Feature

case class Attribute(name: String, ty: String, init: Option[Expr]) extends Feature with Scoped {
  
  var clas = ""

  def setClass(typemap: Map[String, String]) = {
    clas = Main.lookupClass(typemap("self")).get.Name()
  }

  def load(state: LookupTable) = {
    state.locs(name) = scales.Field(clas + "/" + name)
    state.types(name) = ty
    if (init != None) {
      state.put(name, init.get)
    }
  }

}

object Jas {

  def args(tys: List[Typed]) : String = {
    "(" + (tys.map {case Typed(n, ty) => Jas.typecast(ty)}).mkString("") + ")"
  }

  def pop (n: Int) = {
    for (i <- 1 to n) {
      println("  pop")
    }
  }

  def typecast (ty: String): String = {
    if (ty == "Int") {
      "I"
    } else if (ty == "Int[]") {
      "[I"
    } else if (ty == "Bool") {
      "I"
    } else if (ty == "String") {
      "Ljava/lang/String;"
    } else {
      ty
    }
  }
}

case class Method(name: String, args:List[Typed], ty:String, body: Expr)
  extends Feature with Typable {

  var clas = ""

  def call = {
    println("  invokevirtual " + clas + "/" + signature)
  }

  def compile(state: LookupTable) = {
    val scope = loadArgs(state)
    printHeader
    body.compile(scope)
    printFooter
  }


  def loadArgs(state: LookupTable) : LookupTable = {
    val toLoad = List(Typed("self", clas)) ++ args
    val pos = Map[String, Pos]()
    val tys = Map[String, String]()
    toLoad.zipWithIndex.foreach{ case (Typed(n, t), i) =>
      pos(n) = Local(i)
      tys(n) = t    
    }
    state.enterScope(pos, tys) 
  }

  def setClass(cls: String) = clas = cls

  def signature : String = {
    name + Jas.args(args) + Jas.typecast(ty)
  }

  def typecheck(state: Map[String, String]) : String = {
    var methodState = state.clone()
    var argNames = List[String]()
    for(arg <- args)
    {
      //UncoolAid 2.1.2 says that method params hide attrbiutes.
      if(argNames.contains(arg.name))
      {
        Log.error("Duplicate " + arg.name + " in " + name + " args.")
      }
      argNames +:= arg.name
      methodState(arg.name) = arg.ty
    }
    val bty = body.typecheck(methodState);
    if(bty != ty)
    {
      Log.error(name + " returns " + bty + " declares " + ty)
    }
    setClass(Main.lookupClass(state("self")).get.Name())
    ty
  }

  def printFooter = {
    if (ty == "Int") {
      println("  ireturn")
    } else if (ty == "Bool") {
      println("  ireturn")
    } else {
      println("  areturn")
    }
    println(".end method")
  }

  def printHeader = {
    println(".method public " + signature)
    println("  .limit locals 32")
    println("  .limit stack 32")
  }

}
