package scales.exprs
import scala.collection.mutable.Map
import scales.Log
import scales.LookupTable
import scales.Main
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
//      init.get.compile(state)
      state.put(name, init.get, state)
    }
  }

}

object Jasmin {

  def args(tys: List[Typed]) : String = {
    "(" + (tys.map {case Typed(n, ty) => Jasmin.typecast(ty)}).mkString("") + ")"
  }

  def typecast (ty: String): String = {
    if (ty == "Int") {
      "I"
    } else if (ty == "Int[]") {
      "[I"
    } else if (ty == "Bool") {
      "TODO Bool return"
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
    //TODO
    state
  }

  def setClass(cls: String) = clas = cls

  def signature : String = {
    name + Jasmin.args(args) + Jasmin.typecast(ty)
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
    //TODO Typed returns.
    if (ty == "Int") {
      println("  ireturn")
    } else if (ty == "Bool") {
      println(";TODO Bool return")
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
