package scales.exprs
import scala.collection.mutable.Map
import scales.Log
import scales.LookupTable
import scales.Main

abstract class Feature

case class Attribute(name: String, ty: String, init: Option[Expr]) extends Feature
case class Method(name: String, args:List[Typed], ty:String, body: Expr)
  extends Feature with Typable {

  var clas = ""

  def call = {
    println("  invokevirtual " + clas + "/" + signature)
  }

  def compile() = {
    //TODO Needs an arg.
    printHeader
    body.compile(new LookupTable())
    printFooter
  }

  def jasminArgs : String = {
    "(" + (args.map {case Typed(n, ty) => jasminType(ty)}).mkString("") + ")"
  }

  def jasminType (ty: String): String = {
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

  def setClass(cls: String) = clas = cls

  def signature : String = {
    name + jasminArgs + jasminType(ty)
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
