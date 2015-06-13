package scales.exprs
import scala.collection.mutable.Map
import scales.Local
import scales.Log
import scales.LookupTable
import scales.Main
import scales.Pos
import scales.Scoped

/** Tags a variable with a type. */
case class Typed(name: String, ty: String) 

/** Utilities for working with Jasmin. */
object Jas {

  /** Make the args of a method into a string for Jasmin. */
  def args(tys: List[Typed]) : String = {
    "(" + (tys.map {case Typed(n, ty) => Jas.typecast(ty)}).mkString("") + ")"
  }

  /** Pop n variables off the stack. */
  def pop (n: Int) = {
    for (i <- 1 to n) {
      println("  pop")
    }
  }

  /** Format the type in a way that Jasmin wants. */
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
      "L" + ty + ";"
    }
  }
}

abstract class Feature

/** Attribute feature of a class. */
case class Attribute(name: String, ty: String, init: Option[Expr]) extends Feature with Scoped {
  
  var clas = ""

  def setClass(typemap: Map[String, String]) = {
    clas = Main.lookupClass(typemap("self")).get.Name()
  }

  /** Add this Attribute to a state. Compile it if necessary. */
  def load(state: LookupTable) = {
    state.locs(name) = scales.Field(clas + "/" + name)
    state.types(name) = ty
    if (init != None) {
      state.put(name, init.get)
    }
  }

}

/** Method feature of a class. */
case class Method(name: String, args:List[Typed], ty:String, body: Expr)
  extends Feature with Typable {

  var clas = ""

  /** Print the code to invoke this method. */
  def call = {
    if (name == "init") {
      println("  invokespecial " + clas + "/" + signature)
    } else {
      println("  invokevirtual " + clas + "/" + signature)
    }
  }

  /** Compile this by printing its body surrounding by boilerplate. */
  def compile(state: LookupTable) = {
    val scope = loadArgs(state)
    printHeader
    body.compile(scope)
    printFooter
  }

  /** Make the LookupTable used to compile this method. */
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

  /** Store the class this method is part of.
   *
   * We need this to be compiled properly.
   */
  def setClass(cls: String) = clas = cls

  /** Make the LookupTable used to compile this method. */
  def signature : String = {
    if (name == "init") {
      "<" + name + ">" + Jas.args(args) + "V"
    } else {
      name + Jas.args(args) + Jas.typecast(ty)
    }
  }

  /** Typecheck the method and log errors. */
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

  /** Return from the method. */
  def printFooter = {
    if (name == "init") {
      println("  return")
    } else if (ty == "Int") {
      println("  ireturn")
    } else if (ty == "Bool") {
      println("  ireturn")
    } else {
      println("  areturn")
    }
    println(".end method")
  }

  /** Print the header and say how much space we will be using. */
  def printHeader = {
    println(".method public " + signature)
    println("  .limit locals 32")
    println("  .limit stack 32")
  }

}
