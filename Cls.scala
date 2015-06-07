package scales
import scales.exprs._
import scala.collection.mutable.Map

class Cls (name: String, parent: String, feats: List[Feature]){

  override def toString() = ("class " + name + " inherits " + parent + " {" +
                              feats.mkString("; ") + "}")

  def compile () {
    val stdout = Console.out
    Console.setOut(new java.io.FileOutputStream(name + ".j"))
    println(".class public " + name)
    parent match {
      case "" => println(".super UCObject")
      case _ => println(".super " + parent)
    }
    //TODO Output fields.
    val ours = feats.filter(_.isInstanceOf[Attribute]) map
                             (_.asInstanceOf[Attribute])
    for (Attribute(n, t, _) <- ours) {
      println("  .field private " + n + " " + jvmtype(t))
    }
    if (name == "Main") {
      println(".method public static main([Ljava/lang/String;)V")
      println("  .limit stack 2")
      println("  new Main")
      println("  dup")
      println("  invokespecial Main/<init>()V")
      getMethod("main").get.call
      println("  return")
      println(".end method")
    }
    println(".method public <init>()V")
    println("  aload_0")
    println("  invokespecial " + parent + "/<init>()V")
    println("  return")
    println(".end method")
    getMethods().foreach{_.compile}
    Console.setOut(stdout)
  }

  def jvmtype(ty: String) : String = {
    ty match {
      case "Int" => "I"
      case "String" => "Ljava/lang/String;"
    }
  }

  def getAttributes(prog: List[Cls]) : Map[String, String] = {
    var attrs = if (hasSuper()) {
      getSuper(prog).get.getAttributes(prog)
    } else {
      Map[String, String]()
    }
    val ours = feats.filter(_.isInstanceOf[Attribute]) map
                             (_.asInstanceOf[Attribute])
    for (Attribute(n, t, _) <- ours) {
      if (attrs.contains(n)) {
        Log.error("Duplicate field " + n + " in class " + name + ".")
      }
      attrs(n) = t
    }
    attrs
  }
  
  def getMethod(name : String) : Option[Method] = {
    val found = getMethods().find({_.name == name})
    if (found != None) {
      found
    } else if (hasSuper()){
      getSuper(Main.prog).get.getMethod(name)
    } else {
      None
    }
  }

  def getMethods() : List[Method] = {
   val methods = feats.filter(_.isInstanceOf[Method]) map
                             (_.asInstanceOf[Method])
    //TODO Check for duplicates.
    methods
  }

  def getSuper(prog: List[Cls]) : Option[Cls] = {
    prog.find({_.Name() == parent})
  }

  def hasSuper() = parent != ""

  def Feats() = feats
  def Name() = name
  def Parent() = parent

  def typecheck() = {
    if (!Main.builtins.contains(Name())) {
      if (hasSuper() && getSuper(Main.prog) == None) {
        Log.error(Name() + " inherits from undefined " + Parent())
      }
      var typemap = getAttributes(Main.prog)
      typemap("self") = Name()
      
      getMethods().foreach{_.typecheck(typemap)}
    }
  }
}
