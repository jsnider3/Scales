package scales
import exprs._
import scala.collection.mutable.Map

object Main {

  var prog = List[Cls]()
  val builtins = List("Bool", "Int", "Int[]", "String", "Object")

  def compileObjectClass = {
    //TODO Redirect stdout to UCObject.j
    println(".class public UCObject")
    println(".super java/lang/Object")
    println("")
    println(".method public <init>()V")
    println("  aload_0")
    println("  invokespecial java/lang/Object/<init>()V")
    println("  return")
    println(".end method")
    println("")
    println(".method public out_string()")
    println("  getstatic java/lang/System/out Ljava/io/PrintStream;")
    println("  aload_1")
    println("  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V")
    println("  return")
    println(".end method")
    println("")
    println(".method public out_int()")
    println("  iload_1")
    println("  invokestatic java/lang/String/valueOf(I)Ljava/lang/String")
    println("  astore_2")
    println("  getstatic java/lang/System/out Ljava/io/PrintStream;")
    println("  aload_2")
    println("  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V")
    println("  return")
    println(".end method")
    println("")
    println(".method public in_string()")
    println("  ;TODO")
    println("  return")
    println(".end method")
    println("")
    println(".method public in_int()")
    println("  ;TODO")
    println("  return")
    println(".end method")
    println("")
    println(".method public abort()")
    println("  ;TODO")
    println("  return")
    println(".end method")
  }

  def findMain(prog: List[Cls]) : Boolean = {
    var found = false
    val Main = prog.find({_.Name() == "Main"})
    if (Main != None) {
      for (feat <- Main.get.Feats()) {
        feat match {
          case Method("main", _, _, _) => found = true 
          case _ =>
        }
      }
    }
    found
  }

  def lookupClass(name : String) : Option[Cls] = {
    prog.find({_.Name() == name})
  }

  def makeBuiltIns(clses: List[Cls]) : List[Cls] = {
    val tyBool = new Cls("Bool", "", List())
    val tyInt = new Cls("Int", "", List())
    val tyArr = new Cls("Int[]", "", List())
    val tyStr = new Cls("String", "", List())
    val abort = Method("abort", List(),"Object", Constant("Int", "0"))
    val in_int = Method("in_int", List(),"Int", Constant("Int", "0"))
    val in_string = Method("in_string", List(),"String", Constant("Int", "0"))
    val out_int = Method("out_int", List(Attribute("", "Int")),"Int",
      Constant("Int", "0"))
    val out_string = Method("out_string", List(Attribute("", "String")),
      "String", Constant("Int", "0"))
    val tyObj = new Cls("Object", "", List(abort, in_int, in_string, out_int,
      out_string))
    List[Cls](tyBool, tyInt, tyArr, tyStr, tyObj) ++ clses
  }

  def main(args: Array[String]) = {
    val ast : List[Cls] = Uncool.make_ast(args(0))
    if (!findMain(ast)) {
      Log.error("Main not found.")
    }
    prog = makeBuiltIns(ast)
    prog.foreach{_.typecheck}
    if (Log.errors.length > 0) {
      println("The following type errors were found:")
      Log.errors.foreach{println}
    } else {
      compileObjectClass
      prog.filter(c => !builtins.contains(c.Name())).foreach{_.compile}
    } 
  }

  def Prog() : List[Cls] = prog

}
