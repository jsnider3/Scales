package scales
import exprs._
import scala.collection.mutable.Map

object Main {

  var prog = List[Cls]()
  val builtins = List("Bool", "Int", "Int[]", "String", "Object")

  def compileObjectClass = {
    println("; TODO")
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

  def Prog() : List[Cls] = prog

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
}
