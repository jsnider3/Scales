package scales
import exprs._
import scala.collection.mutable.Map

/** Main program to compile Uncool to Jasmin.
 *
 * Parses, typechecks, and outputs Jasmin to files.
 */
object Main {

  var prog = List[Cls]()
  val builtins = List("Bool", "Int", "Int[]", "String", "UCObject")

  /** Compile input methods.
   *
   */
  def compileStdIn = {
    println(".method public in_string()Ljava/lang/String;")
    println("  .limit locals 32")
    println("  .limit stack 32")
    println("  new java/util/Scanner")
    println("  dup")
    println("  getstatic java/lang/System/in Ljava/io/InputStream;")
    println("  invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V")
    println("  invokevirtual java/util/Scanner/next()Ljava/lang/String;")
    println("  areturn")
    println(".end method")
    println("")
    println(".method public in_int()I")
    println("  .limit locals 32")
    println("  .limit stack 32")
    println("  aload_0")
    println("  invokevirtual UCObject/in_string()Ljava/lang/String;")
    println("  invokestatic java/lang/Integer/parseInt(Ljava/lang/String;)I")
    println("  ireturn")
    println(".end method")
    println("")
  }

  /** Compile output methods.
   *
   */
  def compileStdOut = {
    println(".method public out_string(Ljava/lang/String;)Ljava/lang/String;")
    println("  .limit locals 32")
    println("  .limit stack 32")
    println("  getstatic java/lang/System/out Ljava/io/PrintStream;")
    println("  aload_1")
    println("  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V")
    println("  aload_1")
    println("  areturn")
    println(".end method")
    println("")
    println(".method public out_int(I)I")
    println("  .limit locals 32")
    println("  .limit stack 32")
    println("  iload_1")
    println("  invokestatic java/lang/String/valueOf(I)Ljava/lang/String;")
    println("  astore_2")
    println("  getstatic java/lang/System/out Ljava/io/PrintStream;")
    println("  aload_2")
    println("  invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V")
    println("  iload_1")
    println("  ireturn")
    println(".end method")
    println("")
  }

  /** Compile UCObject.
   *
   * UCObject is the implicit parent of everything without an explicit one.
   * It's methods are Uncool's version of a standard libary.
   */
  def compileObjectClass = {
    val stdout = Console.out
    val fileout = new java.io.FileOutputStream("UCObject.j")
    Console.setOut(fileout)
    println(".class public UCObject")
    println(".super java/lang/Object")
    println("")
    println(".method public <init>()V")
    println("  .limit locals 32")
    println("  .limit stack 32")
    println("  aload_0")
    println("  invokespecial java/lang/Object/<init>()V")
    println("  return")
    println(".end method")
    println("")
    compileStdOut
    compileStdIn
    println(".method public abort()V")
    println("  .limit locals 32")
    println("  .limit stack 32")
    println("  bipush 0")
    println("  invokestatic java/lang/System/exit(I)V")
    println("  return")
    println(".end method")
    Console.setOut(stdout)
    fileout.close()
  }

  /** Check for a Main.main method. */
  def hasMain(prog: List[Cls]) : Boolean = {
    val mainClass = prog.find({_.Name() == "Main"})
    mainClass.get.getMethods().find(_.name == "main") != None
  }

  /** Tries to find a class in the program. */
  def lookupClass(name : String) : Option[Cls] = {
    prog.find({_.Name() == name})
  }

  /** Gets the types, methods, and classes built into Uncool. */
  def makeBuiltIns(clses: List[Cls]) : List[Cls] = {
    val tyBool = new Cls(builtins(0), "", List())
    val tyInt = new Cls(builtins(1), "", List())
    val tyArr = new Cls(builtins(2), "", List())
    val tyStr = new Cls(builtins(3), "", List())
    val abort = Method("abort", List(), builtins(4), Constant("Int", "0"))
    abort.setClass(builtins(4))
    val inInt = Method("in_int", List(),"Int", Constant("Int", "0"))
    inInt.setClass(builtins(4))
    val inString = Method("in_string", List(),"String", Constant("Int", "0"))
    inString.setClass(builtins(4))
    val outInt = Method("out_int", List(Typed("", "Int")),"Int",
      Constant("Int", "0"))
    outInt.setClass(builtins(4))
    val outString = Method("out_string", List(Typed("", "String")),
      "String", Constant("Int", "0"))
    outString.setClass(builtins(4))
    val tyObj = new Cls(builtins(4), "", List(abort, inInt, inString, outInt,
      outString))
    List[Cls](tyBool, tyInt, tyArr, tyStr, tyObj) ++ clses
  }

  def main(args: Array[String]) = {
    val ast : List[Cls] = Uncool.make_ast(args(0))
    if (!hasMain(ast)) {
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
