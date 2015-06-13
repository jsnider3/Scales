package scales
import scales.exprs._
import scala.collection.mutable.Map

/** Implements a class in Uncool.
 *
 * A class has a name, an optional parent, and a list of methods and
 * attributes.
 */
class Cls (name: String, parent: String, feats: List[Feature]) {

  override def toString() = ("class " + name + " inherits " + parent + " {" +
                              feats.mkString("; ") + "}")

  /** Write the Jasmin code for the class out to a file.
   *
   * We emit the header and private fields, then our methods with
   * main and init methods being special cases.
   */
  def compile () {
    val stdout = Console.out
    val fileout = new java.io.FileOutputStream(name + ".j")
    Console.setOut(fileout)
    println(".class public " + name)
    parent match {
      case "" => println(".super UCObject")
      case _ => println(".super " + parent)
    }
    val ours = feats.filter(_.isInstanceOf[Attribute]) map
                             (_.asInstanceOf[Attribute])
    for (Attribute(n, t, _) <- ours) {
      println("  .field private " + n + " " + Jas.typecast(t))
    }
    if (name == "Main") {
      compileMain()
    }
    val state = compileInit()
    getMethods().filter(a => a.name != "init").foreach{_.compile(state)}
    Console.setOut(stdout)
    fileout.close()
  }


  /** Compile the classes constructor.
   *
   * Required for all classes, some classes explicitly define other things
   * to do in the constructor.
   * @return: A LookupTable to be used to compile the classes
   *          other methods.
   */
  def compileInit() : LookupTable = {
    val initmeth = getMethod("init")
    if (initmeth == None) {
      println(".method public <init>()V")
    } else {
      println(".method public " + initmeth.get.signature)
    }
    println("  .limit stack 32")
    println("  .limit locals 32")
    println("  aload_0")
    println("  invokespecial " + parent + "/<init>()V")
    val parScope = if (hasSuper()) {
      getSuper(Main.prog).get.state
    } else {
      new LookupTable()
    }
    val state = parScope.enterScope(getAttributes(Main.prog))
    val initstate = if (initmeth != None) {
      initmeth.get.loadArgs(state)
    } else {
      state
    }
    if (initmeth != None) {
      initmeth.get.body.compile(initstate)
    }
    println("  return")
    println(".end method")
    state
  }

  /** Entry point for running the file.
   *
   * Essentially the same as (new Main()).main().
   */
  def compileMain() = {
    println(".method public static main([Ljava/lang/String;)V")
    println("  .limit stack 2")
    println("  new Main")
    println("  dup")
    println("  invokespecial Main/<init>()V")
    getMethod("main").get.call
    println("  return")
    println(".end method")
  }

  /** Get all of the attributes of this class as a Map.
   *
   * Used by typechecker and logs errors for non-unique attribute names.
   */
  def getAttributeMap(prog: List[Cls]) : Map[String, String] = {
    var attrs = if (hasSuper()) {
      getSuper(prog).get.getAttributeMap(prog)
    } else {
      Map[String, String]()
    }
    val ours = feats.filter(_.isInstanceOf[Attribute]) map
                             (_.asInstanceOf[Attribute])
    for (Attribute(n, t, _) <- ours) {
      if (ours.count(b => b.name == n) > 1) {
        Log.error("Duplicate field " + n + " in class " + name + ".")
      }
      attrs(n) = t
    }
    attrs
  }

  /** Get all of the attributes of this class.
   *
   * Includes attributes from supers and shadows them as appropriate.
   */
  def getAttributes(prog: List[Cls]) : List[Attribute] = {
    val attrs = if (hasSuper()) {
      getSuper(prog).get.getAttributes(prog)
    } else {
      List[Attribute]()
    }
    val ours = feats.filter(_.isInstanceOf[Attribute]) map
                             (_.asInstanceOf[Attribute])
    attrs.filter(!ours.contains(_)) ++ ours
  }
  
  /** Get method in class with a given name.
   *
   * Includes shadowing and returns None if not in any scope.
   */
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

  /** Get all of the methods of this class as a List.
   *
   * Used by typechecker and logs errors for non-unique method names.
   */
  def getMethods() : List[Method] = {
   val methods = feats.filter(_.isInstanceOf[Method]) map
                             (_.asInstanceOf[Method])
    methods.foreach{ a =>
      if (methods.count(b => b.name == a.name) > 1) {
        Log.error("Duplicate method " + a.name)
      }
      methods
    }
    methods
  }

  /** Gets our superclass or None if not in the given program. */
  def getSuper(prog: List[Cls]) : Option[Cls] = {
    prog.find({_.Name() == parent})
  }

  def hasSuper() = parent != ""

  def Feats() = feats
  def Name() = name
  def Parent() = parent

  /** Get the LookupTable from which this class's methods inherit. */
  def state : LookupTable = {
    val attrs = feats.filter(_.isInstanceOf[Attribute]) map
                             (_.asInstanceOf[Attribute])
    val pos = Map[String, Pos]()
    val tys = Map[String, String]()
    for (Attribute(n, t, _) <- attrs) {
      pos(n) = Field(name + "/" + n)
      tys(n) = t    
    }
    val parScope = if (hasSuper()) {
      getSuper(Main.prog).get.state
    } else {
      new LookupTable()
    }
    parScope.enterScope(pos, tys) 
  }

  /** Typecheck this class and log any errors. */
  def typecheck() = {
    if (!Main.builtins.contains(name)) {
      if (hasSuper() && getSuper(Main.prog) == None) {
        Log.error(name + " inherits from undefined " + parent)
      }
      var typemap = getAttributeMap(Main.prog)
      typemap("self") = name
      
      getMethods().foreach{_.typecheck(typemap)}
      (feats.filter(_.isInstanceOf[Attribute]) map
                             (_.asInstanceOf[Attribute])) map (_.setClass(typemap))
    }
  }
}
