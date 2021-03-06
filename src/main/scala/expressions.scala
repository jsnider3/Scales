package scales.exprs
import scala.collection.mutable.Map
import scales.{Log, LookupTable, Main, Scoped}

object OP extends Enumeration {
  type OP = Value
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

/** Counters used to make unique labels. */
object Counters {
  
  var cmps = 0
  var ifs = 0
  var whiles = 0

  def nextCmp : Int = {
    cmps = cmps + 1
    cmps
  }

  def nextIf : Int = {
    ifs = ifs + 1
    ifs
  }

  def nextWhile : Int = {
    whiles = whiles + 1
    whiles
  }
}


trait Compilable {
  /** Compile this in the given scope.
   *
   * @return: The amount of variables that this pushes on the stack.
   */
  def compile(state: LookupTable, store: Boolean)
}

trait Typable {
  def typecheck(typemap: Map[String, String]) : String
}

trait Expr extends Compilable with Typable

/** Implements expressions of the form "op expr". */
case class UnaOp(op: OP.Value, x: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val ty = x.typecheck(typemap)
    op match {
      case CMP => if (ty != "Int") {Log.error("Complement of non-int")}
      case NOT => if (ty != "Bool") {Log.error("Negating non-bool")}
    }
    if (op == CMP) {
      "Int"
    } else {
      "Bool"
    }
  }

  def compile(state: LookupTable, store: Boolean) = {
    x.compile(state, true)
    op match {
      case CMP => println("  ineg")
      case NOT => println("  ineg")
      case VOID => println("  ;TODO isVoid x")
    }
    if (!store) {
      Jas.pop(1)
    }
  }

}

/** Implements expressions of the form "expr op expr". */
case class OpExpr(op: OP.Value, x: Expr, y: Expr) extends Expr{
  def typecheck(typemap: Map[String, String]) : String = {
    val tys = Array(x.typecheck(typemap), y.typecheck(typemap))
    if (tys(0) != tys(1) || tys(1) != "Int") {
      Log.error(op + " needs two ints, but has " + x + " " + y)
    }
    if (op == NE || op == GT || op == GE || op == LT || op == LE || op == EQ) {
      "Bool"
    } else {
      "Int"
    }
  }

  def compile(state: LookupTable, store: Boolean) = {
    x.compile(state, true)
    y.compile(state, true)
    println("  ;" + this)
    op match {
      case PLUS => println("  iadd")
      case MINUS => println("  isub")
      case MULT =>  println("  imul")
      case DIV =>  println("  idiv")

      case _ => makeComparison()
    }
    if (!store) {
      Jas.pop(1)
    }
  }

  def makeComparison() = {
    val count = Counters.nextCmp
    val check = op match {
      case NE => "if_icmpne"
      case GT => "if_icmpgt"
      case GE => "if_icmpge"
      case LT => "if_icmplt"
      case LE => "if_icmple"
      case EQ => "if_icmpeq"
    }
    println("  " + check + " true" + count)
    println("  ldc 0")
    println("  goto endcmp" + count) 
    println("true" + count + ":")
    println("  ldc 1")
    println("endcmp" + count + ":")
  }

}

/** Implements expressions of a constant literal. */
case class Constant(ty: String, con: String) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = ty

  def compile(state: LookupTable, store: Boolean) = {
    println("  ;constant " + con)
    if (con == "null") {
      println(" aconst_null")
    } else {
      println("  ldc " + con)
    }
    if (!store) {
      Jas.pop(1)
    }
  }

}

/** Expressions that have a guard, like ifs and loops. */
trait Guarded extends Expr {
  def compileGuard(grd: Expr, target: String, state: LookupTable) = {
    grd match { 
      case OpExpr(NE, a, b) => a.compile(state, true)
        b.compile(state, true)
        println("  if_icmpeq " + target)
      case OpExpr(GT, a, b) => a.compile(state, true)
        b.compile(state, true)
        println("  if_icmple " + target)
      case OpExpr(GE, a, b) => a.compile(state, true)
        b.compile(state, true)
        println("  if_icmplt " + target)
      case OpExpr(LT, a, b) => a.compile(state, true)
        b.compile(state, true)
        println("  if_icmpge " + target)
      case OpExpr(LE, a, b) => a.compile(state, true)
        b.compile(state, true)
        println("  if_icmpgt " + target)
      case OpExpr(EQ, a, b) => a.compile(state, true)
        b.compile(state, true)
        println("  if_icmpne " + target)
      case _ => grd.compile(state, true)
                println("  ifeq " + target)
    }
  }

}

/** Implements conditional expressions. */
case class If(gd: Expr, thn: Expr, els: Expr) extends Guarded {
  def typecheck(typemap: Map[String, String]) : String = {
    if (gd.typecheck(typemap) != "Bool") {
      Log.error("If guard " + gd + " non-bool.")
    }
    val tys = List(thn.typecheck(typemap.clone()), els.typecheck(typemap.clone()))
    //TODO Common types.
    if (tys(0) != tys(1)) {
      Log.error("If branches of differing types.")
    }
    tys(0)
  }

  def compile(state: LookupTable, store: Boolean) = {
    val count = Counters.nextWhile
    compileGuard(gd, "Else" + count, state)
    thn.compile(state, true)
    println("  goto Endif" + count)
    println("  Else" + count+":")
    els.compile(state, true)
    println("  Endif" + count + ":")
    if (!store) {
      Jas.pop(1)
    }
  }
}

/** Implements loop expressions. */
case class While(grd: Expr, bod: Expr) extends Guarded {
  def typecheck(typemap: Map[String, String]) : String = {
    if (grd.typecheck(typemap) != "Bool") {
      Log.error("While guard " + grd + " non-bool.")
    }
    bod.typecheck(typemap)
    "Int"
  }

  def compile(state: LookupTable, store: Boolean) = {
    val count = Counters.nextWhile
    println("  Startloop" + count + ":")
    compileGuard(grd, "Endloop" + count, state)
    bod.compile(state, false)
    println("  goto Startloop" + count)
    println("  Endloop" + count + ":")
    if (store) {
      println("  ldc 0")
    }
  }

}

/** Defines local variables in a new scope. */
case class LetX(lets: List[Let], bod: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val letstate = typemap.clone()
    lets.foreach{_.typecheck(letstate)}
    lets.foreach{_.load(letstate)}
    bod.typecheck(letstate)
  }

  def compile(state: LookupTable, store: Boolean) = {
    val letstate = state.enterScope(lets)
    println("; inscope")
    bod.compile(letstate, store)
  }

}

/** A block of expresions. */
case class Seq(bod: List[Expr]) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    (bod map {_.typecheck(typemap)}).last
  }

  def compile(state: LookupTable, store: Boolean) = {
    bod.reverse.tail.reverse.foreach{x => x.compile(state, false)}
    bod.last.compile(state, store)
  }

}

/** Look up the variable. */
case class Var(id: String) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    if (!typemap.contains(id)) {
      Log.error(id + " is not in-scope.")
      "Int"
    } else {
      typemap(id)
    }
  }

  def compile(state: LookupTable, store: Boolean) = {
    if (store) {
      state.get(id)
    }
  }
}

/** Assign a value to a variable. */
case class Asgn(id: String, rval: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val tys = List(typemap(id), rval.typecheck(typemap))
    if (tys(0) != tys(1)) {
      Log.error("Assignment to " + tys(0) + " " + id + " of type " + tys(1))
    }
    tys(1)
  }

  def compile(state: LookupTable, store: Boolean) = {
    state.put(id, rval)
    if (store) {
      state.get(id)
    }
  }
}

/** Assign a value to an index in an array. */
case class ArrAsgn(id: String, ind: Expr, rval: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val tys = List(typemap(id), ind.typecheck(typemap), rval.typecheck(typemap))
    if (tys(0) != "Int[]") {
      Log.error("Array assignment to non-array.")
    }
    if (tys(1) != "Int") {
      Log.error("Array index " + ind + " of wrong type.")
    }
    if (tys(2) != "Int") {
      Log.error("Array assignment to " + id + " of type " + tys(2))
    }
    "Int"
  }

  def compile(state: LookupTable, store: Boolean) = {
    //TODO This code is terrible.
    state.get(id)
    ind.compile(state, true)
    rval.compile(state, true)
    if (store) {
      println("  dup")
      println("  istore 31")
    }
    println("  iastore")
    if (store) {
      println("  iload 31")
    }
  }
}

/** Create a new array. */
case class ArrDec(size: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val ty = size.typecheck(typemap)
    if (ty != "Int") {
      Log.error("New array declared with size of type " + ty + ".")
    }
    "Int[]"
  }

  def compile(state: LookupTable, store: Boolean) = {
    size.compile(state, store)
    if (store) {
      println("  newarray int")
    }
  }
}

/** Construct a new member of a class. */
case class ClassDec(ty:String, args: List[Expr]) extends Callable {
  def typecheck(typemap: Map[String, String]) : String = {
    val cls = Main.prog.find({_.Name() == ty}).get
    meth = cls.getMethod("init")
    typecheckCall(args, typemap)
    ty
  }

  def compile(state: LookupTable, store: Boolean) = {
    println("  new " + ty)
    println("  dup")
    compileCall(args, state, store)
  }
}

/** Index into an array. */
case class ArrGet(id: Expr, ind: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    if (ind.typecheck(typemap) != "Int") {
      Log.error("Indexing into array with non-Int.")
    }
    "Int"
  }

  def compile(state: LookupTable, store: Boolean) = {
    id.compile(state, store)
    ind.compile(state, store)
    if (store) {
      println("  iaload")
    }
  }
}

/** Shared code for calling various types of methods. */
trait Callable extends Expr{

  var meth : Option[Method] = None

  def typecheckCall(args: List[Expr], typemap: Map[String, String]) = {
    val vals = args map {_.typecheck(typemap)}
    if (vals.length != meth.get.args.length) {
      Log.error("Method call on " + meth.get + " has wrong number of args.")
    }
    val bindings = meth.get.args zip vals
    bindings.foreach{case (Typed(n, t), v) => if (v != t) {
      Log.error("Arg of wrong type")
    }}
  }

  def compileCall(args: List[Expr], state: LookupTable, store: Boolean) = {
    args.foreach{_.compile(state, true)}
    meth.get.call
    if (!store) {
      Jas.pop(1)
    }
  }

}

/** Call a method on self. */
case class MethodCall(id: String, args: List[Expr]) extends  Callable {
  def typecheck(typemap: Map[String, String]) : String = {
    val cls = Main.prog.find({_.Name() == typemap("self")}).get
    meth = cls.getMethod(id)
    if (meth != None) {
      typecheckCall(args, typemap)
    } else {
      Log.error("Could not find method " + id + ".")
    }
    meth.get.ty
  }

  def compile(state: LookupTable, store: Boolean) = {
    println("  aload_0")
    compileCall(args, state, store)
  }
}

/** Call a method on the given object. */
case class ClassCall(self: Expr, id: String, args: List[Expr]) extends Callable {
  def typecheck(typemap: Map[String, String]) : String = {
    val name = self.typecheck(typemap)
    val clsopt = Main.prog.find({_.Name() == name})
    if (clsopt == None) {
      Log.error("class call on non-existent " + name + ".")
    }
    meth = clsopt.get.getMethod(id)
    if (meth != None) {
      typecheckCall(args, typemap)
    } else {
      Log.error("Could not find method " + id + ".")
    }
    meth.get.ty
  }

  def compile(state: LookupTable, store: Boolean) = {
    self.compile(state, true)
    compileCall(args, state, store)
  }
}

/** Define a local variable. */
case class Let(name: String, ty:String, body: Option[Expr]) extends Scoped {
  def load(typemap: Map[String, String]) = {
    typemap(name) = ty
  }

  def load(state: LookupTable) = {
    state.types(name) = ty
    state.locs(name) = state.nextLocal()
    if (body != None) {
      state.put(name, body.get)
    } else if (ty == "Int" || ty == "Bool") {
      state.put(name, Constant(ty, "0"))
    } else {
      state.put(name, Constant("Object", "null"))
    }
  }

  def typecheck(typemap: Map[String, String]) = {
    if (body != None) {
      val bty = body.get.typecheck(typemap)
      if (bty != ty) {
        Log.error("Let " + name + " is not of declared type.")
      }
    }
    ty
  }

}
