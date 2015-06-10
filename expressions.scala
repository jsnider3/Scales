package scales.exprs
import scala.collection.mutable.Map
import scales.Log
import scales.LookupTable
import scales.Main
import scales.Scoped

object OP extends Enumeration {
  type OP = Value
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

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
  def compile(state: LookupTable) : Int
}

trait Typable {
  def typecheck(typemap: Map[String, String]) : String
}

trait Expr extends Compilable with Typable

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

  def compile(state: LookupTable) = {
    x.compile(state)
    op match {
      case CMP => println("  ineg")
      case NOT => println("  ;TODO Not x")
      case VOID => println("  ;TODO isVoid x")
    }
    1
  }

}

case class OpExpr(op: OP.Value, x: Expr, y: Expr) extends Expr{
  def typecheck(typemap: Map[String, String]) : String = {
    val tys = Array(x.typecheck(typemap), y.typecheck(typemap))
    if (tys(0) != tys(1) || tys(1) != "Int") {
      Log.error(op + " needs two ints.")
    }
    if (op == NE || op == GT || op == GE || op == LT || op == LE || op == EQ) {
      "Bool"
    } else {
      "Int"
    }
  }

  def compile(state: LookupTable) = {
    x.compile(state)
    y.compile(state)
    println("  ;" + this)
    op match {
      case PLUS => println("  iadd")
      case MINUS => println("  isub")
      case MULT =>  println("  imul")
      case DIV =>  println("  idiv")

      case _ => makeComparison()
    }
    1
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

case class Constant(ty: String, con: String) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = ty

  def compile(state: LookupTable) = {
    println("  ldc " + con)
    1 
  }

}

trait Guarded extends Expr {
  //TODO Make stack heights consistent.
  //TODO Optimize for OpExprs.
  def compileGuard(grd: Expr, target: String, state: LookupTable) = {
    grd match { 
      case OpExpr(NE, a, b) => a.compile(state)
        b.compile(state)
        println("  if_icmpeq " + target)
      case OpExpr(GT, a, b) => a.compile(state)
        b.compile(state)
        println("  if_icmple " + target)
      case OpExpr(GE, a, b) => a.compile(state)
        b.compile(state)
        println("  if_icmplt " + target)
      case OpExpr(LT, a, b) => a.compile(state)
        b.compile(state)
        println("  if_icmpge " + target)
      case OpExpr(LE, a, b) => a.compile(state)
        b.compile(state)
        println("  if_icmpgt " + target)
      case OpExpr(EQ, a, b) => a.compile(state)
        b.compile(state)
        println("  if_icmpne " + target)
      case _ => grd.compile(state)
                println("  ifeq " + target)
    }
    0
  }

}

case class If(gd: Expr, then: Expr, els: Expr) extends Guarded {
  def typecheck(typemap: Map[String, String]) : String = {
    if (gd.typecheck(typemap) != "Bool") {
      Log.error("If guard " + gd + " non-bool.")
    }
    val tys = List(then.typecheck(typemap.clone()), els.typecheck(typemap.clone()))
    //TODO Common types.
    if (tys(0) != tys(1)) {
      Log.error("If branches of differing types.")
    }
    tys(0)
  }

  def compile(state: LookupTable) = {
    val count = Counters.nextWhile
    compileGuard(gd, "Else" + count, state)
    then.compile(state)
    println("  goto Endif" + count)
    println("  Else" + count+":")
    els.compile(state)
    println("  Endif" + count + ":")
    0
  }
}

case class While(grd: Expr, bod: Expr) extends Guarded {
  def typecheck(typemap: Map[String, String]) : String = {
    if (grd.typecheck(typemap) != "Bool") {
      Log.error("While guard " + grd + " non-bool.")
    }
    bod.typecheck(typemap)
    "Int"
  }

  def compile(state: LookupTable) = {
    val count = Counters.nextWhile
    println("  Startloop" + count + ":")
    compileGuard(grd, "Endloop" + count, state)
    bod.compile(state)
    println("  goto Startloop" + count)
    println("  Endloop" + count + ":")
    println("  ldc 0")
    1
  }

}

case class LetX(lets: List[Let], bod: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    var letstate = typemap.clone()
    lets.foreach{_.typecheck(letstate)}
    lets.foreach{_.load(letstate)}
    bod.typecheck(letstate)
  }

  def compile(state: LookupTable) = {
    println("  ;TODO letx")
    0
  }

}

case class Seq(bod: List[Expr]) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    (bod map {_.typecheck(typemap)}).last
  }

  def compile(state: LookupTable) = {
    bod.reverse.tail.reverse.foreach{x => Jas.pop(x.compile(state))}
    bod.last.compile(state)
    1
  }

}

case class Var(id: String) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    if (!typemap.contains(id)) {
      Log.error(id + " is not in-scope.")
      "Int"
    } else {
      typemap(id)
    }
  }

  def compile(state: LookupTable) = {
    state.get(id)
    1
  }
}

case class Asgn(id: String, rval: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val tys = List(typemap(id), rval.typecheck(typemap))
    if (tys(0) != tys(1)) {
      Log.error("Assignment to " + tys(0) + " " + id + " of type " + tys(1))
    }
    tys(1)
  }

  def compile(state: LookupTable) = {
    state.put(id, rval, state)
    0
  }
}

case class ArrAsgn(id: Expr, ind: Expr, rval: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val tys = List(id.typecheck(typemap), ind.typecheck(typemap), rval.typecheck(typemap))
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

  def compile(state: LookupTable) = {
    println("  ;TODO id[ind] = rval")
    0
  }
}

case class ArrDec(size: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val ty = size.typecheck(typemap)
    if (ty != "Int") {
      Log.error("New array declared with size of type " + ty + ".")
    }
    "Int[]"
  }

  def compile(state: LookupTable) = {
    size.compile(state)
    println("  inewarray")
    1
  }
}

case class ArrGet(id: Expr, ind: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    if (ind.typecheck(typemap) != "Int") {
      Log.error("Indexing into array with non-Int.")
    }
    "Int"
  }

  def compile(state: LookupTable) = {
    println("  ;TODO arrget")
    1
  }
}

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

  def compileCall(args: List[Expr], state: LookupTable) = {
    args.foreach{_.compile(state)}
    meth.get.call
  }

}

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

  def compile(state: LookupTable) : Int = {
    println("  aload_0")
    compileCall(args, state)
    1
  }
}

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

  def compile(state: LookupTable) : Int = {
    self.compile(state)
    compileCall(args, state)
    1
  }
}

case class Let(name: String, ty:String, body: Option[Expr]) {
  def load(typemap: Map[String, String]) = {
    typemap(name) = ty
  }

  def load(state: LookupTable) = {
    //typemap(name) = ty
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

  def compile(state: LookupTable) = {
    if (body == None)
    println("  ;TODO let")
  }
}
