package scales.exprs
import scala.collection.mutable.Map
import scales.Log
import scales.LookupTable
import scales.Main
import scales.Scoped

object OP extends Enumeration {
  type OP = Value
  //TODO Add VOID.
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

trait Compilable {
  def compile(state: LookupTable)
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
      case NOT => println("  ineg")
                  println("  ;TODO Not x")
    }
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
    op match {
      case PLUS => println("  iadd")
      case MINUS => println("  isub")
      case MULT =>  println("  imul")
      case DIV =>  println("  idiv")

      case NE => println("  ;TODO")
      case GT => println("  ;TODO")
      case GE => println("  ;TODO")
      case LT => println("  ;TODO")
      case LE => println("  ;TODO")
      case EQ => println("  ;TODO")
    }
  }

}

case class Constant(ty: String, con: String) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = ty

  def compile(state: LookupTable) = {
    println("  ldc " + con)
  }

}

case class While(grd: Expr, bod: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    if (grd.typecheck(typemap) != "Bool") {
      Log.error("While guard " + grd + " non-bool.")
    }
    bod.typecheck(typemap)
    "Int"
  }

  def compile(state: LookupTable) = {
    println("  ;TODO")
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
    println("  ;TODO")
  }

}

case class Seq(bod: List[Expr]) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    (bod map {_.typecheck(typemap)}).last
  }

  def compile(state: LookupTable) = {
    bod.foreach{_.compile(state)}
  }

}

case class If(gd: Expr, then: Expr, els: Expr) extends Expr {
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
    println("  ;TODO")
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
    rval.compile(state)
    state.put(id)
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
    println("  ;TODO")
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
    println("  ;TODO")
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
    println("  ;TODO")
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

  def compile(state: LookupTable) = {
    println("  aload_0")
    compileCall(args, state)
  }
}

case class ClassCall(self: Expr, id: String, args: List[Expr]) extends Callable {
  def typecheck(typemap: Map[String, String]) : String = {
    val name = self.typecheck(typemap)
    val clsopt = Main.prog.find({_.Name() == name})
    if (clsopt == None) {
      Log.error("class call on non-existent " + name + ".")
    }
    //TODO Check the method exists.
    meth = clsopt.get.getMethod(id)
    if (meth != None) {
      typecheckCall(args, typemap)
    } else {
      Log.error("Could not find method " + id + ".")
    }
    meth.get.ty
  }

  def compile(state: LookupTable) = {
    self.compile(state)
    compileCall(args, state)
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
    println("  ;TODO")
  }
}
