package scales.exprs
import scala.collection.mutable.Map
import scales.Main
import scales.Log

object OP extends Enumeration {
  type OP = Value
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

trait Expr {
  def typecheck(typemap: Map[String, String]) : String
  def compile(state: Map[String, String]) : String
}

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

  def compile(state: Map[String, String]) : String = {
    "TODO"
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

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }

}

case class Constant(ty: String, con: String) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = ty

  def compile(state: Map[String, String]) : String = {
    "TODO"
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

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }

}

case class LetX(lets: List[Let], bod: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    var letstate = typemap.clone()
    lets.foreach{_.typecheck(letstate)}
    lets.foreach{_.load(letstate)}
    bod.typecheck(letstate)
  }

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }

}

case class Seq(bod: List[Expr]) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    (bod map {_.typecheck(typemap)}).last
  }

  def compile(state: Map[String, String]) : String = {
    "TODO"
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

  def compile(state: Map[String, String]) : String = {
    "TODO"
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

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }
}

case class Asgn(id: Expr, rval: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    val tys = List(id.typecheck(typemap), rval.typecheck(typemap))
    if (tys(0) != tys(1)) {
      Log.error("Assignment to " + tys(0) + " " + id + " of type " + tys(1))
    }
    tys(1)
  }

  def compile(state: Map[String, String]) : String = {
    "TODO"
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

  def compile(state: Map[String, String]) : String = {
    "TODO"
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

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }
}

case class ArrGet(id: Expr, ind: Expr) extends Expr {
  def typecheck(typemap: Map[String, String]) : String = {
    if (ind.typecheck(typemap) != "Int") {
      Log.error("Indexing into array with non-Int.")
    }
    "Int"
  }

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }
}

trait Callable {
  def typecheckCall(meth: Method, args: List[Expr], typemap: Map[String, String]) = {
    val vals = args map {_.typecheck(typemap)}
    if (vals.length != meth.args.length) {
      Log.error("Method call on " + meth + " has wrong number of args.")
    }
    val bindings = meth.args zip vals
    bindings.foreach{case (Attribute(n, t), v) => if (v != t) {
      Log.error("Arg of wrong type")
    }}
  }

  def compile(state: Map[String, String]) : String
}

case class MethodCall(id: Expr, args: List[Expr]) extends Expr with Callable {
  def typecheck(typemap: Map[String, String]) : String = {
    val cls = Main.prog.find({_.Name() == typemap("self")}).get
    id match {
      case Var(n) => {
        val meth = cls.getMethod(n)
        typecheckCall(meth, args, typemap)
        meth.ty
      }
      case _ => {
        Log.error("Method call on non-method " + id)
        "UCObject"
      }
    }
  }

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }
}

case class ClassCall(self: Expr, id: Expr, args: List[Expr]) extends Expr
                                                             with Callable{
  def typecheck(typemap: Map[String, String]) : String = {
    (self, id) match {
      case (Var(c), Var(m)) => {
        val clsopt = Main.prog.find({_.Name() == c})
        if (clsopt == None) {
          Log.error("class call on non-existent " + c + ".")
        }
        val meth = clsopt.get.getMethod(m)
        typecheckCall(meth, args, typemap)
        meth.ty
      }
      case _ => {
        Log.error("Inappropriate class call. " + self + " and "
                    + id + " must be ids.")
        "UCObject"
      }
    }
  }

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }
}

trait Let {
  def load(typemap: Map[String, String])
  def typecheck(typemap: Map[String, String]) : String
  def compile(state: Map[String, String]) : String
}

case class LetPln(name: String, ty: String) extends Let {
  def load(typemap: Map[String, String]) = {
    typemap(name) = ty
  }

  def typecheck(typemap: Map[String, String]) = ty

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }
}

case class LetAsgn(name: String, ty:String, body: Expr) extends Let {
  def load(typemap: Map[String, String]) = {
    typemap(name) = ty
  }

  def typecheck(typemap: Map[String, String]) = {
    val bty = body.typecheck(typemap)
    if (bty != ty) {
      Log.error("Let " + name + " is not of declared type.")
    }
    ty
  }

  def compile(state: Map[String, String]) : String = {
    "TODO"
  }
}

abstract class Feature

case class Attribute(name: String, ty: String) extends Feature
case class Method(name: String, args:List[Attribute], ty:String, body: Expr)
  extends Feature {

  var clas = ""

  def compile() = {
    printHeader()
    //TODO main has to be compiled specially.
    body.compile(Map[String,String]())
    println("  return")
    println(".end method")
  }

  def jasminArgs : String = {
    //TODO
    "()"
  }

  def jasminType : String = {
    //TODO 
    "V"
  }

  def typecheck(state: Map[String, String]) = {
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
    clas = Main.lookupClass(state("self")).get.Name()
  }

  def printHeader() = {
    //TODO Args and return type.
    println(".method public " + name + jasminArgs + jasminType)
    println("  .limit locals 32")
    println("  .limit stack 32")
  }

}


