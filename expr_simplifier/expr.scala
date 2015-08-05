sealed abstract class Expr
case class Mult(a:Expr, b:Expr) extends Expr
case class Add(a:Expr, b:Expr) extends Expr
case class Var(name:String) extends Expr
case class Const(c:Int) extends Expr


object ExpSimp {

  def simplifyOne(x:Expr) :Expr = x match {
      case Mult(Const(0),_)        => Const(0)
      case Mult(_,Const(0))        => Const(0)
      case Mult(Const(1),b)        => b
      case Mult(a,Const(1))        => a
      case Mult(Const(a),Const(b)) => Const(a*b)
      case Add(Const(0),b)         => b
      case Add(a,Const(0))         => a
      case Add(Const(a),Const(b))  => Const(a+b)
      case _                       => x
  }

  def simplify(x: Expr) : Expr = simplifyOne {
    x match {
      case Mult(a,b) => Mult(simplify(a), simplify(b))
      case Add(a,b)  => Add(simplify(a), simplify(b))
      case _         => x
    } 
  }

  def show(e:Expr) = e match {
     case Const(x) => print(x)
     case _        => print("Expression not simple enough!")
  }

  def main(args:Array[String]) =  {
    var e = Add(Mult(Add(Const(1),Mult(Const(0),Var("X"))),Const(3)),Const(12))
    var s = simplify(e)
    show(s)
    println("...and that's the answer.")
  }
}


