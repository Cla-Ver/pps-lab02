package task4

enum Expr:
  case Literal(n: Int)
  case Add(e1: Expr, e2: Expr)
  case Multiply(e1: Expr, e2: Expr)


object Expr:
  def evaluate(expr: Expr): Int = expr match
    case Literal(e) => e
    case Add(e1, e2) => evaluate(e1) + evaluate(e2)
    case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

  def show(expr: Expr): String = expr match
    case Literal(e) => e.toString
    case Add(e1, e2) => "(" + show(e1) + "+" + show(e2) + ")"
    case Multiply(e1, e2) => "(" + show(e1) + "*" + show(e2) + ")"
