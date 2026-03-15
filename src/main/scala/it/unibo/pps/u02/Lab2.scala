package it.unibo.pps.u02

import scala.annotation.tailrec

object Lab2 extends App:
  //Task part 2

  val positive: Int => String = v => v match
    case v if v >= 0 => "positive"
    case _ => "negative"

  def positiveDef(v: Int): String = v match
    case v if v >= 0 => "positive"
    case _ => "negative"

  val isStringEmpty: (String => Boolean) = s => s match
    case "" => true
    case _ => false

  val neg: (String => Boolean) => (String => Boolean) = f => f(_) match
    case true => false
    case _ => true

  def negDef(f: String => Boolean): String => Boolean = f(_) match
    case true => false
    case _ => true

  val isStringFull: (String => Boolean) = neg(isStringEmpty)

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y & y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y & y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y & y == z
  def p4(x: Int, y: Int, z:Int): Boolean = x <= y & y == z

  //Task part 3

  val compose: (f: Int => Int, g: Int => Int) => (Int => Int) = (f, g) => g(_) match
     case n => f(n)

  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case 1 => base
    case n if n > 1 => base * power(base, exponent - 1)

  def powerTail(base: Double, exponent: Int): Double =
    @tailrec
    def pow(base: Double, exponent: Int, carrier: Double): Double = exponent match
      case 0 => 1
      case 1 => carrier
      case _ => pow(base, exponent-1, carrier * base)
    pow(base, exponent, base)

  println(powerTail(3, 3))

  def reverseNumber(n: Int): Int =
    @tailrec
    def reverse(remaining: Int, carrier: Int): Int = remaining match
      case 0 => carrier
      case _ => reverse(remaining / 10, (carrier * 10) + (remaining % 10))
    reverse(n, 0)

  //Task part 4

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
