package it.unibo.pps.u02

object es2
  val isStringEmpty: (String => Boolean) = (s: String) => s match
    case "" => true
    case _ => false

  val neg: (String => Boolean) => (String => Boolean) = (f: String => Boolean) => f(_) match
    case true => false
    case _ => true

  val isStringFull: (String => Boolean) = neg(isStringEmpty)

  val p1: Int => Int => Int => Boolean = (x: Int) => (y: Int) => (z: Int) => x <= y & y == z
  val p2: (Int, Int, Int) => Boolean = (x: Int, y: Int, z: Int) => x <= y & y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y & y == z
  def p4(x: Int, y: Int, z:Int): Boolean = x <= y & y == z