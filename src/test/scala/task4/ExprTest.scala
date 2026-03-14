package task4

import org.junit.Test
import org.junit.Assert.*
import task4.Expr.*

class ExprTest:
  val testNumber: Int = 3
  @Test def shouldEvaluateLiteral(): Unit =
    assertEquals(testNumber, evaluate(Literal(testNumber)))

  @Test def shouldEvaluateAddition(): Unit =
    val e: Expr = Literal(testNumber)
    assertEquals(testNumber + testNumber, Expr.evaluate(Add(e, e)))

  @Test def shouldEvaluateMultiplication(): Unit =
    val e: Expr = Literal(testNumber)
    assertEquals(testNumber * testNumber, Expr.evaluate(Multiply(e, e)))

  @Test def shouldEvaluateMultipleAdditions(): Unit =
    val e: Expr = Literal(testNumber)
    assertEquals(testNumber + testNumber + testNumber, evaluate(Add(e, Add(e, e))))

  @Test def shouldEvaluateMultipleMultiplications(): Unit =
    val e: Expr = Literal(testNumber)
    assertEquals(testNumber*testNumber*testNumber, Expr.evaluate(Multiply(e, Multiply(e, e))))

  @Test def shouldEvaluateAdditionAndMultiplication(): Unit =
    val e: Expr = Literal(testNumber)
    assertEquals(testNumber + testNumber * testNumber, Expr.evaluate(Add(e, Multiply(e, e))))

  @Test def shouldProperlyGenerateStringRepresentation(): Unit =
    val e: Expr = Literal(testNumber)
    assertEquals("(" + testNumber + "+"+testNumber +")", show(Add(e, e)))