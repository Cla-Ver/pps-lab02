package task5

import org.junit.*
import org.junit.Assert.*
import Optionals.*

class OptionalIntTest:
  val inc: Int => Int = value => value + 1

  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertTrue(OptionalInt.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = OptionalInt.Just(0)
    assertFalse(OptionalInt.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = OptionalInt.Just(0)
    assertEquals(0, OptionalInt.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertEquals(1, OptionalInt.orElse(empty, 1))

  /** Task 5: do test for map **/
  @Test def mapShouldWorkOnJustValues(): Unit =
    val opt: OptionalInt = OptionalInt.Just(0)
    assertEquals(OptionalInt.Just(1), OptionalInt.mapInt(opt)(inc))

  @Test def mapShouldWorkOnEmptyValues(): Unit =
    assertEquals(OptionalInt.Empty(), OptionalInt.mapInt(OptionalInt.Empty())(inc))

  @Test def filterShouldKeepValues(): Unit =
    val opt: OptionalInt = OptionalInt.Just(1)
    val f: Int => Boolean = _ > 0
    assertEquals(OptionalInt.Just(1), OptionalInt.filter(opt)(f))
    
  @Test def filterShouldDiscardFilteredValues(): Unit =
    val opt: OptionalInt = OptionalInt.Just(0)
    val f: Int => Boolean = _ > 0
    assertEquals(OptionalInt.Empty(), OptionalInt.filter(opt)(f))