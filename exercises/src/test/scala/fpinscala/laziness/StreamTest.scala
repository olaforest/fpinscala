package fpinscala.laziness

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class StreamTest extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks{

  "toList scenarios" - {
    "should return empty list when converting an empty stream to a list" in {
      Stream.empty[Int].toList shouldBe List.empty[Int]
      Stream.empty[Int].toListRecursiveUnsafe shouldBe List.empty[Int]
      Stream.empty[Int].toListRecursive shouldBe List.empty[Int]
    }

    "should return list of elements contained in the stream in order when converting a stream to a list" in {
      Stream("foo", "bar").toList shouldBe List("foo", "bar")
      Stream("foo", "bar").toListRecursiveUnsafe shouldBe List("foo", "bar")
      Stream("foo", "bar").toListRecursive shouldBe List("foo", "bar")
    }
  }
  "take scenarios" - {
    "should return empty stream when take input argument is smaller or equal to zero" in {
      Stream(0, 1, 2, 3, 4, 5).take(-1).toList shouldBe List()
      Stream(0, 1, 2, 3, 4, 5).take(0).toList shouldBe List()
    }

    "should return truncated stream when take input argument is greater than zero but smaller than stream size" in {
      Stream(0, 1, 2, 3, 4, 5).take(3).toList shouldBe List(0, 1, 2)
    }

    "should return original stream when take input argument is greater or equal to stream size" in {
      Stream().take(1).toList shouldBe List()
      Stream(0, 1, 2, 3, 4, 5).take(6).toList shouldBe List(0, 1, 2, 3, 4, 5)
      Stream(0, 1, 2, 3, 4, 5).take(7).toList shouldBe List(0, 1, 2, 3, 4, 5)
    }
  }

  "drop scenarios" - {
    "should return empty stream when drop input argument is greater or equal to stream size" in {
      Stream().drop(0).toList shouldBe List()
      Stream(0, 1, 2, 3, 4, 5).drop(6).toList shouldBe List()
      Stream(0, 1, 2, 3, 4, 5).drop(7).toList shouldBe List()
    }

    "should return truncated stream when drop input argument is greater than zero but smaller than stream size" in {
      Stream(0, 1, 2, 3, 4, 5).drop(3).toList shouldBe List(3, 4, 5)
    }

    "should return original stream when drop input argument is smaller or equal to zero" in {
      Stream(0, 1, 2, 3, 4, 5).drop(-1).toList shouldBe List(0, 1, 2, 3, 4, 5)
      Stream(0, 1, 2, 3, 4, 5).drop(0).toList shouldBe List(0, 1, 2, 3, 4, 5)
    }
  }

  "takeWhile scenarios" - {

    val scenarios = Table[Stream[Int] => (Int => Boolean) => Stream[Int], String](
      ("takeWhile version", "description"),
      (_.takeWhile, "takeWhile"),
      (_.takeWhileViaFoldRight, "takeWhileViaFoldRight"),
    )

    forAll(scenarios) { (takeWhileFn, desc) =>
      s"should return empty stream when predicate is false for all elements for $desc" in {
        takeWhileFn(Stream(0, 1, 2, 3, 4, 5))(_ => false).toList shouldBe List()
      }
    }

    forAll(scenarios) { (takeWhileFn, desc) =>
      s"should return truncated stream with longest prefix matching the given predicate for $desc" in {
        takeWhileFn(Stream(0, 1, 2, 3, 4, 5))(_ < 0).toList shouldBe List()
        takeWhileFn(Stream(0, 1, 2, 3, 4, 5))(_ < 1).toList shouldBe List(0)
        takeWhileFn(Stream(0, 1, 2, 3, 4, 5))(_ < 3).toList shouldBe List(0, 1, 2)
        takeWhileFn(Stream(0, 1, 2, 3, 4, 5))(_ < 5).toList shouldBe List(0, 1, 2, 3, 4)
      }
    }

    forAll(scenarios) { (takeWhileFn, desc) =>
      s"should return original stream when predicate is true for all elements for $desc" in {
        takeWhileFn(Stream[Int]())(_ => true).toList shouldBe List()
        takeWhileFn(Stream(0, 1, 2, 3, 4, 5))(_ => true).toList shouldBe List(0, 1, 2, 3, 4, 5)
      }
    }
  }

  "forAll scenarios" - {
    "should return true when stream is empty, regardless of predicate" in {
      Stream[String]().forAll(_ => true) shouldBe true
      Stream[String]().forAll(_ => false) shouldBe true
    }

    "should return false when any element in the stream does not match the predicate" in {
      Stream(0, 1, 2, 3, 4, 5).forAll(_ < 1) shouldBe false
      Stream(0, 1, 2, 3, 4, 5).forAll(_ < 3) shouldBe false
      Stream(0, 1, 2, 3, 4, 5).forAll(_ < 5) shouldBe false
    }

    "should return true when predicate matches all elements" in {
      Stream(0, 1, 2, 3, 4, 5).forAll(_ < 6) shouldBe true
    }
  }
}
