package fpinscala.laziness

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StreamTest extends AnyFreeSpec with Matchers {

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

  "take while scenarios" - {
    "should return empty stream when take predicate is always false" in {
      Stream(0, 1, 2, 3, 4, 5).takeWhile(_ => false).toList shouldBe List()
    }

    "should return truncated stream with longest prefix matching the given predicate" in {
      Stream(0, 1, 2, 3, 4, 5).takeWhile(_ < 1).toList shouldBe List(0)
      Stream(0, 1, 2, 3, 4, 5).takeWhile(_ < 3).toList shouldBe List(0, 1, 2)
      Stream(0, 1, 2, 3, 4, 5).takeWhile(_ < 5).toList shouldBe List(0, 1, 2, 3, 4)
    }

    "should return original stream when predicate is always true" in {
      Stream[String]().takeWhile(_ => true).toList shouldBe List()
      Stream(0, 1, 2, 3, 4, 5).takeWhile(_ => true).toList shouldBe List(0, 1, 2, 3, 4, 5)
    }
  }
}
