package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    var all = union(s1, union(s2, s3))
    var evens = union(Set(2), union(Set(4), union(Set(6), union(Set(8), Set(10)))))
  }

  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")

      assert(contains(s1, 7) == false, "Singleton")
      assert(contains(s2, 7) == false, "Singleton")
      assert(contains(s3, 7) == false, "Singleton")
    }
  }

  test("union contains all elements of each provided set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect of singletons should not contain others") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("intersect all with s1 should contain only '1'") {
    new TestSets {
      val s = intersect(s1, all)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff of 's1' and 's2' should contain only '1' and not '2'") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
    }
  }

  test("diff of 'all' and 's1' should not contain '1'") {
    new TestSets {
      val s = diff(all, s1)
      assert(!contains(s, 1), "Diff 1")
      assert(contains(s, 2), "Diff 2")
      assert(contains(s, 3), "Diff 3")
    }
  }

  test("set with 'all' should contain only '2' and '3' with filter") {
    new TestSets {
      val s = filter(all, x => x > 1)
      assert(!contains(s, 1), "Filter > 1")
      assert(contains(s, 2), "Filter > 1")
      assert(contains(s, 3), "Filter > 1")
    }
  }

  test("set with '1' should not contain '1' with filter ") {
    new TestSets {
      val s = filter(s1, x => x > 1)
      assert(!contains(s, 1), "Filter > 1")
    }
  }

  test("set with '2' should contain '2' with > 1 filter ") {
    new TestSets {
      val s = filter(s2, x => x > 1)
      assert(contains(s, 2), "Filter > 1")
    }
  }

  test("all sets should contain 2 and 3 with filter ") {
    new TestSets {
      val s = filter(all, x => x > 1)
      assert(contains(s, 2), "Filter > 1")
      assert(contains(s, 3), "Filter > 1")
      assert(!contains(s, 1), "Filter > 1")
    }
  }


  test("for all should return true for even number predicate") {
    new TestSets {
      val actual = forall(evens, x => x % 2 == 0)
      assert(actual == true, "All evens")
    }
  }

  test("for all should return false for a set of odd and evens") {
    new TestSets {
      val addOddOnEnd = union(evens, Set(101));
      printSet(addOddOnEnd)
      var actual = forall(addOddOnEnd, x => x % 2 == 0)

      assert(actual == false, "A Mixed set")

      val addEvenOnEnd = union(addOddOnEnd, Set(102));// to test that we are not returning the first bool result
      printSet(addEvenOnEnd)
      actual =  forall(addEvenOnEnd, x => x % 2 == 0)

      assert(actual == false, "A Mixed set")
    }
  }

  test("exists should return true for '2'") {
    new TestSets {
      printSet(evens)
      val actual = exists(evens, x => x == 2)
      assert(actual == true, "2")
    }
  }

  test("exists should return false for '200'") {
    new TestSets {
      val actual = exists(evens, x => x == 200)
      assert(actual == false, "200")
    }
  }

  test("even numbers with map applied should return set of odd numbers") {
    new TestSets {
      val odds = map(evens, x => x + 1)

      printSet(odds)
      assert(forall(odds, x => x % 2 != 0) == true)
    }
  }
}
