package scala.fp.list

import module1.list
import org.scalatest.flatspec.AnyFlatSpec

class list_test extends AnyFlatSpec {

  "check [1,2,3,4,5] mkString" should "1,2,3,4,5" in  {
    val l: list.List[Int] = list.List(1, 2, 3, 4, 5)
    assert(l.mkString() == "1,2,3,4,5")
  }

  "check [1,2,3,4,5] mkString(sep=':')" should "1:2:3:4:5" in  {
    val l: list.List[Int] = list.List(1, 2, 3, 4, 5)
    assert(l.mkString(sep = ":") == "1:2:3:4:5")
  }

  "check [] mkString.isEmpty" should "true" in  {
    val l: list.List[Int] = list.List()
    assert(l.mkString().isEmpty)
  }

  "check [1,2,3,4,5] reverse" should "[5,4,3,2,1]" in  {
    val l: list.List[Int] = list.List(1, 2, 3, 4, 5)
    assert(l.reverse == list.List(5, 4, 3, 2, 1))
  }

  "check [1] reverse" should "[1]" in  {
    val l: list.List[Int] = list.List(1)
    assert(l.reverse == list.List(1))
  }

  "check [] reverse" should "[]" in  {
    val l: list.List[Int] = list.List()
    assert(l.reverse == list.List())
  }

  "check [1,2,3,4,5] map" should "[1,4,9,16,25]" in  {
    val l: list.List[Int] = list.List(1, 2, 3, 4, 5)
    assert(l.map(x => x * x) == list.List(1, 4, 9, 16, 25))
  }

  "check [1,2,3,4,5] filter (x % 2 == 1)" should "[1,3,5]" in  {
    val l: list.List[Int] = list.List(1, 2, 3, 4, 5)
    assert(l.filter(_ % 2 == 1) == list.List(1, 3, 5))
  }

  "check [1,2,3,4,5] incList" should "[2,3,4,5,6]" in  {
    val l: list.List[Int] = list.List(1, 2, 3, 4, 5)
    assert(list.List.incList(l) == list.List(2, 3, 4, 5, 6))
  }

  "check [1,2,3,4,5] incList" should "[!1,!2,!3,!4,!5]" in  {
    val l: list.List[String] = list.List("1", "2", "3", "4", "5")
    assert(list.List.shoutString(l) == list.List("!1", "!2", "!3", "!4", "!5"))
  }
}
